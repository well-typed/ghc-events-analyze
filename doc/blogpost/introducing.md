Introducing ghc-events-analyze 
==============================

`ghc-events-analyze` is an alternative visualization tool for eventlogs. It
shows CPU activity across all your threads, and supports visualizing arbitrary
user events. It is very useful for profiling code when ghc's own profiler is
not available, or when you need to trace profiling information over time (as
opposed to totals).

Motivating Example
------------------

Suppose we want to understand the runtime performance of the following simple
multi-threaded application:

    import Control.Concurrent (threadDelay)
    import Control.Concurrent.Async (async, wait)
    
    -- Intentionally slow fib
    fib :: Integer -> Integer
    fib 0 = 1
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)
    
    printFib :: Integer -> IO ()
    printFib n = print (fib n)
    
    blips :: IO ()
    blips = do
      putStrLn "BLIP"
      threadDelay 5000000
      putStrLn "BLIP"
    
    main :: IO ()
    main = do
      a1 <- async $ mapM_ printFib [30, 32 .. 38]
      a2 <- async $ mapM_ printFib [31, 33 .. 39]
      threadDelay 5000000
      a3 <- async $ blips
      mapM_ wait [a1, a2, a3]

We can compile this application and run it on two cores, and ask it to produce
an eventlog:

    ghc ex0 -eventlog
    ex0 +RTS -l

But when we open this eventlog in 
[threadscope](http://hackage.haskell.org/package/threadscope) the result is not
particularly enlightning:

![threadscope](ex0-NT-threadscope.png)

The program was compiled without the `-threaded` flag, forcing all work to run
on a single HEC (Haskell Execution Context -- roughly, a CPU core). This makes
it impossible to see the distribution of workload across the various
application threads.  This will be true whenever multiple threads are executed
by a single HEC (i.e., almost always). If we run the same eventlog through
`ghc-events-analyze` instead we get

![ghc-events-analyze, no instrumentation](ex0-NT.timed.png)

Some points to note:

1. `ghc-events-analyze` applies quantization; the total execution time is divided up
   into _n_ buckets (by default 100; for these examples we chose 50) and
   computes for each bucket and each thread what percentage of that bucket 
   the thread was active. 
2. This percentage is used to color each block in the diagram; darker means a
   larger percentage. If the thread was not active at all the block is grey,
   but a percentage _q_ other than 0 is shown as a darkness 0.1 + 0.9 * _q_.
   This means that we can visually see when a thread does anything at all;
   for instance, it is immediately clear from the diagram when the `blips`
   thread (with ID 4) is doing something. If we used the percentage _q_ directly
   as darkness then a thread doing nothing would be visually indistinguishable
   from a thread doing just a print, say.
3. We can see that initially both threads are equally busy (the scheduler is
   assigning approximately 48% CPU time to both), until the first thread
   completes and the second thread gets 97% of CPU (`ghc-events-analyze` also
   generates the same report in textual form with precise values for each
   block). 
4. The thread lifetime of each thread is also immediately clear.

Instrumentation
---------------

If we instrument our code, we can improve this diagram in a number of ways. We
can use `labelThread` from `GHC.Conc` to give our threads names, so that it
becomes easier to see what's what. Moreover, `ghc-events-analyze` supports
user events. To use these user events mark the start of a user event with

    traceEventIO "START <eventName>"

and the end with

    traceEventIO "STOP <eventName>"

Note that these user events are completely independent of threads; they can
overlap each other, span multiple threads, etc. Here's our example application
again, but with some instrumentation added:

    import Control.Concurrent (myThreadId, threadDelay)
    import Control.Concurrent.Async (Async, async, wait)
    import Control.Exception (bracket_)
    import Debug.Trace (traceEventIO)
    import GHC.Conc (labelThread)
    
    event :: String -> IO a -> IO a
    event label =
      bracket_ (traceEventIO $ "START " ++ label)
               (traceEventIO $ "STOP "  ++ label)
    
    async' :: String -> IO a -> IO (Async a)
    async' label act = async $ do
      tid <- myThreadId
      labelThread tid label
      act
    
    -- Intentionally slow fib
    fib :: Integer -> Integer
    fib 0 = 1
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)
    
    printFib :: Integer -> IO ()
    printFib n = event ("fib" ++ show n) $ print (fib n)
    
    blips :: IO ()
    blips = do
      putStrLn "BLIP"
      threadDelay 5000000
      putStrLn "BLIP"
    
    main :: IO ()
    main = do
      a1 <- async' "events" $ mapM_ printFib [30, 32 .. 38]
      a2 <- async' "odds"   $ mapM_ printFib [31, 33 .. 39]
      threadDelay 5000000
      a3 <- async' "blips"  $ blips
      mapM_ wait [a1, a2, a3]

Running `ghc-events-analyze` over the eventlog generated by this code yields

![ghc-events-analyze with instrumentation, not threaded](ex1-NT.timed.png)

If we run the same code using the threaded runtime (but still on a single
core), we get

![ghc-events-analyze with instrumentation, one core](ex1-N1.timed.png)

and if we run it on two cores

![ghc-events-analyze with instrumentation, two cores](ex1-N2.timed.png)

We can see that the `evens` and `odds` threads are now in fact running in
parallel, and that the computation of `fib 38` is finished well before the
computation of `fib 39`. 

Totals
------

Bear in mind, however, that `ghc-events-analyze` divides the total time up
into _n_ buckets, so what you can _not_ see from these last two diagrams is
that the total time taken is less when running on two cores. 

`ghc-events-analyze` also outputs some totals. For the single core case it
tells us

    GC               1343672000ns    1.344s
    
    USER EVENTS (user events are corrected for GC)
    fib39           24480557000ns   24.481s
    fib38           21493145000ns   21.493s
    fib37           12702151000ns   12.702s
    fib36            7823058000ns    7.823s
    fib35            4797324000ns    4.797s
    fib34            2966990000ns    2.967s
    fib33            1800136000ns    1.800s
    fib32            1097888000ns    1.098s
    fib31             663900000ns    0.664s
    fib30             419270000ns    0.419s
    TOTAL           78244419000ns   78.244s
    
    THREAD EVENTS
    1                    138000ns    0.000s
    IOManager (2)        296000ns    0.000s
    3                    106000ns    0.000s
    evens (4)       16826523000ns   16.827s
    odds (5)        27488818000ns   27.489s
    blips (6)             63000ns    0.000s
    7                     27000ns    0.000s
    TOTAL           44315971000ns   44.316s

and for the two cores case

    GC               1171012000ns    1.171s
    
    USER EVENTS (user events are corrected for GC)
    fib39           18769541000ns   18.770s
    fib38           12009913000ns   12.010s
    fib37            7515686000ns    7.516s
    fib36            4692912000ns    4.693s
    fib35            2852639000ns    2.853s
    fib34            1774758000ns    1.775s
    fib33            1095500000ns    1.096s
    fib32             674125000ns    0.674s
    fib31             395699000ns    0.396s
    fib30             240785000ns    0.241s
    TOTAL           50021558000ns   50.022s
    
    THREAD EVENTS
    1                    138000ns    0.000s
    IOManager (2)        269000ns    0.000s
    3                     88000ns    0.000s
    evens (4)       19338615000ns   19.339s
    odds (5)        30086294000ns   30.086s
    blips (6)             73000ns    0.000s
    7                      9000ns    0.000s
    TOTAL           49425486000ns   49.425s

Some notes:

1. The total amount of time for each user event is _less_ in the two core case,
   because in the single core case those user events find themselves "waiting"
   for other threads.
2. However, the total time across all _threads_ is approximately much the same
   in both cases; we are still doing the same amount of work, it's just that 
   in the two core case the work of some of those threads is overlapped.
3. In fact, for this particular example (with the user events created as we
   have) we can see from the totals that in the single core case we are doing a
   lot of waiting (taking 78.2s seconds to do 44.3 seconds worth of work),
   while we are doing almost no waiting at all in the two core case (taking
   50.0 seconds to do 49.4 seconds worth of work).  
4. `ghc-events-analyze` corrects user events for garbage collection;
   basically, it simulates that all user events are stopped when garbage
   collection starts, and are resumed again when garbage collection finishes.
   This matches the stop-the-world nature of ghc's garbage collector, and means
   that you can more meaningfully compare total time for different user events.    

Real World Application 1
------------------------

Well-Typed have been developing a server application for a particular client.
The client reported that after certain kinds of requests the server had
unexpected spikes in CPU usage. For technical reasons we could not compile the
server application with profiling enabled, and hence profiling information was
not available. Moreover, profiling would only give us totals, not CPU usage
over time. However, we could generate an eventlog; visualizing the eventlog
with threadscope yielded 

![server threadscope](server-I0.3-threadscope.png)

We can make certain educated guesses from this picture: the spikes in activity
are probably different requests coming in at the server, and the reported
unexpected CPU usage reported by the client might be related to garbage
collection (the orange blobs that threadscope reports). However, instrumenting
the code (by labelling some threads and adding user events that correspond to
the server handling different kinds of user requests) and then running it
through `ghc-events-analyze` yielded a more informative picture:

![server with -I0.3](server-I0.3.timed.png)

(This uses `ghc-events-analyze`'s `--filter` option to only show certain
events/threads.) The user events clearly show when the server is handing
requests of type A and B, and we see corresponding spikes in CPU activity in
the server's main thread (with ID 6). Threads 4 and 5 handle communication
between the client and server, and we see "blips" at the start and end of each
request, as expected. 

The garbage collection during the A requests is expected, both because of
domain specific knowledge about what type A requests are, but also from the
diagram: there are spikes in CPU usage of the server's Main thread. However,
garbage collection during the B requests is _not_ expected: again, both from
domain specific knowledge about type B rqeuests, but also from the diagram:
there is barely any activity in the system at all, so why so much garbage
collection?

This lead us to suspect "idle GC cycles". The ghc garbage collector will run in
two cases: (1) when required, and (2) when the system is not doing anything
useful anyway. The latter are known as idle GC cycles, and when running an
application you can finetune how often these idle cycles happen. The default is
every 0.3 seconds (provided that there was _some_ activity in the system), but
by specifiying `+RTS -Ix -RTS` you can specify that it should run every _x_
seconds instead. Indeed, running the server with a much lower rate of idle GC
cycles yielded this picture:

![server with -I10](server-I10.timed.png)

Note how we no longer see any garbage collection during B requests; we still
get garbage collection during A requests, but that is expected. Moreover, we
don't see any garbage collection _after_ the second B request either. This was
due to a new thread (199) that was spawned by the second B request. Although
this thread is not very active (low CPU activity), it does just enough to
justify more idle GC runs, even though those idle GC runs are pretty much
wasted effort. 

Real World Application 2
------------------------

Well-Typed was asked to improve the performance of a particular application.
From a high level understanding of the code it was clear that this application
was centered around a single function _foo_, and that the performance of _foo_
might vary dramatically for different inputs. We therefore needed to optimize
_foo_ in different ways given different inputs, but also measure the
performance of _foo_ differently for such different inputs.

The latter fact made it difficult to use ghc's standard profiling tools, and
modifiying the application so that different ways of invoking the function
would be bound to different names (so that we could attach different cost
centres to them) was much too laborious. Moreover, enabling profiling was not
an option for two other reasons. First, this application was very performance
critical, with some parts heavily optimized already, and the instrumentation
added by using a profiling build would skew the results too much. Second, the
program was slow as it is; enabling profiling would make running the program
too slow to work with. 

However, the overhead added by enabling the eventlog is negligible. Moreover,
we can easily use `ghc-events-analyze`'s user events to generate different
kinds of events for different kinds of inputs (like we did in the fib example,
above).  The totals reported by `ghc-events-analyze` enabled us to easily see
what kinds of inputs _foo_ needed to be optimized for the most, and to guide
our improvements (like normal profiling would). 

    GC    25421789435ns   25.422s
    foo   53959674392ns   53.960s
    
    DETAILED BREAKDOWN
    A      3939896208ns    3.940s
    B      1709562291ns    1.710s
    C      2198859200ns    2.199s
    D      4389066320ns    4.389s
    E      1385853161ns    1.386s
    F      5776369439ns    5.776s
    G       462912372ns    0.463s
    H      1128639979ns    1.129s
    I       691345817ns    0.691s
    J       411810877ns    0.412s
    K      3135897321ns    3.136s
    L       810106269ns    0.810s
    M       860537704ns    0.861s
    N       499041244ns    0.499s
    O      2586121945ns    2.586s
    P      1383600793ns    1.384s
    Q      2706127000ns    2.706s
    R      1791326834ns    1.791s
    S      1165241932ns    1.165s
    T     10574202528ns   10.574s
    U       595493497ns    0.595s
    V      1727701880ns    1.728s
    W      2295049375ns    2.295s
    X      1734910406ns    1.735s

_A_, _B_, etc. are the different kinds of events, corresponding to different
ways of invoking _foo_. TODO: describe what kinds of conclusions we can draw
from this.

In this case the visualization of CPU usage over
time didn't tell us much extra:

![foo](foo.timed.png)

except that `foo` is indeed busy very consistently after an initial setup (at
about 60% of CPU, with the garbage collector running at about 25% CPU).

Availability
------------

`ghc-events-analyze` is available from
[Hackage](http://hackage.haskell.org/package/ghc-events-analyze); the source
code is available from [github](http://github.com/edsko/ghc-events-analyze).
Patches for bug fixes or feature enhancements are welcome!
