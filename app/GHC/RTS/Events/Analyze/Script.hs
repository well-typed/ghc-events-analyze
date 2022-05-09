{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -w -W #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module GHC.RTS.Events.Analyze.Script (
    -- * Types
    Script
  , Title
  , EventFilter(..)
  , EventSort(..)
  , Command(..)
    -- * Script execution
  , matchesFilter
    -- * Parsing and unparsing
  , pScript
  , unparseScript
    -- * Quasi-quoting support
  , scriptQQ
  ) where

import Control.Applicative (optional)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec hiding (optional)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P

#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import GHC.RTS.Events.Analyze.Types

{-------------------------------------------------------------------------------
  Script definition
-------------------------------------------------------------------------------}

-- | A script is used to drive the construction of reports
type Script a = [Command a]

-- | Title of a section of an event
type Title = Text

-- | A positive (Include) or negative (Exclude) filter
data NameFilter a
  =
    -- | Negative filter
    -- Examples
    -- > "-finalizer.*"
    Exclude a
    -- | Positive filter
    -- Examples
    -- > "worker.*"
  | Include a
  | IncludeAll
  deriving (Show,Functor,Foldable,Traversable)

-- | Event filters
data EventFilter a =
    -- | A single event
    --
    -- Examples
    -- > GC     -- the GC event
    -- > "foo"  -- user event "foo"
    -- > 5      -- thread ID 5
    Is EventId

    -- | Any user event
    --
    -- Example
    -- > user
  | IsUser

    -- | Any thread event
    --
    -- Example
    -- > thread
    -- > thread "!finalizer"
  | IsThread (NameFilter a)

    -- | Logical or
    --
    -- Example
    -- > [GC, "foo", 5]
  | Any [EventFilter a]
  deriving (Functor, Foldable, Traversable, Show)

-- | Commands
data Command a =
    -- | Start a new section
    --
    -- Example
    -- > section "User events"
    Section Title

    -- | A single event
    --
    -- Example
    -- > "foo"  -- user event "foo"
  | One EventId     (Maybe Title)

    -- | Show all the matching events
    --
    -- Examples
    -- > user by total  -- all user events, sorted
    -- > [4, 2, 3]      -- thread events 4, 2 and 3, in that order
  | All (EventFilter a) (Maybe EventSort)

    -- | Sum over the specified events
    --
    -- Example
    -- > sum user
  | Sum (EventFilter a) (Maybe Title)
  deriving (Functor, Foldable, Traversable, Show)


{-------------------------------------------------------------------------------
  Script execution
-------------------------------------------------------------------------------}

matchesFilter :: EventFilter (ThreadId -> Bool) -> EventId -> Bool
matchesFilter (Is eid') eid = eid' == eid
matchesFilter  IsUser    eid = isUserEvent eid
matchesFilter (IsThread IncludeAll) (isThreadEvent -> Just _) = True
matchesFilter (IsThread (Include r)) (isThreadEvent -> Just tid) = r tid
matchesFilter (IsThread (Exclude r)) (isThreadEvent -> Just tid) = not(r tid)
matchesFilter IsThread{} _ = False
matchesFilter (Any fs) eid = any (`matchesFilter` eid) fs

{-------------------------------------------------------------------------------
  Lexical analysis
-------------------------------------------------------------------------------}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef {
            P.reservedNames = [
                "section"
              , "GC"
              , "user"
              , "thread"
              , "as"
              , "by"
              , "total"
              , "name"
              , "all"
              ]
           }

reserved      = P.reserved      lexer
stringLiteral = P.stringLiteral lexer
natural       = P.natural       lexer
squares       = P.squares       lexer
commaSep1     = P.commaSep1     lexer
whiteSpace    = P.whiteSpace    lexer

{-------------------------------------------------------------------------------
  Syntax analysis

  NOTE: If updating the grammar here should also update the BNF description
  in 'scriptHelp' (@--help-script@).

  NOTE: Make sure parser and unparser are consistent with each other.
-------------------------------------------------------------------------------}

type Parser a = Parsec String () a

pScript :: Parser (Script String)
pScript = whiteSpace *> many1 pCommand <* eof

pCommand :: Parser (Command String)
pCommand = (Section . T.pack <$> (reserved "section" *> stringLiteral))
       <|> (One     <$> pEventId                         <*> pTitle)
       <|> (Sum     <$> (reserved "sum" *> pEventFilter) <*> pTitle)
       <|> (All     <$> (reserved "all" *> pEventFilter) <*> pEventSort)

pEventId :: Parser EventId
pEventId =  (EventUser . T.pack <$> stringLiteral <*> pure 0 <?> "user event")
        <|> (EventThread   <$> pThreadId     <?> "thread event")
        <|> (const EventGC <$> reserved "GC")
  where
    pThreadId = fromIntegral <$> natural

pEventFilter :: Parser (EventFilter String)
pEventFilter =  (Is             <$> pEventId)
            <|> (const IsUser   <$> reserved "user")
            <|> (IsThread <$> (reserved "thread" *> pNameFilter))
            <|> (Any            <$> (squares $ commaSep1 pEventFilter))

pEventSort :: Parser (Maybe EventSort)
pEventSort = optionMaybe $ reserved "by" *> (
                     (const SortByTotal <$> reserved "total")
                 <|> (const SortByName  <$> reserved "name")
                 <|> (const SortByStart <$> reserved "start")
               )

pTitle :: Parser (Maybe Title)
pTitle = optionMaybe $ T.pack <$> (reserved "as" *> stringLiteral)

pNameFilter :: Parser (NameFilter String)
pNameFilter = f <$> optional stringLiteral where
  f Nothing = IncludeAll
  f (Just ('-' : nameRegexp)) = Exclude nameRegexp
  f (Just nameRegexp) = Include nameRegexp

{-------------------------------------------------------------------------------
  Unparsing
-------------------------------------------------------------------------------}

unparseScript :: Script String -> [String]
unparseScript = concatMap unparseCommand

unparseCommand :: Command String -> [String]
unparseCommand (Section title) = ["", "section " ++ show title]
unparseCommand (One eid title) = [unparseEventId eid ++ " " ++ unparseTitle title]
unparseCommand (Sum f title)   = ["sum " ++ unparseFilter f ++ " " ++ unparseTitle title]
unparseCommand (All f sort)    = ["all " ++ unparseFilter f ++ " " ++ unparseSort sort]

unparseEventId :: EventId -> String
unparseEventId EventGC           = "GC"
unparseEventId (EventUser e _)   = show e
unparseEventId (EventThread tid) = show tid

unparseFilter :: EventFilter String -> String
unparseFilter (Is eid) = unparseEventId eid
unparseFilter IsUser   = "user"
unparseFilter (IsThread nameFilter) = "thread" ++ unparseNameFilter nameFilter
unparseFilter (Any fs) = "[" ++ intercalate "," (map unparseFilter fs) ++ "]"

unparseNameFilter :: NameFilter String -> String
unparseNameFilter (IncludeAll) = ""
unparseNameFilter (Include s) = ' ' : s
unparseNameFilter (Exclude s) = ' ' : '-' : s

unparseSort :: Maybe EventSort -> String
unparseSort Nothing            = ""
unparseSort (Just SortByName)  = "by name"
unparseSort (Just SortByTotal) = "by total"
unparseSort (Just SortByStart) = "by start"

unparseTitle :: Maybe Title -> String
unparseTitle Nothing  = ""
unparseTitle (Just t) = "as " ++ show t

{-------------------------------------------------------------------------------
  Quasi-quoting
-------------------------------------------------------------------------------}

$(deriveLiftMany [''EventId, ''EventFilter, ''NameFilter, ''EventSort, ''Command])

scriptQQ :: QuasiQuoter
scriptQQ = QuasiQuoter {
    quoteExp  = \e -> parseScriptString "<<source>>" e >>= lift
  , quotePat  = \_ -> fail "Cannot use script as a pattern"
  , quoteType = \_ -> fail "Cannot use script as a type"
  , quoteDec  = \_ -> fail "Cannot use script as a declaration"
  }

parseScriptString :: MonadFail m => String -> String -> m (Script String)
parseScriptString source input =
  case runParser pScript () source input of
    Left  err    -> fail (show err)
    Right script -> return script
