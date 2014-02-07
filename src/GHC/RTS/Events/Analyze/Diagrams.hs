module GHC.RTS.Events.Analyze.Diagrams (
    diagramQuantTimes
  , blockSize
    -- * Rexports
  , renderSVG
  ) where

import Prelude hiding (filter)
import Data.Function (on)
import Data.List (sortBy)
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude hiding (Options, block, start)
import Graphics.SVGFonts.ReadFont
import qualified Data.Map as Map

import GHC.RTS.Events.Analyze.Utils
import GHC.RTS.Events.Analyze.Types

blockSize :: Double
blockSize = 10

-- Returns the diagram and it's natural size
diagramQuantTimes :: Options -> Quantized -> (SizeSpec2D, Diagram B R2)
diagramQuantTimes options@Options{..} Quantized{..} =
    go [] (Map.toList quantTimes)
  where
    go :: [(EventId, Diagram B R2)]
       -> [((EventId, Int), Double)]
       -> (SizeSpec2D, Diagram B R2)
    go acc [] =
      let sorted :: [(EventId, Diagram B R2)]
          sorted = sortDiagrams acc

          headers :: [Diagram B R2]
          headers = map (mkHeader . fst) sorted

          headerWidth :: Double
          headerWidth = maximum (map width headers) + blockSize

          padHeader :: Diagram B R2 -> Diagram B R2
          padHeader h = translateX (0.5 * blockSize) (h # alignL)
                     <> rect headerWidth blockSize # alignL

          eventDiagram :: ((EventId, Diagram B R2), Diagram B R2)
                       -> Diagram B R2
          eventDiagram ((eid, blocks), title) =
                padHeader title
            -- Add empty block at the start so that the whole thing doesn't shift up
            ||| (blocks <> background eid <> (block 0 # lw 0))

          finalDiagram :: Diagram B R2
          finalDiagram = vcat $ map eventDiagram $ zip sorted headers
      in (sizeSpec2D finalDiagram, finalDiagram)
    go acc (((eid, b), q) : qtimes) =
      go (insertWith (<>) eid (mkBlock eid b q) acc) qtimes

    mkBlock :: EventId -> Int -> Double -> Diagram B R2
    mkBlock eid b q = block b # fcA (eventColor eid `withOpacity` qOpacity q)

    -- For every event and every bucket we record the percentage of that bucket
    -- that the event was using. However, if we use this percentage directly
    -- as the opacity value for the corresponding block in the diagram then
    -- a thread that does _something_, but only a tiny amount, is
    -- indistinguishable from a thread that does nothing -- but typically we
    -- are interested in knowing that a thread does something, anything at all,
    -- rather than nothing, while the difference between using 30% and 40% is
    -- probably less important and hard to visually see anyway.
    qOpacity :: Double -> Double
    qOpacity 0 = 0
    qOpacity q = 0.1 + q * 0.9

    mkHeader :: EventId -> Diagram B R2
    mkHeader eid = stroke (textSVG' (textOpts eid)) # fc black # lc black

    textOpts :: EventId -> TextOpts
    textOpts eid = TextOpts {
                       txt        = showEventId options quantThreadInfo eid
                     , fdo        = lin
                     , mode       = INSIDE_H
                     , spacing    = KERN
                     , underline  = False
                     , textWidth  = 0 -- not important
                     , textHeight = blockSize + 2
                     }

    -- For threads we draw a background showing the thread's lifetime
    background :: EventId -> Diagram B R2
    background EventGC           = mempty
    background (EventUser _)     = mempty
    background (EventThread tid) =
      case Map.lookup tid quantThreadInfo of
        Just (start, stop, _) ->
          mconcat [ block b # fcA (black `withOpacity` 0.1)
                  | b <- [start .. stop]
                  ]
        Nothing ->
          error $ "Invalid thread ID " ++ show tid

    block :: Int -> Diagram B R2
    block i = translateX (blockSize * fromIntegral i)
            $ rect blockSize blockSize

    eventColor :: EventId -> Colour Double
    eventColor EventGC         = red
    eventColor (EventUser _)   = green
    eventColor (EventThread _) = blue

    sortDiagrams :: forall a. [(EventId, a)] -> [(EventId, a)]
    sortDiagrams as =
        case optionsFilter of
          Nothing     -> sortBy (compare `on` fst) as
          Just filter -> map findEvent filter
      where
        findEvent :: EventId -> (EventId, a)
        findEvent eid = case lookup eid as of
                          Just a  -> (eid, a)
                          Nothing -> error $ "Unknown event ID " ++ show eid
