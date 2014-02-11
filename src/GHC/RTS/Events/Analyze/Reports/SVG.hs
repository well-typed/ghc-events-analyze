module GHC.RTS.Events.Analyze.Reports.SVG (
    reportSVG
    -- * Rexports
  , renderSVG
  ) where

import Prelude hiding (lines)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid (mempty, mconcat, (<>))
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude (Diagram, Colour, R2, SizeSpec2D, (#), (|||))
import Graphics.SVGFonts.ReadFont
import qualified Data.Map as Map
import qualified Diagrams.Prelude as D

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Types
-- import GHC.RTS.Events.Analyze.Utils

type D = Diagram B R2

data SVGLine =
    SVGBlocks D D
  | SVGSection D

data AnnotatedQs = AnnQs {
    annQsHeader     :: String
  , annQsColour     :: Colour Double
  , annQsBackground :: Maybe (Int, Int)
  , annQsValues     :: Map Int Double
  }

reportSVG :: EventAnalysis -> Quantized -> Script -> (SizeSpec2D, D)
reportSVG analysis Quantized{..} script =
    let lines = concatMap go script
        final = renderLines lines
    in (D.sizeSpec2D final, final)
  where
    go :: Command -> [SVGLine]
    go (Section title) =
      [SVGSection $ renderText title]
    go (One eid title) =
      [svgBlocks $ annQs title (eid, quantTimesForEvent eid)]
    go (All f sort) =
      map (svgBlocks . annQs Nothing) (sorted sort $ filtered f)
    go (Sum f title) =
      [svgBlocks $ sumAnnQs title $ map (annQs Nothing) (filtered f)]

    svgBlocks :: AnnotatedQs -> SVGLine
    svgBlocks AnnQs{..} =
      let blocks = mconcat . map (mkBlock annQsColour) $ Map.toList annQsValues
      in SVGBlocks (renderText annQsHeader) (blocks <> bgBlocks annQsBackground)

    bgBlocks :: Maybe (Int, Int) -> D
    bgBlocks Nothing         = mempty
    bgBlocks (Just (fr, to)) = mconcat [
                                   block b # D.fcA (D.black `D.withOpacity` 0.1)
                                 | b <- [fr .. to]
                                 ]

    mkBlock :: Colour Double -> (Int, Double) -> Diagram B R2
    mkBlock c (b, q) = block b # D.fcA (c `D.withOpacity` qOpacity q)

    annQs :: Maybe Title -> (EventId, Map Int Double) -> AnnotatedQs
    annQs title (eid, qs) = AnnQs {
        annQsHeader     = showTitle (showEventId quantThreadInfo eid) title
      , annQsColour     = eventColor eid
      , annQsBackground = background eid
      , annQsValues     = qs
      }

    -- For threads we draw a background showing the thread's lifetime
    background :: EventId -> Maybe (Int, Int)
    background EventGC           = Nothing
    background (EventUser _)     = Nothing
    background (EventThread tid) =
      case Map.lookup tid quantThreadInfo of
        Just (start, stop, _) -> Just (start, stop)
        Nothing               -> error $ "Invalid thread ID " ++ show tid

    quantTimesForEvent :: EventId -> Map Int Double
    quantTimesForEvent eid =
      case Map.lookup eid quantTimes of
        Nothing    -> error $ "Invalid event ID " ++ show eid ++ ". "
                           ++ "Valid IDs are " ++ show (Map.keys quantTimes)
        Just times -> times

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter -> [(EventId, Map Int Double)]
    filtered f = filter (matchesFilter f . fst) (Map.toList quantTimes)

eventColor :: EventId -> Colour Double
eventColor EventGC         = D.red
eventColor (EventUser _)   = D.green
eventColor (EventThread _) = D.blue

-- | Translate quantized value to opacity
--
-- For every event and every bucket we record the percentage of that bucket
-- that the event was using. However, if we use this percentage directly as the
-- opacity value for the corresponding block in the diagram then a thread that
-- does _something_, but only a tiny amount, is indistinguishable from a thread
-- that does nothing -- but typically we are interested in knowing that a
-- thread does something, anything at all, rather than nothing, while the
-- difference between using 30% and 40% is probably less important and hard to
-- visually see anyway.
qOpacity :: Double -> Double
qOpacity 0 = 0
qOpacity q = 0.1 + q * 0.9

showTitle :: String -> Maybe Title -> String
showTitle _   (Just title) = title
showTitle def Nothing      = def

renderLines :: [SVGLine] -> D
renderLines lines = D.vcat $ map renderLine lines
  where
    renderLine :: SVGLine -> D
    renderLine (SVGBlocks header blocks) =
      -- Add empty block at the start so that the whole thing doesn't shift up
      padHeader blockSize header ||| (blocks <> (block 0 # D.lw 0))
    renderLine (SVGSection title) =
      padHeader (2 * blockSize) title

    padHeader :: Double -> D -> D
    padHeader height h =
         D.translateX (0.5 * blockSize) h
      <> D.rect headerWidth height # D.alignL

    headerWidth :: Double
    headerWidth = blockSize + (maximum . catMaybes . map headerWidthFor $ lines)

    headerWidthFor :: SVGLine -> Maybe Double
    headerWidthFor (SVGBlocks header _) = Just (D.width header)
    headerWidthFor _                    = Nothing

renderText :: String -> D
renderText str =
    D.stroke (textSVG' (textOpts str)) # D.fc D.black # D.lc D.black # D.alignL

textOpts :: String -> TextOpts
textOpts str =
    TextOpts {
        txt        = str
      , fdo        = lin
      , mode       = INSIDE_H
      , spacing    = KERN
      , underline  = False
      , textWidth  = 0 -- not important
      , textHeight = blockSize + 2
      }

block :: Int -> D
block i = D.translateX (blockSize * fromIntegral i)
        $ D.rect blockSize blockSize

blockSize :: Double
blockSize = 10

sumAnnQs :: Maybe Title -> [AnnotatedQs] -> AnnotatedQs
sumAnnQs title qs = AnnQs {
      annQsHeader     = showTitle "TOTAL" title
    , annQsColour     = annQsColour (head qs)
    , annQsBackground = foldr1 combineBG $ map annQsBackground qs
    , annQsValues     = Map.unionsWith (+) $ map annQsValues qs
    }
  where
    combineBG :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
    combineBG (Just (fr, to)) (Just (fr', to')) = Just (min fr fr', max to to')
    combineBG _ _ = Nothing


