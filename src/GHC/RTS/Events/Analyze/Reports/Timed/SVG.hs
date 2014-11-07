module GHC.RTS.Events.Analyze.Reports.Timed.SVG (
    writeReport
  ) where

import Data.Maybe (catMaybes)
import Data.Monoid (mempty, mconcat, (<>))
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude (Diagram, Colour, R2, SizeSpec2D, (#), (|||))
import GHC.RTS.Events (Timestamp)
import Graphics.SVGFonts.ReadFont (TextOpts(..))
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Diagrams.Prelude           as D
import qualified Graphics.SVGFonts.ReadFont as F

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Reports.Timed hiding (writeReport)

writeReport :: Options -> Quantized -> Report -> FilePath -> IO ()
writeReport options quantized report path =
  uncurry (renderSVG path) $ renderReport options quantized report

type D = Diagram B R2

renderReport :: Options -> Quantized -> Report -> (SizeSpec2D, D)
renderReport Options{optionsNumBuckets}
             Quantized{quantBucketSize}
             report = (D.sizeSpec2D rendered, rendered)
  where
    rendered :: D
    rendered = D.vcat $ map renderSVGFragment (SVGTimeline : fragments)

    fragments :: [SVGFragment]
    fragments = map renderFragment $ zip report (cycle allColors)

    renderSVGFragment :: SVGFragment -> D
    renderSVGFragment (SVGSection title) =
      padHeader (2 * blockSize) title
    renderSVGFragment (SVGLine header blocks) =
      -- Add empty block at the start so that the whole thing doesn't shift up
      padHeader blockSize header ||| (blocks <> (block 0 # D.lw D.none))
    renderSVGFragment SVGTimeline =
      padHeader blockSize mempty ||| timeline optionsNumBuckets quantBucketSize

    padHeader :: Double -> D -> D
    padHeader height h =
         D.translateX (0.5 * blockSize) h
      <> D.rect headerWidth height # D.alignL # D.lw D.none

    headerWidth :: Double
    headerWidth = blockSize -- extra padding
                + (maximum . catMaybes . map headerWidthOf $ fragments)

    headerWidthOf :: SVGFragment -> Maybe Double
    headerWidthOf (SVGLine header _) = Just (D.width header)
    headerWidthOf _                  = Nothing

data SVGFragment =
    SVGTimeline
  | SVGSection D
  | SVGLine D D

renderFragment :: (ReportFragment, Colour Double) -> SVGFragment
renderFragment (ReportSection title,_) = SVGSection (renderText title (blockSize + 2))
renderFragment (ReportLine line,c)     = uncurry SVGLine $ renderLine c line

renderLine :: Colour Double -> ReportLine -> (D, D)
renderLine lc line@ReportLineData{..} =
    ( renderText lineHeader (blockSize + 2)
    , blocks lc <> bgBlocks lineBackground
    )
  where
    blocks :: Colour Double -> D
    blocks c = mconcat . map (mkBlock $ lineColor c line)
             $ Map.toList lineValues

    mkBlock :: Colour Double -> (Int, Double) -> D
    mkBlock c (b, q) = block b # D.fcA (c `D.withOpacity` qOpacity q)

lineColor :: Colour Double -> ReportLine -> Colour Double
lineColor c = eventColor c . head . lineEventIds

eventColor :: Colour Double -> EventId -> Colour Double
eventColor _ EventGC         = D.red
eventColor c (EventUser _ _) = c
eventColor _ (EventThread _) = D.blue

bgBlocks :: Maybe (Int, Int) -> D
bgBlocks Nothing         = mempty
bgBlocks (Just (fr, to)) = mconcat [
                               block b # D.fcA (D.black `D.withOpacity` 0.1)
                             | b <- [fr .. to]
                             ]

renderText :: String -> Double -> D
renderText str size =
    D.stroke (F.textSVG' (textOpts str size)) # D.fc D.black # D.lc D.black # D.alignL # D.lw D.none


textOpts :: String -> Double -> TextOpts
textOpts str size =
    TextOpts {
        txt        = str
      , fdo        = F.lin
      , mode       = F.INSIDE_H
      , spacing    = F.KERN
      , underline  = False
      , textWidth  = 0 -- not important
      , textHeight = size
      }

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
qOpacity q = 0.3 + q * 0.7

block :: Int -> D
block i = D.translateX (blockSize * fromIntegral i)
        $ D.rect blockSize blockSize # D.lw (D.Global 0.01)

blockSize :: Double
blockSize = 10

timeline :: Int -> Timestamp -> D
timeline numBuckets bucketSize =
    mconcat [ timelineBlock b # D.translateX (fromIntegral b * blockSize)
            | b <- [0 .. numBuckets - 1]
            ]
  where
    timelineBlock b
      | b `rem`5 == 0 = D.strokeLine bigLine   # D.lw (D.Global 0.5)
                     <> (renderText (bucketTime b) 9 # D.translateY 8)
      | otherwise     = D.strokeLine smallLine # D.lw (D.Global 0.5) # D.translateY 1

    bucketTime :: Int -> String
    bucketTime b = let timeNs :: Timestamp
                       timeNs = fromIntegral b * bucketSize

                       timeMS :: Double
                       timeMS = fromIntegral timeNs / 1000000
                   in printf "%0.1fms" timeMS

    bigLine   = mkLine [(0, 4), (blockSize, 0)]
    smallLine = mkLine [(0, 3), (blockSize, 0)]
    mkLine    = D.fromSegments . map (D.straight . D.r2)

-- copied straight out of export list for Data.Colour.Names
allColors :: (Ord a, Floating a) => [Colour a]
allColors =
  [D.blueviolet
  ,D.brown
  ,D.cadetblue
  ,D.coral
  ,D.cornflowerblue
  ,D.crimson
  ,D.cyan
  ,D.darkcyan
  ,D.darkgoldenrod
  ,D.darkgreen
  ,D.darkorange
  ,D.goldenrod
  ,D.green
  ]
