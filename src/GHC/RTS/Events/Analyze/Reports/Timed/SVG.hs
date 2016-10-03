{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
module GHC.RTS.Events.Analyze.Reports.Timed.SVG (
    writeReport
  ) where

import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude (QDiagram, Colour, V2, N, Any, (#), (|||))
import GHC.RTS.Events (Timestamp)
import Graphics.SVGFonts.Text (TextOpts(..))
import Text.Printf (printf)
import qualified Data.Map                   as Map
import qualified Diagrams.Prelude           as D
import qualified Graphics.SVGFonts.Fonts    as F
import qualified Graphics.SVGFonts.Text     as F

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty, mconcat)
#endif

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Reports.Timed hiding (writeReport)

writeReport :: Options -> Quantized -> Report -> FilePath -> IO ()
writeReport options quantized report path =
  uncurry (renderSVG path) $ renderReport options quantized report

type D        = QDiagram B V2 (N B) Any
type SizeSpec = D.SizeSpec V2 Double

renderReport :: Options -> Quantized -> Report -> (SizeSpec, D)
renderReport options@Options{..}
             Quantized{quantBucketSize}
             report
           = (sizeSpec, rendered)
  where
    sizeSpec = let w = Just $ D.width  rendered
                   h = Just $ D.height rendered
               in D.mkSizeSpec2D w h

    rendered :: D
    rendered = D.vcat $ map (uncurry renderSVGFragment)
                      $ zip (cycle [D.white, D.ghostwhite])
                            (SVGTimeline : fragments)

    fragments :: [SVGFragment]
    fragments = map (renderFragment options) $ zip report (cycle allColors)

    renderSVGFragment :: Colour Double -> SVGFragment -> D
    renderSVGFragment _ (SVGSection title) =
      padHeader (2 * optionsBucketHeight) title
    renderSVGFragment bg (SVGLine header blocks) =
      -- Add empty block at the start so that the whole thing doesn't shift up
      (padHeader optionsBucketHeight header ||| (blocks <> (block options 0 # D.lw D.none)))
        `D.atop`
      (D.rect lineWidth optionsBucketHeight # D.alignL # D.fc bg # D.lw D.none)
    renderSVGFragment _ SVGTimeline =
          padHeader optionsBucketHeight mempty
      ||| timeline options optionsNumBuckets quantBucketSize

    lineWidth :: Double
    lineWidth = headerWidth + fromIntegral optionsNumBuckets * optionsBucketWidth

    padHeader :: Double -> D -> D
    padHeader height h =
         D.translateX (0.5 * optionsBucketWidth) h
      <> D.rect headerWidth height # D.alignL # D.lw D.none

    headerWidth :: Double
    headerWidth = optionsBucketWidth -- extra padding
                + (maximum . catMaybes . map headerWidthOf $ fragments)

    headerWidthOf :: SVGFragment -> Maybe Double
    headerWidthOf (SVGLine header _) = Just (D.width header)
    headerWidthOf _                  = Nothing

data SVGFragment =
    SVGTimeline
  | SVGSection D
  | SVGLine D D

renderFragment :: Options -> (ReportFragment, Colour Double) -> SVGFragment
renderFragment options@Options{..} = go
  where
    go :: (ReportFragment, Colour Double) -> SVGFragment
    go (ReportSection title,_) = SVGSection (renderText title (optionsBucketHeight + 2))
    go (ReportLine line,c)     = uncurry SVGLine $ renderLine options c line

renderLine :: Options -> Colour Double -> ReportLine -> (D, D)
renderLine options@Options{..} lc line@ReportLineData{..} =
    ( renderText lineHeader (optionsBucketHeight + 2)
    , blocks lc <> bgBlocks options lineBackground
    )
  where
    blocks :: Colour Double -> D
    blocks c = mconcat . map (mkBlock $ lineColor c line)
             $ Map.toList lineValues

    mkBlock :: Colour Double -> (Int, Double) -> D
    mkBlock c (b, q) = block options b # D.fcA (c `D.withOpacity` qOpacity q)

lineColor :: Colour Double -> ReportLine -> Colour Double
lineColor c = eventColor c . head . lineEventIds

eventColor :: Colour Double -> EventId -> Colour Double
eventColor _ EventGC         = D.red
eventColor c (EventUser _ _) = c
eventColor _ (EventThread _) = D.blue

bgBlocks :: Options -> Maybe (Int, Int) -> D
bgBlocks options = go
  where
    go :: Maybe (Int, Int) -> D
    go Nothing         = mempty
    go (Just (fr, to)) = mconcat [
        block options b # D.fcA (D.black `D.withOpacity` 0.1)
      | b <- [fr .. to]
      ]

renderText :: String -> Double -> D
renderText str size =
    D.stroke textSVG # D.fc D.black # D.lc D.black # D.alignL # D.lw D.none
  where
    textSVG = F.textSVG' (textOpts size) str

textOpts :: Double -> TextOpts Double
textOpts size =
    TextOpts {
        textFont   = F.lin
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

block :: Options -> Int -> D
block Options{..} i =
      D.translateX (optionsBucketWidth * fromIntegral i)
    $ D.rect optionsBucketWidth optionsBucketHeight # D.lw borderWidth
  where
    borderWidth | optionsBorderWidth == 0 = D.none
                | otherwise               = D.global optionsBorderWidth

timeline :: Options -> Int -> Timestamp -> D
timeline Options{..} numBuckets bucketSize =
    mconcat [ timelineBlock b # D.translateX (fromIntegral tb * timelineBlockWidth)
            | -- bucket number
              b <- [0 .. numBuckets - 1]
              -- timeline block number, index within this timeline block @(0 .. optionsTickEvery - 1)@
            , let (tb, tidx) = b `divMod` optionsTickEvery
              -- we show the timeline block when the index is 0
            , tidx == 0
            ]
  where
    timelineBlockWidth :: Double
    timelineBlockWidth = fromIntegral optionsTickEvery * optionsBucketWidth

    -- Single block on the time-line; every 5 blocks a larger line and a time
    -- label; for the remainder just a shorter line
    timelineBlock :: Int -> D
    timelineBlock tb
      | tb `rem` 5 == 0
          = D.strokeLine bigLine   # D.lw (D.local 0.5)
         <> (renderText (bucketTime tb) optionsBucketHeight # D.translateY (optionsBucketHeight - 2))
      | otherwise
          = D.strokeLine smallLine # D.lw (D.local 0.5) # D.translateY 1

    bucketTime :: Int -> String
    bucketTime tb = case optionsGranularity of
                     TimelineMilliseconds -> printf "%0.1fms" timeMS
                     TimelineSeconds      -> printf "%0.1fs"  timeS
      where
        timeNs :: Timestamp
        timeNs = fromIntegral (tb * optionsTickEvery) * bucketSize

        timeS :: Double
        timeS = fromIntegral timeNs / 1000000000

        timeMS :: Double
        timeMS = fromIntegral timeNs / 1000000

    bigLine   = mkLine [(0, 4), (timelineBlockWidth, 0)]
    smallLine = mkLine [(0, 3), (timelineBlockWidth, 0)]
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
