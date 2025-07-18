{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module GHC.RTS.Events.Analyze.Reports.Timed.SVG (
    writeReport
  ) where

import Control.Lens (itoList)
import Data.List (foldl')
import Diagrams.Backend.SVG (B, renderSVG)
import Diagrams.Prelude (QDiagram, Colour, V2, N, Any, (#), (|||))
import GHC.RTS.Events (Timestamp)
import Graphics.SVGFonts.Text (TextOpts(..))
import Text.Printf (printf)
import Graphics.SVGFonts (fit_height, set_envelope)

import qualified Data.Text as T
import qualified Diagrams.Prelude           as D
import qualified Graphics.SVGFonts.Fonts    as F
import qualified Graphics.SVGFonts.Text     as F
import qualified Graphics.SVGFonts.ReadFont as F
import qualified Diagrams.TwoD.Text as TT

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Reports.Timed hiding (writeReport)

writeReport :: Options -> Quantized -> Report -> FilePath -> IO ()
writeReport options quantized report path = do
  font <- F.bit
  uncurry (renderSVG path) $ renderReport options quantized report font

type D        = QDiagram B V2 (N B) Any
type SizeSpec = D.SizeSpec V2 Double

renderReport :: Options -> Quantized -> Report -> F.PreparedFont Double -> (SizeSpec, D)
renderReport options@Options{..}
             Quantized{quantBucketSize}
             report
             font
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
    fragments = map (renderFragment options font) $ zip report (cycle allColors)

    renderSVGFragment :: Colour Double -> SVGFragment -> D
    renderSVGFragment _ (SVGSection title) =
      padHeader (2 * optionsBucketHeight) title
    renderSVGFragment bg (SVGLine header blocks) =
      -- Add empty block at the start so that the whole thing doesn't shift up
      (padHeader optionsBucketHeight (renderText header (optionsBucketHeight + 2)) ||| (blocks <> (block options 0 # D.lw D.none)))
        `D.atop`
      (D.rect lineWidth optionsBucketHeight # D.alignL # D.fc bg # D.lw D.none)
    renderSVGFragment _ SVGTimeline =
          padHeader optionsBucketHeight mempty
      ||| timeline options optionsNumBuckets quantBucketSize font

    lineWidth :: Double
    lineWidth = headerWidth + fromIntegral optionsNumBuckets * optionsBucketWidth

    padHeader :: Double -> D -> D
    padHeader height h =
         D.translateX (0.5 * optionsBucketWidth) h
      <> D.rect headerWidth height # D.alignL # D.lw D.none

    -- optimisation: find the longest text header, render
    -- it then check the rendered size and use that for
    -- width; it does not necessarily mean it's the right
    -- width to use but it's good enough considering speed
    -- trade-off
    headerWidth :: Double
    headerWidth = optionsBucketWidth + widestHeader -- extra padding

    widestHeader :: Double
    widestHeader =
      let headers = [ (header, length header) | SVGLine header _ <- fragments ]
          (maxHeader, _) = foldl' (\(s, l) (s', l') ->
                                     if l' > l then (s', l') else (s, l))
                                  ("", 0) headers
      in D.width $! mkSVGText maxHeader (optionsBucketHeight + 2) font

data SVGFragment =
    SVGTimeline
  | SVGSection D
  | SVGLine String D

renderFragment :: Options -> F.PreparedFont Double -> (ReportFragment, Colour Double) -> SVGFragment
renderFragment options@Options{..} font = go
  where
    go :: (ReportFragment, Colour Double) -> SVGFragment
    go (ReportSection title,_) = SVGSection (mkSVGText (T.unpack title) (optionsBucketHeight + 2) font)
    go (ReportLine line,c)     = uncurry SVGLine $ renderLine options c line

renderLine :: Options -> Colour Double -> ReportLine -> (String, D)
renderLine options lc line@ReportLineData{..} =
    ( T.unpack lineHeader -- renderText lineHeader (optionsBucketHeight + 2)
    , blocks lc <> bgBlocks options lineBackground
    )
  where
    blocks :: Colour Double -> D
    blocks c = mconcat . map (mkBlock $ lineColor c line)
             $ itoList lineValues

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

-- | Create a text diagram that is sized (as opposed to 'renderText').
-- The problem with this function is that it's *extremely* slow and
-- memory hungry in comparison to something simple like 'TT.text'.
-- This function should therefore be used as little as possible.
mkSVGText :: String -> Double -> F.PreparedFont Double -> D
mkSVGText str size font = textSVG # D.fc D.black # D.lc D.black # D.alignL # D.lw D.none
  where
    textSVG = F.svgText (textOpts font) str # fit_height size # set_envelope

textOpts :: F.PreparedFont Double -> TextOpts Double
textOpts font =
    TextOpts {
        textFont   = font
      , spacing    = F.KERN
      , underline  = False
      }

-- | Render text with diagram's own engine. The issue with this text
-- is that it has no size: we can not tell how wide it is. For a
-- sized-text see 'mkSVGText'.
renderText :: String -> Double -> D
renderText str size =
  TT.fontSizeL (size / 2) $ TT.alignedText 0 0.5 str

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

timeline :: Options -> Int -> Timestamp -> F.PreparedFont Double -> D
timeline Options{..} numBuckets bucketSize font =
   let timeBlocks = [ tb
                    | b <- [0 .. numBuckets - 1]
                    -- timeline block number, index within this timeline block @(0 .. optionsTickEvery - 1)@
                    , let (tb, tidx) = b `divMod` optionsTickEvery
                    -- we show the timeline block when the index is 0
                    , tidx == 0 ]

   -- memoize the rendering of the last time label: if it's the same
   -- for the next 10 displays, why render it 10 times? Text is expensive.
   in case foldl' (\acc tb -> timelineBlock acc tb) mempty timeBlocks of
     (_, _, fullDiag) -> fullDiag

  where
    timelineBlockWidth :: Double
    timelineBlockWidth = fromIntegral optionsTickEvery * optionsBucketWidth

    moveAlongTimeline :: Int -> D -> D
    moveAlongTimeline tb = D.translateX (fromIntegral tb * timelineBlockWidth)

    -- Single block on the time-line; every 5 blocks a larger line and a time
    -- label; for the remainder just a shorter line
    timelineBlock :: (String, D, D) -> Int -> (String, D, D)
    timelineBlock (lastStr, lastNumD, fullDiag) tb
      | tb `rem` 5 == 0 =
        let btime = bucketTime tb
            myNum = if lastStr == btime
                    then lastNumD
                    else mkSVGText btime optionsBucketHeight font # D.translateY (optionsBucketHeight - 2)
            myDiag = D.strokeLine bigLine # D.lw (D.local 0.5) <> myNum
        in (btime, myNum, fullDiag <> myDiag # moveAlongTimeline tb)
      | otherwise =
        let myDiag :: D
            myDiag = D.strokeLine smallLine
                     # D.lw (D.local 0.5)
                     # D.translateY 1
                     # moveAlongTimeline tb
        in (lastStr, lastNumD, fullDiag <> myDiag)

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
