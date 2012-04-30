{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}


--module IO.Export.SVG.RenderSVG where
module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Blaze.ByteString.Builder as B
import Data.Colour (AlphaColour,withOpacity)

import qualified Data.List as L
import qualified Data.Vector as Vec
import qualified Data.IntMap as IM
import Data.Vector (Vector)
import Data.IntMap (IntMap)

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.SVG
import Diagrams.TwoD.Text

import Hammer.Math.Vector hiding (Vector)

import Debug.Trace

sizeSpec (width, height) = case (width, height) of
  (Nothing, Nothing) -> Absolute
  (Just w, Nothing)  -> Width (fromIntegral w)
  (Nothing, Just h)  -> Height (fromIntegral h)
  (Just w, Just h)   -> Dims (fromIntegral w) (fromIntegral h)


renderPlot :: String -> Plot2D SVG -> IO ()
renderPlot fileName (Plot2D{..}) = let
  size = sizeSpec (Just $ plotWidth plotSize, Just $ plotHeight plotSize)
  build = renderDia SVG (SVGOptions fileName size) plotDia
  in BS.writeFile fileName (B.toLazyByteString build)

renderSVG :: String -> SizeSpec2D -> Diagram SVG R2 -> IO ()
renderSVG fileName sizeSpec dia = let
  build = renderDia SVG (SVGOptions fileName sizeSpec) dia
  in BS.writeFile fileName (B.toLazyByteString build)

v2r (Vec2 x y) = r2 (x,y)

v2p (Vec2 x y) = p2 (x,y)




-- ======================= LinePlot ====================================================
data LinePlot = LinePlot
                { linePlotData :: [(Double, Double)]  
                } deriving (Show)
                                 
instance PlotableData LinePlot where                                 
  data DataStyle LinePlot = LinePlotStyle
                            { lineColour :: AlphaColour Double
                            , lineWith   :: Double
                            } deriving (Show)
                                
  defDataStyle = LinePlotStyle
               { lineColour = (blue `withOpacity` 0.8)
               , lineWith   = 0.01
               } 
  
  getXRange (LinePlot{..}) = let xs = map fst linePlotData in (L.minimum xs, L.maximum xs)
  getYRange (LinePlot{..}) = let ys = map snd linePlotData in (L.minimum ys, L.maximum ys)
  
  renderData (LinePlotStyle{..}) (DataCanvas{..}) lp@(LinePlot{..}) = let
    ps = map (dataTrans . p2) linePlotData
    in strokeT (fromVertices ps)
       # translate (head ps .-. origin)
       # lcA lineColour
       # lw lineWith
     
     
-- ======================= Histogram ====================================================
data Histogram = Hist
                 { histData :: [Double]
                 , histStep :: Double
                 , startX   :: Double  
                 } deriving (Show)
                                 
instance PlotableData Histogram where                                 
  data DataStyle Histogram = HistStyle
                             { colourBin  :: AlphaColour Double
                             , countorBin :: Maybe (Double)
                             } deriving (Show)
                                
  defDataStyle = HistStyle
               { colourBin  = (blue `withOpacity` 0.8)
               , countorBin = Just 1
               } 
  
  getXRange (Hist{..}) = let final = startX + histStep * fromIntegral (length histData) in (startX, final)
  getYRange (Hist{..}) = (L.minimum histData, L.maximum histData)
  
  renderData (HistStyle{..}) (DataCanvas{..}) hist@(Hist{..}) = let
    (initial, final) = getXRange hist 
    xAxis  = [initial, initial + histStep .. final]
    bar x y
      | y == 0    = mempty
      | otherwise = let
        v = r2 (x, y/2)
        in rect histStep y
           # lc black
           # fcA colourBin
           # translate v
    in mconcat $ zipWith bar xAxis histData
     
-- ============================== Types =============================================

data PlotSize = PlotSize { plotWidth::Int, plotHeight::Int } deriving (Show)

data Plot2D a = Plot2D { plotSize :: PlotSize
                       , plotDia  :: Diagram a R2}

class (PlotableData a, PlotableAxis b)=> PlotableChart a b where
  data Chart a b
  getDefChart   :: a -> Chart a b
  renderChart   :: PlotSize -> Chart a b -> Plot2D c
  
  getDataCanvas :: PlotSize -> Chart a b -> (DataStyle a, DataCanvas, a)
  getXAxis      :: PlotSize -> Chart a b -> (AxisStyle b, AxisPath b, Axis b)
  getYAxis      :: PlotSize -> Chart a b -> (AxisStyle b, AxisPath b, Axis b)
  
  fmapData,  (##) :: Chart a b -> (DataStyle a -> DataStyle a) -> Chart a b
  fmapXAxis, (#-) :: Chart a b -> (AxisStyle b -> AxisStyle b) -> Chart a b
  fmapYAxis, (#|) :: Chart a b -> (AxisStyle b -> AxisStyle b) -> Chart a b

  (#|) = fmapYAxis
  (#-) = fmapXAxis
  (##) = fmapData
  


instance PlotableChart Histogram Double where
  data Chart Histogram Double = HistChart
                                { axisX  :: Axis Double
                                , styleX :: AxisStyle Double
                                , axisY  :: Axis Double
                                , styleY :: AxisStyle Double
                                , histChartData  :: Histogram 
                                , histChartStyle :: DataStyle Histogram }
  
  getDefChart hist = HistChart { axisX  = AutoAxis AutoTick
                               , styleX = defAxisStyle
                               , axisY  = AutoAxis AutoTick
                               , styleY = defAxisStyle
                               , histChartData = hist 
                               , histChartStyle = defDataStyle }
  
  renderChart size chart = undefined --   :: SizeSpec2D -> Chart a b -> Plot2D c
  
  -- :: Chart a b -> (DataStyle a, DataCanvas, a)
  getDataCanvas size chart = let 
    style = histChartStyle chart
    hist  = histChartData chart
    
    in undefined
    
  getXAxis size chart      = undefined -- :: Chart a b -> (AxisStyle b, TickPath b, Axis b)
   -- :: Chart a b -> (AxisStyle b, TickPath b, Axis b)
  getYAxis (PlotSize{..}) (HistChart{..}) = case axisX of
    NoAxis -> (styleY, path 0,  axisY)
    _      -> (styleY, path axisWidth,  axisY)
    where
      axisWidth = if plotHeight > plotWidth then 0.1 * fromIntegral plotWidth else 0.1 * fromIntegral plotHeight
      path base = AxisPath 
           { valueAxisRange = getYRange histChartData
           , beginOfAxis  = Vec2 axisWidth base
           , endOfAxis    = Vec2 axisWidth (fromIntegral plotHeight)
           , axisHeight   = axisWidth
           }
  
  fmapData  chart func = chart {histChartStyle = func $ histChartStyle chart}
  fmapXAxis chart func = chart {styleX = func $ styleX chart}
  fmapYAxis chart func = chart {styleY = func $ styleY chart}
     
-- ============================== CartesianPlot =============================================

data Axis a = NoAxis
          | FixedAxis (Tick a) a a
          | AutoAxis  (Tick a)
          | RatioAxis (Tick a) Double
            
data Tick a = NoTick
            | AutoTick
            | FixStepTick a a
            | FixMajorTick [a]
            
data DataCanvas = DataCanvas
                { dataOrigin :: P2
                , dataTrans  :: (P2 -> P2)
                } deriving (Show)
                                 
type AxisTransformation = Mat2

class (PlotableTick a) => PlotableAxis a where
  data AxisStyle a
  defAxisStyle :: AxisStyle a
  renderAxis   :: (Renderable (Path R2) b, Renderable Text b)=> AxisStyle a -> AxisPath a -> Axis a -> (Diagram b R2, AxisTransformation)

class PlotableTick a where
  data TickStyle a
  defTickStyle :: TickStyle a
  renderTick   :: (Renderable (Path R2) b, Renderable Text b)=> TickStyle a -> TickPath a -> Tick a -> Diagram b R2

class PlotableData a where
  data DataStyle a
  defDataStyle :: DataStyle a
  renderData   :: (Renderable (Path R2) b)=> DataStyle a -> DataCanvas -> a -> Diagram b R2
  getXRange    :: a -> (Double, Double)
  getYRange    :: a -> (Double, Double)

            
data AxisPath a = AxisPath 
                  { valueAxisRange :: (a, a)
                  , beginOfAxis    :: Vec2
                  , endOfAxis      :: Vec2
                  , axisHeight     :: Double
                  } deriving (Show)                                   


data TickPath a = TickPath 
                  { valueTickRange :: (a, a)
                  , posTickRange   :: (Vec2, Vec2)
                  , tickHeight     :: Double
                  } deriving (Show)

data TickPosition = Inside
                  | Outside
                  | Bothsides
                  deriving (Show)                             

instance PlotableTick Double where
  data TickStyle Double = TickStyle_Double
                          { fontSize          :: Int
                          , majorTickPosition :: TickPosition
                          , minorTickPosition :: TickPosition
                          , majorTickSize     :: Double
                          , minorTickSize     :: Double
                          , majorTickWidth    :: Double
                          , minorTickWidth    :: Double
                          } deriving (Show)
                            
  defTickStyle = TickStyle_Double
                 { fontSize          = 10
                 , majorTickPosition = Bothsides
                 , minorTickPosition = Bothsides
                 , majorTickSize     = 1
                 , minorTickSize     = 0.5
                 , majorTickWidth    = 1
                 , minorTickWidth    = 0.5
                 }
                 
  renderTick cfg (path@TickPath{..}) tick = 
    case tick of
      NoTick   -> mempty
      AutoTick -> let
        (_, _, pos) = autoRange valueTickRange
        in drawFx pos
      FixStepTick major minor -> undefined
      FixMajorTick pos        -> drawFx pos
      where
        sizeFactor = tickHeight
        dir    = let (posStart, posEnd) = posTickRange in posEnd &- posStart
        drawFx = mconcat . map (renderLittleTick cfg dir sizeFactor . interpolate path)

autoRange::(Num a, RealFrac a, Floating a, Enum a, Show a)=> (a, a) -> (a, a, [a])
autoRange (rangeMin, rangeMax) = let
  delta      = rangeMax - rangeMin
  step       = delta / 10
  grossOrder = fromIntegral . floor . logBase 10 $ step
  fineOrder  = fromIntegral . round $ step / (10^grossOrder) 
  finalStep  = fineOrder * (10^grossOrder)
  start      = finalStep * (fromIntegral . floor   $ rangeMin / finalStep)
  end        = finalStep * (fromIntegral . ceiling $ rangeMax / finalStep)
  pos        = [start, start + finalStep .. end]
  in trace ("autoRange: "++show(delta, step, grossOrder, fineOrder, finalStep, start, end, pos)) $ (start, end, pos)

interpolate::TickPath Double -> Double -> (Double ,P2)
interpolate path x = let
  (rangeMin, rangeMax) = valueTickRange path
  (posMin, posMax)     = posTickRange path
  deltaRange           = rangeMax - rangeMin
  deltaPos             = posMax &- posMin
  u                    = (x - rangeMin) / deltaRange
  in (x, v2p $ posMin &+ u *& deltaPos)
     
renderLittleTick:: (Renderable (Path R2) b, Renderable Text b)=> 
                   TickStyle Double -> 
                   Vec2             -> 
                   Double           ->
                   (Double, P2)     -> 
                   Diagram b R2
renderLittleTick cfg dir sizeFactor (x, pos1) = let
  tickSize = majorTickSize cfg * sizeFactor
  v    = let Vec2 x y = tickSize *& normalize dir
         in r2 (-y, x)
  tick = pos1 ~~ (pos1 .+^ v)
  
  drawText = scale (sizeFactor * 0.5) (text (show x))
             # fc black
             # translate ((1.5 *^ v ))
  
  drawTick = strokeT tick
             # fcA (green `withOpacity` 0.8)
             # lw (sizeFactor * 0.2)
             # lc green
             # translate (-0.5 *^ v)
     
  in translate (pos1 .-. origin) $ drawTick <> drawText
     
     
data AxisType = Vertical     
              | Horizontal
              deriving (Show)

data AxisEnd = ClearCut     
             | Arrow {arrowSize:: Double}
               deriving (Show)
                        
instance PlotableAxis Double where
  data AxisStyle Double = AxisStyle_Double
                          { axisType    :: AxisType
                          , axisOpenEnd :: AxisEnd 
                          , axisWidth   :: Double
                          } deriving (Show)
                            
  defAxisStyle = AxisStyle_Double
               { axisType    = Horizontal
               , axisOpenEnd = ClearCut
               , axisWidth   = 1.0
               }
                 
  renderAxis cfg path@(AxisPath{..}) axis = 
    case axis of
      NoAxis        -> let
        (newStart, newEnd, pos) = autoRange valueAxisRange
        newPath = path {valueAxisRange = (newStart, newEnd)}
        trans   = calcAxisMTrans newPath
        in (mempty, trans)
      
      AutoAxis tick -> let
        (newStart, newEnd, pos) = autoRange valueAxisRange
        newPath = path {valueAxisRange = (newStart, newEnd)}
        in draw newPath tick
      
      FixedAxis tick newStart newEnd -> let
        newPath = path {valueAxisRange = (newStart, newEnd)}
        in draw newPath tick
      
      RatioAxis tick ratio       -> undefined
      
      where
        draw path tick = let
          dataPosRange = (beginOfAxis, endOfAxis)
          diaAxis = trace ("axis") $ drawAxis beginOfAxis endOfAxis axisHeight
          diaTick = renderTick defTickStyle (TickPath valueAxisRange dataPosRange axisHeight) tick
          trans   = calcAxisMTrans path
          in (diaAxis <> diaTick, trans)


calcAxisMTrans::AxisPath Double -> Mat2
calcAxisMTrans path@(AxisPath{..}) = let
  dir   = endOfAxis &- beginOfAxis
  delta = let (posStart, posEnd) = valueAxisRange in posEnd - posStart
  linTrans@(Vec2 x y) = dir &* (1 / delta)
  nd    = normalize $ Vec2 (-y) x
  in Mat2 linTrans nd


drawAxis::(Renderable (Path R2) b)=> Vec2 -> Vec2 -> Double -> Diagram b R2
drawAxis orig final sizeFactor = let
  axis = v2p orig ~~ v2p final
  in strokeT axis
     # lw (sizeFactor * 0.2)
     # lc black
     # translate (v2r orig)
                                   

test size = let
  path1 = AxisPath { valueAxisRange = (10, 50)
                  , beginOfAxis     = Vec2 0 0 
                  , endOfAxis       = Vec2 20 0
                  , axisHeight      = size                    
                  } :: AxisPath Double
  path2 =  AxisPath { valueAxisRange = (-50.0, 0.0 )
                  , beginOfAxis      = Vec2 0 0 
                  , endOfAxis        = Vec2 0 10
                  , axisHeight       = size                 
                  } :: AxisPath Double
  -- dia = renderTick defTickStyle path AutoTick <> renderTick defTickStyle path2 AutoTick
  (dia1, m1) = renderAxis defAxisStyle path1 (AutoAxis AutoTick)
  (dia2, m2) = renderAxis defAxisStyle path2 (AutoAxis AutoTick)
  in do
    print m1
    print m2
    renderSVG "/home/edgar/Desktop/tick.svg" (sizeSpec (Just 300, Just 300)) $ dia1 <> dia2
   