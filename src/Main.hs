module Main where

import Prelude

import Util

import Graphics.UI.GLUT

import Debug.Trace

enablePinkColor  =  color3f 0.349019607843137 0.094117647058824 0.47843137254902

enableTealColor  =  color3f 0 0.3764705882 0.3450980392

x = [7.5,16.3,0.2,8.5,16.8,18.9,7.7,11.1,9.6,10]
y = [1.1,6.4,10.9,12.5,5.9,15.0,15.5,2.1,15.5,16.8]

matrixD :: [[Double]] ->Double
-- matrixD [[a11,a12,a13],[a21,a22,a23],[a31,a32,a33]] = (a11 * (a22*a33 - a23*a32)) - (a12* (a21*a33-a23*a31)) + (a13*(a21*a32 - a22*a31))
matrixD [[a11,a12,a13]
        ,[a21,a22,a23]
        ,[a31,a32,a33]] = a11*a22*a33 + a12*a23*a31 + a21*a32*a13 - a13*a22*a31 - a21*a12*a33 - a32*a23*a11

sumN = 10

sumX = concatListToSingleValue (\a b -> a + b) x 0
sumXDual = concatListToSingleValue (\a b -> a*a + b) x 0
sumXTetra = concatListToSingleValue (\a b -> a*a*a + b) x 0
sumXQuadra = concatListToSingleValue (\a b -> a*a*a*a + b) x 0

sumY = concatListToSingleValue (\a b -> a + b) y 0
sumY_X = concatListToSingleValue (\a b -> a + b) (mergeList (\a1 b1->a1*b1) x y) 0
sumY_XDouble = concatListToSingleValue (\a b -> a + b) (mergeList (\a1 b1->a1*a1*b1) x y) 0

det = [[sumN,       sumX,       sumXDual],
       [sumX,       sumXDual,   sumXTetra],
       [sumXDual,   sumXTetra,  sumXQuadra]]

detA = [[sumY,          sumX,       sumXDual],
       [sumY_X,         sumXDual,   sumXTetra],
       [sumY_XDouble,   sumXTetra,  sumXQuadra]]

detB = [[sumN,      sumY,           sumXDual],
       [sumX,       sumY_X,         sumXTetra],
       [sumXDual,   sumY_XDouble,   sumXQuadra]]

detC = [[sumN,      sumX,       sumY],
       [sumX,       sumXDual,   sumY_X],
       [sumXDual,   sumXTetra,  sumY_XDouble]]

a = (matrixD detA) / (matrixD det)
b = (matrixD detB) / (matrixD det)
c = (matrixD detC) / (matrixD det)

al = (sumY*sumXDual - sumX*sumY_X)/(sumN*sumXDual - sumX*sumX)
bl = (sumY_X-a*sumX) / sumXDual



test1 = concatListToSingleValue  (\x y -> x + y) [1,2 .. 5] 0

sourcePoints ::[(Double,Double,Double)]
sourcePoints = zip3 x y [0,0,0,0,0,0,0,0,0,0 ]

main :: IO ()
main = do
    putStrLn (show (mergeList (\a1 b1->a1*b1) [0,1,2,3] [10,10,10,10]))
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "lab #2"
    displayCallback $= display
    mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  pointSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  pointSize $= 7
  drawSurface
  enablePinkColor
  drawGrafic
  enableTealColor
  drawLineGrafic
  renderPrimitive Points $ do
     mapM_ (\(x, y, z) -> vertex $ Vertex3 (x*step) (y*step) (z*step)) sourcePoints
  color3f 0 0 0
  flush

drawSurface = do
  color3f 0.4 0.4 0.4
  renderPrimitive Lines $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) template
  color3f 1 1 1

step = 0.05
size = 0.01

template :: [(Double,Double,Double)]
template = [(-1,0,0),(1,0,0),(0,-1,0),(0,1,0)] ++
   concat [[(k,-size,0),(k,size,0),(-size,k,0),(size,k,0)] | k<-[-1,-1 + step .. 1]]

graphic :: [(Double,Double,Double)]
graphic =  [(k*step,(c*k*k + b*k + a)*step,0) | k<-[-20, -19.9 .. 20]]

lineGraphic :: [(Double,Double,Double)]
lineGraphic =  [(k*step,(bl*k + al)*step,0) | k<-[-20, -19.9 .. 20]]


label :: Double -> Double -> [(Double,Double,Double)]
label position step = [(position,-step,0),(position,step,0),(step,position,0),(-step,position,0)]

drawGrafic = do
  renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) graphic

drawLineGrafic = do
  renderPrimitive LineStrip $ do
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z ) lineGraphic

c