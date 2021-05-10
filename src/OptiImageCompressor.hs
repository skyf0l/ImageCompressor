module OptiImageCompressor
  ( optiImageCompressor,
  )
where

import System.Random (StdGen (..), randomR)
import Types
  ( Cluster (..),
    Color (..),
    CompressedImage (..),
    CompressionType (..),
    OptiCluster (..),
    Pixel (..),
    Pixels (..),
    Pos (..),
    genEmptyColor,
    genOptiClusterFromColor,
    optiClustersToClusters,
  )

getDistBetweenColors :: Color -> Color -> Int
getDistBetweenColors (Color rP gP bP) (Color rC gC bC) =
  (r * r) + (g * g) + (b * b)
  where
    r = rC - rP
    g = gC - gP
    b = bC - bP

getDistFromPixelToCluster :: Pixel -> OptiCluster -> Int
getDistFromPixelToCluster (Pixel _ pixCol) (OptiCluster clustCol _ _ _) =
  pixCol `getDistBetweenColors` clustCol

makeCluster :: StdGen -> Int -> [Pixel] -> (OptiCluster, StdGen)
makeCluster gen nbPixels pixels = (cluster, nextGen)
  where
    (randomPixelID, nextGen) = randomR (0, nbPixels - 1) gen
    randomPixel = pixels !! randomPixelID
    clusterColor = pixelColor randomPixel
    cluster = genOptiClusterFromColor clusterColor

isColorAlreadyInClusters :: Color -> [OptiCluster] -> Bool
isColorAlreadyInClusters _ [] = False
isColorAlreadyInClusters color (cluster : clusters)
  | color == optiClusterColor cluster = True
  | otherwise = isColorAlreadyInClusters color clusters

makeUniqueCluster :: StdGen -> Int -> [Pixel] -> [OptiCluster]
  -> (OptiCluster, StdGen)
makeUniqueCluster gen nbPixels pixels clusters
  | not (isColorAlreadyInClusters clusterColor clusters) = (cluster, nextGen)
  | otherwise = makeUniqueCluster nextGen nbPixels pixels clusters
  where
    (cluster, nextGen) = makeCluster gen nbPixels pixels
    clusterColor = optiClusterColor cluster

subMakeClusters :: Int -> StdGen -> Int -> [Pixel] -> ([OptiCluster], StdGen)
subMakeClusters 0 gen _ _ = ([], gen)
subMakeClusters k gen nbPixels pixels = (cluster : clusters, nextGen')
  where
    (clusters, nextGen) = subMakeClusters (k - 1) gen nbPixels pixels
    (cluster, nextGen') = makeUniqueCluster nextGen nbPixels pixels clusters

makeClusters :: Int -> StdGen -> [Pixel] -> ([OptiCluster], StdGen)
makeClusters k gen pixels = subMakeClusters k gen nbPixels pixels
  where
    nbPixels = length pixels

getDistsFromPixelToClusters :: Pixel -> [OptiCluster] -> [Int]
getDistsFromPixelToClusters _ [] = []
getDistsFromPixelToClusters pixel (cluster : clusters) =
  currentDist : nextDists
  where
    currentDist = getDistFromPixelToCluster pixel cluster
    nextDists = getDistsFromPixelToClusters pixel clusters

-- [tab] -> (minPos, minValue)
-- [1, 2, 5] -> (0, 1)
-- [10, 2, 5] -> (1, 2)
getMinPos :: [Int] -> (Int, Int)
getMinPos [x] = (0, x)
getMinPos [x, y]
  | x > y = (1, y)
  | otherwise = (0, x)
getMinPos (x : dists)
  | x < nextMinValue = (0, x)
  | otherwise = (nextMinPos + 1, nextMinValue)
  where
    (nextMinPos, nextMinValue) = getMinPos dists

putPixelInCluster :: Bool -> Pixel -> OptiCluster -> OptiCluster
putPixelInCluster pushPixls pixel (OptiCluster color pixels colorSum nbPixels)
  = cluster'
  where
    pixels'
      | pushPixls = pixel : pixels
      | otherwise = pixels
    colorSum' = colorSum + pixelColor pixel
    nbPixels' = nbPixels + 1
    cluster' = OptiCluster color pixels' colorSum' nbPixels'

putPixelInClustersAt :: Bool -> Pixel -> [OptiCluster] -> Int -> [OptiCluster]
putPixelInClustersAt pushPixels pixel (cluster : clusters) 0 =
  currentCluster : clusters
  where
    currentCluster = putPixelInCluster pushPixels pixel cluster
putPixelInClustersAt pushPixels pixel (cluster : clusters) pos =
  cluster : nextClusters
  where
    nextClusters = putPixelInClustersAt pushPixels pixel clusters (pos - 1)

sortPixelInClusters :: Bool -> Pixel -> [OptiCluster] -> [OptiCluster]
sortPixelInClusters pushPixels pixel clusters = newClusters
  where
    allDists = getDistsFromPixelToClusters pixel clusters
    clusterID = fst (getMinPos allDists)
    newClusters = putPixelInClustersAt pushPixels pixel clusters clusterID

sortPixelsInClusters :: Bool -> [Pixel] -> [OptiCluster] -> [OptiCluster]
sortPixelsInClusters pushPixels [] clusters = clusters
sortPixelsInClusters pushPixels (pixel : pixels) clusters =
  sortPixelsInClusters pushPixels pixels newClusters
  where
    newClusters = sortPixelInClusters pushPixels pixel clusters

sumOfAllPixels :: [Pixel] -> Color
sumOfAllPixels [] = genEmptyColor
sumOfAllPixels [Pixel _ color] = color
sumOfAllPixels (Pixel _ color : pixels) = color + sumOfAllPixels pixels

calculateClusterAverage :: OptiCluster -> OptiCluster
calculateClusterAverage (OptiCluster _ _ _ 0) =
  genOptiClusterFromColor genEmptyColor
calculateClusterAverage (OptiCluster color pixels colorSum nbPixels) =
  clusterResult
  where
    Color rSum gSum bSum = colorSum
    r = rSum `div` nbPixels
    g = gSum `div` nbPixels
    b = bSum `div` nbPixels
    clusterColor = Color r g b
    clusterResult = OptiCluster clusterColor pixels genEmptyColor 0

calculateClustersAverage :: [OptiCluster] -> [OptiCluster]
calculateClustersAverage [] = []
calculateClustersAverage [cluster] = [calculateClusterAverage cluster]
calculateClustersAverage (cluster : clusters) =
  clusterAverage : nextClusterAverage
  where
    clusterAverage = calculateClusterAverage cluster
    nextClusterAverage = calculateClustersAverage clusters

isConvLimitReached :: Float -> [OptiCluster] -> [OptiCluster] -> Bool
isConvLimitReached convLim [lastCluster] [cluster] =
  fromIntegral (lastClustCol `getDistBetweenColors` clustCol) <= convLim
  where
    lastClustCol = optiClusterColor lastCluster
    clustCol = optiClusterColor cluster
isConvLimitReached convLim (lastCluster : lastClusters) (cluster : clusters)
  | isConvLimitReached convLim [lastCluster] [cluster] = True
  | otherwise = isConvLimitReached convLim lastClusters clusters

clearClusterContent :: [OptiCluster] -> [OptiCluster]
clearClusterContent [OptiCluster color _ _ _] = [genOptiClusterFromColor color]
clearClusterContent (OptiCluster color _ _ _ : clusters) = cluster : clusters'
  where
    cluster = genOptiClusterFromColor color
    clusters' = clearClusterContent clusters

iterKMeansUntilConv :: Float -> [Pixel] -> [OptiCluster] -> [OptiCluster]
iterKMeansUntilConv convLimit pixels clusters
  | isConvLimitReached convLimit clusters nextClusterIter = nextClusterIter
  | otherwise = iterKMeansUntilConv convLimit pixels nextClusterIter
  where
    nextRawClusterIter = sortPixelsInClusters False pixels clusters
    nextClusterIter = calculateClustersAverage nextRawClusterIter

firstIter :: [Pixel] -> [OptiCluster] -> [OptiCluster]
firstIter pixels clusters = firstClusterIter
  where
    firstRawClusterIter = sortPixelsInClusters False pixels clusters
    firstClusterIter = calculateClustersAverage firstRawClusterIter

optiImageCompressor :: StdGen -> CompressionType -> Pixels -> CompressedImage
optiImageCompressor gen (CompressionType nbCols convLimit) (Pixels pixels) =
  CompressedImage $ optiClustersToClusters bestClustersFilled
  where
    convLimit' = convLimit * convLimit
    (randomClusters, _) = makeClusters nbCols gen pixels
    firstClusterIter = firstIter pixels randomClusters
    bestClusters = iterKMeansUntilConv convLimit' pixels firstClusterIter
    bestEmptyClusters = clearClusterContent bestClusters
    bestRawClustersFilled = sortPixelsInClusters True pixels bestEmptyClusters
    bestClustersFilled = calculateClustersAverage bestRawClustersFilled
