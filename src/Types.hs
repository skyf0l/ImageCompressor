{-# OPTIONS_GHC -Wno-missing-methods #-}

module Types
  ( ICArgs (..),
    Pos (..),
    Color (..),
    Pixel (..),
    Pixels (..),
    Cluster (..),
    OptiCluster (..),
    CompressionType (..),
    CompressedImage (..),
    genEmptyColor,
    clearClusters,
    clearOptiCluster,
    clearOptiClusters,
    genOptiClusterFromColor,
    optiClusterToCluster,
    optiClustersToClusters,
  )
where

data ICArgs = ICArgs
  { argNumberOfColors :: Int,
    argConvergenceLimit :: Float,
    argFilePath :: String
  }

data Pos = Pos
  { x :: Int,
    y :: Int
  }

instance Show Pos where
  show (Pos x y) = '(' : show x ++ "," ++ show y ++ ")"

instance Read Pos where
  readsPrec _ r = [(uncurry Pos value, "")]
    where
      value = read r :: (Int, Int)

data Color = Color
  { r :: Int,
    g :: Int,
    b :: Int
  }

instance Show Color where
  show (Color r g b) = '(' : show r ++ "," ++ show g ++ "," ++ show b ++ ")"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

instance Read Color where
  readsPrec _ r = [(uncurry3 Color value, "")]
    where
      value = read r :: (Int, Int, Int)

instance Num Color where
  (+) (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1 + r2) (g1 + g2) (b1 + b2)
instance Eq Color where 
  (==) (Color r1 g1 b1) (Color r2 g2 b2)
    | r1 /= r2 = False
    | g1 /= g2 = False
    | b1 /= b2 = False
    | otherwise = True


genEmptyColor :: Color
genEmptyColor = Color 0 0 0

data Pixel = Pixel
  { pixelPos :: Pos,
    pixelColor :: Color
  }

instance Show Pixel where
  show (Pixel pos color) = show pos ++ " " ++ show color

newtype Pixels = Pixels {pixels :: [Pixel]}

instance Show Pixels where
  show (Pixels []) = ""
  show (Pixels [pixel]) = show pixel
  show (Pixels (pixel : pixels)) = show pixel ++ "\n" ++ show next
    where
      next = Pixels pixels

spaceToComma :: Char -> Char
spaceToComma ' ' = ','
spaceToComma other = other

formatPixelRead :: String -> String
formatPixelRead str = '(' : withComma ++ ")"
  where
    withComma = map spaceToComma str

instance Read Pixel where
  readsPrec _ r = [(uncurry Pixel (uncurry Pos pos, uncurry3 Color col), "")]
    where
      (pos, col) =
        read (formatPixelRead r) ::
          ((Int, Int), (Int, Int, Int))

data Cluster = Cluster
  { clusterColor :: Color,
    clusterPixels :: [Pixel]
  }

instance Show Cluster where
  show (Cluster clusterColor []) =
    "--\n" ++ show clusterColor ++ "\n-"
  show (Cluster clusterColor clusterPixels) =
    "--\n" ++ show clusterColor ++ "\n-\n" ++ show (Pixels clusterPixels)

clearClusters :: [Cluster] -> [Cluster]
clearClusters [Cluster color _] = [Cluster color []]
clearClusters (Cluster color _ : clusters) = Cluster color [] : clusters

data OptiCluster = OptiCluster
  { optiClusterColor :: Color,
    optiClusterPixels :: [Pixel],
    optiClusterColorSum :: Color,
    optiClusterNbPixels :: Int
  }

genOptiClusterFromColor :: Color -> OptiCluster
genOptiClusterFromColor color = OptiCluster color [] genEmptyColor 0

clearOptiCluster :: OptiCluster -> OptiCluster
clearOptiCluster optiCluster = genOptiClusterFromColor color
  where
    color = optiClusterColor optiCluster

clearOptiClusters :: [OptiCluster] -> [OptiCluster]
clearOptiClusters [optiCluster] = [genOptiClusterFromColor color]
  where
    color = optiClusterColor optiCluster
clearOptiClusters (optiCluster : optiClusters) = optiCluster' : optiClusters'
  where
    optiCluster' = genOptiClusterFromColor (optiClusterColor optiCluster)
    optiClusters' = clearOptiClusters optiClusters

optiClusterToCluster :: OptiCluster -> Cluster
optiClusterToCluster (OptiCluster color pixels colorSum nbPixels) = cluster
  where
    cluster = Cluster color pixels

optiClustersToClusters :: [OptiCluster] -> [Cluster]
optiClustersToClusters [optiCluster] = [optiClusterToCluster optiCluster]
optiClustersToClusters (optiCluster : optiClusters) = cluster : clusters
  where
    cluster = optiClusterToCluster optiCluster
    clusters = optiClustersToClusters optiClusters

data CompressionType = CompressionType
  { numberOfColors :: Int,
    convergenceLimit :: Float
  }

newtype CompressedImage = CompressedImage {clusters :: [Cluster]}

instance Show CompressedImage where
  show (CompressedImage []) = ""
  show (CompressedImage [cluster]) = show cluster
  show (CompressedImage (cluster : clusters)) = show cluster ++ "\n" ++ show next
    where
      next = CompressedImage clusters
