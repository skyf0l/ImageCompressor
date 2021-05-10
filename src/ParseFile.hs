module ParseFile
  ( getPixelsFromFileContent,
  )
where

import Control.Exception (IOException, catch, try)
import System.IO ()
import Types
  ( Color (..),
    Pixel (..),
    Pixels (..),
    Pos (..),
  )

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

parseLine :: [String] -> Maybe Pixels
parseLine [] = Just $ Pixels []
parseLine (l : ls) = case readMaybe l of
  Just pixel -> case parseLine ls of
    Just (Pixels pixels) -> Just $ Pixels $ pixel : pixels
    Nothing -> Nothing
  Nothing -> Nothing

getPixelsFromFileContent :: String -> Maybe Pixels
getPixelsFromFileContent content = case parseLine ls of
  Just (Pixels []) -> Nothing
  a -> a
  where
    ls = lines content