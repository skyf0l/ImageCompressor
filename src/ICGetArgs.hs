module ICGetArgs
  ( getICArgs,
  )
where

import Data.Char (ord)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Types (ICArgs (..))

parseArgs :: ICArgs -> [String] -> Maybe ICArgs
parseArgs args [] = Just args
parseArgs _ [_] = Nothing
parseArgs (ICArgs _ l f) ("-n" : x : xs) = case readMaybe x :: Maybe Int of
  Just nCols -> parseArgs (ICArgs nCols l f) xs
  Nothing -> Nothing
parseArgs (ICArgs n _ f) ("-l" : x : xs) = case readMaybe x :: Maybe Float of
  Just convLimit -> parseArgs (ICArgs n convLimit f) xs
  Nothing -> Nothing
parseArgs (ICArgs n l _) ("-f" : x : xs) =
  parseArgs (ICArgs n l x) xs
parseArgs _ _ = Nothing

checkArgs :: Maybe ICArgs -> Maybe ICArgs
checkArgs (Just (ICArgs n l f))
  | n <= 0 = Nothing
  | l < 0 = Nothing
  | null f = Nothing
  | otherwise = Just (ICArgs n l f)
checkArgs Nothing = Nothing

getICArgs :: [String] -> Maybe ICArgs
getICArgs args = checkArgs parsedArgs
  where
    icArgs = ICArgs (-1) (-1) ""
    parsedArgs = parseArgs icArgs args
