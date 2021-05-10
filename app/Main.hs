module Main where

import Control.Exception (IOException, catch, try)
import ICGetArgs (getICArgs)
import OptiImageCompressor (optiImageCompressor)
import ParseFile (getPixelsFromFileContent)
import System.Environment (getArgs)
import System.Exit
  ( ExitCode (ExitFailure),
    exitWith,
  )
import System.Random (StdGen (..), newStdGen)
import Types
  ( Color (..),
    CompressionType (..),
    ICArgs (..),
    Pixels (..),
  )
import Usage (usage)

safeReadFile :: String -> IO (Maybe String)
safeReadFile path = (Just <$> readFile path) `catch` handleExists
  where
    handleExists :: IOException -> IO (Maybe String)
    handleExists e = return Nothing

runImageCompressor :: StdGen -> ICArgs -> String -> IO ()
runImageCompressor gen (ICArgs nbCols convLim path) content =
  case getPixelsFromFileContent content of
    Just pixels -> print $ optiImageCompressor gen compressType pixels
    Nothing -> exitWith $ ExitFailure 84
  where
    compressType = CompressionType nbCols convLim

prepareRunImageCompressor :: StdGen -> ICArgs -> IO ()
prepareRunImageCompressor gen (ICArgs nbCols convLim path) = do
  content <- safeReadFile path
  case content of
    Just content' -> runImageCompressor gen icArgs content'
    Nothing -> exitWith $ ExitFailure 84
  where
    icArgs = ICArgs nbCols convLim path

printUsageAndExit :: IO ()
printUsageAndExit = do
  _ <- usage
  exitWith $ ExitFailure 84

main :: IO ()
main = do
  args <- getArgs
  gen <- newStdGen
  case args of
    [] -> printUsageAndExit
    _ -> case getICArgs args of
      Just icArgs -> prepareRunImageCompressor gen icArgs
      Nothing -> printUsageAndExit
