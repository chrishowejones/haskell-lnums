module Main (main) where

import System.Environment ( getArgs, getProgName )
import Lib ( numberAllLines, prettyNumberedLines, PadMode(PadLeft) )

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArguments cliArgs
  maybe
    (printHelpText "Missing filename")
    (\filePath -> do
        fileLines <- readLines filePath
        let numbered = numberAllLines fileLines
            prettyNumbered = prettyNumberedLines PadLeft numbered
        mapM_ putStrLn prettyNumbered)
    mFilePath
