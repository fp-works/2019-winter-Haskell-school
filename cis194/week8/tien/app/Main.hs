module Main where

import Party
import System.Directory
import System.IO

main :: IO ()
main = do
  dir <- getCurrentDirectory
  handle <- openFile (dir ++ "/src/company.txt") ReadMode
  contents <- hGetContents handle
  putStrLn $ "Total Fun: " ++ (show . totalFun . read $ contents)
  mapM_ putStrLn . totalEmpNameList . read $ contents
  hClose handle
