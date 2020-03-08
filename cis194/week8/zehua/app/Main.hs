module Main where

import           Party

main :: IO ()
main = do
  cStr <- readFile "../company.txt"
  let c = read cStr
      gl = maxFun c
  putStrLn ("Total fun: " ++ show (getFun gl))
  mapM_ putStrLn (getSortedNames gl)
