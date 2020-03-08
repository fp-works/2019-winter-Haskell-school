module Main where

import           Control.Monad.Random (evalRandIO)
import           Data.Maybe           (fromMaybe)
import           Risk                 (Battlefield (..), successProb)
import           System.Environment   (getArgs)


getParamsFromArgs :: [String] -> Maybe (IO (Int, Int))
getParamsFromArgs [a, d] = Just . return $ (read a, read d)
getParamsFromArgs _      = Nothing

getParamsFromStdin :: IO (Int, Int)
getParamsFromStdin = do
  putStrLn "Enter number of attackers: "
  a <- readLn
  putStrLn "Enter number of defenders: "
  d <- readLn
  return (a, d)

main :: IO ()
main = do
  args <- getArgs
  -- get from command line args first, if not from stdin
  (a, d) <- fromMaybe getParamsFromStdin $ getParamsFromArgs args
  prob <- evalRandIO . successProb $ Battlefield { attackers = a, defenders = d }
  putStrLn $ "Probability of winning is " ++ (show prob)
