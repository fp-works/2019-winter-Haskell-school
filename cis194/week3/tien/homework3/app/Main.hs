module Main where

import           Golf

main :: IO ()
main = print . skips $ "ABCD"
