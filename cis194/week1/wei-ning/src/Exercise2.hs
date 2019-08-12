module Exercise2 ( doubleEveryOther ) where

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []           = []
doubleEveryOther [x]          = [x]
doubleEveryOther (x : y : xs) = x : y * 2 : doubleEveryOther xs
