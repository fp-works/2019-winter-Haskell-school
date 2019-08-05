module CIS194.Homework01.Exercise04 (validate) where

import CIS194.Homework01.Exercise01 (toDigits)
import CIS194.Homework01.Exercise02 (doubleEveryOther)
import CIS194.Homework01.Exercise03 (sumDigits)

type CreditCardNumber = Integer

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f &&& g) a = f a && g a

hasSixteenDigits :: CreditCardNumber -> Bool
hasSixteenDigits = (>= (10 ^ 15)) &&& (< (10 ^ 16))

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy divisor num = num `rem` divisor == 0

sumDigitsDivisibleByTen :: CreditCardNumber -> Bool
sumDigitsDivisibleByTen = isDivisibleBy 10 . sumDigits . doubleEveryOther . toDigits

validate :: CreditCardNumber -> Bool
validate = hasSixteenDigits &&& sumDigitsDivisibleByTen
