module CIS194.Homework01.Exercise04 (validate) where

import CIS194.Homework01.Exercise01 (toDigits)
import CIS194.Homework01.Exercise02 (doubleEveryOther)
import CIS194.Homework01.Exercise03 (sumDigits)

type CreditCardNumber = Integer

hasSixteenDigits :: CreditCardNumber -> Bool
hasSixteenDigits = (16 == ) . length . toDigits

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy divisor num = num `rem` divisor == 0

{-
  This validation rule requires the following steps:

  * Double the value of every second digit beginning from the right.
    That is, the last digit is unchanged; the second-to-last digit is doubled;
    the third-to-last digit is unchanged; and so on.
    For example, [1,3,8,6] becomes [2,3,16,6].

  * Add the digits of the doubled values and the undoubled digits from the original number.
    For example, [2,3,16,6] becomes 2+3+1+6+6 = 18.

  * Calculate the remainder when the sum is divided by 10.
    For the above example, the remainder would be 8.

  * If the result equals 0, then the number is valid.
-}
sumDigitsDivisibleByTen :: CreditCardNumber -> Bool
sumDigitsDivisibleByTen = isDivisibleBy 10 . sumDigits . doubleEveryOther . toDigits

validate :: CreditCardNumber -> Bool
validate = (&&) <$> hasSixteenDigits <*> sumDigitsDivisibleByTen
