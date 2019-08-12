module Exercise4 ( validateCreditcard ) where

import           Exercise1 (toDigitsRev)
import           Exercise2 (doubleEveryOther)
import           Exercise3 (sumDigits)

validateCreditcard :: Integer -> Bool
validateCreditcard = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigitsRev
