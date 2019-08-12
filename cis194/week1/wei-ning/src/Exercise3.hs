module Exercise3 ( sumDigits ) where

import           Exercise1 (toDigits)

sumDigits :: [Integer] -> Integer
sumDigits ns = sum (sum . toDigits <$> ns)
