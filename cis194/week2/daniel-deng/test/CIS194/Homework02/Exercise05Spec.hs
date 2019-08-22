module CIS194.Homework02.Exercise05Spec where

import CIS194.Homework02.Exercise01 (parseMessage)
import CIS194.Homework02.Exercise05

import Test.Tasty.Hspec

spec_whatWentWrong :: Spec
spec_whatWentWrong =

  it "returns a chronological high severity error messages" $ do

    let logText = [ "I 6 Completed armadillo processing"
                  , "I 1 Nothing to report"
                  , "E 99 10 Flange failed!"
                  , "I 4 Everything normal"
                  , "I 11 Initiating self-destruct sequence"
                  , "E 70 3 Way too many pickles"
                  , "E 65 8 Bad pickle-flange interaction detected"
                  , "W 5 Flange is due for a check-up"
                  , "I 7 Out for lunch, back in two time steps"
                  , "E 20 2 Too many pickles"
                  , "I 9 Back from lunch" ]

    let input   = map parseMessage logText
    let output  = [ "Way too many pickles"
                  , "Bad pickle-flange interaction detected"
                  , "Flange failed!" ]

    whatWentWrong input `shouldBe` output
