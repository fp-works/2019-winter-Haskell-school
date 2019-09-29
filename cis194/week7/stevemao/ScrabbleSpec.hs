{-# LANGUAGE BlockArguments #-}
module ScrabbleSpec where
import Scrabble
import Test.Hspec
import JoinList
import Sized
import Buffer

type JL = JoinList (Score, Size) String

spec :: Spec
spec = do
  describe "score" do
    it "score a" do
      score 'a' `shouldBe` Score 1
      score 'G' `shouldBe` Score 2
    it "score others" do
      score '"' `shouldBe` Score 0
      
  describe "scoreString" do
    it "scoreString a" do
      scoreString "AbC" `shouldBe` Score 7
      scoreString "'AbC" `shouldBe` Score 7
  
  describe "getScore" do
    it "getScore a" do
      getScore (Score 7) `shouldBe` 7
  
  describe "instance Buffer (JoinList (Score, Size) String)" do
    describe "toString" do
      it "toString Empty" do
        let jl = (Empty :: JL) 
        toString jl `shouldBe` ""
      it "toString Single" do
        let jl = (Single (Score 1, Size 1) "A")
        toString jl `shouldBe` "A"
      it "toString Append" do
        let jl = (Append (11, 2) (Single (1, 1) "A") (Single (10, 1) "Q") :: JL)
        toString jl `shouldBe` "A\nQ"

    describe "fromString" do
      it "fromString \"\"" do
        let jl = (Empty :: JL) 
        fromString "" `shouldBe` jl
      it "fromString A" do
        let jl = Single (Score 1, Size 1) "A"
        fromString "A" `shouldBe` jl
      it "fromString A\nq" do
        let jl = Append (Score 11, Size 2) (Single (1, 1) "A") (Single (10, 1) "q")
        fromString "A\nq" `shouldBe` jl

    describe "line" do
      it "line Empty" do
        let jl = (Empty :: JL)
        line 0 jl `shouldBe` Nothing
      it "line Single" do
        let jl = Single (Score 1, Size 1) "A"
        line 0 jl `shouldBe` Just "A"
        line 1 jl `shouldBe` Nothing
      it "line Append" do
        let jl = Append (Score 11, Size 2) (Single (1, 1) "A") (Single (10, 1) "q")
        line 0 jl `shouldBe` Just "A"
        line 1 jl `shouldBe` Just "q"
        line 2 jl `shouldBe` Nothing
  
    describe "replaceLine" do
      it "replaceLine Empty" do
        let jl = (Empty :: JL)
        replaceLine 0 "abc" jl `shouldBe` jl
      it "replaceLine Single" do
        let jl = Single (Score 1, Size 1) "A"
        replaceLine 0 "aq" jl `shouldBe` Single (Score 11, Size 1) "aq"
        replaceLine 1 "aq" jl `shouldBe` jl
      it "replaceLine Append" do
        let jl = Append (Score 11, Size 2) (Single (1, 1) "A") (Single (10, 1) "q")
        replaceLine 0 "aq" jl `shouldBe` Append (Score 21, Size 2) (Single (11, 1) "aq") (Single (10, 1) "q")
        replaceLine 1 "aq" jl `shouldBe` Append (Score 12, Size 2) (Single (1, 1) "A") (Single (11, 1) "aq")
        replaceLine 2 "aq" jl `shouldBe` jl
  
    describe "numLines" do
      it "numLines Empty" do
        let jl = (Empty :: JL)
        numLines jl `shouldBe` 0
      it "numLines Single" do
        let jl = Single (Score 1, Size 1) "A"
        numLines jl `shouldBe` 1
      it "numLines Append" do
        let jl = Append (Score 11, Size 2) (Single (1, 1) "A") (Single (10, 1) "q")
        numLines jl `shouldBe` 2
  
    describe "value" do
      it "value Empty" do
        let jl = (Empty :: JL)
        value jl `shouldBe` 0
      it "value Single" do
        let jl = Single (Score 1, Size 1) "A"
        value jl `shouldBe` 1
      it "value Append" do
        let jl = Append (Score 11, Size 2) (Single (1, 1) "A") (Single (10, 1) "q")
        value jl `shouldBe` 11