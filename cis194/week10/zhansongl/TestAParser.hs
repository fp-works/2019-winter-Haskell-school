import AParser

import Test.Hspec
import Test.QuickCheck
import Data.Char (ord, chr)
import Control.Applicative (Alternative(..))

main = hspec $ do
  describe "Functor instance for Parser" $ do
    it "should satifsy identity law" $ do
      let char' = fmap id (char 'C')
      runParser char' "Cantor" `shouldBe` Just ('C', "antor")
      runParser char' "Cantor" `shouldBe` runParser (char 'C') "Cantor"
      runParser char' "z" `shouldBe` Nothing
      runParser char' "z" `shouldBe` runParser (char 'C') "z"

    it "should satisfy composition law" $ do
      let f = ord -- Char -> Int
      let g = succ -- Char -> Char
      let lhs = fmap (f . g) (char 'C')
      let rhs = (fmap f . fmap g) (char 'C')
      runParser lhs "Convolution" `shouldBe` Just (ord 'D', "onvolution")
      runParser lhs "Convolution" `shouldBe` runParser rhs "Convolution"
      runParser lhs "whiff" `shouldBe` Nothing
      runParser lhs "Whiff" `shouldBe` runParser rhs "Whiff"

    it "should have a working replace function" $ do
      let p = 12 <$ posInt
      runParser p "588" `shouldBe` Just (12, "")
      runParser p "buzz" `shouldBe` Nothing

  describe "Applicative instance for Parser" $ do
    it "should satisfy identity law" $ do
      let char' = pure id <*> (char 'C')
      runParser char' "Caballero" `shouldBe` Just ('C', "aballero")
      runParser char' "Caballero" `shouldBe` runParser (char 'C') "Caballero"
      runParser char' "z" `shouldBe` Nothing
      runParser char' "z" `shouldBe` runParser (char 'C') "z"

    it "should satisfy composition law" $ do
      let u = pure show -- Parser (Int -> String)
      let v = pure ord -- Parser (Char -> Int)
      let w = (char 'm') -- Parser Char
      let lhs = pure (.) <*> u <*> v <*> w
      let rhs = u <*> (v <*> w)
      runParser lhs "moose" `shouldBe` Just (show . ord $ 'm', "oose")
      runParser lhs "moose" `shouldBe` runParser rhs "moose"
      runParser lhs "loose" `shouldBe` Nothing
      runParser lhs "loose" `shouldBe` runParser rhs "loose"

    it "should satisfy homomorphism rule" $ do
      let lhs = pure ord <*> pure 'd'
      let rhs = pure (ord 'd')
      runParser lhs "doggo" `shouldBe` Just (ord 'd', "doggo")
      runParser lhs "doggo" `shouldBe` runParser rhs "doggo"

    it "should satisfy interchange law" $ do
      let u = pure ord :: Parser (Char -> Int)
      let y = 'z'
      let lhs = u <*> pure y
      let rhs = pure ($ y) <*> u
      runParser lhs "zany" `shouldBe` Just (ord 'z', "zany")
      runParser lhs "zany" `shouldBe` runParser rhs "zany"

  describe "abParser" $ do
    it "should return something for abcdef" $ do
      runParser abParser "abcdef" `shouldBe` Just (('a', 'b'), "cdef")
    it "should return Nothing for baecdf" $ do
      runParser abParser "baecdf" `shouldBe` Nothing

  describe "abParser_" $ do
    it "should return something for abcdef" $ do
      runParser abParser_ "abcdef" `shouldBe` Just ((), "cdef")
    it "should return Nothing for baecdf" $ do
      runParser abParser_ "baecdf" `shouldBe` Nothing

  describe "intPair" $ do
    it "should return [12, 34] for \"12 34\"" $ do
      runParser intPair "12 34" `shouldBe` Just ([12, 34], "")

  describe "Alternative" $ do
    it "should satisfy the identity rule" $ do
      runParser (empty <|> pure "123") "S" `shouldBe` Just ("123", "S")
      runParser (pure "123" <|> empty) "S" `shouldBe` Just ("123", "S")
    it "should return one of the choices" $ do
      runParser (char 'C' <|> char 'D') "CID" `shouldBe` Just ('C', "ID")
      runParser (char 'C' <|> char 'D') "DID" `shouldBe` Just ('D', "ID")
    it "should satisfy associative law" $ do
      let lhs = (char 'C' <|> char 'D') <|> char 'E'
      let rhs = char 'C' <|> (char 'D' <|> char 'E')
      runParser lhs "CID" `shouldBe` Just ('C', "ID")
      runParser lhs "CID" `shouldBe` runParser rhs "CID"
      runParser lhs "DID" `shouldBe` Just ('D', "ID")
      runParser lhs "DID" `shouldBe` runParser rhs "DID"
      runParser lhs "EID" `shouldBe` Just ('E', "ID")
      runParser lhs "EID" `shouldBe` runParser rhs "EID"
      runParser lhs "no" `shouldBe` Nothing
      runParser lhs "no" `shouldBe` runParser rhs "no"

  describe "intOrUppercase" $ do
    it "should choose integer with integer prefix" $ do
      runParser intOrUppercase "342abcd" `shouldBe` Just ((), "abcd")
    it "should choose uppercase with capital letter prefix" $ do
      runParser intOrUppercase "XYZ" `shouldBe` Just ((), "YZ")
    it "should return nothing if neither choices work" $ do
      runParser intOrUppercase "foo" `shouldBe` Nothing
