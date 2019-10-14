module Week10Spec where
import Test.Hspec
import AParser

spec :: Spec
spec = do 
  describe "fmap on Parser" $ do
    it "is Functor" $ do
      runParser (fmap (+1) posInt) "1Iamstupid" `shouldBe` Just (2, "Iamstupid")
      runParser (fmap (+1) posInt) "Iamstupid" `shouldBe` Nothing

 
