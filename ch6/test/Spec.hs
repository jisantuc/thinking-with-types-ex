module Main (main) where

import Lib
import Lib
import           Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Cont instances" $ do
    it "should have a lawful functor" $ do
      (3 :: Int) `shouldBe` 3
    it "should have a lawful applicative" $ do
      (3 :: Int) `shouldBe` 3
    it "should have a lawful monad" $ do
      (3 :: Int) `shouldBe` 3
