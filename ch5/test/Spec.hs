module Main (main) where

import Lib
import           Test.Hspec

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec = do
  describe "Showing tests" $ do
    it "should pretty print empty lists" $ do
      show HNil `shouldBe` "[]"
    it "should pretty print non-empty lists" $ do
      (show $ 'a' :# True :# Just "abcde" :# Just 'b' :# HNil) `shouldBe`
        ("'a' :# True :# Just \"abcde\" :# Just 'b' :# []" :: String)
  describe "Ord tests" $ do
    it "should order equal lists correctly for empty HLists" $ do
      HNil < HNil `shouldBe` False
      HNil > HNil `shouldBe` False
      HNil <= HNil `shouldBe` True
      HNil >= HNil `shouldBe` True
    it "should order equal lists correctly for non-empty HLists" $ do
      ((3 :: Int) :# Just 'a' :# HNil) < (3 :: Int) :# Just 'a' :# HNil `shouldBe`
        False
      ((3 :: Int) :# Just 'a' :# HNil) > (3 :: Int) :# Just 'a' :# HNil `shouldBe`
        False
      ((3 :: Int) :# Just 'a' :# HNil) <= (3 :: Int) :# Just 'a' :# HNil `shouldBe`
        True
      ((3 :: Int) :# Just 'a' :# HNil) >= (3 :: Int) :# Just 'a' :# HNil `shouldBe`
        True
    it "should compare on the first different element" $ do
      ((Just 'a' :# (4 :: Int) :# (5 :: Int) :# HNil) `compare`
       (Just 'a' :# (3 :: Int) :# (6 :: Int) :# HNil)) `shouldBe` GT
      ((Just 'a' :# (4 :: Int) :# (5 :: Int) :# HNil) `compare`
       (Just 'a' :# (5 :: Int) :# (5 :: Int) :# HNil)) `shouldBe` LT
