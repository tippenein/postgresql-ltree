{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Function ((&))
import Data.Text (Text)
import Database.PostgreSQL.LTree (Label(unLabel), mkLabel)
import Test.Hspec (describe, hspec, it, shouldBe)
import Test.QuickCheck (Gen, arbitrary, forAll, property)

import qualified Data.Text as Text

main :: IO ()
main = hspec $ do
  describe "mkLabel" $ do
    it "should not modify the input text" $
      property $ forAll genAnyText $ \t ->
        mkLabel t & either
          (const True)
          (\label -> unLabel label == t)
    it "should fail for null bytes" $
      mkLabel "foo\0bar"
        `shouldBe` Left "Invalid ltree label chars found: \"\\NUL\""
  where
  genAnyText :: Gen Text
  genAnyText = Text.pack <$> arbitrary
