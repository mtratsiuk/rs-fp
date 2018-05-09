{-# LANGUAGE ScopedTypeVariables #-}

module BaseSpec where

import Test.Hspec
import Test.QuickCheck

import Prelude hiding (flip)

import Base

spec :: Spec
spec = do
  describe "identity" $ do
    it "returns it's argument" $ property $
      \(x :: Int) -> identity x == x

  describe "constant" $ do
    it "returns first argument" $ property $
      \(x :: Int) (y :: Int) -> constant x y == x

  describe "apply" $ do
    it "applies function to argument" $ do
      apply reverse "123" `shouldBe` "321"

  describe "compose" $ do
    it "returns composed function" $ do
      compose (+ 2) (* 10) 4 `shouldBe` 42

  describe "flip" $ do
    it "returns function with flipped arguments" $ do
      flip (++) "hello" "world" `shouldBe` "worldhello"
