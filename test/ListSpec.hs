{-# LANGUAGE ScopedTypeVariables #-}

module ListSpec where

import Test.Hspec
import Test.QuickCheck

import Prelude hiding (foldl, foldr, length, sum, product, reverse, map, filter, concat, repeat, concatMap)

import List

list = (Cons 1 (Cons 2 (Cons 3 Empty)))

spec :: Spec
spec = do
  describe "foldl" $ do
    context "foldl (+) 0 [1,2,3]" $ do
      it "returns 6" $ do
        foldl (+) 0 list `shouldBe` 6

    context "foldl (\\a c -> a ++ (show c)) \"\" [1,2,3]" $ do
      it "returns \"123\"" $ do
        foldl (\a c -> a ++ (show c)) "" list `shouldBe` "123"


  describe "foldr" $ do
    context "foldr (+) 0 [1,2,3]" $ do
      it "returns 6" $ do
        foldr (+) 0 list `shouldBe` 6

    context "foldr (\\c a -> a ++ (show c)) \"\" [1,2,3]" $ do
      it "returns \"321\"" $ do
        foldr (\c a -> a ++ (show c)) "" list `shouldBe` "321"


  describe "prepend" $ do
    context "prepend 0 [1,2,3]" $ do
      it "returns [0,1,2,3]" $ do
        prepend 0 list `shouldBe` (Cons 0 (Cons 1 (Cons 2 (Cons 3 Empty))))


  describe "append" $ do
    context "append 4 [1,2,3]" $ do
      it "returns [1,2,3,4]" $ do
        append 4 list `shouldBe` (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))


  describe "sum" $ do
      it "returns sum of list elements" $ property $
        \(x :: Int) (y :: Int) (z :: Int) -> sum (Cons x (Cons y (Cons z Empty))) == x + y + z


  describe "product" $ do
      it "returns product of list elements" $ property $
        \(x :: Int) (y :: Int) (z :: Int) -> product (Cons x (Cons y (Cons z Empty))) == x * y * z


  describe "length" $ do
    context "length [1,2,3]" $ do
      it "returns 3" $ do
        length list `shouldBe` 3


  describe "reverse" $ do
    context "reverse [1,2,3]" $ do
      it "returns [3,2,1]" $ do
        reverse list `shouldBe` (Cons 3 (Cons 2 (Cons 1 Empty)))


  describe "map" $ do
    context "map (+ 1) [1,2,3]" $ do
      it "returns [2,3,4]" $ do
        map (+ 1) list `shouldBe` (Cons 2 (Cons 3 (Cons 4 Empty)))


  describe "filter" $ do
    context "filter (== 2) [1,2,3]" $ do
      it "returns [2]" $ do
        filter (== 2) list `shouldBe` (Cons 2 Empty)


  describe "concat" $ do
    context "concat [[1],[2],[3]]" $ do
      it "returns [1,2,3]" $ do
        concat (Cons (Cons 1 Empty) (Cons (Cons 2 Empty) (Cons (Cons 3 Empty) Empty))) `shouldBe` list


  describe "range" $ do
    context "range 1 3" $ do
      it "returns [1,2,3]" $ do
        range 1 3 `shouldBe` list


  describe "zeroTo" $ do
    context "zeroTo 2" $ do
      it "returns [0,1,2]" $ do
        zeroTo 2 `shouldBe` (Cons 0 (Cons 1 (Cons 2 Empty)))


  describe "repeat" $ do
    context "repeat 3 9" $ do
      it "returns [9,9,9]" $ do
        repeat 3 9 `shouldBe` (Cons 9 (Cons 9 (Cons 9 Empty)))


  describe "concatMap" $ do
    context "concatMap (repeat 2) [1,2,3]" $ do
      it "returns [1,1,2,2,3,3]" $ do
        concatMap (repeat 2) list `shouldBe` (Cons 1 (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 3 Empty))))))
