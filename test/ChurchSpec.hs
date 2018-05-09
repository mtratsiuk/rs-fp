{-# LANGUAGE RankNTypes #-}

module ChurchSpec where

import Test.Hspec

import Church

assertBoolBinOp :: (ChurchBool -> ChurchBool -> ChurchBool) -> ChurchBool -> ChurchBool -> ChurchBool -> Expectation
assertBoolBinOp op left right expected = unchurchBool (op left right) `shouldBe` (unchurchBool expected)

assertAnd = assertBoolBinOp _and
assertOr = assertBoolBinOp _or
assertXor = assertBoolBinOp _xor

spec :: Spec
spec = do
  describe "_and" $ do
    context "False & False" $ do
      it "returns False" $ do
        assertAnd _false _false _false

    context "False & True" $ do
      it "returns False" $ do
        assertAnd _false _true _false

    context "True & False" $ do
      it "returns False" $ do
        assertAnd _true _false _false

    context "True & True" $ do
      it "returns True" $ do
        assertAnd _true _true _true


  describe "_or" $ do
    context "False | False" $ do
      it "returns False" $ do
        assertOr _false _false _false

    context "False | True" $ do
      it "returns True" $ do
        assertOr _false _true _true

    context "True | False" $ do
      it "returns True" $ do
        assertOr _true _false _true

    context "True | True" $ do
      it "returns True" $ do
        assertOr _true _true _true


  describe "_xor" $ do
    context "False ^ False" $ do
      it "returns False" $ do
        assertXor _false _false _false

    context "False ^ True" $ do
      it "returns True" $ do
        assertXor _false _true _true

    context "True ^ False" $ do
      it "returns True" $ do
        assertXor _true _false _true

    context "True ^ True" $ do
      it "returns False" $ do
        assertXor _true _true _false
