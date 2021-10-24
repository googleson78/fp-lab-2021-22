{-# LANGUAGE BlockArguments #-}
module CanSpec (canSpec) where

import Test.Hspec
import Can
import Binary
import Test.Hspec.QuickCheck
import Instances ()

canSpec :: Spec
canSpec = describe "Can.hs" do
  prop "forgetTrailingOne always generates a binary with a trailing one" do
    (== Just One) . lastBinary . forgetTrailingOne
  prop "only represents canonical binary numbers" do
    not . hasLeadingZero . forget
  prop "forget . Can.canonicalise === Binary.canonicalise" do
    \bin -> forget (Can.canonicalise bin) `shouldBe` Binary.canonicalise bin
  prop "Can.canonicalise . forget === id" do
    \bin -> Can.canonicalise (forget bin) `shouldBe` bin


lastBinary :: Binary -> Maybe Bit
lastBinary End = Nothing
lastBinary (End :. Zero) = Just Zero
lastBinary (End :. One) = Just One
lastBinary (bi :. _) = lastBinary bi
