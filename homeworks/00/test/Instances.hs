{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

module Instances () where

import Binary (Binary(..), Bit(..))
import Test.QuickCheck (Arbitrary(..), chooseInt, vectorOf, frequency, suchThat)
import Generic.Random (genericArbitraryU)
import GHC.Generics (Generic)
import Can (canZero, canOne, Can(..), TrailingOne(..))

deriving instance Eq Binary
deriving instance Eq Bit
deriving instance Eq Can
deriving instance Eq TrailingOne

instance Arbitrary Binary where
  arbitrary =
    let
      bitsToBinary :: [Bit] -> Binary
      bitsToBinary = foldr (flip (:.)) End
     in bitsToBinary <$> do
          n <- chooseInt (0, 16)
          vectorOf n arbitrary

instance Arbitrary Bit where
  arbitrary = do
    bool <- arbitrary
    pure $ if bool then One else Zero

deriving instance Generic TrailingOne
deriving instance Generic Can

instance Arbitrary TrailingOne where
  arbitrary = do
    let nonOne = genericArbitraryU `suchThat` (/= canOne)
    frequency [(1, pure canOne), (4, nonOne)]

instance Arbitrary Can where
  arbitrary =
    frequency [(1, pure canZero), (4, genericArbitraryU `suchThat` (/= canZero))]
