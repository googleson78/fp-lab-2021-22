{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances () where

import Binary (Binary(..), Bit(..))
import Test.QuickCheck (Arbitrary(..), chooseInt, vectorOf)

deriving instance Eq Binary
deriving instance Eq Bit

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
