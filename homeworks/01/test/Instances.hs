{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Instances where

import Expr (solvingSum, Expr(..), OperType(..))
import GHC.Generics (Generic)
import Generic.Random (withBaseCase, genericArbitraryRecG, genericArbitraryU, W, (%), Weights)
import Test.QuickCheck (Arbitrary (..), Gen, elements, oneof, genericShrink, suchThat)

deriving instance Eq OperType

deriving instance Eq Expr

deriving instance Generic OperType

deriving instance Generic Expr

instance Arbitrary OperType where
  arbitrary = genericArbitraryU

instance Arbitrary Expr where
  arbitrary =
    genericArbitraryRecG varGen weights
      `withBaseCase` oneof [Var <$> varGen, Val <$> arbitrary]
    where
      weights :: Weights Expr
      weights =
        varWeight %
        valWeight %
        operWeight %
        ifWeight %
        sumListWeight %
        sumWeight %
        ()
  shrink = genericShrink

varWeight :: W "Var"
varWeight = 4

valWeight :: W "Val"
valWeight = 6

operWeight :: W "Oper"
operWeight = 3

ifWeight :: W "If"
ifWeight = 3

sumListWeight :: W "SumList"
sumListWeight = 2

sumWeight :: W "Sum"
sumWeight = if solvingSum then 2 else 0

positiveGen :: Gen Integer
positiveGen = arbitrary `suchThat` (> 0)

varGen :: Gen String
varGen =
  elements $ map (: []) "xyzu"
