{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Hspec (hspec)
import BinarySpec (binarySpec)
import CanSpec (canSpec)

main :: IO ()
main = hspec do
  binarySpec
  canSpec
