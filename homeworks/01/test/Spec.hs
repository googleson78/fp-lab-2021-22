{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Hspec (hspec)
import ExprSpec (spec)

main :: IO ()
main = hspec spec
