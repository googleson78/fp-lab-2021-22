module Can where

import Binary (Binary(..), Bit(..))

data TrailingOne
  deriving Show

canOne :: TrailingOne
canOne = undefined

data Can
  deriving Show

canZero :: Can
canZero = undefined

snoc :: Can -> Bit -> Can
snoc = undefined

forgetTrailingOne :: TrailingOne -> Binary
forgetTrailingOne = undefined

forget :: Can -> Binary
forget = undefined

canonicalise :: Binary -> Can
canonicalise = undefined
