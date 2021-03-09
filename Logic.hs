module Logic where

import Prelude(Bool(..), Show(..), Eq(..))

true :: Bool
true = True

false :: Bool
false = False

not :: Bool -> Bool
not True = False
not False = True

and :: Bool -> Bool -> Bool
and True x = x
and False _ = False

or :: Bool -> Bool -> Bool
or True _ = True
or False x = x

xor :: Bool -> Bool -> Bool
xor x y = or (and (not x) y) (and x (not y))


ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ e = e

