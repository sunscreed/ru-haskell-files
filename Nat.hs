module Nat where

data Nat = Zero | Succ Nat
     deriving (Eq, Show, Ord)

instance Num Nat where
     (+) a Zero = a
     (+) a (Succ b) = Succ (a + b)
     negate _ = error "negate is undefined for Nat"
     (*) a Zero = Zero
     (*) a (Succ b) = a + (a * b)
     abs x = x
     signum Zero = Zero
     signum _ = Succ Zero
     fromInteger 0 = Zero
     fromInteger n = Succ (fromInteger (n - 1))

meanInt :: Int -> Int -> Double
meanInt a b = fromIntegral (a + b) / 2

beside :: Nat -> Nat -> Bool
beside a b = if (a == Succ b || b == Succ a) then True else False

besideNum :: Nat -> Nat -> Nat -> Bool
besideNum a b n = if (a == (b + n) || b == (a + n)) then True else False

-- pow :: Nat -> Nat -> Nat
-- pow num exp | exp == 0 = 1
--             | exp == 1 = num
--             | otherwise = helper num 1 exp
-- helper num counter exp | counter == exp = num
--                        | otherwise = num * pow num (counter + 1) exp
 

