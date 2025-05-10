module TestExample where

data Nat = Z | S Nat
  deriving (Eq)

instance Show Nat where
  show  = show . toInt

mkNat :: Int -> Nat
mkNat n = iterate S Z !! n

toInt :: Nat -> Int 
toInt Z = 0
toInt (S n) = 1 + toInt n

fibs = [1,1] ++ zipWith (+) fibs (tail fibs)

fib :: Int -> Int
fib = (fibs !!)