module Chapter6.Dense where

data Dight = Zero | One deriving (Show, Eq)

type Nat = [Dight]

inc :: Nat -> Nat
inc []          = [One]
inc (Zero : ds) = One : ds
inc (One  : ds) = Zero : inc ds

dec :: Nat -> Nat
dec [One      ] = []
dec (One  : ds) = Zero : ds
dec (Zero : ds) = One : dec ds
dec _           = undefined

add :: Nat -> Nat -> Nat
add ds           []           = ds
add []           ds           = ds
add (d    : ds1) (Zero : ds2) = d : add ds1 ds2
add (Zero : ds1) (d    : ds2) = d : add ds1 ds2
add (One  : ds1) (One  : ds2) = Zero : inc (add ds1 ds2)
