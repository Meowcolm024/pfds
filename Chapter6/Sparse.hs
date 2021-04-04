module Chapter6.Sparse where

type Nat = [Int]

carry :: Int -> Nat -> Nat
carry w []             = [w]
carry w ws@(w' : rest) = if w < w' then w : ws else carry (2 * w) rest

borrow :: Int -> Nat -> Nat
borrow w ws@(w' : rest) = if w == w' then rest else w : borrow (2 * w) ws
borrow _ _              = undefined

inc :: Nat -> Nat
inc = carry 1

dec :: Nat -> Nat
dec = borrow 1

add :: Nat -> Nat -> Nat
add ws [] = ws
add [] ws = ws
add m@(w1 : ws1) n@(w2 : ws2) | w1 < w2   = w1 : add ws1 n
                              | w2 < w1   = w2 : add m ws2
                              | otherwise = carry (2 * w1) (add ws1 ws2)
