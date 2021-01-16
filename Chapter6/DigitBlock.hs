module Digitblock where

data Digitblock = Zeros Int | Ones Int deriving (Show, Eq)

type Nat = [Digitblock]

zeros :: Int -> Nat -> Nat
zeros _ []               = []
zeros i (Zeros j : blks) = Zeros (i + j) : blks
zeros 0 blks             = blks
zeros i blks             = Zeros i : blks

ones :: Int -> Nat -> Nat
ones i (Ones j : blks) = Ones (i + j) : blks
ones 0 blks            = blks
ones i blks            = Ones i : blks

inc :: Nat -> Nat
inc []               = [Ones 1]
inc (Zeros i : blks) = ones 1 (zeros (i - 1) blks)
inc (Ones  i : blks) = Zeros i : inc blks

dec :: Nat -> Nat
dec (Ones  i : blks) = zeros 1 (zeros (i - 1) blks)
dec (Zeros i : blks) = Ones i : dec blks
dec _                = undefined
