module BankersQueue where

data Queue a = Queue
  { f :: [a],
    lenF :: Int,
    r :: [a],
    lenR :: Int
  }
  deriving (Show)

empty :: Queue a
empty = Queue [] 0 [] 0

isEmpty :: Queue a -> Bool
isEmpty = (== 0) . lenF

queue :: Queue a -> Queue a
queue q@(Queue fx lf rx lr) =
    if lr <= lf then q else Queue (fx ++ reverse rx) (lf + lr) [] 0

snoc :: Queue a -> a -> Queue a
snoc (Queue fx lf rx lr) x = queue $ Queue fx lf (x : rx) (lr + 1)

head' :: Queue a -> a
head' (Queue []      _ _ _) = error "Empty Queue!"
head' (Queue (x : _) _ _ _) = x

tail' :: Queue a -> Queue a
tail' (Queue []       _  _  _ ) = error "Empty Queue!"
tail' (Queue (_ : fx) lf rx lr) = queue $ Queue fx (lf - 1) rx lr

fromList :: [a] -> Queue a
fromList = foldl snoc empty
