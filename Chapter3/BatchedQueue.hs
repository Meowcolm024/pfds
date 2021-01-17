module BatchedQueue where

data Queue a = Queue
  { f :: [a],
    r :: [a]
  }
  deriving (Show)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue x _) = null x

head' :: Queue a -> a
head' (Queue []      _) = error "Empty Queue!"
head' (Queue (x : _) _) = x

queue :: Queue a -> Queue a
queue (Queue [] x) = Queue (reverse x) []
queue q            = q

snocQ :: Queue a -> a -> Queue a
snocQ (Queue xs ys) y = queue $ Queue xs (y : ys)

tail' :: Queue a -> Queue a
tail' (Queue []       _ ) = error "Empty Queue!"
tail' (Queue (_ : xs) ys) = queue $ Queue xs ys

listToQueue :: [a] -> Queue a
listToQueue [] = empty
listToQueue xs =
    let (p, q) = splitAt (length xs `div` 2) xs in Queue p (reverse q)
