module Chapter4.RTQueue where

data Queue a = Queue
  { f :: [a],
    r :: ![a],
    s :: [a]
  }
  deriving (Show)

-- |s| = |f| - |r|

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y : _) a = y : a
rotate (x : f') (y : r') a = x : rotate f' r' (y : a)
rotate _ _ _ = undefined

empty :: Queue a
empty = Queue [] [] []

isEmpty :: Queue a -> Bool
isEmpty Queue {f = f'} = null f'

queue :: [a] -> [a] -> [a] -> Queue a
queue f r (_ : s) = Queue f r s
queue f r [] = let f' = rotate f r [] in Queue f' [] f'

snoc :: Queue a -> a -> Queue a
snoc (Queue f r s) x = queue f (x : r) s

head' :: Queue a -> a
head' (Queue [] _ _) = error "Empty Queue!"
head' (Queue (x : _) _ _) = x

tail' :: Queue a -> Queue a
tail' (Queue [] _ _) = error "Empty Queue!"
tail' (Queue (_ : f) r s) = queue f r s

fromList :: [a] -> Queue a
fromList = foldl snoc empty
