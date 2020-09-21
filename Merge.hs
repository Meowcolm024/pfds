module Merge where

data Sortable a = Sortable
  { less :: a -> a -> Bool,
    size :: Int,
    segments :: [[a]]
  }

instance (Show a) => Show (Sortable a) where
  show (Sortable _ s seg) = "Size: " ++ show s ++ "\n Seg: " ++ show seg

merge :: (Ord a, Eq a) => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f xss yss = mrg xss yss
  where
    mrg [] ys = ys
    mrg xs [] = xs
    mrg (x : xs) (y : ys)
      | f x y = x : mrg xs (y : ys)
      | otherwise = y : mrg (x : xs) ys

new :: (Ord a, Eq a) => (a -> a -> Bool) -> Sortable a
new f = Sortable f 0 []

add :: (Ord a, Eq a) => a -> Sortable a -> Sortable a
add x (Sortable f size' segs') = Sortable f (size' + 1) (addSeg [x] segs' size')
  where
    addSeg s sg sz = if even sz then s : sg else addSeg (merge f s (head sg)) (tail sg) (sz `div` 2)

sort :: (Ord a, Eq a) => Sortable a -> [a]
sort (Sortable f _ segs) = mergeAll [] segs
  where
    mergeAll xs [] = xs
    mergeAll xs (y : ys) = mergeAll (merge f xs y) ys

listToSortable :: (Ord a, Eq a) => [a] -> Sortable a
listToSortable xs = Sortable (<) (length xs) (map pure xs)
