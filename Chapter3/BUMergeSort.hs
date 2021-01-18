module BUMergeSort where -- Bottom Up Merge-Sort

data Sortable a = Sortable
  { less :: a -> a -> Bool,
    size :: Int,
    segments :: [[a]]
  }

instance Show a => Show (Sortable a) where
    show (Sortable _ s seg) = "Size: " ++ show s ++ "\nSeg: " ++ show seg

merge :: Ord a => (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f = mrg
  where
    mrg [] ys = ys
    mrg xs [] = xs
    mrg (x : xs) (y : ys) =
        if x `f` y then x : mrg xs (y : ys) else y : mrg (x : xs) ys

new :: Ord a => (a -> a -> Bool) -> Sortable a
new f = Sortable f 0 []

add :: Ord a => a -> Sortable a -> Sortable a
add x (Sortable f size' segs') = Sortable f
                                          (size' + 1)
                                          (addSeg [x] segs' size')
  where
    addSeg seg segs sz = if even sz
        then seg : segs
        else addSeg (merge f seg (head segs)) (tail segs) (sz `div` 2)

sort :: Ord a => Sortable a -> [a]
sort (Sortable f _ segs) = mergeAll [] segs
  where
    mergeAll xs []       = xs
    mergeAll xs (y : ys) = mergeAll (merge f xs y) ys
    -- note: mergeAll == foldl (merge f)

fromList :: Ord a => [a] -> Sortable a
fromList = foldr add (new (<))
