module Chapter6.SBRList where    -- Skew Binary Random Access List

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)
type RList a = [(Int, Tree a)]

empty :: RList a
empty = []

isEmpty :: RList a -> Bool
isEmpty = null

cons :: a -> RList a -> RList a
cons x ts@((w1, t1) : (w2, t2) : rest) =
    if w1 == w2 then (1 + w1 + w2, Node x t1 t2) : rest else (1, Leaf x) : ts
cons x ts = (1, Leaf x) : ts

head' :: RList a -> a
head' ((1, Leaf x    ) : _) = x
head' ((_, Node x _ _) : _) = x
head' _                     = error "Empty List!"

tail' :: RList a -> RList a
tail' ((1, Leaf _      ) : ts) = ts
tail' ((w, Node _ t1 t2) : ts) = (w `div` 2, t1) : (w `div` 2, t2) : ts
tail' _                        = error "Empty List!"

lookup' :: RList a -> Int -> a
lookup' ((w, t) : ts) i =
    if i < w then lookupTree w t i else lookup' ts (i - w)
lookup' _ _ = error "Index too large!"

lookupTree :: Int -> Tree a -> Int -> a
lookupTree 1 (Leaf x      ) 0 = x
lookupTree _ (Node x _  _ ) 0 = x
lookupTree w (Node _ t1 t2) i = if i < w `div` 2
    then lookupTree (w `div` 2) t1 (i - 1)
    else lookupTree (w `div` 2) t2 (i - 1 - w `div` 2)
lookupTree _ _ _ = error "Index too large!"

update :: RList a -> Int -> a -> RList a
update ((w, t) : ts) i y =
    if i < w then (w, updateTree w t i y) : ts else (w, t) : update ts (i - w) y
update _ _ _ = error "Index too large!"

updateTree :: Int -> Tree a -> Int -> a -> Tree a
updateTree 1 (Leaf _      ) 0 y = Leaf y
updateTree _ (Node _ t1 t2) 0 y = Node y t1 t2
updateTree w (Node x t1 t2) i y = if i < w `div` 2
    then Node x (updateTree (w `div` 2) t1 (i - 1) y) t2
    else Node x t1 (updateTree (w `div` 2) t2 (i - 1 - w `div` 2) y)
updateTree _ _ _ _ = error "Index too large!"

fromList :: [a] -> RList a
fromList = foldr cons []
