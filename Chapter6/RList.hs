module RList where

data Tree a = Leaf a | Node Int (Tree a) (Tree a) deriving (Eq)

data Digit a = Zero | One (Tree a) deriving (Show, Eq)

type RList a = [Digit a]

instance (Show a) => Show (Tree a) where
    show (Leaf x    ) = " " ++ show x ++ " "
    show (Node _ l r) = show l ++ show r

empty :: RList a
empty = []

isEmpty :: RList a -> Bool
isEmpty = null

cons :: a -> RList a -> RList a
cons x ts = insTree (Leaf x) ts

insTree :: Tree a -> RList a -> RList a
insTree t  []            = [One t]
insTree t  (Zero   : ts) = One t : ts
insTree t1 (One t2 : ts) = Zero : insTree (link t1 t2) ts

link :: Tree a -> Tree a -> Tree a
link l@(Leaf _    ) r@(Leaf _    ) = Node 2 l r
link l@(Node x _ _) r@(Leaf _    ) = Node (x + 1) l r
link l@(Leaf _    ) r@(Node x _ _) = Node (x + 1) l r
link l@(Node x _ _) r@(Node y _ _) = Node (x + y) l r

borrowTree :: RList a -> (Tree a, RList a)
borrowTree [One v     ] = (v, [])
borrowTree (One t : ts) = (t, ts)
borrowTree (Zero : ts) =
    let ((Node _ t1 t2), ts') = borrowTree ts in (t1, One t2 : ts')
borrowTree _ = error "empty list"

head' :: RList a -> a
head' ts = let (Leaf x, _) = borrowTree ts in x

tail' :: RList a -> RList a
tail' ts = let (Leaf _, ts') = borrowTree ts in ts'

lookup' :: RList a -> Int -> a
lookup' (Zero : ts) i = lookup' ts i
lookup' (One t : ts) i =
    if i < size t then lookupTree t i else lookup' ts (i - size t)
lookup' _ _ = error "index too large"

size :: Tree a -> Int
size (Leaf _    ) = 1
size (Node x _ _) = x

lookupTree :: Tree p -> Int -> p
lookupTree (Leaf x) 0 = x
lookupTree (Node w t1 t2) i =
    if i < w `div` 2 then lookupTree t1 i else lookupTree t2 (i - w `div` 2)
lookupTree _ _ = error "index too large"

update :: RList a -> Int -> a -> RList a
update (Zero  : ts) i y = Zero : update ts i y
update (One t : ts) i y = if i < size t
    then One (updateTree t i y) : ts
    else One t : update ts (i - size t) y
update _ _ _ = error "index too large"

updateTree :: Tree a -> Int -> a -> Tree a
updateTree (Leaf _      ) 0 y = Leaf y
updateTree (Node w t1 t2) i y = if i < w `div` 2
    then Node w (updateTree t1 i y) t2
    else Node w t1 (updateTree t2 (i - w `div` 2) y)
updateTree _ _ _ = error "index too large"

fromList :: [a] -> RList a
fromList = foldr cons []
