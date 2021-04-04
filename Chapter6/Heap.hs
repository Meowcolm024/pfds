{-# LANGUAGE GADTs #-}

module Chapter6.Heap where

data Tree a where
  Node :: Ord a => Int -> a -> [Tree a] -> Tree a

type Heap a = [Tree a]

instance Show a => Show (Tree a) where
    show (Node _ v []) = " " ++ show v ++ " "
    show (Node _ v xs) = show v ++ " -< " ++ concatMap show xs ++ " > "

rank :: Tree a -> Int
rank (Node r _ _) = r

root :: Tree a -> a
root (Node _ x _) = x

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

link :: Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2 then Node (r + 1) x1 (t2 : c1) else Node (r + 1) x2 (t1 : c2)

insTree :: Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 ts@(t2 : rest) =
    if rank t1 < rank t2 then t1 : ts else insTree (link t1 t2) rest

insert :: Ord a => a -> Heap a -> Heap a
insert x = insTree (Node 0 x [])

merge :: Heap a -> Heap a -> Heap a
merge ts [] = ts
merge [] ts = ts
merge (t1 : ts1) (t2 : ts2) | r1 < r2   = t1 : merge ts1 (t2 : ts2)
                            | r2 < r1   = t2 : merge (t1 : ts1) ts2
                            | otherwise = insTree (link t1 t2) (merge ts1 ts2)
  where
    r1 = rank t1
    r2 = rank t2

findMin :: Ord a => Heap a -> a
findMin []  = error "Empty Heap!"
findMin [t] = root t
findMin (t : ts) =
    let x = root t
        y = findMin ts
    in  if x <= y then x else y

deleteMin :: Ord a => Heap a -> Heap a
deleteMin [] = error "Empty Heap!"
deleteMin ts = let (Node _ _ ts1, ts2) = getMin ts in merge (reverse ts1) ts2
  where
    getMin [t] = (t, [])
    getMin (t : ts) =
        let (t', ts') = getMin ts
        in  if root t <= root t' then (t, ts) else (t', t : ts')
    getMin _ = undefined

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert empty
