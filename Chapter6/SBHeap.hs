{-# LANGUAGE GADTs #-}
module Chapter6.SBHeap where     -- Skew Binomial Heap

data Tree a where
    Node :: Ord a => Int -> a -> [a] -> [Tree a] -> Tree a

type Heap a = [Tree a]

instance Show a => Show (Tree a) where
    show (Node _ v vs []) = show v ++ " " ++ show vs
    show (Node _ v vs ts) =
        show v ++ " " ++ show vs ++ " -< " ++ concatMap show ts ++ " >- "

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

rank :: Tree a -> Int
rank (Node r _ _ _) = r

root :: Tree a -> a
root (Node _ x _ _) = x

link :: Tree a -> Tree a -> Tree a
link t1@(Node r x1 xs1 c1) t2@(Node _ x2 xs2 c2) = if x1 <= x2
    then Node (r + 1) x1 xs1 (t2 : c1)
    else Node (r + 1) x2 xs2 (t1 : c2)

skewLink :: Ord a => a -> Tree a -> Tree a -> Tree a
skewLink x t1 t2 =
    let Node r y ys c = link t1 t2
    in  if x < y then Node r x (y : ys) c else Node r y (x : ys) c

insert :: Ord a => a -> Heap a -> Heap a
insert x ts@(t1 : t2 : rest) =
    if rank t1 == rank t2 then skewLink x t1 t2 : rest else Node 0 x [] [] : ts
insert x ts = Node 0 x [] [] : ts

insTree :: Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t1 (t2 : ts) =
    if rank t1 < rank t2 then t1 : t2 : ts else insTree (link t1 t2) ts

normalize :: Heap a -> Heap a
normalize []       = []
normalize (t : ts) = insTree t ts

mergeTrees :: Heap a -> Heap a -> Heap a
mergeTrees ts1 []  = ts1
mergeTrees []  ts2 = ts2
mergeTrees (t1 : ts1) (t2 : ts2)
    | r1 < r2   = t1 : mergeTrees ts1 (t2 : ts2)
    | r2 < r1   = t2 : mergeTrees (t1 : ts1) ts2
    | otherwise = insTree (link t1 t2) (mergeTrees ts1 ts2)
  where
    r1 = rank t1
    r2 = rank t2

merge :: Heap a -> Heap a -> Heap a
merge ts1 ts2 = mergeTrees (normalize ts1) (normalize ts2)

findMin :: Ord a => Heap a -> a
findMin []  = error "Empty Heap!"
findMin [t] = root t
findMin (t : ts) =
    let x = root t
        y = findMin ts
    in  min x y

deleteMin :: Ord a => Heap a -> Heap a
deleteMin [] = error "Empty Heap!"
deleteMin ts =
    let (Node _ _ xs c, ts') = getMin ts
    in  insertAll xs (mergeTrees (reverse c) (normalize ts'))
  where
    getMin []  = error "Empty Heap!"
    getMin [t] = (t, [])
    getMin (t : ts) =
        let (t', ts') = getMin ts
        in  if root t <= root t' then (t, ts) else (t', t : ts')
    insertAll []       ts = ts
    insertAll (x : xs) ts = insertAll xs (insert x ts)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert []
