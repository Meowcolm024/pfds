module Queue where

data Queue a = Queue [a] [a] deriving (Show)

{- 
tailQ' :: Queue a -> Queue a
tailQ' (Queue [_] r) = Queue (reverse r) []
tailQ' (Queue (_ : xs) ys) = Queue xs ys

-- snoc <--> cons ; add from the back :)
snocQ' :: Queue a -> a -> Queue a
snocQ' (Queue [] _) x = Queue [x] []
snocQ' (Queue xs ys) y = Queue xs (y : ys)
 -}

emptyQ :: Queue a
emptyQ = Queue [] []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Queue x _) = null x

headQ :: Queue a -> a
headQ (Queue [] _) = undefined
headQ (Queue (x : _) _) = x

queueQ :: Queue a -> Queue a
queueQ (Queue [] x) = Queue (reverse x) []
queueQ q = q

snocQ :: Queue a -> a -> Queue a
snocQ (Queue xs ys) y = queueQ $ Queue xs (y : ys)

tailQ :: Queue a -> Queue a
tailQ (Queue [] _) = undefined
tailQ (Queue (_ : xs) ys) = queueQ $ Queue xs ys

listToQueue :: [a] -> Queue a
listToQueue [] = emptyQ
listToQueue xs =
  let (p, q) = splitAt (length xs `div` 2) xs
   in Queue p (reverse q)
