{-# LANGUAGE BangPatterns #-}

module Chapter3.PhysQueue where      -- Physicists Queue

data Queue a = Queue
  { w :: ![a],
    f :: [a],
    lenF :: Int,
    r :: [a],
    lenR :: Int
  }
  deriving (Show)

empty :: Queue a
empty = Queue [] [] 0 [] 0

isEmpty :: Queue a -> Bool
isEmpty Queue { lenF = l } = l == 0

checkW :: Queue a -> Queue a
checkW (Queue [] !f' lenF' r' lenR') = Queue f' f' lenF' r' lenR'
checkW q                             = q

checkR :: Queue a -> Queue a
checkR q@(Queue _ f' lenF' r' lenR') = if lenR' <= lenF'
    then q
    else
        let w' = f'
        in  w' `seq` Queue w' (w' ++ reverse r') (lenF' + lenR') [] 0

queue :: Queue a -> Queue a
queue = checkW . checkR

snoc :: Queue a -> a -> Queue a
snoc (Queue w' f' lenF' r' lenR') x =
    queue $ Queue w' f' lenF' (x : r') (lenR' + 1)

head' :: Queue a -> a
head' Queue { w = [] }      = error "Empty Queue!"
head' Queue { w = (x : _) } = x

tail' :: Queue a -> Queue a
tail' Queue { w = [] } = error "Empty Queue!"
tail' (Queue (_ : w') !f' lenF' r' lenR') =
    queue $ Queue w' (tail f') (lenF' - 1) r' lenR'

fromList :: [a] -> Queue a
fromList = foldl snoc empty
