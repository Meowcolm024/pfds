module QueueClass where

class QueueClass q where
    empty :: q a
    isEmpty :: q a -> Bool
    hd :: q a -> a
    tl :: q a -> q a
    snoc :: q a -> a -> q a
    queue :: q a -> q a

    fromList :: [a] -> q a
    fromList = foldl snoc empty
