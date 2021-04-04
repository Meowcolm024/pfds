module Chapter7.BsQueue where
  -- BootstrappedQueue
import           QueueClass

data Queue a = Empty | Queue {
    front :: ![a],
    mid :: Queue [a],
    lenFM :: !Int,
    rear :: ![a],
    lenR :: !Int
} deriving Show

instance QueueClass Queue where
    isEmpty Empty = True
    isEmpty _     = False

    empty = Empty

    snoc Empty x = Queue [x] Empty 1 [] 0
    snoc (Queue f m lenfm r lenr) x =
        queue $ Queue f m lenfm (x : r) (lenr + 1)

    hd (Queue (x : _) _ _ _ _) = x
    hd _                       = error "Empty queue!"

    tl (Queue (_ : f) m lenfm r lenr) = queue $ Queue f m (lenfm - 1) r lenr
    tl _                              = error "Empty queue!"

    queue Empty                      = Empty
    queue q@(Queue f m lenfm r lenr) = if lenr <= lenfm
        then checkF q
        else checkF $ Queue f (snoc m (reverse r)) (lenfm + lenr) [] 0
      where
        checkF :: Queue a -> Queue a
        checkF (Queue [] Empty _     _ _   ) = Empty
        checkF (Queue [] m     lenfm r lenr) = Queue (hd m) (tl m) lenfm r lenr
        checkF q                             = q
