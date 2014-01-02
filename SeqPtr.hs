{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
module SeqPtr where

import Data.Maybe(maybeToList,listToMaybe)

data SeqDir = Prev | Next deriving Eq

otherSeqDir :: SeqDir -> SeqDir
otherSeqDir Prev = Next
otherSeqDir Next = Prev

data SeqPtr a = SeqPtr { seqBefore :: [a], seqCur :: Maybe a, seqAfter :: [a] }

instance Functor SeqPtr where
  fmap f SeqPtr { .. } = SeqPtr { seqBefore = fmap f seqBefore
                                , seqCur    = fmap f seqCur
                                , seqAfter  = fmap f seqAfter
                                }

seqEmpty :: SeqPtr a
seqEmpty = SeqPtr { seqBefore = [], seqCur = Nothing, seqAfter = [] }

newSeq :: Ordering -> [a] -> [a] -> [a] -> SeqPtr a
newSeq side before after new =
  case side of
    LT -> SeqPtr { seqBefore = before
                 , seqCur    = Nothing
                 , seqAfter  = new ++ after }

    EQ -> SeqPtr { seqBefore = before
                 , seqCur    = listToMaybe new
                 , seqAfter  = drop 1 new ++ after
                 }

    GT -> SeqPtr { seqBefore = reverse new ++ before
                 , seqCur    = Nothing
                 , seqAfter  = after }


seqExtract :: SeqDir -> SeqPtr a -> Maybe (a, SeqPtr a)

seqExtract Prev SeqPtr { .. } =
  case seqBefore of
    []     -> Nothing
    x : xs -> Just (x, SeqPtr { seqBefore = xs, .. })

seqExtract Next SeqPtr { .. } =
  case seqAfter of
    []     -> Nothing
    x : xs -> Just (x, SeqPtr { seqAfter = xs, .. })



seqMoves :: [SeqDir] -> SeqPtr a -> SeqPtr a
seqMoves ms s = foldl (flip seqMove) s ms

seqMove :: SeqDir -> SeqPtr a -> SeqPtr a
seqMove Prev SeqPtr { .. } =
  case seqCur of
    Just a  -> SeqPtr { seqAfter = a : seqAfter, seqCur = Nothing, .. }
    Nothing ->
      case seqBefore of
        []     -> SeqPtr { .. }
        a : as -> SeqPtr { seqBefore = as
                         , seqCur    = Just a
                         , ..
                         }

seqMove Next SeqPtr { .. } =
  case seqCur of
    Just a -> SeqPtr { seqBefore = a : seqBefore, seqCur = Nothing, .. }
    Nothing ->
      case seqAfter of
        []     -> SeqPtr { .. }
        a : as -> SeqPtr { seqAfter = as
                         , seqCur   = Just a
                         , ..
                         }

seqSet :: Maybe a -> SeqPtr a -> SeqPtr a
seqSet a SeqPtr { .. } = SeqPtr { seqCur = a, .. }

seqToList :: SeqPtr a -> [a]
seqToList SeqPtr { .. } = jnR seqBefore (maybeToList seqCur ++ seqAfter)

seqFromList :: [a] -> SeqPtr a
seqFromList seqAfter = SeqPtr { seqBefore = [], seqCur = Nothing, .. }


--------------------------------------------------------------------------------
-- Append a reversed and a normal list
jnR :: [a] -> [a] -> [a]
jnR xs ys = foldl (flip (:)) ys xs

