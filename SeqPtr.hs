{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
module SeqPtr where

import Data.Maybe(maybeToList)

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

