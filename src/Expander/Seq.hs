-- |

module Expander.Seq where

import RIO hiding (Seq, seq)

type Seq = Int
data SeqState = SeqState { seq :: Seq, seqs :: [Seq] }

seqStateWith start = SeqState { seq = start, seqs = [] }

class Monad m => HasSeqState m where
  getSeqState :: m SeqState
  putSeqState :: SeqState -> m ()


getSeq :: HasSeqState m => m Seq
getSeq = seq <$> getSeqState

getSeqs :: HasSeqState m => m [Seq]
getSeqs = seqs <$> getSeqState

modifySeqTemporarily :: HasSeqState m => (Seq -> Seq) -> m a -> m a
modifySeqTemporarily f ma = do
  seqS <- getSeqState
  modifySeq f
  a <- ma
  putSeqState seqS
  return a

modifySeqsTemporarily :: HasSeqState m => ([Seq] -> [Seq]) -> m a -> m a
modifySeqsTemporarily f ma = do
  seqS <- getSeqState
  modifySeqs f
  a <- ma
  putSeqState seqS
  return a

modifySeqs :: HasSeqState m => ([Seq] -> [Seq]) -> m ()
modifySeqs f = do
  seqS <- getSeqState
  seqs <- getSeqs
  putSeqState $ seqS { seqs = f seqs }

modifySeq :: HasSeqState m => (Seq -> Seq) -> m ()
modifySeq f =  do
  seqS <- getSeqState
  seq <- getSeq
  putSeqState $ seqS { seq = f seq }
