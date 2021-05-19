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

putSeqs :: HasSeqState m => [Seq] -> m ()
putSeqs seqs' = do
  seqs <- getSeqState
  putSeqState (seqs { seqs = seqs' })

resetSeq :: HasSeqState m => m ()
resetSeq = do
  seqs <- getSeqState
  putSeqState (seqs { seq = -1 })

incSeq :: HasSeqState m => m ()
incSeq = do
  seqs <- getSeqState
  putSeqState (seqs { seq = seq seqs + 1 })
