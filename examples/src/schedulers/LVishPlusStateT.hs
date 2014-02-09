{-# LANGUAGE TypeFamilies, DataKinds, Rank2Types #-}

-- | Test how much a (useless) StateT transformer screws up optimizations and adds
-- overheads, if at all.

module LVishPlusStateT
       (
         runPar, Par,
         module Control.Par.Class 
       ) where

import qualified Control.LVish as L 
import Control.Par.Class
import Control.Par.StateT
import qualified Control.Monad.State.Strict as S

type Par e s a = S.StateT () (L.Par e s) a

runPar :: (forall s . Par (L.Ef L.P L.G L.NF L.B L.NI) s a) -> a
runPar m =
  L.runPar $ 
  S.evalStateT m ()

instance SplittableState () where
  splitState () = ((),())
