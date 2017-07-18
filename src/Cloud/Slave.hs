{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Slave(
  slaveProcess,
  __remoteTable
)where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.List.Split
import Cloud.Type (Vect, IterationCount, Result,
  MSG (ARG, RESPONSE, EXIT), Task (taskId_, taskData_, Task))
import System.CPUTime


slaveProcess :: Closure(Vect->IterationCount) -> Process ()
slaveProcess cF = do
  f <- unClosure cF
  slaveProcess' f
  return ()

slaveProcess' :: (Vect->IterationCount) -> Process ()
slaveProcess' f = do
  command <- expect
  case command of
    ARG arg -> runSlaveTask arg f
    _ -> return ()

runSlaveTask :: (ProcessId, Task) -> (Vect->IterationCount) -> Process ()
runSlaveTask (master, task) f = do
  us <- getSelfPid
  results <- forM (taskData_ task) $ \vec -> do
    let result = f vec
    return (vec, result)
  send master (RESPONSE (us, results))
  slaveProcess' f

remotable ['slaveProcess]
