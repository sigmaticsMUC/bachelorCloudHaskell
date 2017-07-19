{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Kernel(
  spawnProcesses,
  spawnProcesses2,
  distribute,
  __remoteTable
)where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.List.Split
import Cloud.Slave
import qualified MandelBulb.Utils.Domain as DM
import Cloud.Type (Vect, IterationCount, Result,
  MSG (ARG, RESPONSE, EXIT), Task (taskId_, taskData_, Task))


slaveProcess :: (ProcessId, Closure(Vect->IterationCount)) -> Process ()
slaveProcess (master, cF) = do
  us <- getSelfPid
  f <- unClosure cF
  ARG (_, args) <- expect
  results <- forM (taskData_ args) $ \vec -> do
    let result = f vec
    return (vec, result)
  send master (RESPONSE (us, results))


{-
slaveProcess :: (ProcessId, Float, Closure(Vect->IterationCount)) -> Process ()
slaveProcess (master, h, cF) = do
  us <- getSelfPid
  f <- unClosure cF
  say "Waiting for input..."
  ARG (_, (a, b)) <- expect
  let domain = DM.rowMajor $ DM.generateDomain a b h
  results <- forM domain $ \vec -> do
    let result = f vec
    return (vec, result)
  let results' = filter (\(_, i) -> i /= 255) results
  --liftIO $ putStrLn $ show $ results'
  send master (RESPONSE (us, results'))
-}

remotable ['slaveProcess, 'slaveProcess2]

spawnProcesses :: Float -> ProcessId -> Closure (Vect->IterationCount) -> [NodeId] -> Process [ProcessId]
spawnProcesses h master cF nodes = do
  pids <- forM nodes $ \nid -> do
    let cSlave = ($(mkClosure 'slaveProcess) (master, h, cF))
    spawn nid cSlave
  return pids

spawnProcesses2 :: ProcessId -> Closure (Vect->IterationCount) -> [NodeId] -> Process [ProcessId]
spawnProcesses2 master cF nids = do
  pids <- forM nids $ \nid -> do
    let cSlave = ($(mkClosure 'slaveProcess2) cF)
    spawn nid cSlave
  return pids

distribute :: ProcessId -> [Vect] -> [ProcessId] -> Process ()
distribute master args pids = do
  splices <- chunkData args pids
  let spliceData = zip splices pids
  forM_ spliceData $ \(args', pid) -> do
    --let a = head args'
    --let b = last args'
    --let b = (head . reverse) args'
    send pid (ARG (master, Task {taskId_ = 0, taskData_ = args'}))
  return()

chunkData :: [Vect] -> [ProcessId] -> Process [[Vect]]
chunkData args pids = do
  let numAs = length args
  let numNs = length pids
  let spliceLength = div numAs numNs
  let splices = chunksOf spliceLength args
  return splices
