{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Kernel(
  spawnProcesses,
  distribute,
  __remoteTable
)where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.List.Split
import Cloud.Type (Vect, Result, MSG (ARG, RESPONSE))


slaveProcess :: (ProcessId, Closure(Vect->Result)) -> Process ()
slaveProcess (master, cF) = do
  us <- getSelfPid
  f <- unClosure cF
  say "Waiting for input..."
  ARG (_, args) <- expect
  results <- forM args $ \vec -> do
    let result = f vec
    return (vec, result)
  send master (RESPONSE (us, results))

remotable ['slaveProcess]

spawnProcesses :: ProcessId -> Closure (Vect->Result) -> [NodeId] -> Process [ProcessId]
spawnProcesses master cF nodes = do
  pids <- forM nodes $ \nid -> do
    say $ "spawning on" ++ (show nid)
    let cSlave = ($(mkClosure 'slaveProcess) (master, cF))
    spawn nid cSlave
  return pids

distribute :: ProcessId -> [Vect] -> [ProcessId] -> Process ()
distribute master args pids = do
  splices <- chunkData args pids
  let spliceData = zip splices pids
  forM_ spliceData $ \(args', pid) -> do
    say $ "sending data to " ++ (show pid)
    send pid (ARG (master, args'))
  return()

chunkData :: [Vect] -> [ProcessId] -> Process [[Vect]]
chunkData args pids = do
  let numAs = length args
  let numNs = length pids
  let spliceLength = div numAs numNs
  let splices = chunksOf spliceLength args
  return splices
