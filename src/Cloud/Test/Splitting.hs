{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Test.Splitting(
  mProcess,
  mCoreProcess,
  --main,
  __remoteTable
) where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad


import Cloud.Utils.DistribUtils

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

import Data.List.Split
import Data.List

import System.IO

type Index = Integer

type Vect = (Double, Double, Double)
type Iter = Integer


data Message = Ping (ProcessId, [Vect]) | Pong (ProcessId, [Iter])
  deriving (Typeable, Generic)          -- <1>

instance Binary Message

mCoreProcess :: (ProcessId, Closure (Vect->Iter)) -> Process ()
mCoreProcess (master, cf) = do
  us <- getSelfPid
  f <- unClosure cf
  liftIO $ putStrLn "WAITING FOR INPUT"
  Ping (_, l) <- expect
  let res = map f l
  liftIO $ putStrLn $ show res
  send master (Pong (us, res))

--remotable ['mCoreProcess, 'f_]
remotable ['mCoreProcess]

spawnProcesses :: ProcessId -> Closure (Vect->Iter) -> [NodeId] -> Process [ProcessId]
spawnProcesses master cf nodes = do
  ps <- forM nodes $ \nid -> do
    liftIO $ putStrLn $ "spawning on " ++ (show nid)
    let cls = ($(mkClosure 'mCoreProcess) (master, cf))
    spawn nid cls
  return ps

distribData :: ProcessId -> [Vect] -> [ProcessId] -> Process ()
distribData master as pids = do
  let numAs = length as
  let numNs = length pids
  let spliceLength = div numAs numNs
  let splices = chunksOf spliceLength as
  let distrbData = zip splices pids
  forM_ distrbData $ \(as', pid) -> do
    liftIO $ putStrLn $ "sending data to " ++ (show pid)
    send pid (Ping (master, as'))
  return ()

mProcess :: Closure (Vect->Iter) -> [Vect] -> [NodeId] -> Process ()
mProcess cf as nodes = do
  master <- getSelfPid
  liftIO $ putStrLn "spawning processes"
  ps <- spawnProcesses master cf nodes
  liftIO $ putStrLn "sending data"
  distribData master as ps
  result <- waitForSplices ps [[]]
  let rslt = concat result
  liftIO $ putStrLn $ show rslt
  return ()

waitForSplices :: [ProcessId] -> [[Iter]] -> Process ([[Iter]])
waitForSplices [] ls = return ls
waitForSplices pids ls = do
  m <- expect
  case m of
    Pong (p, l) -> waitForSplices (filter (/= p) pids) (l : ls)
    _ -> say "MASTER ending" >> terminate

--main = mProcess ($(mkClosure 'f_) ()) a
