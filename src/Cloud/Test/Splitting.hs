{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
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

data Message = Ping (ProcessId, [Double])
  deriving (Typeable, Generic)          -- <1>

instance Binary Message

mCoreProcess :: (ProcessId, Closure (Double->Double)) -> Process ()
mCoreProcess (master, cf) = do
  us <- getSelfPid
  f <- unClosure cf
  liftIO $ putStrLn "WAITING FOR INPUT"
  Ping (_, l) <- expect
  let res = map f l
  liftIO $ putStrLn $ show res
  send master (Ping (us, res))

f :: Double -> Double
f x = x + 1

f_ :: () -> (Double -> Double)
f_ () = f

a :: [Double]
a = [1..91]


--remotable ['mCoreProcess, 'f_]
remotable ['mCoreProcess]


spawnProcesses :: ProcessId -> Closure (Double->Double) -> [NodeId] -> Process [ProcessId]
spawnProcesses master cf nodes = do
  ps <- forM nodes $ \nid -> do
    liftIO $ putStrLn $ "spawning on " ++ (show nid)
    let cls = ($(mkClosure 'mCoreProcess) (master, cf))
    spawn nid cls
  return ps

distribData :: ProcessId -> [Double] -> [ProcessId] -> Process ()
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

mProcess :: Closure (Double->Double) -> [Double] -> [NodeId] -> Process ()
mProcess cf as nodes = do
  master <- getSelfPid
  liftIO $ putStrLn "spawning processes"
  ps <- spawnProcesses master cf nodes
  liftIO $ putStrLn "sending data"
  distribData master as ps
  result <- waitForSplices ps []
  liftIO $ putStrLn $ show result
  return ()

waitForSplices :: [ProcessId] -> [Double] -> Process ([Double])
waitForSplices [] ls = return ls
waitForSplices pids ls = do
  m <- expect
  case m of
    Ping (p, l) -> waitForSplices (filter (/= p) pids) (ls ++ l)
    _ -> say "MASTER ending" >> terminate

--main = mProcess ($(mkClosure 'f_) ()) a
