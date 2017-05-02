{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Test.Gen(
  MSG (MSG)
)where

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

data MSG a = MSG (ProcessId, [a])
  deriving (Typeable, Generic, Show)

instance Binary a => Binary (MSG a)

mCoreProcess :: (Binary a, Binary b, Typeable a, Typeable b) => (ProcessId, Closure(a->b)) -> Process ()
mCoreProcess (master, cF) = do
  us <- getSelfPid
  f <- unClosure cF
  liftIO $ putStrLn "Waiting for input..."
  MSG (_, args) <- expect
  let res = fmap f args
  send master (MSG (us, res))

$(remotable ['mCoreProcess])


spawnProcesses :: (Binary a, Binary b, Typeable a, Typeable b) => ProcessId -> Closure (a->b) -> [NodeId] -> Process [ProcessId]
spawnProcesses master cF nodes = do
  pids <- forM nodes $ \nid -> do
    liftIO $ putStrLn $ "spawning on" ++ (show nid)
    let closure = ($(mkClosure 'mCoreProcess) (master, cF))
    spawn nid closure
  return pids


distribData :: (Binary a, Typeable a) => ProcessId -> [a] -> [ProcessId] -> Process ()
distribData master args pids = do
  let numAs = length args
  let numNs = length pids
  let spliceLength = div numAs numNs
  let splices = chunksOf spliceLength args
  let spliceData = zip splices pids
  forM_ spliceData $ \(args', pid) -> do
    liftIO $ putStrLn $ "sending data to " ++ (show pid)
    send pid (MSG (master, args'))
  return()


mProcess :: (Binary a, Binary b, Typeable a, Typeable b) => Closure (a->b) -> [a] -> [NodeId] -> Process [b]
mProcess cF args nodes = do
  master <- getSelfPid
  liftIO $ putStr "start spawning processes... "
  ps <- spawnProcesses master cF nodes
  liftIO $ putStrLn "Done!\nsending data"
  distribData master args ps
  res <- waitForChuncks ps [[]]
  let ans = concat res
  liftIO $ putStrLn "Computation done!"
  liftIO $ putStrLn $ show ans
  return ans


waitForChuncks :: [ProcessId] -> [[b]] -> Process ([[b]])
waitForChuncks [] ls = return ls
waitForSplices pids ls = undefined
