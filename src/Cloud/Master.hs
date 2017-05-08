{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Cloud.Master(
  masterProcess
)where

import Control.Distributed.Process hiding (Message)

import Cloud.Type (Vect, IterationCount, Result, MSG (RESPONSE))
import Cloud.Kernel (spawnProcesses, distribute)

masterProcess :: Closure(Vect->IterationCount) -> [Vect] -> [NodeId] -> Process ()
masterProcess cF args nodes = do
  master <- getSelfPid
  say "spawning processes..."
  ps <- spawnProcesses master cF nodes
  say $ (show $ length nodes) ++ " processes spawned!"
  distribute master args ps
  response <- waitForChuncks ps [[]]
  let result = filter (\(_, i) -> i /= 256) (concat response)
  say (show result)
  return ()

waitForChuncks :: [ProcessId] -> [[Result]] -> Process ([[Result]])
waitForChuncks [] ls = return ls
waitForChuncks pids ls = do
  m <- expect
  case m of
    RESPONSE (p, l) -> waitForChuncks (filter (/= p) pids) (l : ls)
    _ -> say "MASTER ending" >> terminate
