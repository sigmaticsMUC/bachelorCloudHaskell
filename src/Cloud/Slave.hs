{-# LANGUAGE TemplateHaskell#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Slave(
  slaveProcess2,
  --__remoteTable
)where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Data.List.Split
import Cloud.Type (Vect, IterationCount, Result,
  MSG (ARG, RESPONSE, RESPONSE2, EXIT, START), Task (taskId_, taskData_, Task))
import System.CPUTime


slaveProcess2 :: Closure(Vect->IterationCount) -> Process ()
slaveProcess2 cF = do
  liftIO $ putStrLn "### PROCESS SPAWNED ###"
  f <- unClosure cF
  slaveProcess' f
  liftIO $ putStrLn "### PROCESS CLOSED ###"
  return ()

slaveProcess' :: (Vect->IterationCount) -> Process ()
slaveProcess' f = do
  liftIO $ putStrLn "-> WAITING FOR INPUT"
  command <- expect
  us <- getSelfPid
  case command of
    ARG arg -> runSlaveTask arg f
    START m -> startSlave f m
    _ -> return ()

startSlave :: (Vect->IterationCount) -> ProcessId -> Process ()
startSlave f master = do
  us <- getSelfPid
  send master (START us)
  slaveProcess' f
  return ()

runSlaveTask :: (ProcessId, Task) -> (Vect->IterationCount) -> Process ()
runSlaveTask (master, task) f = do
  us <- getSelfPid
  liftIO $ putStr $ "-> RUNNING TASK: " ++ (show $ taskId_ task)
  results <- forM (taskData_ task) $ \vec -> do
    let result = f vec
    return (vec, result)
  send master (RESPONSE2 (us, (taskId_ task), results))
  liftIO $ putStr $ " :: DONE \n"
  slaveProcess' f

remotable ['slaveProcess2]
