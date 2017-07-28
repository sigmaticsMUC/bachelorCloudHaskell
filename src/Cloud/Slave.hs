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
import System.IO




slaveProcess2 :: Closure(Vect->IterationCount) -> Process ()
slaveProcess2 cF = do
  liftIO $ display "### PROCESS SPAWNED ###"
  f <- unClosure cF
  slaveProcess' f
  liftIO $ display "### PROCESS CLOSED ###"
  return ()

slaveProcess' :: (Vect->IterationCount) -> Process ()
slaveProcess' f = do
  liftIO $ display "-> WAITING FOR INPUT"
  start <- liftIO getCPUTime
  command <- expect
  end <- liftIO getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  liftIO $ display $ "-> RECIEVED INPUT: " ++ (show diff)
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
  liftIO $ display $ "-> RUNNING TASK: " ++ (show $ taskId_ task)
  start <- liftIO $ getCPUTime
  results <- forM (taskData_ task) $ \vec -> do
    let result = f vec
    return (vec, result)
  let response = filter (\(_, i) -> i == 256) results
  end <- liftIO $ getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  liftIO $ display $  "-> Computation time: " ++ (show (diff :: Double))
  liftIO $ display $ "-> SENDING RESPONSE"
  start2 <- liftIO $ getCPUTime
  send master (RESPONSE2 (us, (taskId_ task), response))
  end2 <- liftIO $ getCPUTime
  let diff2 = (fromIntegral (end2 - start2)) / (10^12)
  liftIO $ display $  "-> Sending time: " ++ (show (diff2 :: Double))
  liftIO $ display $ "\n"
  slaveProcess' f


display :: String -> IO ()
display info = do
    hSetBuffering stdout NoBuffering
    putStrLn info
    return ()

remotable ['slaveProcess2]
