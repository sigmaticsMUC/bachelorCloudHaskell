module Cloud.Handler(
  handlerProcess
)where

import Cloud.Type
import Cloud.Utils.DistControlStruct
import Control.Distributed.Process
import System.Console.ANSI
import System.IO
import System.CPUTime



handlerProcess :: Settings -> [Vect] -> Process DistControlStruct
handlerProcess settings domain = do
  let ctrlStruct = initStructure settings domain
  resStruct <- handlerProcess' ctrlStruct
  return resStruct

handlerProcess' :: DistControlStruct -> Process DistControlStruct
handlerProcess' ctrl = do
  startTime <- liftIO getCPUTime
  response <- expect
  endTime <- liftIO getCPUTime
  let diff = (fromIntegral (endTime - startTime)) / (10^12)
  --liftIO $ putStrLn $ "-> WAITING: " ++ (show diff)
  start <- liftIO getCPUTime
  ctrlUpdate <- handleResponse response ctrl
  end <- liftIO getCPUTime
  let diff2 = (fromIntegral (end - start)) / (10^12)
  --liftIO $ putStrLn $ "-> HANDLING TIME: " ++ (show diff2)
  --liftIO $ putStrLn $ show ctrlUpdate
  liftIO $ displayProgress ctrlUpdate
  --liftIO $ putStrLn ""
  if isFinished ctrlUpdate
    then return ctrlUpdate
    else handlerProcess' ctrlUpdate

handleResponse :: MSG -> DistControlStruct -> Process DistControlStruct
handleResponse response ctrl = case response of
  RESPONSE2 (pid, tid, timestamp, res) -> do
    let insertedCtrl = insertTimeStamp timestamp (insertResponse res (removeTaskComplete tid ctrl))
    updateCtrl <- feadSlave pid insertedCtrl
    return updateCtrl
  START pid -> do
    updateCtrl <- feadSlave pid ctrl
    return updateCtrl
  _ -> return ctrl

feadSlave :: ProcessId -> DistControlStruct -> Process DistControlStruct
feadSlave pid ctrl = do
  us <- getSelfPid
  let currentTask = if (numOpenTasks_ ctrl) > 0 then (openTasks_ ctrl) else []
  case currentTask of
    [] -> return ctrl
    tasks -> do
      let task = head tasks
      spawnLocal $ send pid (ARG(us, task))
      --send pid (ARG(us, task))
      let updateCtrl = removeOpenTask (taskId_ task) (insertRunningTask task ctrl)
      return updateCtrl



displayProgress :: DistControlStruct -> IO ()
displayProgress ctrl = do
  putStrLn $ "Tasks open: " ++ (show $ numOpenTasks_ ctrl)
  hFlush stdout
  return ()
