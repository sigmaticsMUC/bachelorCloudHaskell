module Cloud.Handler(
  handlerProcess
)where

import Cloud.Type
import Cloud.Utils.DistControlStruct
import Control.Distributed.Process



handlerProcess :: Settings -> [Vect] -> Process DistControlStruct
handlerProcess settings domain = do
  let ctrlStruct = initStructure settings domain
  resStruct <- handlerProcess' ctrlStruct
  return resStruct

handlerProcess' :: DistControlStruct -> Process DistControlStruct
handlerProcess' ctrl = do
  response <- expect
  ctrlUpdate <- handleResponse response ctrl
  liftIO $ putStrLn $ show ctrlUpdate
  if isFinished ctrlUpdate
    then return ctrlUpdate
    else handlerProcess' ctrlUpdate

handleResponse :: MSG -> DistControlStruct -> Process DistControlStruct
handleResponse response ctrl = case response of
  RESPONSE2 (pid, tid, res) -> do
    updateCtrl <- feadSlave pid (insertResponse res (removeTaskComplete tid ctrl))
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
      send pid (ARG(us, task))
      let updateCtrl = removeOpenTask (taskId_ task) (insertRunningTask task ctrl)
      return updateCtrl
