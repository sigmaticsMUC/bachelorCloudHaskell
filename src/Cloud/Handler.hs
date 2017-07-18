module Handler(

)where

import Cloud.Type
import Cloud.Utils.DistControlStruct


handlerProcess :: Settings -> [Vect] -> Process DistControlStruct
handlerProcess settings domain = do
  let ctrlStruct = initStructure settings domain
  resStruct <- handlerProcess ctrlStruct
  return resStruct


handlerProcess' :: DistControlStruct -> Process DistControlStruct
handlerProcess' ctrl = do
  response <- expect
  ctrlUpdate <- handleResponse response ctrl
  if isFinished ctrlUpdate
    then return ctrlUpdate
    else handlerProcess' ctrlUpdate

handleResponse :: MSG -> DistControlStruct -> Process DistControlStruct
handleResponse response ctrl = case response of
  RESPONSE2 (pid, tid, res) -> do
    let updateCtrl <- feadSlave (insertResponse res (removeTaskComplete tid ctrl)) pid
    return updateCtrl
  _ -> return ctrl

feadSlave :: ProcessId -> DistControlStruct -> Process DistControlStruct
feadSlave pid ctrl = do
  us <- getSelfPid
  let currentTask = if (numOpenTasks_ ctrl) /= 0 then (openTasks_ ctrl) else []
  case currentTask of
    [] -> return ctrl
    task -> do
      send ARG(us, task)
      let updateCtrl = insertRunningTask task
      return updateCtrl
