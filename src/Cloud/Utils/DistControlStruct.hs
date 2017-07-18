{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Utils.DistControlStruct(
  DistControlStruct (DistControlStruct, timeStamps_, runningTasks_,
    openTasks_, responses_, numOpenRunns_, numOpenTasks_, numResponses_),
  empty,
  initStructure,
  insertTimeStamp,
  insertResponse,
  insertRunningTask,
  removeOpenTask,
  removeOpenTask
)where

import Cloud.Type
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Data.List.Split (chunksOf)


data DistControlStruct  = DistControlStruct {
  timeStamps_ :: [TimeStamp],
  runningTasks_ :: [Task],
  openTasks_ :: [Task],
  responses_ :: [[Result]],
  numResponses_ :: Int,
  numOpenTasks_ :: Int,
  numOpenRunns_ :: Int
}
  deriving(Show, Eq, Typeable, Generic)


empty :: DistControlStruct
empty = DistControlStruct {
  timeStamps_ = [],
  runningTasks_ = [],
  openTasks_ = [],
  responses_ = [[]],
  numResponses_ = 0,
  numOpenTasks_ = 0,
  numOpenRunns_ = 0
}

initStructure :: Settings -> [Vect] -> DistControlStruct
initStructure settings domain = DistControlStruct {
    timeStamps_ = [],
    runningTasks_ = [],
    openTasks_ = openTasks,
    responses_ = [[]],
    numResponses_ = 0,
    numOpenTasks_ = numOpen,
    numOpenRunns_ = 0
  }
  where (numOpen, openTasks) = initTasks settings domain


initTasks :: Settings -> [Vect] -> (Int, [Task])
initTasks settings domain
  | (chunkSize_ settings) == -1 = initTasksWith (estimateChunkSize domain) domain
  | otherwise = initTasksWith (chunkSize_ settings) domain


initTasksWith :: Int -> [Vect] -> (Int, [Task])
initTasksWith size domain = (numTasks ,zipWith (\x y -> Task {taskId_ = y, taskData_ = x}) splices ids)
  where numAs = length domain
        spliceLength = div numAs size
        splices = chunksOf spliceLength domain :: [[Vect]]
        numTasks = (length splices)
        ids = take numTasks [0..]



-- INSERT FUNCTIONS --


insertTimeStamp :: TimeStamp -> DistControlStruct -> DistControlStruct
insertTimeStamp t s = DistControlStruct {
    timeStamps_ = t : (timeStamps_ s),
    runningTasks_ = runningTasks_ s,
    openTasks_ = openTasks_ s,
    responses_ = responses_ s,
    numResponses_ = numResponses_ s,
    numOpenTasks_ = numOpenTasks_ s,
    numOpenRunns_ = numOpenRunns_ s
  }

insertRunningTask :: Task -> DistControlStruct -> DistControlStruct
insertRunningTask t s = DistControlStruct {
    timeStamps_ = timeStamps_ s,
    runningTasks_ = t : (runningTasks_ s),
    openTasks_ = openTasks_ s,
    responses_ = responses_ s,
    numResponses_ = numResponses_ s,
    numOpenTasks_ = numOpenTasks_ s,
    numOpenRunns_ = (numOpenTasks_ s) + 1
  }

insertResponse :: [Result] -> DistControlStruct -> DistControlStruct
insertResponse t s = DistControlStruct {
    timeStamps_ = timeStamps_ s,
    runningTasks_ = runningTasks_ s,
    openTasks_ = openTasks_ s,
    responses_ = t : (responses_ s),
    numResponses_ = (numResponses_ s) + 1,
    numOpenTasks_ = numOpenTasks_ s,
    numOpenRunns_ = numOpenTasks_ s
  }


-- REMOVE FUNCTIONS --

removeRunningTask :: ID -> DistControlStruct -> DistControlStruct
removeRunningTask id' s = DistControlStruct {
    timeStamps_ = timeStamps_ s,
    runningTasks_ = filter (\task -> (taskId_ task) /= id') (runningTasks_ s),
    openTasks_ = openTasks_ s,
    responses_ = responses_ s,
    numResponses_ = numResponses_ s,
    numOpenTasks_ = numOpenTasks_ s,
    numOpenRunns_ = (numOpenTasks_ s) - 1
  }

removeOpenTask :: ID -> DistControlStruct -> DistControlStruct
removeOpenTask id' s = DistControlStruct {
    timeStamps_ = timeStamps_ s,
    runningTasks_ = runningTasks_ s,
    openTasks_ = filter (\task -> (taskId_ task) /= id')  (openTasks_ s),
    responses_ = responses_ s,
    numResponses_ = numResponses_ s,
    numOpenTasks_ = (numOpenTasks_ s) - 1,
    numOpenRunns_ = numOpenTasks_ s
  }

removeTaskComplete :: ID -> DistControlStruct -> DistControlStruct
removeTaskComplete id' s = DistControlStruct {
    timeStamps_ = timeStamps_ s,
    runningTasks_ = filter (\task -> (taskId_ task) /= id') (runningTasks_ s),
    openTasks_ = filter (\task -> (taskId_ task) /= id')  (openTasks_ s),
    responses_ = responses_ s,
    numResponses_ = numResponses_ s,
    numOpenTasks_ = (numOpenTasks_ s) - 1,
    numOpenRunns_ = (numOpenTasks_ s) - 1
  }


-- HELPER FUCNTIONS --

estimateChunkSize :: [Vect] -> Int
estimateChunkSize domain = undefined


isFinished :: DistControlStruct -> Bool
isFinished s
  | (numOpenTasks_ s) == 0 && (numOpenRunns_ s) == 0 = True
  | otherwise = False
