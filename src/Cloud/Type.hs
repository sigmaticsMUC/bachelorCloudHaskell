{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Type(
  Vect,
  Result,
  IterationCount,
  MSG (ARG, RESPONSE),
  DistControlStruct(timeStamps_, runningTasks_, openTasks_, responses_, DistControlStruct),
  Task (Task),
  TimeStamp (TimeStamp),
  initStructure
)where


import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process hiding (Message)


type Vect = (Float, Float, Float)
type IterationCount = Integer
type Result = (Vect, IterationCount)

data MSG = ARG (ProcessId, (Vect, Vect)) | RESPONSE (ProcessId, [Result])
  deriving (Typeable, Generic)

type ID = Int
type Comptime = Double

data TimeStamp = TimeStamp { stampId_ :: ID, comptime_ :: Comptime }
  deriving(Show, Eq)
data Task = Task { taskId_ :: ID, domain_ :: (Vect, Vect)}
  deriving(Show, Eq)

data DistControlStruct  = DistControlStruct {
  timeStamps_ :: [TimeStamp],
  runningTasks_ :: [Task],
  openTasks_ :: [Task],
  responses_ :: [[Result]]
}
  deriving(Show, Eq)


initStructure :: DistControlStruct
initStructure = DistControlStruct {
  timeStamps_ = [],
  runningTasks_ = [],
  openTasks_ = [],
  responses_ = [[]]
}

instance Binary MSG
