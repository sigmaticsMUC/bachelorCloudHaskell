{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Type(
  Vect,
  Result,
  IterationCount,
  MSG (START, ARG, RESPONSE, EXIT, RESPONSE2),
  Task (taskId_, taskData_, Task),
  TimeStamp (TimeStamp, stampId_, comptime_, waittime_),
  Settings (Settings, chunkSize_, outputPath_, numNodes_),
  ID
)where


import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process (ProcessId)


type Vect = (Float, Float, Float)
type IterationCount = Integer
type Result = (Vect, IterationCount)

data MSG = START ProcessId | ARG (ProcessId, Task) | RESPONSE (ProcessId, [Result]) | EXIT | RESPONSE2 (ProcessId, ID, TimeStamp, [Result])
  deriving (Show, Typeable, Generic)

type ID = Int
type Time = Double

data TimeStamp = TimeStamp { stampId_ :: ID, comptime_ :: Time, waittime_ :: Time }
  deriving (Eq, Typeable, Generic)

data Task = Task { taskId_ :: ID, taskData_ :: [Vect]}
  deriving (Show, Eq, Typeable, Generic)

data Settings = Settings {
  chunkSize_ :: Int,
  outputPath_ :: String,
  numNodes_ :: Int -- -1 if using all nodes
}

instance Binary MSG
instance Binary TimeStamp
instance Binary Task


instance Show TimeStamp where
  show t = "ID: " ++ (show $ stampId_ t) ++ "\nCOMPTIME: " ++ (show $ comptime_ t) ++ "\nWAITTIME: " ++ (show $ waittime_ t) ++ "\n\n"
