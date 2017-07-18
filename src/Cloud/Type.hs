{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Type(
  Vect,
  Result,
  IterationCount,
  MSG (ARG, RESPONSE, EXIT),
  Task (taskId_, taskData_, Task),
  TimeStamp (TimeStamp),
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

data MSG = ARG (ProcessId, Task) | RESPONSE (ProcessId, [Result]) | EXIT | RESPONSE2 (ProcessId, ID, [Result])
  deriving (Typeable, Generic)

type ID = Int
type Comptime = Double

data TimeStamp = TimeStamp { stampId_ :: ID, comptime_ :: Comptime }
  deriving (Show, Eq, Typeable, Generic)

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
