{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Type(
  Vect,
  Result,
  IterationCount,
  MSG (ARG, RESPONSE)
)where


import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process hiding (Message)


type Vect = (Double, Double, Double)
type IterationCount = Integer
type Result = (Vect, IterationCount)

data MSG = ARG (ProcessId, [Vect]) | RESPONSE (ProcessId, [Result])
  deriving (Typeable, Generic)

instance Binary MSG
