{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric#-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Type(
  Vect,
  Result,
  MSG (ARG, RESPONSE)
)where


import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Control.Distributed.Process hiding (Message)


type Vect = (Double, Double, Double)
type Result = Integer

data MSG = ARG (ProcessId, [Vect]) | RESPONSE (ProcessId, [(Vect, Result)])
  deriving (Typeable, Generic)

instance Binary MSG
