{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad

import Cloud.Utils.DistribUtils

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

--import qualified Cloud.Test.Kernel as CK
--import qualified Cloud.Test.Clsering as CC
import qualified Cloud.Test.Splitting as CS



f :: Double -> Double
f x = x + 1

f_ :: () -> (Double -> Double)
f_ () = f

a :: [Double]
a = [1..91]

remotable ['f_]

master = CS.mProcess ($(mkClosure 'f_) ()) a

main :: IO ()
--main = distribMain master Main.__remoteTable
--main = CK.main
--main = CC.main
main = do
  let rtable = CS.__remoteTable . Main.__remoteTable
  distribMain master rtable
