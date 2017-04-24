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
import MandelBulb.Math.Core (doBulb)
import qualified MandelBulb.Utils.Domain as DM


import System.CPUTime





f :: Double -> Double
f x = x * x

f_ :: () -> (Double -> Double)
f_ () = f

a :: [Double]
a = [1..20]

aa = map (\i -> (i, i, i)) a
n = 8

type Vect = (Double, Double, Double)


--v_ :: () -> (Vect -> Vect)
--v_ () = v n


domain_ = DM.rowMajor $ DM.generateDomain (0,0,0) (1,1,1) 0.01
mbulb = doBulb 0 256 4.0

mbulb_ :: () -> (Vect -> Integer)
mbulb_ () = mbulb


remotable ['f_, 'mbulb_]


master = CS.mProcess ($(mkClosure 'mbulb_) ()) domain_


main :: IO ()
--main = distribMain master Main.__remoteTable
--main = CK.main
--main = CC.main
main = do
  start <- getCPUTime
  let rtable = CS.__remoteTable . Main.__remoteTable
  distribMain master rtable
  end<- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double))
--main = do
  --let r = v 2 (1,1,1)
  --putStrLn $ show r
  --return ()
--main = do
  --let res = map mbulb domain_
  --putStrLn $ show res
