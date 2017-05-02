{-# LANGUAGE TemplateHaskell #-}
module Main where

import MandelBulb.Math.Core (doBulb)
import Cloud.Utils.DistribUtils (distribMain)
import qualified MandelBulb.Utils.Domain as DM
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Cloud.Master (masterProcess)
import Cloud.Type (Vect, Result, MSG (ARG, RESPONSE))
import System.CPUTime



domain_ = DM.rowMajor $ DM.generateDomain (0,0,0) (1,1,1) 0.01
mbulb = doBulb 0 256 4.0

mbulb_ :: () -> (Vect -> Result)
mbulb_ () = mbulb

remotable ['mbulb_]

master :: [NodeId] -> Process ()
master = masterProcess ($(mkClosure 'mbulb_) ()) domain_


main :: IO ()
main = do
  start <- getCPUTime
  distribMain master Main.__remoteTable
  end<- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double))
