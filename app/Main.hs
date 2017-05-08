{-# LANGUAGE TemplateHaskell #-}
module Main where

import MandelBulb.Math.Core (doBulb)
import Cloud.Utils.DistribUtils (distribMain)
import qualified MandelBulb.Utils.Domain as DM
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Cloud.Master (masterProcess)
import Cloud.Type (Vect, IterationCount, Result, MSG (ARG, RESPONSE))
import System.CPUTime

import Graphics.EasyPlot



domain_ = DM.rowMajor $ DM.generateDomain (0,0,0) (1,1,1) 0.5
mbulb = doBulb 8 0 256 4.0

mbulb_ :: () -> (Vect -> IterationCount)
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


{-
main :: IO ()
main = do
  start <- getCPUTime
  let res = map mbulb domain_
  putStrLn $ show res
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double))
-}
{-
main :: IO ()
main = do
  plot X11 $ Data3D [(Title "Test"), (Style Dots)] [] [(1,1,1), (2,2,2)]
  return ()
-}
