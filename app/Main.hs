{-# LANGUAGE TemplateHaskell #-}

module Main where


import MandelBulb.Math.Core (doBulb, norm)
import Cloud.Utils.DistribUtils (distribMain)
import qualified MandelBulb.Utils.Domain as DM
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import IOUtils.ColorMap as CM
import Cloud.Master (masterProcess, masterProcess2)
--import Cloud.Type (Vect, IterationCount, Result, MSG (RESPONSE), DistControlStruct, Task, TimeStamp, initStructure)
import System.CPUTime
import Graphics.EasyPlot
import System.IO
import Codec.Picture
import Cloud.Type
import Cloud.Utils.DistControlStruct

h = 0.01
domain_ = DM.rowMajor $ DM.generateDomain (-1,-1,-1) (0, 0, 0) h
mbulb = doBulb 8 0 256 4.0

mbulb_ :: () -> (Vect -> IterationCount)
mbulb_ () = mbulb

remotable ['mbulb_]

settings = Settings {  chunkSize_ = 10000, outputPath_ = "DD2", numNodes_ = -1}


test :: IO ()
test = do
  start <- getCPUTime
  let i = mbulb (0.1, 0.1, 0.1)
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double)) ++ " with " ++ (show i)

master :: [NodeId] -> Process ()
master = masterProcess2 settings ($(mkClosure 'mbulb_) ()) domain_


main :: IO ()
main = distribMain master Main.__remoteTable
