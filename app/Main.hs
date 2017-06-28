{-# LANGUAGE TemplateHaskell #-}
module Main where

import MandelBulb.Math.Core (doBulb, norm)
import Cloud.Utils.DistribUtils (distribMain)
import qualified MandelBulb.Utils.Domain as DM
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad
import Cloud.Master (masterProcess)
import Cloud.Type (Vect, IterationCount, Result, MSG (ARG, RESPONSE))
import System.CPUTime
import Graphics.EasyPlot
import System.IO

h = 0.01

--domain_ = DM.rowMajor $ DM.generateDomain (0, 0, 0) (0.5,0.5,1) 0.01
domain_ = DM.rowMajor $ DM.generateDomain (-2,-2,-2) (2, 2, 2) h
mbulb = doBulb 8 0 256 4.0

mbulb_ :: () -> (Vect -> IterationCount)
mbulb_ () = mbulb

remotable ['mbulb_]

master :: [NodeId] -> Process ()
master = masterProcess ($(mkClosure 'mbulb_) ()) domain_ h


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

mapColor :: Double -> Vect
mapColor i = (20, j, 30)
  where j = i*i*i

countToColorString :: Double -> String
countToColorString = vecToString . mapColor

vecToString :: Vect -> String
vecToString (a, b, c) = (show a) ++ "," ++ (show b) ++ "," ++(show c) ++ " "

iterToRgbString :: Integer -> String
iterToRgbString i = show $ 256 * i + i

{-
toCSVLine :: (Integer, Vect) -> String
toCSVLine (i, v) = (vecToString v) ++ (countToColorString (norm v)) ++ "\n"
-}

toCSVLine :: (Integer, Vect) -> String
toCSVLine (i, v) = (vecToString v) ++ "," ++ (iterToRgbString i) ++ "\n"

{-
main :: IO ()
main = do
  let fullData = map mbulb domain_
  let zipped = zip fullData domain_
  let dataS = filter (\(i, _) -> i == 256) zipped
  let vec = map snd dataS
  putStrLn $ show $ length vec
  putStrLn $ show $ length fullData
  --putStrLn $ show $ fullData
  outh <- openFile "../test3.txt" WriteMode
  hPutStrLn outh $ "x, y, z, c\n" ++ (concat (map toCSVLine dataS))
  hClose outh
  --plot' [Interactive] X11 $ Data3D [(Title "Test"), (Style Dots)] [] vec
  return ()
-}
