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

h = 0.1
domain_ = DM.rowMajor $ DM.generateDomain (-1,-1,-1) (1, 1, 1) h
mbulb = doBulb 8 0 256 4.0

mbulb_ :: () -> (Vect -> IterationCount)
mbulb_ () = mbulb

remotable ['mbulb_]

settings = Settings {  chunkSize_ = 1000, outputPath_ = "", numNodes_ = -1}

master :: [NodeId] -> Process ()
master = masterProcess2 settings ($(mkClosure 'mbulb_) ()) domain_


main :: IO ()
main = do
  start <- getCPUTime
  distribMain master Main.__remoteTable
  end <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double))
{-
h = 0.1

--domain_ = DM.rowMajor $ DM.generateDomain (0, 0, 0) (0.5,0.5,1) 0.01
domain_ = DM.rowMajor $ DM.generateDomain (-1,-1,-1) (1, 1, 1) h
mbulb = doBulb 8 0 256 4.0

mbulb_ :: () -> (Vect -> IterationCount)
mbulb_ () = mbulb

remotable ['mbulb_]

master :: [NodeId] -> Process ()
master = masterProcess ($(mkClosure 'mbulb_) ()) domain_ h

{-
main :: IO ()
main = do
  start <- getCPUTime
  distribMain master Main.__remoteTable
  end<- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  putStrLn $  "Computation time: " ++ (show (diff :: Double))
-}
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

mapColor :: Float -> (Float, Float, Float)
mapColor i = CM.heightToRGB8 2.0 (i, i, i)
  where j = i + 2.0

countToColorString :: Float -> String
countToColorString = vecToString . mapColor

vecToString :: Show a => (a, a, a) -> String
vecToString (a, b, c) = (show a) ++ "," ++ (show b) ++ "," ++ (show c)

iterToRgbString :: Integer -> String
iterToRgbString i = show $ i

toCSVLine :: (Integer, Vect) -> String
toCSVLine (i, v@(_, _, z)) = (vecToString v) ++ "," ++ (countToColorString z) ++ "\n"

{-
toCSVLine :: (Integer, Vect) -> String
toCSVLine (i, v) = (vecToString v) ++ "," ++ (iterToRgbString i) ++ "\n"
-}
Integer
main :: IO ()
main = do
  let fullData = map mbulb domain_
  let zipped = zip fullData domain_
  let dataS = filter (\(i, _) -> i == 256) zipped
  let vec = map snd dataS
  putStrLn $ show $ length vec
  putStrLn $ show $ length fullData
  let ctrl = initStructure
  putStrLn $ show ctrl
  --putStrLn $ show $ fullData
  outh <- openFile "./data.txt" WriteMode
  --hPutStrLn outh $ "x, y, z, c\n" ++ (concat (map toCSVLine dataS))
  hPutStrLn outh $ (concat (map toCSVLine dataS))
  hClose outh
  --plot' [] X11 $ Data3D [(Title "Test"), (Style Dots)] [] vec
  return ()
-}
