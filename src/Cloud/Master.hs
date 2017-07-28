{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Cloud.Master(
  masterProcess,
  masterProcess2
)where

import Control.Distributed.Process hiding (Message)
import Cloud.Type
import Cloud.Handler
import Cloud.Utils.DistControlStruct
import Cloud.Kernel (spawnProcesses, spawnProcesses2, distribute)
import System.IO
import Control.Monad
import Codec.Picture
import System.CPUTime
import Graphics.EasyPlot
import IOUtils.ColorMap as CM



vecToString :: Vect -> String
vecToString (a, b, c) = (show a) ++ "," ++ (show b) ++ "," ++(show c) ++ " "

iterToRgbString :: Integer -> String
iterToRgbString i = show $ 256 * i + i


toCSVLine :: (Vect, Integer) -> String
toCSVLine (v, i) = (vecToString v) ++ "," ++ (countToColorString (norm v)) ++ "\n"


mapColor :: Float -> (Float, Float, Float)
mapColor i = CM.heightToRGB8 2.5 (i, i, i)

countToColorString :: Float -> String
countToColorString l = show rgbInteger
  where (r, g, b) = getRGBs l
        rgbInteger = 65536 * r + 256 * g + b


getRGBs :: Float -> (Float, Float, Float)
getRGBs l = (0, 0, bpart)
  where bpart = l * 100

{-
toCSVLine :: (Vect, Integer) -> String
toCSVLine (v, i) = (vecToString v) ++ "," ++ (iterToRgbString $ norm v) ++ "\n"
-}

masterProcess :: Closure(Vect->IterationCount) -> [Vect] -> Float -> [NodeId] -> Process ()
masterProcess cF args h nodes = do
  master <- getSelfPid
  let ctrl = empty
  say "spawning processes..."
  ps <- spawnProcesses h master cF nodes
  say $ (show $ length nodes) ++ " processes spawned!"
  distribute master args ps
  response <- waitForChuncks ps ctrl
  --say (show result)
  liftIO $ writeToFile "./Default.txt" ("x, y, z, c\n" ++ (concat (map toCSVLine (concat (responses_ response)))))
  return ()


masterProcess2 :: Settings -> Closure(Vect->IterationCount) -> [Vect] -> [NodeId] -> Process ()
masterProcess2 s cF args nids = do
  start <- liftIO getCPUTime
  master <- getSelfPid
  ps <- spawnProcesses2 master cF (takeNodes s nids)
  forM_ ps $ \pid -> do
    send pid (START master)
  ctrl <- handlerProcess s args
  forM_ ps $ \pid -> do
    send pid EXIT
  --let results = concat (responses_ ctrl)
  let fileContent = ("x, y, z, c\n" ++ concat (map toCSVLine (concat (responses_ ctrl))))
  end <- liftIO getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  liftIO $ putStrLn $ "-> Computation time: " ++ (show (diff :: Double))
  --liftIO $ writeToFile (outputPath_ s) fileContent
  --liftIO $ writeToFile (outputPath_ s) ("x, y, z, c\n" ++ concat (map toCSVLine results))
  --liftIO $ plot' [] X11 $ Data3D [(Title "Test"), (Style Dots)] [] (concat $ map fst results)
  return ()



takeNodes :: Settings -> [NodeId] -> [NodeId]
takeNodes s nids
    | num > 0 = take num nids
    | otherwise = nids
      where num = numNodes_ s

writeToFile :: String -> String -> IO ()
writeToFile path dataS = do
  outh <- openFile path WriteMode
  hPutStrLn outh dataS
  hClose outh
  return ()

{-
waitForChuncks :: [ProcessId] -> [[Result]] -> Process ([[Result]])
waitForChuncks [] ls = return ls
waitForChuncks pids ls = do
  m <- expect
  case m of
    RESPONSE (p, l) -> waitForChuncks (filter (/= p) pids) (l : ls)
    _ -> say "MASTER ending" >> terminate
-}

norm :: Vect -> Float
norm (x, y, z) = sqrt $ (x*x) + (y*y) + (z*z)

waitForChuncks :: [ProcessId] -> DistControlStruct -> Process (DistControlStruct)
waitForChuncks [] struct = return struct
waitForChuncks pids struct = do
  m <- expect
  case m of
    RESPONSE (p, l) -> waitForChuncks (filter (/= p) pids) (insert' struct l)
    _ -> say "MASTER ending" >> terminate

insert' :: DistControlStruct -> [Result] -> DistControlStruct
insert' struct v = DistControlStruct {
  timeStamps_ = timeStamps_ struct,
  runningTasks_ = runningTasks_ struct,
  openTasks_ = openTasks_ struct,
  responses_ = v : (responses_ struct)
}
