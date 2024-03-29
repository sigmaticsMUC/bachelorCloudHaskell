{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Cloud.Master(
  masterProcess,
  masterProcess2,
  masterProcess''
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
import System.Directory
import Graphics.EasyPlot
import IOUtils.ColorMap as CM
import qualified MandelBulb.Utils.Domain as D
import qualified MandelBulb.Utils.Domain as DM

type Point = (Float, Float, Float)



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
  let timeStamps = concat (map show (timeStamps_ ctrl))
  end <- liftIO getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  liftIO $ putStrLn $ "-> Computation time: " ++ (show (diff :: Double))
  liftIO $ storeResults (outputPath_ s) timeStamps fileContent
  --liftIO $ writeToFile (outputPath_ s) fileContent
  --liftIO $ writeToFile (outputPath_ s) ("x, y, z, c\n" ++ concat (map toCSVLine results))
  --liftIO $ plot' [] X11 $ Data3D [(Title "Test"), (Style Dots)] [] (concat $ map fst results)
  return ()

masterProcess'' :: Settings -> Float -> (Point, Point) -> Closure(Vect->IterationCount) -> [NodeId] -> Process ()
masterProcess'' settings h ((xS, yS, zS), (xE, yE, zE)) cF nids = do
  let numX = abs(xS - xE)/h
  let numY = abs(yS - yE)/h
  let numPoints = numX * numY
  --let deltaZ = (64 * 10^6)/numPoints
  let deltaZ = (1 * 10^6)/numPoints
  zList <- liftIO $ buildZList h deltaZ zS zE
  let ids = [0..(length zList)]
  let zippedZ = zip ids zList
  liftIO $ putStrLn $ show zList
  liftIO $ createDirectory (outputPath_ settings)
  start <- liftIO getCPUTime
  master <- getSelfPid
  ps <- spawnProcesses2 master cF (takeNodes settings nids)
  forM_ zippedZ $ \(zID, zis) -> do
    --liftIO $ putStrLn "RUNMASTER"
    let zes = if (zis + (deltaZ * h)) > zE then zE else zis + (deltaZ * h)
    --liftIO $ putStrLn $ (show (xS, yS, zis)) ++ (show (xE, yE, zes))
    let domain = DM.rowMajor $ DM.generateDomain (xS,yS,zis) (xE, yE, zes) h
    runMaster ps settings cF domain nids zID
    --liftIO $ putStrLn "DONE"
  forM_ ps $ \pid -> do
    send pid EXIT
  end <- liftIO getCPUTime
  return ()


runMaster :: [ProcessId] -> Settings -> Closure(Vect->IterationCount) -> [Vect] -> [NodeId] -> Int -> Process ()
runMaster pids s cF args nids zID = do
  --liftIO $ putStrLn "STARTING HANDLER"
  master <- getSelfPid
  forM_ pids $ \pid -> do
    send pid (START master)
  ctrl <- handlerProcess s args
  let fileContent = ("x, y, z, c\n" ++ concat (map toCSVLine (concat (responses_ ctrl))))
  let timeStamps = concat (map show (timeStamps_ ctrl))
  liftIO $ storeResults path timeStamps fileContent
  return ()
    where path = (outputPath_ s) ++ "/" ++ (outputPath_ s) ++ "_" ++ (show zID)

buildZList :: Float -> Float -> Float -> Float -> IO [Float]
buildZList h dZ zS zE = return zValues
          where numEle = (((abs(zS-zE))/h)/dZ)
                eleList = if numEle >= 1 then [0..(numEle-1)] else [0]
                zValues = map (\z -> zS + h * z * dZ) eleList


storeResults :: String -> String -> String -> IO ()
storeResults setName stamps points = do
  createDirectory setName
  writeToFile (setName ++ "/timestamps.txt") stamps
  writeToFile (setName ++ "/points.txt") points
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
