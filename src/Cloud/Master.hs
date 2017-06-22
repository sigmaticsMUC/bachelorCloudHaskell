{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Cloud.Master(
  masterProcess
)where

import Control.Distributed.Process hiding (Message)

import Cloud.Type (Vect, IterationCount, Result, MSG (RESPONSE))
import Cloud.Kernel (spawnProcesses, distribute)
import System.IO


vecToString :: Vect -> String
vecToString (a, b, c) = (show a) ++ "," ++ (show b) ++ "," ++(show c) ++ " "

iterToRgbString :: Integer -> String
iterToRgbString i = show $ 256 * i + i

{-
toCSVLine :: (Integer, Vect) -> String
toCSVLine (i, v) = (vecToString v) ++ (countToColorString (norm v)) ++ "\n"
-}

toCSVLine :: (Vect, Integer) -> String
toCSVLine (v, i) = (vecToString v) ++ "," ++ (iterToRgbString i) ++ "\n"


masterProcess :: Closure(Vect->IterationCount) -> [Vect] -> [NodeId] -> Process ()
masterProcess cF args nodes = do
  master <- getSelfPid
  say "spawning processes..."
  ps <- spawnProcesses master cF nodes
  say $ (show $ length nodes) ++ " processes spawned!"
  distribute master args ps
  response <- waitForChuncks ps [[]]
  let result = filter (\(_, i) -> i == 255) (concat response)
  --say (show result)
  liftIO $ writeToFile $ "x, y, z, c\n" ++ (concat (map toCSVLine result))
  return ()

writeToFile :: String -> IO ()
writeToFile dataS = do
  outh <- openFile "../test3.txt" WriteMode
  hPutStrLn outh dataS
  hClose outh
  return ()

waitForChuncks :: [ProcessId] -> [[Result]] -> Process ([[Result]])
waitForChuncks [] ls = return ls
waitForChuncks pids ls = do
  m <- expect
  case m of
    RESPONSE (p, l) -> waitForChuncks (filter (/= p) pids) (l : ls)
    _ -> say "MASTER ending" >> terminate
