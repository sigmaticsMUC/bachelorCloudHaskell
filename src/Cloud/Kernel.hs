{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Kernel where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad

import DistribUtils

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Message = Ping (ProcessId, [Integer])
  deriving (Typeable, Generic)          -- <1>

instance Binary Message                 -- <2>


serverIO :: (Integer->Integer) -> [Integer] -> IO [Integer]
serverIO f list = do
    let b = map f list :: [Integer]
    putStrLn $ show b
    return b

otherServer :: (Integer->Integer) -> [Integer] -> Process [Integer]
otherServer f list = do
    res <- liftIO $ serverIO f list
    return res

  -- <<pingServer
pingServer :: Integer -> Process ()
pingServer f = do
    Ping (from, inputList) <- expect                              -- <1>
    mypid <- getSelfPid
    res <- otherServer (\x-> x * x) inputList                             -- <3>
    send from (Ping (mypid, res))

{-
    pingServer :: Process ()
    pingServer = do
        liftIO $ print "Waiting for ping..."
        Ping from <- expect                              -- <1>
        liftIO $ putStrLn $ "received ping from " ++ (show from) -- <2>
        mypid <- getSelfPid
        otherServer                              -- <3>
        send from (Pong mypid)
-}

remotable ['pingServer]
  -- >>
f :: Integer -> Integer
f x = x * x

master :: [NodeId] -> Process ()
master peers = do
  ps <- forM peers $ \nid -> do
    let cls = $(mkClosure 'pingServer) (2 :: Integer)
    spawn nid cls

  mypid <- getSelfPid

  forM_ ps $ \pid -> do
    let list = [1..5] :: [Integer]
    send pid (Ping (mypid, list))

  res <- waitForPongs ps 0
  liftIO $ putStrLn $ "result is " ++ (show res)
  terminate
{-}
  master :: [NodeId] -> Process ()
  master peers = do
    ps <- forM peers $ \nid -> do
      say $ printf "spawning on %s" (show nid)
      spawn nid $(mkStaticClosure 'pingServer)

    mypid <- getSelfPid

    forM_ ps $ \pid -> do
      say $ printf "pinging %s" (show pid)
      send pid (Ping mypid)

    waitForPongs ps

    say "All pongs successfully received"
    terminate
-}


waitForPongs :: [ProcessId] -> Integer -> Process Integer
waitForPongs [] a = return a
waitForPongs ps i = do
  m <- expect
  case m of
    Ping (p, l) -> waitForPongs (filter (/= p) ps) (i + (sum l))
    _ -> say "MASTER received ping" >> terminate
{-
master :: Process ()
master = do
    node <- getSelfNode                               -- <1>

    say $ printf "spawning on %s" (show node)
    pid <- spawn node $(mkStaticClosure 'pingServer)  -- <2>

    mypid <- getSelfPid                               -- <3>
    say $ printf "sending ping to %s" (show pid)
    send pid (Ping mypid)                             -- <4>

    Pong _ <- expect                                  -- <5>
    say "pong."

    terminate                                         -- <6>
-}
  -- <<main
main :: IO ()
main = distribMain master __remoteTable
  -- >>
