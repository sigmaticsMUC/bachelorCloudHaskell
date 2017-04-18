{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Cloud.Test.Clsering where

import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Closure
import Control.Monad

import Cloud.Utils.DistribUtils

import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)

data Message = Ping (ProcessId, [Integer])
  deriving (Typeable, Generic)          -- <1>

instance Binary Message

data Func = Func {
  kernel :: Integer -> Integer
}

f :: Integer -> Integer
f x = x + x

g :: Integer -> Integer
g x = x * x

function :: Func
function = Func {
  kernel = f
}

function_ :: () -> Func
function_ () = function

g_ :: () -> (Integer -> Integer)
g_ () = g


serverIO :: (Integer->Integer) -> [Integer] -> IO [Integer]
serverIO f list = do
    let b = map f list :: [Integer]
    putStrLn $ show b
    return b

otherServer :: (Integer->Integer) -> [Integer] -> Process [Integer]
otherServer f list = do
    res <- liftIO $ serverIO f list
    return res

mProcess :: (ProcessId, Closure(Func)) -> Process ()
mProcess (master, fClosure) = do
  us <- getSelfPid
  ff <- unClosure fClosure
  Ping (_, inputList) <- expect
  res <- otherServer (kernel ff) inputList
  liftIO $ putStrLn $ show res
  send master (Ping (us, res))

remotable ['mProcess, 'function_, 'g_]

masterProcess :: Closure (Func) -> [NodeId] -> Process ()
masterProcess fClosure peers = do
  ff <- unClosure fClosure
  master <- getSelfPid
  ps <- forM peers $ \nid -> do
    let cls = ($(mkClosure 'mProcess) (master, fClosure))
    spawn nid cls

  forM_ ps $ \pid -> do
    let list = [1..5] :: [Integer]
    send pid (Ping (master, list))

  res <- waitForPongs ps 0
  liftIO $ putStrLn $ show res
  return ()


masterProcess2 :: Closure (Integer -> Integer) -> [NodeId] -> Process ()
masterProcess2 fClosure peers = do
    ff <- unClosure fClosure
    master <- getSelfPid
    ps <- forM peers $ \nid -> do
      let cls = ($(mkClosure 'mProcess) (master, fClosure))
      spawn nid cls

    forM_ ps $ \pid -> do
      let list = [1..5] :: [Integer]
      send pid (Ping (master, list))

    res <- waitForPongs ps 0
    liftIO $ putStrLn $ show res
    return ()

waitForPongs :: [ProcessId] -> Integer -> Process Integer
waitForPongs [] a = return a
waitForPongs ps i = do
    m <- expect
    case m of
      Ping (p, l) -> waitForPongs (filter (/= p) ps) (i + (sum l))
      _ -> say "MASTER received ping" >> terminate

master = masterProcess ($(mkClosure 'function_) ())

snde = masterProcess2 ($(mkClosure 'g_) ())

main = distribMain master __remoteTable
