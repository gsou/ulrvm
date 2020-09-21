
-- CAN reprogramming
module Flasher.CAN (
 SerialCANAdapter(..),
 SerialCANIO(..),
 ) where

import qualified System.Hardware.Serialport as S
import Data.ByteString.Char8 (pack, unpack)
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Bits
import Data.Word (Word32)

import Numeric (readHex)

import Control.Monad
import Control.Concurrent

import Flasher.Class

import Compiler.AST

-- | Can interface via a custom serial adapter
-- | ++<CANID>+<LENGTH>+<DATA0>+...+<DATA7>
data SerialCANAdapter = SerialCANAdapter
  Word32 -- ^ CAN id used
  S.SerialPort -- ^ Serial interface

data SerialCANIO = SerialCANIO
  Word32 -- ^ CAN id used
  
instance Reprog SerialCANAdapter where
  flash (SerialCANAdapter canId s) (Flash ix code) = do
    forM_ (zip [0::Integer ..] code) $ \(n,v) -> do
      S.send s $ pack $ "++" ++ show canId ++ "+6" ++ formatI16 ix ++ formatI16 n ++ formatI16 v ++ "\n"
      S.flush s
      threadDelay 2000
    S.send s $ pack $ "++" ++ show canId ++ "+2" ++ formatI16 ix ++ "\n"
    S.flush s
    threadDelay 2000
    --receive s canId ""
  flash (SerialCANAdapter canId s) (Run i) = do
    S.send s $ pack $ "++" ++ show canId ++ "+2" ++ formatI16 i ++ "\n"
    S.flush s

instance Reprog SerialCANIO where
  flash (SerialCANIO canId) (Flash ix code) = do
    forM_ (zip [0::Integer ..] code) $ \(n,v) -> do
      putStr $ " + +" ++ show canId ++ " +6" ++ formatI16 ix ++ formatI16 n ++ formatI16 v ++ "\n"
    putStr $ " + +" ++ show canId ++ " +2" ++ formatI16 ix ++ "\n"
  flash (SerialCANIO canId) (Run i) = do
    putStr $ " + +" ++ show canId ++ " +2" ++ formatI16 i ++ "\n"

formatI16 :: (Show a, Num a, Bits a) => a -> String
formatI16 i = " +" ++ show (i.&.0xFF) ++ " +" ++ show ((i `shift` (-8)).&.0xFF)

