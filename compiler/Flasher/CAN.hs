
-- CAN reprogramming
module Flasher.CAN (
 SerialCANAdapter(..),
 ) where

import qualified System.Hardware.Serialport as S
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Data.Bits
import Data.Word (Word32)

import Control.Monad
import Control.Concurrent

import Flasher.Class

import Compiler.AST

-- | Can interface via a custom serial adapter
-- | ++<CANID>+<LENGTH>+<DATA0>+...+<DATA7>
data SerialCANAdapter = SerialCANAdapter
  Word32 -- ^ CAN id used
  S.SerialPort -- ^ Serial interface

instance Reprog SerialCANAdapter where
  flash (SerialCANAdapter canId s) (Flash ix code) = do
    forM_ (zip [0::Integer ..] code) $ \(n,v) -> do
      S.send s $ pack $ "++" ++ show canId ++ "+6" ++ formatI16 ix ++ formatI16 n ++ formatI16 v
      S.flush s
      threadDelay 1000
    S.send s $ pack $ "++" ++ show canId ++ "+2" ++ formatI16 ix
    S.flush s
  flash (SerialCANAdapter canId s) (Run i) = do
    S.send s $ pack $ "++" ++ show canId ++ "+2" ++ formatI16 i
    S.flush s


formatI16 :: (Show a, Num a, Bits a) => a -> String
formatI16 i = "+" ++ show (i.&.0xFF) ++ "+" ++ show ((i `shift` (-8)).&.0xFF)

