

-- Serial reprogramming
module Flasher.Serial where

import qualified System.Hardware.Serialport as S
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Flasher.Class

import Compiler.AST

newtype FlashSerial = FlashSerial S.SerialPort

instance Reprog FlashSerial where
  flash (FlashSerial s) (Flash ix code) = S.send s (pack $ "++" ++ show ix ++ "+ " ++ show (length code) ++ ' ': intercalate " " (map (('+':) . show) code) ++ " +") >> S.flush s
  flash (FlashSerial s) (Run i) = S.send s (pack "*") >> S.flush s
