{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Main where

import System.Environment (getArgs)
import qualified System.Hardware.Serialport as S

import Compiler.Lexer
import Compiler.Parser
import Compiler.Generate

import Flasher.Class
import Flasher.Serial
import Flasher.CAN

import Interface.Cli

import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-d", port, baud] -> initInterface port (S.defaultSerialSettings {S.commSpeed = genCS baud})
    ["-i", port, baud, sys] -> S.withSerial port (S.defaultSerialSettings {S.commSpeed = genCS baud}) $ \s -> forever $ do
      putStr "> "
      src <- getLine
      recompWith (FlashSerial s) sys $ "@repl void main() { " ++ src ++ "}"
    ["-s", port, baud, sys, sn] -> S.withSerial port (S.defaultSerialSettings {S.commSpeed = genCS baud}) $ \s -> recompWith (FlashSerial s) sys =<< readFile sn
    ["-c", canId, port, baud, sys, sn] -> S.withSerial port (S.defaultSerialSettings {S.commSpeed = genCS baud}) $ \s -> recompWith (SerialCANAdapter (read canId) s) sys =<< readFile sn
    ["-r", sys, sn] -> recompWith DebugRaw sys =<< readFile sn
    [sn] -> do
      str <- readFile sn
      case lexer sn str >>= parser sn of
        Left e -> print e
        Right ast -> case compile ast of
          Right (st, header) -> do
            writeFile "system.map.h" header
            writeFile "system.def.h" $ show $ genSystem ast st
          Left err -> print err
    _ -> putStrLn usage
 where usage =
         "Compile source:\n" ++
         "    ulrvmc source\n" ++
         "Recompile symbol:\n" ++
         "    ulrvmc -r symbol source\n" ++
         "    ulrvmc -s /dev/ttyACM0 9600 symbol source\n" ++
         "    ulrvmc -c <canid> /dev/ttyACM0 9600 symbol source\n" ++
         "    ulrvmc -i /dev/ttyACM0 9600 symbol\n"
       recompWith fl sys str = do
         system <- read <$> readFile sys
         case lexer "src" str >>= parser "src" of
           Left e -> print e
           Right ast -> case recompile system ast of
             Right (_, source) -> mapM_ (flash fl) source
             Left err -> print err
       genCS "110"    = S.CS110
       genCS "300"    = S.CS300
       genCS "600"    = S.CS600
       genCS "1200"   = S.CS1200
       genCS "2400"   = S.CS2400
       genCS "4800"   = S.CS4800
       genCS "9600"   = S.CS9600
       genCS "19200"  = S.CS19200
       genCS "38400"  = S.CS38400
       genCS "57600"  = S.CS57600
       genCS "115200" = S.CS115200
       genCS _ = error "Invalid baudrate"
