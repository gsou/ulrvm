
-- General utilities to flash recompiled code to targets
module Flasher.Class where

import Data.List (intercalate)

import Compiler.AST

class Reprog flasher where
  -- Flash bytecode to a targetl
  flash :: flasher -> FIR -> IO ()

-- * Debugging flashers

-- | Display the FIR in a string format
data DebugFlash = DebugFlash

instance Reprog DebugFlash where
  flash DebugFlash ir = print ir

-- | Output the FIR in an ascii serial format
data DebugRaw = DebugRaw

instance Reprog DebugRaw where
  flash DebugRaw (Flash ix code) = putStrLn $ "++" ++ show ix ++ "+ " ++ show (length code) ++ ' ': intercalate " " (map (('+':) . show) code) ++ " +"