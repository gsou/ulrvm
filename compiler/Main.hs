{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Main where

import System.Environment (getArgs)

import Compiler.Lexer
import Compiler.Parser
import Compiler.Generate

import Flasher.Class

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-r", sys, sn] -> do
      system <- read <$> readFile sys
      str <- readFile sn
      case lexer sn str >>= parser sn of
        Left e -> print e
        Right ast -> case recompile system ast of
          Right (_, source) -> mapM_ (flash DebugRaw) source
          Left err -> print err
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
         "    ulrvmc -r symbol source\n"
