{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Main where

import Lexer
import Parser
import Generate

-- TODO Proper argument parsing
main :: IO ()
main = do
  str <- getContents
  let sn = "ulrvm"
  {-
  case lexer sn str of
    Left e -> print e
    Right lex -> mapM_ print lex
  -}
  case lexer sn str >>= parser sn of
    Left e -> print e
    Right ast -> case compile ast of
      Right (st, header) -> do
        writeFile "system.map.h" header
        writeFile "system.def.h" $ show $ genSystem ast st
      Left err -> print err
  
