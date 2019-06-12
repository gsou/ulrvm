module Compiler.Preprocessor (preprocess) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)

trim = dropWhile isSpace . dropWhileEnd isSpace

preprocess :: String -> IO String
preprocess = fmap unlines . mapM preprocess' . lines
  where preprocess' x | "#include " `isPrefixOf` trim x = let str = (trim $ drop 9 $ x)
                                                            in if length str > 2
                                                                  then if head str == '"' && last str == '"'
                                                                       then readFile $ init $ tail $ str
                                                                       else pure x
                                                               else pure x
        preprocess' x = pure x
