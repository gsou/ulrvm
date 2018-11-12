{-# LANGUAGE FlexibleContexts #-}
-- | Phase one of compilation
module Lexer (lexer, KW(..), Token(..), TokenPos) where

import Data.Char (toLower, toUpper)
import Data.Int (Int16)
import Numeric (readHex)
import Text.Parsec
import Text.Read (readMaybe)

import AST

-- | Keywords
data KW = KWIf | KWElse | KWWhile | KWReturn | KWDo
  deriving (Show, Eq)

-- | Token Type 
data Token =
    -- Top level
    InlineC String String -- ^ Inline c code for native hook
  | HookT String Bool -- ^ Declare a code image

   -- C like level
  | TypeT TypeR -- ^ A type value
  | AtomT String -- ^ Atom
  | ConstT Prim
  | BracketL -- ^ (
  | BracketR -- ^ )
  | ExprSep -- ^ ,
  | StmtSep -- ^ ;
  | Operator String -- ^ Operator
  | BlockL -- ^ {
  | BlockR -- ^ }
  | Keyword KW
  | AsmStart -- ^ @{
  | AsmStop -- ^ }@

  -- Assembly
  | LabelT String -- ^ Attach a label to a code location
  | InstT Inst -- ^ An insruction literal
  | RawT Int16 -- ^ A raw emit literal
  | Then -- ^ Then syntax
  | Else -- ^ Else syntax
  | CallT String -- ^ A native call from the inline c generator 
  | WhileT -- ^ While syntax
  deriving (Show, Eq)

type TokenPos = (Token, SourcePos)

-- | Lex a string into tokens 
lexer :: SourceName -> String -> Either ParseError [TokenPos]
lexer = parse (spaces *> tok <* eof)

tok = do
  pos <- getPosition
  token <- rawToken
  spaces
  case token of
    Just AsmStart -> ((AsmStart, pos):) <$> irTok
    Just t -> ((t, pos):) <$> tok
    Nothing -> eof >> pure []

irTok = do
  pos <- getPosition
  token <- irToken
  spaces
  case token of
    Just AsmStop -> ((AsmStop, pos):) <$> tok
    Just t -> ((t, pos):) <$> irTok
    Nothing -> fail "File ended before end of inline asm"

rawToken =
  -- Hi level constructs
      try (fmap Just inlineC)
  <|> kwAtom
  <|> (Just (AtomT "#") <$ char '#')
  <|> (string "//" >> many (noneOf "\n") >> spaces >> rawToken)
  <|> try (char '@' *> fmap (Just . flip HookT True) atom)
  <|> try (string "@@" *> fmap (Just . flip HookT False) atom)
  <|> (Just StmtSep <$ char ';')
  <|> (Just ExprSep <$ char ',')
  <|> (Just BracketL <$ char '(')
  <|> (Just BracketR <$ char ')')
  <|> (Just BlockL <$ char '{')
  <|> ((Just . Operator) <$> many1 (oneOf "+-<>?:!="))
  <|> (Just BlockR <$ char '}')
  <|> try (string "0x" *> fmap (Just . ConstT . I . fst . head . readHex) (many1 hexDigit))
  <|> ((Just . ConstT . I) <$> number )
  <|> (char '&' *> fmap (Just . ConstT . P) symbol)
  <|> (Just AsmStart <$ string "@{")
  -- End
  <|> pure Nothing
 where kwAtom = do
         str <- (:) <$> letter <*> many (oneOf $ "_0123456789" ++ ['a'..'z'] ++ ['A'..'Z'])
         pure $ Just $ case str of
           "int" -> TypeT Num
           "int32" -> TypeT Num32
           "void" -> TypeT Void
           "if" -> Keyword KWIf
           "else" -> Keyword KWElse
           "do" -> Keyword KWDo
           "while" -> Keyword KWWhile
           "return" -> Keyword KWReturn
           _ -> AtomT str
           
-- | Lex a single token
irToken = (char ':' *> fmap (Just . LabelT) symbol)
  <|> (Just AsmStop <$ string "}@")
  <|> (Just BlockL <$ char '{')
  <|> (Just BlockR <$ char '}')
  -- <|> try (fmap Just inlineC)
  <|> (Just Then <$ string "THEN")
  <|> (Just Else <$ string "ELSE")
  <|> (Just WhileT <$ string "WHILE")
  <|> try (fmap (Just . CallT) $ atom <* string "()")
  -- <|> (char '.' *> fmap (Just . flip HookT False) atom)
  -- <|> (char ',' *> fmap (Just . flip HookT True) atom)
  <|> try (string "0x" *> fmap (Just . InstT . Lit . I . fst . head . readHex) (many1 hexDigit))
  <|> ((Just . InstT . Lit . I) <$> number )
  <|> (char '&' *> fmap (Just . InstT . Lit . P) symbol)
  <|> (char '%' *> fmap (Just . RawT . read) (many1 digit))
  <|> (fmap (fmap InstT . readMaybe) inst)
  <|> (char ';' >> many (noneOf "\n") >> spaces >> irToken)
  -- <|> try (fmap (Just . FlagN) (char '=' >> number))
  -- <|> try (fmap (Just . FlagS) (char '=' >> symbol))

inlineC = do
    name <- atom
    string "={"
    code <- inlineC' 1 "{"
    pure $ InlineC name code
  where inlineC' 0 acc = pure acc
        inlineC' x acc = (char '{' >> inlineC' (x+1) (acc ++ "{"))
                   <|> (char '}' >> inlineC' (x-1) (acc ++ "}"))
                   <|> (many1 (noneOf "{}") >>= (inlineC' x . (acc ++)))
number = read <$> ((:) <$> digit <*> many digit)
symbol = many1 alphaNum
atom = many1 $ oneOf $ "_0123456789" ++ ['a'..'z'] ++ ['A'..'Z']
inst = (:) <$> fmap toUpper letter <*> fmap (map toLower) (many1 letter)
