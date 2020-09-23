{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
-- | Phase one of compilation
module Compiler.Lexer (lexer, KW(..), Token(..), TokenPos) where

import Data.Char (toLower, toUpper)
import Data.Int (Int16, Int32)
import Numeric (readHex)
import Text.Parsec
import Text.Read (readMaybe)

import Compiler.AST

-- | Keywords
data KW = KWIf | KWElse | KWWhile | KWReturn | KWDo | KwExtern | KwFn | KwStruct | KwUnion
  deriving (Show, Eq)

-- | Token Type 
data Token =
    -- Top level
    HookT String Bool -- ^ Declare a code image
  | LitBlock String -- ^ A raw string block {{ }}

   -- C like level
  | TypeT TypeR -- ^ A type value
  | AtomT String -- ^ Atom
  | ConstT Lit
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
      kwAtom
  <|> (Just (AtomT "#") <$ char '#')
  <|> try ( string "{{" >> (Just . LitBlock . ('{':) . (++"}")) <$> manyTill anyChar (try (string "}}")) )
  <|> (try (string "//") >> many (noneOf "\n") >> spaces >> rawToken)
  <|> try ( string "/*" >> manyTill anyChar (try (string "*/")) >> spaces >> rawToken )
  <|> try (char '@' *> fmap (Just . flip HookT True) atom)
  <|> try (string "@@" *> fmap (Just . flip HookT False) atom)
  <|> (Just StmtSep <$ char ';')
  <|> (Just ExprSep <$ char ',')
  <|> (Just BracketL <$ char '(')
  <|> (Just BracketR <$ char ')')
  <|> (Just BlockL <$ char '{')
  <|> ((Just . Operator) <$> many1 (oneOf "+-*/<>?:!="))
  <|> (Just BlockR <$ char '}')
  <|> try ((string "0x" *> fmap (Just . ConstT . LongLit . fst . head . readHex) (many1 hexDigit)) <* oneOf "lL")
  <|> try (string "0x" *> fmap (Just . ConstT . Primitive . I . fst . head . readHex) (many1 hexDigit))
  <|> try ((Just . ConstT . FloatLit) <$> floatNumber)
  <|> try (((Just . ConstT . LongLit) <$> number) <* oneOf "lL")
  <|> ((Just . ConstT . Primitive . I) <$> number )
  <|> (char '&' *> fmap (Just . ConstT . Primitive . P) symbol)
  <|> (Just AsmStart <$ string "@{")
  -- End
  <|> pure Nothing
 where kwAtom = do
         str <- (:) <$> (oneOf $ '_' : ['a'..'z'] ++ ['A'..'Z']) <*> many (oneOf $ "_0123456789." ++ ['a'..'z'] ++ ['A'..'Z'])
         pure $ Just $ case str of
           "int" -> TypeT Num
           "int32" -> TypeT Num32
           "float" -> TypeT Float32
           "int64" -> TypeT Num64
           "void" -> TypeT Void
           "if" -> Keyword KWIf
           "else" -> Keyword KWElse
           "do" -> Keyword KWDo
           "while" -> Keyword KWWhile
           "return" -> Keyword KWReturn
           "extern" -> Keyword KwExtern
           "fn" -> Keyword KwFn
           "struct" -> Keyword KwStruct
           "union" -> Keyword KwUnion
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

number = read <$> ((:) <$> digit <*> many digit)
floatNumber = read <$> ((\n _ m -> n ++ '.' : m) <$> many digit <*> char '.' <*> many1 digit)
symbol = many1 alphaNum
atom = many1 $ oneOf $ "_0123456789." ++ ['a'..'z'] ++ ['A'..'Z']
inst = (:) <$> fmap toUpper letter <*> fmap (map toLower) (many1 letter)
