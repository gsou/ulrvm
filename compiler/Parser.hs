-- | Phase two of compilation
module Parser (parser) where

import Control.Lens
import Text.Parsec hiding (satisfy)

import Lexer
import AST

-- | Parse a token list
parser :: SourceName -> [TokenPos] -> Either ParseError AST
parser = parse (parser' $ AST [] [])

-- | Parser for the AST
parser' acc = (do
                  HookT h dyn <- tok isHook
                  code <- many hir
                  parser' $ comps %~ (CompilationUnit h dyn False code :) $ acc
  ) <|> (do
     InlineC n code <- tok isInlineC
     parser' $ inlines %~ ((n,code):) $ acc
  ) <|> (acc <$ eof)
{-
parser' :: AST -> Parsec [TokenPos] () AST
parser' acc = (do
     HookT h dyn <- tok isHook
     code <- many ir
     parser' $ comps %~ (CompilationUnit h dyn False code :) $ acc
     ) <|> (do
     InlineC n code <- tok isInlineC
     parser' $ inlines %~ ((n,code):) $ acc
     ) <|> (acc <$ eof)
-}
hir :: Parsec [TokenPos] () HIR
hir = do
  (t,a) <- typeAndAtom
  tok' BracketL
  params <- sepEndBy typeAndAtom (tok' ExprSep)
  tok' BracketR
  body <- stmts
  pure $ FunDef t a params body

statement = 
        flip label "declaration" (do
            (t,a) <- typeAndAtom
            assignT
            e <- expression
            tok' StmtSep
            pure $ Declaration t a e
  ) <|> flip label "if construct" (do
            tok' $ Keyword KWIf
            tok' BracketL
            cond <- expression
            tok' BracketR
            t <- stmts
            e <- (tok' (Keyword KWElse) >> stmts ) <|> pure []
            pure $ If cond t e
  ) <|> flip label "while construct" (do
            tok' $ Keyword KWDo
            t <- stmts
            tok' $ Keyword KWWhile
            tok' BracketL
            cond <- expression
            tok' BracketR
            tok' StmtSep
            pure $ While cond t
  ) <|> flip label "return statement" (do
            tok' $ Keyword KWReturn
            Return <$> (Just <$> expression <|> pure Nothing) <* tok' StmtSep
  ) <|> flip label "inline assembly" (
            fmap InlineAsm $ tok' AsmStart *> many ir <* tok' AsmStop
  ) <|> (Raw <$> expression <* tok' StmtSep)

expression = Fx <$> choice [
    try (BinaryOp <$> expression1 <*> (operator <?> "binary operator") <*> expression) <?> "binary operator application"
  , do
      TypeT t <- tok' BracketL *> tok isType <* tok' BracketR  <?> "cast"
      e <- expression
      pure $ Cast t e
  , unFix <$> expression1
  ] <?> "expression"

expression1 = Fx <$> choice [
    tok' BracketL *> (unFix <$> expression) <* tok' BracketR                                                   <?> "bracketed expression"
  , UnaryOp <$> operator <*> expression                                                            <?> "Unary operation"
  , CCall <$> (try $ atom <* tok' BracketL) <*> (sepBy expression (tok' ExprSep) <* tok' BracketR) <?> "function call"
  , Assign <$> (try $ atom <* assignT) <*> expression                                              <?> "assignement"
  , fmap InlineAsmExp (tok' AsmStart *> many ir <* tok' AsmStop) <?> "inline assembly"
  , do {ConstT c <- tok isConst; pure $ Constant c}                                                <?> "constant"
  , Atom <$> atom                                                                                  <?> "variable"
  ]

typeAndAtom = do
  TypeT t <- tok isType
  AtomT a <- tok isAtom
  pure(t,a)

atom = do
  AtomT a <- tok isAtom
  pure a

operator = do
  Operator o <- tok isOp
  pure o

-- | Parse a single ir token
ir :: Parsec [TokenPos] () IR
ir =  (do {InstT i <- tok isInst; pure $ InstIR i})
  <|> parseLabel
  <|> (do {RawT r <- tok isRaw; pure $ RawIR r})
  <|> (do {CallT c <- tok isCall; pure $ CallIR c})
  <|> (do
          tok' Then
          t <- block
          e <- (tok' Else >> block) <|> pure []
          pure $ ThenElse t e)
  <|> (tok' WhileT >> fmap WhileIR block)

parseLabel = do
  LabelT l <- tok isLabel
  -- table <- (False <$ tok' (FlagS "v")) <|> pure True
  pure $ LabelIR l True

-- * Helpers
tok :: (Token -> Bool) -> Parsec [TokenPos] () Token
tok is = fst <$> satisfy (is . fst)
tok' :: Token -> Parsec [TokenPos] () Token
tok' tok = fst <$> satisfy ((== tok) . fst)
isHook (HookT _ _) = True
isHook _ = False
isConst (ConstT _) = True
isConst _ = False
isInlineC (InlineC _ _) = True
isInlineC _ = False
isLabel (LabelT _) = True
isLabel _ = False
--isFlagN (FlagN _) = True
--isFlagN _ = False
isInst (InstT _) = True
isInst _ = False
isRaw (RawT _) = True
isRaw _ = False
isCall (CallT _) = True
isCall _ = False
isType (TypeT _) = True
isType _ = False
isAtom (AtomT _) = True
isAtom _ = False
isOp (Operator "=") = False
isOp (Operator _) = True
isOp _ = False
assignT = tok' $ Operator "="
satisfy fn = tokenPrim show (\p _ s -> case s of { [] -> p; (_,p):_ -> p}) (\t -> if fn t then Just t else Nothing)

block = (do
  tok' BlockL
  irs <- many ir
  tok' BlockR
  pure irs) <|> fmap pure ir

stmts = (do
  tok' BlockL
  irs <- many statement
  tok' BlockR
  pure irs) <|> fmap pure statement
