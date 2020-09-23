{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-| Typechecker and expression generator -}
module Compiler.Typechecker where

import Compiler.F
import Compiler.AST

import Data.List (find, findIndex)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Bits

import Control.Lens
import Control.Monad
import Control.Monad.RWS.Lazy
import Control.Monad.Except

import Data.List.Split

import GHC.Float (castFloatToWord32)
{-
instance Functor ExpRF where
  fmap = fmapDefault

instance Foldable ExpRF where
  foldMap = foldMapDefault

instance Traversable ExpRF where
  traverse :: Applicative f => (a -> f b) -> ExpRF a -> f (ExpRF b)
  traverse _ a@(Atom _) = a
  traverse _ a@(Constant _) = a
  traverse f (UnaryOp op a) = UnaryOp op <$> f a
  traverse f (BinaryOp a op b) = (\a b -> BinaryOp a op b) <$> f a <*> f b
  traverse f (CCall String as) = CCall String <$> traverse f as
  traverse f (Ternary a b c) = Ternary <$> f a <*> f b <*> fc
  traverse f (Assign String a) = Assign 
-}

-- * Type checking

-- | Algebra
type Algebra m f a = f a -> m a

-- | Catamorphism
cata :: (Monad m, Traversable f) => Algebra m f a -> Fx f -> m a
cata alg = sequenceA . fmap (cata alg) . unFix >=> alg


-- | Algebra to determine the type and generate 
-- (typechecks at the same time since the monad is a MonadError)
{-
typeOf :: Algebra HF ExprRF (Maybe TypeR)
typeOf (Atom s) = do
  t <- fmap fst <$> uses _2 (find ((==s).snd))
  case t of
    Just _ -> pure t
    Nothing -> throwError $ InvalidVar s
typeOf (Constant (I _)) = pure (Just Num)
typeOf (Constant (P _)) = pure Nothing
typeOf a@(UnaryOp "-" t) = case t of
                             Just l -> pure t
                             _ -> throwError $ CantTypeCheck "UnaryOp -"
typeOf (BinaryOp t _ u) = pure t
typeOf (CCall s _) = undefined
typeOf (Ternary a b c) = undefined
typeOf (Assign String a) = undefined
typeOf (Cast typ a) = pure (Just typ)
typeOf a = error $ "Non exhaustive pattern for typeOf : " ++ show a
-}

operatorName "==" = "EQ"
operatorName "!=" = "NEQ"
operatorName ">" = "GT"
operatorName "<" = "LT"
operatorName "+" = "ADD"
operatorName "-" = "SUB"
operatorName "*" = "MUL"
operatorName "/" = "DIV"
operatorName "%" = "MOD"
operatorName "<=" = "LE"
operatorName ">=" = "GE"

pushExpr = cata pushExpr'

pushExpr' (Atom "#") = pure Nothing
pushExpr' (Atom s) = do
  natMap <- lift $ getNatives
  let d = "__get__" ++ s
  case M.lookup d natMap of
    Nothing -> Just <$> copyAtom s
    Just (_, [], ret) -> Just ret <$ tell [CallIR d]
pushExpr' (Constant (Primitive (I a))) = Just Num <$ tell [InstIR $ Lit $ I a]
pushExpr' (Constant (Primitive (P a))) = Nothing  <$ tell [InstIR $ Lit $ P a]
pushExpr' (Constant (LongLit num)) = Just Num32  <$ tell [InstIR $ Lit $ I $ fromIntegral $ (num `shift` (-16)) .&. 0xFFFF,
                                                          InstIR $ Lit $ I $ fromIntegral $ num .&. 0xFFFF]
pushExpr' (Constant (FloatLit fl)) = let num = castFloatToWord32 fl in
  Just Float32 <$ tell [InstIR $ Lit $ I $ fromIntegral $ (num `shift` (-16)) .&. 0xFFFF,
                        InstIR $ Lit $ I $ fromIntegral $ num .&. 0xFFFF]

pushExpr' (UnaryOp "-" (Just Num)) = Just Num <$ tell [InstIR $ Lit (I (-1)), InstIR Mul]
pushExpr' (UnaryOp op (Just a)) = do
  natMap <- lift getNatives
  let d = "__" ++ operatorName op ++ "__" ++ typeLabel a
  case M.lookup d natMap of
    Just (_, [t], ret) -> if a /= t
      then throwError $ TypeMismatch t a
      else Just ret <$ tell [CallIR d]
    Nothing -> throwError $ OperatorType op (Just a) Nothing
pushExpr' (UnaryOp o a) = throwError $ OperatorType o a Nothing
pushExpr' (BinaryOp (Just Num) "==" (Just Num)) = Just Num <$ tell [InstIR Eq]
pushExpr' (BinaryOp (Just Num) "!=" (Just Num)) = Just Num <$ tell [InstIR Neq]
pushExpr' (BinaryOp (Just Num) ">"  (Just Num)) = Just Num <$ tell [InstIR Gt]
pushExpr' (BinaryOp (Just Num) "<"  (Just Num)) = Just Num <$ tell [InstIR Lt]
pushExpr' (BinaryOp (Just Num) "+"  (Just Num)) = Just Num <$ tell [InstIR Add]
pushExpr' (BinaryOp (Just Num) "-"  (Just Num)) = Just Num <$ tell [InstIR Sub]
pushExpr' (BinaryOp (Just Num) "*"  (Just Num)) = Just Num <$ tell [InstIR Mul]
pushExpr' (BinaryOp (Just Num) "/"  (Just Num)) = Just Num <$ tell [InstIR Divmod, InstIR Drop]
pushExpr' (BinaryOp (Just Num) "%"  (Just Num)) = Just Num <$ tell [InstIR Divmod, InstIR Swap, InstIR Drop]
pushExpr' (BinaryOp (Just a) op (Just b)) = do
  natMap <- lift getNatives
  let d = "__" ++ operatorName op ++ "__" ++ typeLabel a ++ "_" ++ typeLabel b
  case M.lookup d natMap of
    Just (_, [t, u], ret) -> case (a == t, b == u) of
      (False, _) -> throwError $ TypeMismatch t a
      (_, False) -> throwError $ TypeMismatch u b
      (True, True) -> Just ret <$ tell [CallIR d]
    Nothing -> throwError $ OperatorType op (Just a) (Just b)
pushExpr' (BinaryOp a o b) = throwError $ OperatorType o a b
-- TODO Function dictionnary
pushExpr' (CCall d types) = do
  natMap <- lift $ getNatives
  case M.lookup d natMap of
    Nothing -> throwError $ NoSuchNative d
    Just (_, tps, ret) -> do
      unless (length types == length tps) $ throwError $ NumberOfArgError d (length tps) (length types)
      forM_ (zip types tps) $ \fff -> case fff of
        (Just actual, expected) -> unless (actual == expected) $ throwError $ TypeMismatch expected actual
        _                       -> pure ()
      Just ret <$ tell [CallIR d]
pushExpr' (Assign s e) = do
  natMap <- lift $ getNatives
  let d = "__set__" ++ s
  case M.lookup d natMap of
    -- TODO Assignment should not be void for native ?
    Just (_, [t], Void) -> case e of
      Just e -> if t == e
        then Just Void <$ tell [CallIR d]
        else throwError $ TypeMismatch t e
      Nothing -> throwError $ CantTypeCheck $ "that assigns to " ++ s
    Nothing -> case e of
      Just e  -> Just <$> pasteAtom s e
      Nothing -> throwError $ CantTypeCheck $ "that assigns to " ++ s
-- TODO WARN Here
pushExpr' (Cast typ Nothing) = pure (Just typ)
pushExpr' (Cast typ (Just typ2)) | typ == typ2 = pure (Just typ)
pushExpr' (Cast Num32 (Just Num)) = Just Num32 <$ tell [InstIR $ Lit $ I 0, InstIR Swap]
pushExpr' (Cast Num64 (Just Num)) = Just Num64 <$ tell [InstIR $ Push, InstIR $ Lit $ I 0, InstIR $ Lit $ I 0, InstIR $ Lit $ I 0, InstIR Pop]
pushExpr' (Cast a (Just b)) = throwError $ ConversionError a b
pushExpr' (InlineAsmExp ir) = Nothing <$ tell ir

copyAtom :: (MonadError Err m) => String -> HF m TypeR
copyAtom name = do
  let (s:ss) = splitOn "." name
  ix <- uses _2 (findIndex ((s==).snd))
  case ix of
    Just i  -> do
      varType <- fromJust .fmap fst <$> uses _2 (find ((s==).snd))
      (typ, insideOffset) <- case typeOffsetIn varType ss of
                               Just t -> pure t
                               Nothing -> throwError $ VarDotError varType name
      offset <- uses _2 (sum . map (sizeOf . fst) . take i)
      forM_ (take (sizeOf typ) [(offset+insideOffset)..]) $ \i -> tell [InstIR $ Lit $ I $ fromIntegral i, InstIR Copy]
      pure typ
    Nothing -> throwError $ InvalidVar name

pasteAtom :: (MonadError Err m) => String -> TypeR -> HF m TypeR
pasteAtom name tp = do
  let (s:ss) = splitOn "." name
  ix <- uses _2 (findIndex ((s==).snd))
  case ix of
    Just i  -> do
      varType <- fromJust . fmap fst <$> uses _2 (find ((s==).snd))
      (typ, insideOffset) <- case typeOffsetIn varType ss of
                               Just t -> pure t
                               Nothing -> throwError $ VarDotError varType name
      unless (tp == typ) $ throwError $ TypeMismatch typ tp
      offset <- uses _2 (sum . map (sizeOf . fst) . take i)
      tell [InstIR $ Lit $ I $ sizeOf typ, InstIR $ Lit $ I $ fromIntegral (offset + insideOffset), InstIR Paste]
      pure typ
    Nothing -> throwError $ InvalidVar name
