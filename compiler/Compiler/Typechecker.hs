{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
{-| Typechecker and expression generator -}
module Compiler.Typechecker where

import Compiler.F
import Compiler.AST

import Data.List (find, findIndex)
import Data.Maybe (fromJust)
import Data.Bits

import Control.Lens
import Control.Monad
import Control.Monad.RWS.Lazy
import Control.Monad.Except

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

pushExpr = cata pushExpr'

pushExpr' (Atom s) = do
  if s == "#"
     then pure (Nothing)
     else Just <$> copyAtom s
pushExpr' (Constant (Primitive (I a))) = Just Num <$ tell [InstIR $ Lit $ I a]
pushExpr' (Constant (Primitive (P a))) = Nothing  <$ tell [InstIR $ Lit $ P a]
pushExpr' (Constant (LongLit num)) = Just Num32  <$ tell [InstIR $ Lit $ I $ fromIntegral $ (num `shift` (-16)) .&. 0xFFFF,
                                                          InstIR $ Lit $ I $ fromIntegral $ num .&. 0xFFFF]
pushExpr' (UnaryOp "-" (Just Num)) = Just Num <$ tell [InstIR $ Lit (I (-1)), InstIR Mul]
pushExpr' (UnaryOp o a) = throwError $ OperatorType o a Nothing
pushExpr' (BinaryOp (Just Num) "==" (Just Num)) = Just Num <$ tell [InstIR Eq]
pushExpr' (BinaryOp (Just Num) "!=" (Just Num)) = Just Num <$ tell [InstIR Neq]
pushExpr' (BinaryOp (Just Num) ">"  (Just Num)) = Just Num <$ tell [InstIR Gt]
pushExpr' (BinaryOp (Just Num) "<"  (Just Num)) = Just Num <$ tell [InstIR Lt]
pushExpr' (BinaryOp (Just Num) "+"  (Just Num)) = Just Num <$ tell [InstIR Add]
-- TODO num32 functions
pushExpr' (BinaryOp (Just Num) "-"  (Just Num)) = Just Num <$ tell [InstIR Sub]
pushExpr' (BinaryOp (Just Num) "*"  (Just Num)) = Just Num <$ tell [InstIR Mul]
pushExpr' (BinaryOp (Just Num) "/"  (Just Num)) = Just Num <$ tell [InstIR Divmod, InstIR Drop]
pushExpr' (BinaryOp (Just Num) "%"  (Just Num)) = Just Num <$ tell [InstIR Divmod, InstIR Swap, InstIR Drop]
pushExpr' (BinaryOp a o b) = throwError $ OperatorType o a b
-- TODO Function dictionnary
pushExpr' (CCall d types) = Nothing <$ tell [CallIR d]
pushExpr' (Assign s e) = case e of
  Just e  -> Just <$> pasteAtom s e
  Nothing -> throwError $ CantTypeCheck $ "that assigns to " ++ s
-- TODO WARN Here 
pushExpr' (Cast typ Nothing) = pure (Just typ)
pushExpr' (Cast typ (Just typ2)) | typ == typ2 = pure (Just typ)
pushExpr' (Cast Num32 (Just Num)) = Just Num32 <$ tell [InstIR $ Lit $ I 0, InstIR Swap]
pushExpr' (Cast a (Just b)) = throwError $ ConversionError a b
pushExpr' (InlineAsmExp ir) = Nothing <$ tell ir
  
copyAtom :: (MonadError Err m) => String -> HF m TypeR
copyAtom s = do
  ix <- uses _2 (findIndex ((s==).snd))
  case ix of
    Just i  -> do
      typ <- fromJust .fmap fst <$> uses _2 (find ((s==).snd))
      offset <- uses _2 (sum . map (sizeOf . fst) . take i)
      forM_ (take (sizeOf typ) [offset..]) $ \i -> tell [InstIR $ Lit $ I $ fromIntegral i, InstIR Copy]
      pure typ
    Nothing -> throwError $ InvalidVar s

pasteAtom :: (MonadError Err m) => String -> TypeR -> HF m TypeR
pasteAtom s tp = do
  ix <- uses _2 (findIndex ((s==).snd))
  case ix of
    Just i  -> do
      typ <- fromJust . fmap fst <$> uses _2 (find ((s==).snd))
      unless (tp == typ) $ throwError $ TypeMismatch typ tp
      offset <- uses _2 (sum . map (sizeOf . fst) . take i)
      tell [InstIR $ Lit $ I $ sizeOf typ, InstIR $ Lit $ I offset, InstIR Paste]
      pure typ
    Nothing -> throwError $ InvalidVar s
