{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-| The F monad and its derivatives, with operations -}
module Compiler.F where

import Compiler.AST

import Data.Int (Int16)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Lens
import Control.Lens.TH
import Control.Monad.RWS.Lazy
import Control.Monad.Except

-- | The errors that can be thrown from F
data Err = CantDefSymbol String
         | DuplicateSymbol (String, String)
         | NewUnit String
         | NoSuchNative String
         | InvalidNativeVar TypeR String
         | MissingSymbols (S.Set (String, String))
         | LinkError (String, String)
         | InvalidOp String Bool
         | InvalidVar String
         | NumberOfArgError String Int Int
         | TypeMismatch TypeR TypeR
         | SymbolMismatch String String
         | OperatorType String (Maybe TypeR) (Maybe TypeR)
         | CantTypeCheck String
         | ConversionError TypeR TypeR
         | VarDotError TypeR String
         | AmbiguousRawType

instance Show Err where
  show (CantDefSymbol s) = "Can't define a new symbol: " ++ s
  show (LinkError (cu, sym)) = "Can't find symbol " ++ sym ++ " in unit " ++ cu
  show (DuplicateSymbol (cu, sym)) = "Duplicate symbol " ++ sym ++ " in unit " ++ cu
  show (NewUnit u) = "Can't create new unit " ++ u ++ " due to hardware limitation."
  show (NoSuchNative u) = "Can't find native call " ++ u ++ "."
  show (InvalidNativeVar t s) = "Can't bind native var " ++ show s ++ " with type " ++ show t
  show (MissingSymbols s) = "Missing symbols for proper recompilation: " ++ show s
  show (InvalidOp s b) = "Invalid " ++ (if b then "binary" else "unary") ++ " operator (" ++ s ++ ")"
  show (NumberOfArgError f expect actual) = "Bad number of arguments calling function " ++ f ++ " expecting " ++ show expect ++ " , got " ++ show actual ++ " "
  show (InvalidVar s) = "Can't find variable " ++ s
  show (TypeMismatch t u) = "Type mismatch expecting " ++ show t ++ " got " ++ show u
  show (SymbolMismatch t u) = "Symbol mismatch expecting " ++ show t ++ " got " ++ show u
  show (CantTypeCheck s) = "Can't typecheck expression " ++ s
  show (OperatorType op (Just fst) Nothing) = "Operator " ++ op ++ " can't take type " ++ show fst
  show (OperatorType op (Just fst) (Just snd)) = "Operator " ++ op ++ " can't take types " ++ show fst ++ " and " ++ show snd
  show (OperatorType op _ _) = "Operator " ++ op ++ " used with ambiguous types"
  show (ConversionError a b) = "Can't convert from type " ++ show a ++ " to type " ++ show b
  show (VarDotError t str) = "Variable ident \"" ++ show str ++ "\" is not a valid subtype of type " ++ show t
  show AmbiguousRawType = "Type of raw statement is ambiguous"

-- | State representing the lowlevel organization of a system.
-- Is it generated on the first compilation.
data VMState = VMState {
  _intGen :: Int16, -- ^ Used internally to generate symbols
  _nativeCalls :: M.Map String (Int16, [TypeR], TypeR), -- ^ Native calls allowed
  _localSymbols :: M.Map (String, String) Int16, -- ^ The local symbol table.
  _defFuncs :: M.Map String ([TypeR], TypeR), -- ^ User defined functions
  _symbolTable :: S.Set (String, String) -- ^ The symbol table vector
  } deriving (Show, Read, Eq)
makeLenses ''VMState

data System = System {
  _imageTable :: M.Map String Int16, -- ^ Index of the image table
  _lastSymbols :: M.Map (String, String) Int16, -- ^ The local symbol table.
  _lastNativeCalls :: M.Map String (Int16, [TypeR], TypeR), -- ^ Native calls allowed
  _symbols :: S.Set (String, String) -- ^ The symbol table of the system.
 } deriving (Show, Read, Eq)
makeLenses ''System

-- | The monad for standard compilation generating
type F = RWST () String VMState (Except Err)

-- | The monad used for recompilation
type R = RWST System [FIR] VMState (Except Err)

-- | Get the underlying system
class MonadSystem m where
  askSystem :: m (Maybe System)
  getNatives :: m (M.Map String (Int16, [TypeR], TypeR))

instance MonadSystem F where
  askSystem = pure Nothing
  getNatives = use nativeCalls

instance MonadSystem R where
  askSystem = pure <$> ask
  getNatives = M.union <$> use nativeCalls <*> view lastNativeCalls

class (MonadSystem m, MonadState VMState m, MonadError Err m) => MonadCompile m where
instance MonadCompile F where
instance MonadCompile R where

-- | Double layer monad used when generating from Hi level c code
type HFState = (Int16, [(TypeR, String)])
type HF m = RWST (TypeR, [(TypeR, String)]) [IR] HFState m
