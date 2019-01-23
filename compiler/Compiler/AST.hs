{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Abstract representation of the code
module Compiler.AST where

import Data.Int (Int8, Int16)
import Control.Lens
import Control.Lens.TH

-- * Typing

-- | Primitive types supported by the language
data Prim = I Int16  -- ^ Base 16 integer
          | P String -- ^ Atom symbol pointer 
  deriving (Show, Read, Eq)

-- * Instructions

-- | Instruction set
data Inst = Nop | Lit Prim | Dup | Drop | Swap | Push | Pop | Jump | Jnz | Call | Ccall | Nat | Ret | Eq | Neq | Lt | Gt | Copy | Paste | Fetch | Store | Add | Sub | Mul | Divmod | And | Or | Xor | Shift | Zret | End deriving (Show, Read, Eq)

-- | The bytecode number representing the instruction 
tag Nop     = 0
tag (Lit _) = 1
tag Dup     = 2
tag Drop    = 3
tag Swap    = 4
tag Push    = 5
tag Pop     = 6
tag Jump    = 7
tag Jnz     = 8
tag Call    = 9
tag Ccall   = 10
tag Nat     = 11
tag Ret     = 12
tag Eq      = 13
tag Neq     = 14
tag Lt      = 15
tag Gt      = 16
tag Copy    = 17
tag Paste   = 18
tag Fetch   = 19
tag Store   = 20
tag Add     = 21
tag Sub     = 22
tag Mul     = 23
tag Divmod  = 24
tag And     = 25
tag Or      = 26
tag Xor     = 27
tag Shift   = 28
tag Zret    = 29
tag End     = 30

-- | Check if an instruction can be packed
packable x = x `notElem` [Jump, Jnz, Call, Ccall, Ret, Zret, End]

-- * Abstract data

-- | Flashable intermediate representation
data FIR = Run Int16 -- ^ Execute a function
         | Flash Int16 [Int16] -- ^ Flash a page
  deriving (Show, Read, Eq)

-- | Lower intermediate representation for a code stream
data LIR = LabelLIR String Bool
         | InstLIR Inst
         | PackedLIR Inst Inst Inst
         | RawLIR Int16
  deriving (Show, Read, Eq)

-- | Upper intermediate representation for a code stream
data IR = LabelIR String Bool -- ^ A label creation request
        | InstIR Inst -- ^ An instruction insert request
        | RawIR Int16 -- ^ A raw value insert request
        | CallIR String -- ^ A call to a native function
        | ThenElse [IR] [IR] -- ^ If construct
        | WhileIR [IR] -- ^ Simple loop construct
  deriving (Show, Read, Eq)

-- | High level type information
data TypeR = Num -- ^ Native word type
           | Num32 -- ^ 32 bit integer type
           -- | FunPtr 
           | Void -- ^ Empty type
  deriving (Show, Read, Eq)

-- Size of in machine word of types
sizeOf Void  = 0
sizeOf Num   = 1
sizeOf Num32 = 2



-- | High level Expression information
data ExpRF a = Atom String
          | Constant Prim
          | UnaryOp String a
          | BinaryOp a String a
          | CCall String [a]
          | Ternary a a a
          | Assign String a
          | Cast TypeR a
          | InlineAsmExp [IR]
  deriving (Show, Read, Eq, Functor, Foldable, Traversable)

-- | Fixed point
newtype Fx f = Fx { unFix :: (f (Fx f)) }

type ExpR = Fx ExpRF

-- | Imperative representation
data StatementR = Declaration TypeR String ExpR
                | If ExpR [StatementR] [StatementR]
                | While ExpR [StatementR]
                | Raw ExpR
                | RawPush TypeR ExpR
                | Return (Maybe ExpR)
                | InlineAsm [IR]

-- | Higher IR
data HIR = FunDef TypeR String [(TypeR, String)] [StatementR]

makeLenses ''HIR
-- | A container that can be compiled standalone
data CompilationUnit a = CompilationUnit {
  _unitName :: String, -- ^ The name of the unit
  _unitDynamic :: Bool, -- ^ Generate recompilable code?
  _unitGenSymbolLookup :: Bool, -- ^ Generate a lookup table for dynamic calls (e.g. RunDyn(unit, "main"), or RunDynPath("unit.main"))?
  _unitIR :: [a] -- ^ The source code of the unit in a certain representation
  }
makeLenses ''CompilationUnit

-- | The tree of the program
data AST = AST {
  _inlines :: [(String,String)], -- ^ Inline C code for which to provide a handler
  _comps :: [CompilationUnit HIR] -- ^ Units to compute
  }
makeLenses ''AST