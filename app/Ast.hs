module Ast where

import Tokens

-- names (https://ocaml.org/manual/5.3/names.html)
newtype ModulePath = ModulePath [String]
  deriving (Show)

newtype ExtendedModulePath = ExtendedModulePath [ExtendedModuleName]
  deriving (Show)

-- a module name applied to zero or more functor arguments, e.g. F(A.B)(C)
data ExtendedModuleName = ExtendedModuleName String [ExtendedModulePath]
  deriving (Show)

-- [ module-path . ] name
data Path = Path (Maybe ModulePath) String
  deriving (Show)

-- [ extended-module-path . ] name
data ExtendedPath = ExtendedPath (Maybe ExtendedModulePath) String
  deriving (Show)

data Constant
  = ConstantInt IntLiteral
  | ConstantDouble Double
  | ConstantChar Char
  | ConstantString String
  | ConstantBool Bool
  | ConstantUnit
  | ConstantList -- the empty list []
  | ConstantArray -- the empty array [||]
  | ConstantPolymorphicVariant String -- `Tagname would give ConstantPolymorphicVariant "Tagname"
  | ConstantVariant Path -- constant variants are variants without arguments
  deriving (Show)

data TypeExpr = TypeExpr
  deriving (Show)

data RecordField = RecordField Path (Maybe TypeExpr) Expr
  deriving (Show)

data Expr
  = ExprConstant Constant
  | -- refers to a (potentially module-qualified) name
    ExprPath Path
  | -- Path to the constructor, Expr
    ExprNonconstantVariant Path Expr
  | -- `Tagname expr would give (ExprNonconstantPolymorphicVariant "Tagname" expr)
    ExprNonconstantPolymorphicVariant String Expr
  | ExprAnnotated Expr TypeExpr
  | ExprTuple [Expr]
  | -- the expressions are in order of appearance (left to right)
    -- utop evaluates the expression from right to left, but technically
    -- the manual says the order is unspecified; same applies to lists (which is why
    -- we represent them as ExprCons too)
    ExprCons [Expr]
  | ExprArray [Expr]
  | -- source record, changed fields
    ExprRecord (Maybe Expr) [RecordField]
  deriving (Show)

data DefinitionOrExpr
  = Definition
  | Expr Expr
  deriving (Show)

data ModuleItems = ModuleItems [DefinitionOrExpr]
  deriving (Show)

data UnitInterface = UnitInterface
  deriving (Show)

data UnitImplementation = UnitImplementation (Maybe ModuleItems)
  deriving (Show)

data Program = Program UnitInterface UnitImplementation
  deriving (Show)