module Ast where

import Tokens

-- names (https://ocaml.org/manual/5.3/names.html)
newtype ModulePath = ModulePath [String]
  deriving (Show, Eq)

newtype ExtendedModulePath = ExtendedModulePath [ExtendedModuleName]
  deriving (Show, Eq)

-- a module name applied to zero or more functor arguments, e.g. F(A.B)(C)
data ExtendedModuleName = ExtendedModuleName String [ExtendedModulePath]
  deriving (Show, Eq)

-- [ module-path . ] name
data Path = Path (Maybe ModulePath) String
  deriving (Show, Eq)

-- [ extended-module-path . ] name
data ExtendedPath = ExtendedPath (Maybe ExtendedModulePath) String
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data TypeExpr = TypeExpr
  deriving (Show, Eq)

data RecordField = RecordField Path (Maybe TypeExpr) Expr
  deriving (Show, Eq)

data Argument
  = UnlabeledArgument Expr
  | LabeledArgument String (Maybe Expr)
  | OptionalArgument String (Maybe Expr)
  deriving (Show, Eq)

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
  | ExprFunctionApplication Expr [Argument]
  deriving (Show, Eq)

data DefinitionOrExpr
  = Definition
  | Expr Expr
  deriving (Show, Eq)

data ModuleItems = ModuleItems [DefinitionOrExpr]
  deriving (Show, Eq)

data UnitInterface = UnitInterface
  deriving (Show, Eq)

data UnitImplementation = UnitImplementation (Maybe ModuleItems)
  deriving (Show, Eq)

data Program = Program UnitInterface UnitImplementation
  deriving (Show, Eq)