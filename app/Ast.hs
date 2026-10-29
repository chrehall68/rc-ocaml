module Ast where

import Tokens

data AstNode
  = AstInt IntLiteral
  | AstFloat Float
  deriving (Show)

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
