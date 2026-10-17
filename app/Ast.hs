module Ast where

import Tokens

data AstNode
  = AstInt IntLiteral
  deriving (Show)
