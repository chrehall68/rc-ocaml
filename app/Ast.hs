module Ast where

import Tokens

data AstNode
  = AstInt IntLiteral
  | AstFloat Float
  deriving (Show)
