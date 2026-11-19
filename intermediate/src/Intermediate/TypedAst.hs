-- typed AST produced by type checking
-- every node carries an annotation t (which holds the in-progress or resolved Type).
-- This allows automatic traversal (since Traversable requires this kind)
-- Note: while the typed AST guarantees types match up,
-- it doesn't guarantee that matches are exhaustive

-- We also lower some language constructs:
--   - f a b                     -> TEApply (TEApply f a) b
--   - a + b, -a                 -> applications of the operator's TEVar
--   - a.(i), a.(i) <- v, s.[i]  -> Array.get / Array.set / String.get calls
--   - fun p1 p2 -> e            -> TEFunction [p1 -> TEFunction [p2 -> e]]
--   - function cases            -> TEFunction cases
--   - let f x y = e             -> let f = fun x y -> e
--   - if c then e               -> if c then e else ()
--   - a :: b :: c, [a; b]       -> nested TECons
--   - [p1; p2] (pattern)        -> nested TPCons

module Intermediate.TypedAst where

import Data.List.NonEmpty (NonEmpty)
import Frontend.Ast (Constant, Path, TypeDefinition)
import Intermediate.Type (ArgLabel)

-- t is always the value of the result of the expression
-- ie for a function, it would be the result of the function application (not the type of the function)
data TExpr t
  = TEConstant t Constant
  | -- a (potentially module-qualified) value name, including operators like (+)
    TEVar t Path
  | -- constructor applied to its argument; constant constructors stay in TEConstant
    TEVariant t Path (TExpr t)
  | -- TODO: needs row types before it can be typed
    TEPolymorphicVariant t String (TExpr t)
  | -- a tuple always has at least two elements
    TETuple t (TExpr t) (TExpr t) [TExpr t]
  | -- head, tail
    TECons t (TExpr t) (TExpr t)
  | TEArray t (NonEmpty (TExpr t))
  | -- source record, changed fields; record literals have no source
    TERecord t (Maybe (TExpr t)) [(Path, TExpr t)]
  | -- function, argument
    -- labeled arguments are reordered to match the function's parameter order,
    -- and omitted optional arguments are filled in with None
    TEApply t (TExpr t) (TExpr t)
  | TEFieldAccess t (TExpr t) Path
  | TEFieldAssignment t (TExpr t) Path (TExpr t)
  | -- if is cond then else
    TEIf t (TExpr t) (TExpr t) (TExpr t)
  | -- while is cond body
    TEWhile t (TExpr t) (TExpr t)
  | -- loop variable, start, end, isTo, body
    TEFor t String (TExpr t) (TExpr t) Bool (TExpr t)
  | -- left right
    TESequence t (TExpr t) (TExpr t)
  | TEMatch t (TExpr t) [TCase t]
  | -- a one-argument function that matches its argument against the cases
    -- the default is only present for optional parameters, e.g. ?(x = 1)
    TEFunction t ArgLabel (Maybe (TExpr t)) [TCase t]
  | TETry t (TExpr t) [TCase t]
  | -- isRec, bindings, body
    TELet t Bool [TLetBinding t] (TExpr t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- pattern, guard, body
data TCase t = TCase (TPattern t) (Maybe (TExpr t)) (TExpr t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data TLetBinding t = TLetBinding (TPattern t) (TExpr t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data TPattern t
  = TPVariable t String
  | TPWildcard t
  | TPConstant t Constant
  | TPAlias t (TPattern t) String
  | TPOr t (TPattern t) (TPattern t)
  | TPVariant t Path (TPattern t)
  | -- TODO: needs row types before it can be typed
    TPPolymorphicVariant t String (TPattern t)
  | -- #typ, matches any polymorphic variant tag in typ
    TPType t Path
  | TPTuple t [TPattern t]
  | -- head, tail
    TPCons t (TPattern t) (TPattern t)
  | TPArray t [TPattern t]
  | -- the trailing ; _ doesn't matter once the record type is known
    TPRecord t [(Path, TPattern t)]
  | TPRange t Char Char
  | TPLazy t (TPattern t)
  | TPException t (TPattern t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data TDefinition t
  = TDefinitionType TypeDefinition
  | -- isRec, bindings
    TDefinitionLet Bool [TLetBinding t]
  | TDefinitionExpr (TExpr t)
  deriving (Show, Eq, Functor, Foldable, Traversable)

newtype TProgram t = TProgram [TDefinition t]
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- the type of an expression is the annotation on its outermost node
exprAnnotation :: TExpr t -> t
exprAnnotation e = case e of
  TEConstant t _ -> t
  TEVar t _ -> t
  TEVariant t _ _ -> t
  TEPolymorphicVariant t _ _ -> t
  TETuple t _ _ _ -> t
  TECons t _ _ -> t
  TEArray t _ -> t
  TERecord t _ _ -> t
  TEApply t _ _ -> t
  TEFieldAccess t _ _ -> t
  TEFieldAssignment t _ _ _ -> t
  TEIf t _ _ _ -> t
  TEWhile t _ _ -> t
  TEFor t _ _ _ _ _ -> t
  TESequence t _ _ -> t
  TEMatch t _ _ -> t
  TEFunction t _ _ _ -> t
  TETry t _ _ -> t
  TELet t _ _ _ -> t

patternAnnotation :: TPattern t -> t
patternAnnotation p = case p of
  TPVariable t _ -> t
  TPWildcard t -> t
  TPConstant t _ -> t
  TPAlias t _ _ -> t
  TPOr t _ _ -> t
  TPVariant t _ _ -> t
  TPPolymorphicVariant t _ _ -> t
  TPType t _ -> t
  TPTuple t _ -> t
  TPCons t _ _ -> t
  TPArray t _ -> t
  TPRecord t _ -> t
  TPRange t _ _ -> t
  TPLazy t _ -> t
  TPException t _ -> t
