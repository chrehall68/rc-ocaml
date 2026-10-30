{
module MyParser where
-- Import any AST or token types needed here
import Ast
import Tokens
import Data.Char
}

%monad { ParseResult } { (>>=) } { return }
%name parse Program
%tokentype { Token }
%error { parseError }

-- lowest precedence first
-- see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity
%nonassoc BELOW_COMMA
%right ';'
%left ','
%right '::'
%nonassoc CONSTR_APPLICATION -- for constructors, polymorphic variants, and function applications

%token
      -- literals and identifiers
      INT         { TokenInt $$ }
      DOUBLE      { TokenDouble $$ }
      CHAR        { TokenChar $$ }
      STRING      { TokenString $$ }
      LABEL       { TokenLabel $$ }
      OPTLABEL    { TokenOptionalLabel $$ }
      UIDENT      { TokenCapitalizedIdentifier $$ }
      LIDENT      { TokenLowercaseIdentifier $$ }
      INFIX       { TokenInfix $$ }
      PREFIX      { TokenPrefix $$ }

      -- keywords
      AND         { TokenAnd }
      AS          { TokenAs }
      ASR         { TokenAsr }
      ASSERT      { TokenAssert }
      BEGIN       { TokenBegin }
      CLASS       { TokenClass }
      CONSTRAINT  { TokenConstraint }
      DO          { TokenDo }
      DONE        { TokenDone }
      DOWNTO      { TokenDownto }
      ELSE        { TokenElse }
      END         { TokenEnd }
      EXCEPTION   { TokenException }
      EXTERNAL    { TokenExternal }
      FALSE       { TokenFalse }
      FOR         { TokenFor }
      FUN         { TokenFun }
      FUNCTION    { TokenFunction }
      FUNCTOR     { TokenFunctor }
      IF          { TokenIf }
      IN          { TokenIn }
      INCLUDE     { TokenInclude }
      INHERIT     { TokenInherit }
      INITIALIZER { TokenInitializer }
      LAND        { TokenLand }
      LAZY        { TokenLazy }
      LET         { TokenLet }
      LOR         { TokenLor }
      LSL         { TokenLsl }
      LSR         { TokenLsr }
      LXOR        { TokenLxor }
      MATCH       { TokenMatch }
      METHOD      { TokenMethod }
      MOD         { TokenMod }
      MODULE      { TokenModule }
      MUTABLE     { TokenMutable }
      NEW         { TokenNew }
      NONREC      { TokenNonrec }
      OBJECT      { TokenObject }
      OF          { TokenOf }
      OPEN        { TokenOpen }
      OR          { TokenOr }
      PRIVATE     { TokenPrivate }
      REC         { TokenRec }
      SIG         { TokenSig }
      STRUCT      { TokenStruct }
      THEN        { TokenThen }
      TO          { TokenTo }
      TRUE        { TokenTrue }
      TRY         { TokenTry }
      TYPE        { TokenType }
      VAL         { TokenVal }
      VIRTUAL     { TokenVirtual }
      WHEN        { TokenWhen }
      WHILE       { TokenWhile }
      WITH        { TokenWith }

      -- keyword symbols
      '!='        { TokenPhysicalNE }
      '#'         { TokenHash }
      '&'         { TokenAmpersand }
      '&&'        { TokenAmpersandAmpersand }
      QUOTE       { TokenQuote } -- single quote
      '('         { TokenLParen }
      ')'         { TokenRParen }
      '*'         { TokenStar }
      '+'         { TokenPlus }
      ','         { TokenComma }
      '-'         { TokenMinus }
      '-.'        { TokenMinusDot }
      '->'        { TokenArrow }
      '.'         { TokenDot }
      '..'        { TokenDotDot }
      '.~'        { TokenDotTilde }
      ':'         { TokenColon }
      '::'        { TokenColonColon }
      ':='        { TokenColonEq }
      ':>'        { TokenColonGt }
      ';'         { TokenSemi }
      ';;'        { TokenSemiSemi }
      '<'         { TokenLt }
      '<-'        { TokenLeftArrow }
      '='         { TokenEq }
      '>'         { TokenGt }
      '>]'        { TokenGtRBracket }
      '>}'        { TokenGtRBrace }
      '?'         { TokenQuestion }
      '['         { TokenLBracket }
      '[<'        { TokenLBracketLt }
      '[>'        { TokenLBracketGt }
      '[|'        { TokenLBracketPipe }
      ']'         { TokenRBracket }
      '_'         { TokenUnderscore }
      BACKTICK    { TokenBacktick }
      '{'         { TokenLBrace }
      '{<'        { TokenLBraceLt }
      '|'         { TokenPipe }
      '|]'        { TokenPipeRBracket }
      '||'        { TokenPipePipe }
      '}'         { TokenRBrace }
      '~'         { TokenTilde }

      EOF         { TokenEOF }

%%

-- Define your BNF grammar rules here

-- Macros for faster development
-- allows equivalent of p?
opt(p)
  : p          { Just $1 }
  | {- empty -} { Nothing }

-- helper for `many`
many_rev(p)
  :                { [] }
  | many_rev(p) p  { $2 : $1 }
-- allows equivalent of p*
many(p)  : many_rev(p)  { reverse $1 }

-- helper for `many1`
many1_rev(p)
  : p              { [$1] }
  | many1_rev(p) p { $2 : $1 }
-- allows equivalent of p+
many1(p) : many1_rev(p) { reverse $1 }

-- 11.3 Names (https://ocaml.org/manual/5.3/names.html)

-- naming objects
ValueName
  : LIDENT                      { $1 }
  | '(' OperatorName ')'        { $2 }

OperatorName
  : PREFIX                      { $1 }
  | InfixOp                     { $1 }

InfixOp
  : INFIX                       { $1 }
  | '*'                         { "*" }
  | '+'                         { "+" }
  | '-'                         { "-" }
  | '-.'                        { "-." }
  | '='                         { "=" }
  | '!='                        { "!=" }
  | '<'                         { "<" }
  | '>'                         { ">" }
  | OR                          { "or" }
  | '||'                        { "||" }
  | '&'                         { "&" }
  | '&&'                        { "&&" }
  | ':='                        { ":=" }
  | MOD                         { "mod" }
  | LAND                        { "land" }
  | LOR                         { "lor" }
  | LXOR                        { "lxor" }
  | LSL                         { "lsl" }
  | LSR                         { "lsr" }
  | ASR                         { "asr" }

ConstrName      : UIDENT        { $1 }
TagName         : UIDENT        { $1 }
TypeconstrName  : LIDENT        { $1 }
FieldName       : LIDENT        { $1 }
ModuleName      : UIDENT        { $1 }
ModtypeName     : UIDENT        { $1 }
                | LIDENT        { $1 }
ClassName       : LIDENT        { $1 }
InstVarName     : LIDENT        { $1 }
MethodName      : LIDENT        { $1 }

-- referring to named objects
-- ValuePath and Constr both start with a module path, so they share ModulePath
-- and only diverge after the last `.` (LIDENT/operator => value, otherwise the
-- final UIDENT of the path is the constructor)
ValuePath
  : ValueName                   { Path Nothing $1 }
  | ModulePath '.' ValueName    { Path (Just $1) $3 }

Constr
  : ModulePath                  { constrOfPath $1 }

Typeconstr
  : TypeconstrName                          { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' TypeconstrName   { ExtendedPath (Just $1) $3 }

Field
  : FieldName                   { Path Nothing $1 }
  | ModulePath '.' FieldName    { Path (Just $1) $3 }

ModtypePath
  : ModtypeName                             { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' ModtypeName      { ExtendedPath (Just $1) $3 }

ClassPath
  : ClassName                   { Path Nothing $1 }
  | ModulePath '.' ClassName    { Path (Just $1) $3 }

ClasstypePath
  : ClassName                               { ExtendedPath Nothing $1 }
  | ExtendedModulePath '.' ClassName        { ExtendedPath (Just $1) $3 }

ModulePath
  : ModuleName                  { ModulePath [$1] }
  | ModulePath '.' ModuleName   { appendModule $1 $3 }

DotExtendedModuleName : '.' ExtendedModuleName { $2 }
ExtendedModulePath
  : ExtendedModuleName many(DotExtendedModuleName)                  { ExtendedModulePath ($1 : $2) }

ExtendedModuleName
  : ModuleName                                      { ExtendedModuleName $1 [] }
  | ExtendedModuleName '(' ExtendedModulePath ')'   { applyFunctor $1 $3 }

-- 11.4 Type Expressions (https://ocaml.org/manual/5.3/types.html)
-- TODO: fill out this section

-- 11.5 Constants (https://ocaml.org/manual/5.3/const.html)
-- had to break this up into multiple rules to avoid ambiguity
Constant
  : LiteralConstant      { $1 }
  | BACKTICK TagName     { ConstantPolymorphicVariant $2 }
  | Constr               { ConstantVariant $1 }

-- constants that can't be confused with constructor application
LiteralConstant
  : INT                  { ConstantInt $1 }
  | DOUBLE               { ConstantDouble $1 }
  | CHAR                 { ConstantChar $1 }
  | STRING               { ConstantString $1 }
  | FALSE                { ConstantBool False }
  | TRUE                 { ConstantBool True }
  | '(' ')'              { ConstantUnit }
  | BEGIN END            { ConstantUnit }
  | '[' ']'              { ConstantList }
  | '[|' '|]'            { ConstantArray }

-- 11.6 Patterns (https://ocaml.org/manual/5.3/patterns.html)
-- TODO: fill out this section
TypeExpr 
  : {- empty -}             { TypeExpr }

-- 11.7 Expressions (https://ocaml.org/manual/5.3/expr.html)
-- To avoid ambiguity, we've broken expressions up into tiers:
--   AtomExpr   - can be the head of a function application
--   SimpleExpr - can be an argument (an atom, or a nullary constructor/tag)
--   Expr       - everything else
-- Arguments and constructor arguments are SimpleExprs, so that `f a b` can't
-- be read as `f (a b)`, `(f a) b`, or `f a b`. Also, nullary constructors aren't atoms,
-- so `C 1` can only be constructor application.
Expr
  : SimpleExpr                { $1 }
  | ExprCommaList %prec BELOW_COMMA  { ExprTuple (reverse $1) }
  | Constr SimpleExpr %prec CONSTR_APPLICATION  { ExprNonconstantVariant $1 $2 }
  | BACKTICK TagName SimpleExpr %prec CONSTR_APPLICATION  { ExprNonconstantPolymorphicVariant $2 $3 }
  | ExprConsList %prec BELOW_COMMA                  { ExprCons $1 }
  -- function application
  | AtomExpr many1(Argument)  { ExprFunctionApplication $1 $2 }

SimpleExpr
  : AtomExpr                  { $1 }
  | Constr                    { ExprConstant (ConstantVariant $1) }
  | BACKTICK TagName          { ExprConstant (ConstantPolymorphicVariant $2) }

AtomExpr
  : LiteralConstant            { ExprConstant $1 }
  | ValuePath                  { ExprPath $1 }
  | '(' Expr ')'               { $2 }
  | BEGIN Expr END             { $2 }
  | '(' Expr ':' TypeExpr ')'  { ExprAnnotated $2 $4 }
  | '[' ExprList ']'   { ExprCons $2 }
  | '[|' ExprList '|]' { ExprArray (init $2) }
  | '{' ExprFieldList '}'  { ExprRecord Nothing $2 }
  | '{' Expr WITH ExprFieldList '}'  { ExprRecord (Just $2) $4 }

-- the resulting list is in reverse order of appearance in the tuple
ExprCommaList
  : Expr ',' Expr               { [$3, $1] }
  | ExprCommaList ',' Expr      { $3 : $1 }

ExprConsList
  : Expr '::' Expr                       { [$1, $3] }
  | Expr '::' ExprConsList               { $1: $3 }

ExprList
  : Expr ';' ExprList  { $1 : $3 }
  | Expr ';'           { [$1, ExprConstant ConstantList] }  -- it's ok to have a trailing `;`
  | Expr               { [$1, ExprConstant ConstantList] }

RecordField
  : Field          { RecordField $1 Nothing (ExprPath $1) }
  | Field ':' TypeExpr { RecordField $1 (Just $3) (ExprPath $1) }
  | Field '=' Expr     { RecordField $1 Nothing $3 }
  | Field ':' TypeExpr '=' Expr { RecordField $1 (Just $3) $5 }
ExprFieldList
  : RecordField ';' ExprFieldList  { $1 : $3 }
  | RecordField ';'                 { [$1] } -- it's ok to have a trailing `;`
  | RecordField                     { [$1] }

-- LABEL/OPTLABEL already include the trailing `:` (`~x:`)
Argument
  : SimpleExpr           { UnlabeledArgument $1 }
  | '~' LIDENT           { LabeledArgument $2 Nothing }
  | LABEL SimpleExpr     { LabeledArgument $1 (Just $2) }
  | '?' LIDENT           { OptionalArgument $2 Nothing }
  | OPTLABEL SimpleExpr  { OptionalArgument $1 (Just $2) }


-- 11.8 Types and Exception Definitions (https://ocaml.org/manual/5.3/typedecl.html)
-- TODO: fill out this section

-- 11.9 Classes (https://ocaml.org/manual/5.3/classes.html)
-- TODO: fill out this section

-- 11.10 Module Types (https://ocaml.org/manual/5.3/modtypes.html)
-- TODO: fill out this section

-- 11.11 Module Expressions (https://ocaml.org/manual/5.3/modules.html)
-- The manual writes this as
--   { ;; } ( definition | expr ) { { ;; } definition | ;; expr } { ;; }
-- but written out literally it is ambiguous: in `;; ;; expr` the parser
-- can't tell which `;;` belongs to the `{ ;; }` run and which to `;; expr`.
-- so, instead, we give every `;;` exactly one role:
-- either it introduces an expr, or it is a stray separator.
ModuleItems
  : many(';;') DefinitionOrExpr many(ModuleItem)  { ModuleItems ($2 : concat $3) }

ModuleItem
  : Definition        { [Definition] }
  | ';;' Expr         { [Expr $2] }
  | ';;'              { [] }

DefinitionOrExpr 
  : Definition        { Definition }
  | Expr              { Expr $1 }

-- TODO: use actual definitions
Definition 
  : LET                  {  }


-- 11.12 Compilation Units (https://ocaml.org/manual/5.3/compunit.html)

Program
  : UnitInterface UnitImplementation           { Program $1 $2 }

-- UnitInterface : many(Specification opt(';;'))             { $1 }
UnitInterface :                               { UnitInterface }
 
UnitImplementation : opt(ModuleItems)         { UnitImplementation $1 }


{

type ParseResult a = Either String a
-- TODO - recover from errors
-- see https://haskell-happy.readthedocs.io/en/latest/using.html#handling-parse-errors
parseError :: [Token] -> ParseResult a
parseError t = Left ("Parse error on " ++ (show t))

appendModule :: ModulePath -> String -> ModulePath
appendModule (ModulePath ms) m = ModulePath (ms ++ [m])

-- A.B.C -> the constructor C qualified by A.B
constrOfPath :: ModulePath -> Path
constrOfPath (ModulePath ms) = case reverse ms of
  [c] -> Path Nothing c
  c : rest -> Path (Just (ModulePath (reverse rest))) c
  [] -> error "empty module path"

applyFunctor :: ExtendedModuleName -> ExtendedModulePath -> ExtendedModuleName
applyFunctor (ExtendedModuleName m args) arg = ExtendedModuleName m (args ++ [arg])

}