{
module MyParser where
-- Import any AST or token types needed here
import Ast
import Tokens
import Data.Char
}

%monad { ParseResult } { (>>=) } { return }
%name parse Program
%name parsePattern Pattern
%tokentype { Token }
%error { parseError }

-- lowest precedence first
-- see https://ocaml.org/manual/5.3/expr.html#ss%3Aprecedence-and-associativity
%nonassoc BELOW_COMMA
%right ';'
%right ':='
%left ','
%right PREC_OR OR '||'
%right PREC_AND '&' '&&'
%left PREC_INFIX0 INFIXOP0 '=' '<' '>' '!='
%right PREC_INFIX1 INFIXOP1
%right '::'
%left PREC_INFIX2 INFIXOP2 '+' '-' '-.'
%left PREC_INFIX3 INFIXOP3 '*' MOD LAND LOR LXOR
%right PREC_INFIX4 INFIXOP4 LSL LSR ASR
%nonassoc NEGATE_OP -- for prefix negation
%nonassoc CONSTR_APPLICATION -- for constructors, polymorphic variants, and function applications
%left HASHOP

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
      INFIXOP0    { TokenInfix0 $$ }
      INFIXOP1    { TokenInfix1 $$ }
      INFIXOP2    { TokenInfix2 $$ }
      INFIXOP3    { TokenInfix3 $$ }
      INFIXOP4    { TokenInfix4 $$ }
      HASHOP      { TokenHashOp $$ }
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

-- infix operators, grouped by precedence level (see precedence table at
-- the top, and https://ocaml.org/manual/5.3/expr.html#ss:precedence-and-associativity).
-- Each level is its own nonterminal so that expressions can give each level a precedence
InfixOp
  : InfixOpOr                   { $1 }
  | InfixOpAnd                  { $1 }
  | InfixOp0                    { $1 }
  | InfixOp1                    { $1 }
  | InfixOp2                    { $1 }
  | InfixOp3                    { $1 }
  | InfixOp4                    { $1 }
  | ':='                        { ":=" }
  | HASHOP                      { $1 }

InfixOpOr
  : OR                          { "or" }
  | '||'                        { "||" }

InfixOpAnd
  : '&'                         { "&" }
  | '&&'                        { "&&" }

InfixOp0
  : INFIXOP0                    { $1 }
  | '='                         { "=" }
  | '!='                        { "!=" }
  | '<'                         { "<" }
  | '>'                         { ">" }

InfixOp1
  : INFIXOP1                    { $1 }

InfixOp2
  : INFIXOP2                    { $1 }
  | '+'                         { "+" }
  | '-'                         { "-" }
  | '-.'                        { "-." }

InfixOp3
  : INFIXOP3                    { $1 }
  | '*'                         { "*" }
  | MOD                         { "mod" }
  | LAND                        { "land" }
  | LOR                         { "lor" }
  | LXOR                        { "lxor" }

InfixOp4
  : INFIXOP4                    { $1 }
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
  | ExtendedModuleName '.' Typeconstr       { qualifyType $1 $3 }

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
-- These forms allow type constraints in expressions, patterns, and records.
-- Additional type forms can be added here as they are implemented.
TypeExpr
  : ArrowType                          { $1 }

ArrowType
  : TupleType '->' ArrowType           { TypeArrow $1 $3 }
  | TupleType                          { $1 }

TupleType
  : AppliedType                         { $1 }
  | AppliedType '*' AppliedType TypeTupleTail { TypeTuple ($1 : $3 : $4) }

TypeTupleTail
  : {- empty -}                         { [] }
  | '*' AppliedType TypeTupleTail       { $2 : $3 }

AppliedType
  : AtomicType                          { $1 }
  | AppliedType Typeconstr              { TypeApplication [$1] $2 }

AtomicType
  : Typeconstr                          { TypeName $1 }
  | QUOTE LIDENT                        { TypeVariable $2 }
  | '(' TypeExpr ')'                    { $2 }
  | '(' TypeExpr ',' TypeExpr TypeArgumentsTail ')' Typeconstr
                                        { TypeApplication ($2 : $4 : $5) $7 }

TypeArgumentsTail
  : {- empty -}                         { [] }
  | ',' TypeExpr TypeArgumentsTail      { $2 : $3 }

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
-- Each tier follows the OCaml precedence table: constructors and lazy bind
-- tighter than ::, which binds tighter than tuples, |, and finally as.
Pattern
  : OrPattern                           { $1 }
  | OrPattern AS ValueName              { PatternAlias $1 $3 }

OrPattern
  : TuplePattern                        { $1 }
  | OrPattern '|' TuplePattern          { PatternOr $1 $3 }

TuplePattern
  : ConsPattern                         { $1 }
  | ConsPattern ',' ConsPattern PatternTupleTail
                                        { PatternTuple ($1 : $3 : $4) }

PatternTupleTail
  : {- empty -}                         { [] }
  | ',' ConsPattern PatternTupleTail    { $2 : $3 }

ConsPattern
  : AppliedPattern                      { $1 }
  | AppliedPattern '::' ConsPattern     { PatternCons $1 $3 }

AppliedPattern
  : SimplePattern                       { $1 }
  | Constr SimplePattern                { PatternVariant $1 $2 }
  | BACKTICK TagName SimplePattern      { PatternPolymorphicVariant $2 $3 }
  | '#' Typeconstr                      { PatternType $2 }
  | LAZY AppliedPattern                 { PatternLazy $2 }
  | EXCEPTION AppliedPattern            { PatternException $2 }

SimplePattern
  : ValueName                           { PatternVariable $1 }
  | '_'                                 { PatternWildcard }
  | INT                                 { PatternConstant (ConstantInt $1) }
  | '-' INT                             { PatternConstant (ConstantInt (negativeInt $2)) }
  | DOUBLE                              { PatternConstant (ConstantDouble $1) }
  | '-' DOUBLE                          { PatternConstant (ConstantDouble (negate $2)) }
  | CHAR                                { PatternConstant (ConstantChar $1) }
  | CHAR '..' CHAR                      { PatternRange $1 $3 }
  | STRING                              { PatternConstant (ConstantString $1) }
  | TRUE                                { PatternConstant (ConstantBool True) }
  | FALSE                               { PatternConstant (ConstantBool False) }
  | '(' ')'                             { PatternConstant ConstantUnit }
  | Constr                              { PatternConstant (ConstantVariant $1) }
  | BACKTICK TagName                    { PatternConstant (ConstantPolymorphicVariant $2) }
  | '(' Pattern ')'                     { $2 }
  | '(' Pattern ':' TypeExpr ')'        { PatternAnnotated $2 $4 }
  | '[' PatternElements ']'             { PatternList $2 }
  | '[|' PatternElements '|]'           { PatternArray $2 }
  | '{' RecordPatternFields '}'         { PatternRecord (fst $2) (snd $2) }
  | ModulePath '.' '(' Pattern ')'      { PatternLocalOpen $1 $4 }
  | ModulePath '.' '[' PatternElements ']'  { PatternLocalOpen $1 (PatternList $4) }
  | ModulePath '.' '[|' PatternElements '|]' { PatternLocalOpen $1 (PatternArray $4) }
  | ModulePath '.' '{' RecordPatternFields '}' { PatternLocalOpen $1 (PatternRecord (fst $4) (snd $4)) }

PatternElements
  : {- empty -}                         { [] }
  | PatternElementList opt(';')         { $1 }

PatternElementList
  : Pattern                             { [$1] }
  | PatternElementList ';' Pattern      { $1 ++ [$3] }

RecordPatternFields
  : RecordPatternField RecordPatternTail
                                        { let (fields, open) = $2 in ($1 : fields, open) }

RecordPatternTail
  : {- empty -}                         { ([], False) }
  | ';'                                 { ([], False) }
  | ';' '_'                             { ([], True) }
  | ';' '_' ';'                         { ([], True) }
  | ';' RecordPatternField RecordPatternTail
                                        { let (fields, open) = $3 in ($2 : fields, open) }

RecordPatternField
  : Field                               { PatternField $1 Nothing (PatternVariable (pathName $1)) }
  | Field ':' TypeExpr                  { PatternField $1 (Just $3) (PatternVariable (pathName $1)) }
  | Field '=' Pattern                   { PatternField $1 Nothing $3 }
  | Field ':' TypeExpr '=' Pattern      { PatternField $1 (Just $3) $5 }

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
  | '-' Expr %prec NEGATE_OP    { ExprPrefix "-" $2 }
  | '-.' Expr %prec NEGATE_OP  { ExprPrefix "-." $2 }
  -- infix operators; need to list them all out to get precendence right
  | Expr ':=' Expr                       { ExprInfix ":=" $1 $3 }
  | Expr InfixOpOr Expr  %prec PREC_OR   { ExprInfix $2 $1 $3 }
  | Expr InfixOpAnd Expr %prec PREC_AND  { ExprInfix $2 $1 $3 }
  | Expr InfixOp0 Expr   %prec PREC_INFIX0  { ExprInfix $2 $1 $3 }
  | Expr InfixOp1 Expr   %prec PREC_INFIX1  { ExprInfix $2 $1 $3 }
  | Expr InfixOp2 Expr   %prec PREC_INFIX2  { ExprInfix $2 $1 $3 }
  | Expr InfixOp3 Expr   %prec PREC_INFIX3  { ExprInfix $2 $1 $3 }
  | Expr InfixOp4 Expr   %prec PREC_INFIX4  { ExprInfix $2 $1 $3 }
  | Expr HASHOP Expr                     { ExprInfix $2 $1 $3 }

SimpleExpr
  : AtomExpr                  { $1 }
  | Constr                    { ExprConstant (ConstantVariant $1) }
  | BACKTICK TagName          { ExprConstant (ConstantPolymorphicVariant $2) }

AtomExpr
  : LiteralConstant            { ExprConstant $1 }
  -- putting prefix here ensures that we get `(!f) x` from `!f x`
  | PREFIX SimpleExpr          { ExprPrefix $1 $2 }
  | ValuePath                  { ExprPath $1 }
  | '(' Expr ')'               { $2 }
  | BEGIN Expr END             { $2 }
  | '(' Expr ':' TypeExpr ')'  { ExprAnnotated $2 $4 }
  | '[' ExprList ']'   { ExprCons $2 }
  | '[|' ExprList '|]' { ExprArray (init $2) }
  | '{' ExprFieldList '}'  { ExprRecord Nothing $2 }
  -- since `=` is an infix operator, `{ x = e ... }` would be ambiguous with `{ (x = e) with ... }`
  -- so we put this rule here instead
  | '{' SimpleExpr WITH ExprFieldList '}'  { ExprRecord (Just $2) $4 }

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

qualifyType :: ExtendedModuleName -> ExtendedPath -> ExtendedPath
qualifyType m (ExtendedPath path name) =
  let modules = case path of
        Nothing -> []
        Just (ExtendedModulePath ms) -> ms
   in ExtendedPath (Just (ExtendedModulePath (m : modules))) name

pathName :: Path -> String
pathName (Path _ n) = n

negativeInt :: IntLiteral -> IntLiteral
negativeInt n = n { value = negate (value n) }

}
