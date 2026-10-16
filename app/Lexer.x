{
module Lexer  where
import Ast
import Data.Array
import Data.Char
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                        ;
  let                            { \_ -> TokenLet }
  in                             { \_ -> TokenIn }
  $alpha+                        { \s -> TokenVar s }
  $digit+                        { \s -> TokenInt (read s) }
  \+                             { \_ -> TokenPlus }
  \-                             { \_ -> TokenMinus }
  \=                             { \_ -> TokenEq }
  \*                             { \_ -> TokenTimes }
  \/                             { \_ -> TokenDiv }
  \(                             { \_ -> TokenOB }
  \)                             { \_ -> TokenCB }