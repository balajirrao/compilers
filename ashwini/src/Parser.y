{
module Parser where

import Data.Functor.Foldable (Fix(Fix))

import AST
import qualified Lexer as L

}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
    '+'    { L.Plus }
    '/'    { L.Divide }
    '-'    { L.Minus }
    '*'    { L.Times }
    num    { L.Number $$ }
    case   { L.Keyword L.CaseKw }
    defn   { L.Keyword L.DefnKw }
    of     { L.Keyword L.OfKw }
    data   { L.Keyword L.DataKw }
    '='    { L.Equal }
    '->'   { L.PatternArrow }
    '{'    { L.OpenCurly }
    '}'    { L.ClosedCurly }
    '('    { L.OpenParen }
    ')'    { L.ClosedParen }
    ','    { L.Comma }
    lcaseid { L.LCaseId $$ }
    ucaseid { L.UCaseId $$ }

%%


Program : Definitions { $1 }

Definitions : Definition Definitions { $1 : $2 }
            | Definition { [$1] }

Definition : defn lcaseid LowercaseParams '=' '{' Add '}' { Defn $2 $3 $6 }
            | data ucaseid '=' '{' Constructors '}' { Data $2 $5 }

LowercaseParams : {- empty -} { [] }
                | lcaseid LowercaseParams { $1 : $2 }

UppercaseParams : {- empty -} { [] }
                | ucaseid UppercaseParams { $1 : $2 }

Constructors : Constructors ',' Constructor { $3 : $1 }
            | Constructor { [ $1 ]}

Constructor : ucaseid UppercaseParams { Constructor $1 $2 }

Add : Add '+' Mul { Fix $ BinOp "+" $1 $3 }
    | Add '-'  Mul { Fix $ BinOp "-" $1 $3 }
    | Mul { $1 }

Mul
    : Mul '*' App { Fix $ BinOp "*" $1 $3 }
    | Mul '/' App { Fix $ BinOp "/" $1 $3 }
    | App { $1 }

App
    : App AppBase { Fix $ App $1 $2 }
    | AppBase { $1 }

AppBase
    : num { Fix $ Number $1 }
    | lcaseid { Fix $ LowerVar $1 }
    | ucaseid { Fix $ UpperVar $1 }
    | '(' Add ')' { $2 }
    | case Add of '{' Branches '}' { Fix $ Case $2 $5 }

Branches
    : Branch Branches { $1 : $2 }
    | Branch { [ $1 ] }

Branch
    : Pattern '->' '{' Add '}' { Branch $1 $4  }

Pattern
    : lcaseid  { PatternVar $1 }
    | ucaseid LowercaseParams { PatternCons $1 $2 }

{
parseError :: [L.Token] -> a
parseError _ = error "Parse error"
}