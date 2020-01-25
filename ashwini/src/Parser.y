{
module Parser where

import AST
import Lexer

}

%name parse
%tokentype { Token }
%error { parseError }

%token
    '+'    { Plus }
    '-'    { Minus }
    '*'    { Times }
    '/'    { Divide }
    num    { Number $$ }
    case   { Keyword CaseKw }
    defn   { Keyword DefnKw }
    of     { Keyword OfKw }
    data   { Keyword DataKw }
    '='    { Equal }
    '->'   { PatternArrow }
    '{'    { OpenCurly }
    '}'    { ClosedCurly }
    '('    { OpenParen }
    ')'    { ClosedParen }
    ','    { Comma }
    lcaseid { LCaseId $$ }
    ucaseid { UCaseId $$ }

%%


Program : Definitions { $1 }

Definitions : Definition Definitions { $1 : $2 }
            | Definition { [$1] }

Definition : defn lcaseid LowercaseParams '=' '{' Add '}' { Defn $2 $3 $6 }
            | data ucaseid '=' '{' Constructors '}' { Data $2 $5 }

LowercaseParams : {- empty -} { [] }
                | LowercaseParams lcaseid { $2 : $1 }

UppercaseParams : {- empty -} { [] }
                | UppercaseParams ucaseid { $2 : $1 }

Constructors : Constructors ',' Constructor { $3 : $1 }
            | Constructor { [ $1 ]}

Constructor : ucaseid UppercaseParams { Cons $1 $2 }

Add : Add '+' Mul { APlus $1 $3 }
    | Add '-'  Mul { AMinus $1 $3 }
    | Mul { Add0 $1 }

Mul
    : Mul '*' App { MTimes $1 $3 }
    | Mul '/' App { MDivide $1 $3 }
    | App { Mul0 $1 }

App
    : App AppBase { AppR $1 $2 }
    | AppBase { App0 $1 }

AppBase
    : num { BNumber $1 }
    | lcaseid { LowerVar $1 }
    | ucaseid { UpperVar $1 }
    | '(' Add ')' { BaseAdd $2 }
    | Case { Base0 $1 }

Case
    : case Add of '{' Branches '}'
        { Case $2 $5 }

Branches
    : Branches Branch { LBR $2 $1 }
    | Branch { LB0 $1 }

Branch
    : Pattern '->' '{' Add '}' { R0 $1 $4  }

Pattern
    : lcaseid { NLowerVar $1 }
    | ucaseid LowercaseParams { NUpperVar $1 $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}