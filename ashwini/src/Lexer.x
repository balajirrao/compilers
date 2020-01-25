{
    module Lexer (scanTokens, Token(..), Keyword(..) ) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
    $white+                                 ;
    \+                                      {\s -> Plus}
    \-                                      {\s -> Minus}
    \*                                      {\s -> Times}
    \/                                      {\s -> Divide}
    $digit+                                {\s -> Number (read s)}
    case                                    {\s -> Keyword CaseKw}
    defn                                    {\s -> Keyword DefnKw}
    of                                      {\s -> Keyword OfKw}
    data                                    {\s -> Keyword DataKw}
    \{                                      {\s -> OpenCurly }
    \}                                      {\s -> ClosedCurly }
    \(                                      {\s -> OpenParen }
    \)                                      {\s -> ClosedParen }
    \,                                      {\s -> Comma }
    \-\>                                    {\s -> PatternArrow }
    \=                                      {\s -> Equal }
    [a-z]$alpha*                            {\s -> LCaseId s }
    [A-Z]$alpha*                            {\s -> UCaseId s }

{

data Keyword = DefnKw | DataKw | CaseKw | OfKw deriving (Show, Eq)

data Token =
      Plus
    | Times
    | Minus
    | Divide
    | Number Int
    | Keyword Keyword
    | OpenCurly
    | ClosedCurly
    | OpenParen
    | ClosedParen
    | Comma
    | PatternArrow
    | Equal
    | LCaseId String
    | UCaseId String
    deriving (Show, Eq)

scanTokens = alexScanTokens

}