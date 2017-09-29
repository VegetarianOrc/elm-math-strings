module Math.Strings.Tokens exposing (..)

import Math.Strings.Operators as Operators exposing (Operator)
import Char


{-| Union type to represent possible valid characters.
-}
type Token
    = Op Operator
    | Digit Char
    | Whitespace


{-| Converts a Char to a Token while accounting for unkown Operators

    parseToken '2' == Ok (Digit '2')

-}
parseToken : Char -> Result String Token
parseToken ch =
    if ch == ' ' then
        Ok Whitespace
    else if Char.isDigit ch then
        Ok (Digit ch)
    else
        Result.map Op (Operators.fromChar ch)
