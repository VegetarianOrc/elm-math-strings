module Math.Strings.Operators exposing (..)

{-| A type to represent valid Operators.
-}


type Operator
    = Addition
    | Subtraction
    | Multiplication
    | Division
    | OpenParen
    | CloseParen
    | Exponent


{-| Applies the given Operator to the two inputs. If the Operator cannot be applied, returns a String describing the error.

    applyOperator Addition 2 2 == Ok 4
    applyOperator Subtraction 2 4 == Ok 2

-}
applyOperator : Operator -> Int -> Int -> Result String Int
applyOperator op b a =
    case op of
        Addition ->
            Ok (a + b)

        Subtraction ->
            Ok (a - b)

        Multiplication ->
            Ok (a * b)

        Division ->
            case b of
                0 ->
                    Err "Cannot divide by 0."

                _ ->
                    Ok (a // b)

        Exponent ->
            Ok (a ^ b)

        _ ->
            let
                errString =
                    (toString op) ++ " cannot be applied."
            in
                Err errString


{-| Returns True if op1 has equal or higher precedence than op2

    hasPrecedence Multiplication Subtraction == True

-}
hasPrecedence : Operator -> Operator -> Bool
hasPrecedence op1 op2 =
    if (op2 == OpenParen) || (op2 == CloseParen) then
        True
    else if op1 == Exponent && (op2 == Exponent || op2 == Multiplication || op2 == Division || op2 == Addition || op2 == Subtraction) then
        True
    else if (op1 == Multiplication || op1 == Division) && (op2 == Addition || op2 == Subtraction) then
        True
    else
        False


{-| Returns the Operator that the given Char represents. If the Char does not represent an Operator, returns a message saying so.

    charTopOperator '+' == Ok Addition

-}
fromChar : Char -> Result String Operator
fromChar ch =
    case ch of
        '+' ->
            Ok Addition

        '-' ->
            Ok Subtraction

        '*' ->
            Ok Multiplication

        '/' ->
            Ok Division

        '(' ->
            Ok OpenParen

        ')' ->
            Ok CloseParen

        '^' ->
            Ok Exponent

        _ ->
            let
                errString =
                    "'" ++ (String.fromChar ch) ++ "' is not a supported Operator."
            in
                Err errString
