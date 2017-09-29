module Math.Strings.OperatorsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple, char)
import Test exposing (..)
import Math.Strings.Operators as Operators exposing (Operator(..))


notOpFuzzer : Fuzzer Char
notOpFuzzer =
    Fuzz.conditional { retries = 10, fallback = \c -> 'x', condition = \c -> not (List.member c [ '+', '-', '*', '/', '(', ')', '^' ]) } char


suite : Test
suite =
    describe "Operators"
        [ describe "applyOperator"
            [ fuzz (tuple ( int, int )) "applies addition" <|
                \( a, b ) ->
                    let
                        expected : Result String Int
                        expected =
                            Ok (a + b)

                        actual =
                            Operators.applyOperator Addition b a
                    in
                        Expect.equal expected actual
            , fuzz (tuple ( int, int )) "applies subtraction" <|
                \( a, b ) ->
                    let
                        expected : Result String Int
                        expected =
                            Ok (a - b)

                        actual =
                            Operators.applyOperator Subtraction b a
                    in
                        Expect.equal expected actual
            , fuzz (tuple ( int, int )) "applies multiplication" <|
                \( a, b ) ->
                    let
                        expected : Result String Int
                        expected =
                            Ok (a * b)

                        actual =
                            Operators.applyOperator Multiplication b a
                    in
                        Expect.equal expected actual
            , fuzz (tuple ( int, int )) "applies division" <|
                \( a, b ) ->
                    Operators.applyOperator Division b a
                        |> case b of
                            0 ->
                                Expect.err

                            _ ->
                                Expect.equal (Ok (a // b))
            , fuzz (tuple ( int, int )) "applies exponents" <|
                \( a, b ) ->
                    Operators.applyOperator Exponent b a
                        |> Expect.equal (Ok (a ^ b))
            , test "does not apply OpenParen" <|
                \_ ->
                    Operators.applyOperator OpenParen 1 1
                        |> Expect.err
            , test "does not apply CloseParen" <|
                \_ ->
                    Operators.applyOperator CloseParen 1 1
                        |> Expect.err
            ]
        , describe "hasPrecedence"
            [ describe "OpenParen"
                [ test "has equal or higher precedence than OpenParen" <|
                    \_ ->
                        Expect.true "OpenParen" (Operators.hasPrecedence OpenParen OpenParen)
                , test "has equal or higher precedence than CloseParen" <|
                    \_ ->
                        Expect.true "CloseParen" (Operators.hasPrecedence CloseParen OpenParen)
                , test "has lower precedence than Addition" <|
                    \_ ->
                        Expect.true "Addition" (Operators.hasPrecedence Addition OpenParen)
                , test "has lower precedence than Subtraction" <|
                    \_ ->
                        Expect.true "Subtraction" (Operators.hasPrecedence Subtraction OpenParen)
                , test "has lower precedence than Multiplication" <|
                    \_ ->
                        Expect.true "Multiplication" (Operators.hasPrecedence Multiplication OpenParen)
                , test "has lower precedence than Division" <|
                    \_ ->
                        Expect.true "Division" (Operators.hasPrecedence Division OpenParen)
                ]
            , describe "CloseParen"
                [ test "has equal or higher precedence than OpenParen" <|
                    \_ ->
                        Expect.true "OpenParen" (Operators.hasPrecedence OpenParen CloseParen)
                , test "has equal or higher precedence than CloseParen" <|
                    \_ ->
                        Expect.true "CloseParen" (Operators.hasPrecedence CloseParen CloseParen)
                , test "has lower precedence than Addition" <|
                    \_ ->
                        Expect.true "Addition" (Operators.hasPrecedence Addition CloseParen)
                , test "has lower precedence than Subtraction" <|
                    \_ ->
                        Expect.true "Subtraction" (Operators.hasPrecedence Subtraction CloseParen)
                , test "has lower precedence than Multiplication" <|
                    \_ ->
                        Expect.true "Multiplication" (Operators.hasPrecedence Multiplication CloseParen)
                , test "has lower precedence than Division" <|
                    \_ ->
                        Expect.true "Division" (Operators.hasPrecedence Division CloseParen)
                , describe "Addition"
                    [ test "has equal or higher precedence than OpenParen" <|
                        \_ ->
                            Expect.false "OpenParen" (Operators.hasPrecedence OpenParen Addition)
                    , test "has equal or higher precedence than CloseParen" <|
                        \_ ->
                            Expect.false "CloseParen" (Operators.hasPrecedence CloseParen Addition)
                    , test "has equal or higher precedence than Addition" <|
                        \_ ->
                            Expect.false "Addition" (Operators.hasPrecedence Addition Addition)
                    , test "has equal or higher precedence than Subtraction" <|
                        \_ ->
                            Expect.false "Subtraction" (Operators.hasPrecedence Subtraction Addition)
                    , test "has lower precedence than Multiplication" <|
                        \_ ->
                            Expect.true "Multiplication" (Operators.hasPrecedence Multiplication Addition)
                    , test "has lower precedence than Division" <|
                        \_ ->
                            Expect.true "Division" (Operators.hasPrecedence Division Addition)
                    ]
                , describe "Subtraction"
                    [ test "has equal or higher precedence than OpenParen" <|
                        \_ ->
                            Expect.false "OpenParen" (Operators.hasPrecedence OpenParen Subtraction)
                    , test "has equal or higher precedence than CloseParen" <|
                        \_ ->
                            Expect.false "CloseParen" (Operators.hasPrecedence CloseParen Subtraction)
                    , test "has equal or higher precedence than Addition" <|
                        \_ ->
                            Expect.false "Addition" (Operators.hasPrecedence Addition Subtraction)
                    , test "has equal or higher precedence than Subtraction" <|
                        \_ ->
                            Expect.false "Subtraction" (Operators.hasPrecedence Subtraction Subtraction)
                    , test "has lower precedence than Multiplication" <|
                        \_ ->
                            Expect.true "Multiplication" (Operators.hasPrecedence Multiplication Subtraction)
                    , test "has lower precedence than Division" <|
                        \_ ->
                            Expect.true "Division" (Operators.hasPrecedence Division Subtraction)
                    ]
                , describe "Multiplication"
                    [ test "has equal or higher precedence than OpenParen" <|
                        \_ ->
                            Expect.false "OpenParen" (Operators.hasPrecedence OpenParen Multiplication)
                    , test "has equal or higher precedence than CloseParen" <|
                        \_ ->
                            Expect.false "CloseParen" (Operators.hasPrecedence CloseParen Multiplication)
                    , test "has equal or higher precedence than Addition" <|
                        \_ ->
                            Expect.false "Addition" (Operators.hasPrecedence Addition Multiplication)
                    , test "has equal or higher precedence than Subtraction" <|
                        \_ ->
                            Expect.false "Subtraction" (Operators.hasPrecedence Subtraction Multiplication)
                    , test "has equal or higher precedence than Multiplication" <|
                        \_ ->
                            Expect.false "Multiplication" (Operators.hasPrecedence Multiplication Multiplication)
                    , test "has equal or higher precedence than Division" <|
                        \_ ->
                            Expect.false "Division" (Operators.hasPrecedence Division Multiplication)
                    ]
                , describe "Division"
                    [ test "has equal or higher precedence than OpenParen" <|
                        \_ ->
                            Expect.false "OpenParen" (Operators.hasPrecedence OpenParen Division)
                    , test "has equal or higher precedence than CloseParen" <|
                        \_ ->
                            Expect.false "CloseParen" (Operators.hasPrecedence CloseParen Division)
                    , test "has equal or higher precedence than Addition" <|
                        \_ ->
                            Expect.false "Addition" (Operators.hasPrecedence Addition Division)
                    , test "has equal or higher precedence than Subtraction" <|
                        \_ ->
                            Expect.false "Subtraction" (Operators.hasPrecedence Subtraction Division)
                    , test "has equal or higher precedence than Multiplication" <|
                        \_ ->
                            Expect.false "Multiplication" (Operators.hasPrecedence Multiplication Division)
                    , test "has equal or higher precedence than Division" <|
                        \_ ->
                            Expect.false "Division" (Operators.hasPrecedence Division Division)
                    ]
                ]
            ]
        , describe "fromChar"
            [ test "parses '+'" <|
                \_ ->
                    Operators.fromChar '+'
                        |> Expect.equal (Ok Addition)
            , test "parses '-'" <|
                \_ ->
                    Operators.fromChar '-'
                        |> Expect.equal (Ok Subtraction)
            , test "parses '*'" <|
                \_ ->
                    Operators.fromChar '*'
                        |> Expect.equal (Ok Multiplication)
            , test "parses '/'" <|
                \_ ->
                    Operators.fromChar '/'
                        |> Expect.equal (Ok Division)
            , test "parses '('" <|
                \_ ->
                    Operators.fromChar '('
                        |> Expect.equal (Ok OpenParen)
            , test "parses ')'" <|
                \_ ->
                    Operators.fromChar ')'
                        |> Expect.equal (Ok CloseParen)
            , test "parses '^'" <|
                \_ ->
                    Operators.fromChar '^'
                        |> Expect.equal (Ok Exponent)
            , fuzz notOpFuzzer "fails on unexpected input" <|
                \c ->
                    Operators.fromChar c
                        |> Expect.err
            ]
        ]
