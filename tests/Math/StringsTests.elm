module Math.StringsTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple, char)
import Test exposing (..)
import Math.Strings
import Debug


suite : Test
suite =
    describe "evaluateExpression"
        [ test "solves an expression" <|
            \_ ->
                Math.Strings.evaluateExpression "1 + 1"
                    |> Expect.equal (Ok 2)
        , test "solves an expression with differing precedences" <|
            \_ ->
                Math.Strings.evaluateExpression "1 + 2 * 5"
                    |> Expect.equal (Ok 11)
        , test "solves an expression with parens" <|
            \_ ->
                Math.Strings.evaluateExpression "(1 + 2) * 5"
                    |> Expect.equal (Ok 15)
        , test "solves an expression with nested parens" <|
            \_ ->
                Math.Strings.evaluateExpression "5 * (10 / (10 - 8)) + 3"
                    |> Expect.equal (Ok 28)
        , test "solves an expression with exponents" <|
            \_ ->
                Math.Strings.evaluateExpression "5^5 * (10 / (10 - 8)) + 3"
                    |> Expect.equal (Ok 15628)
        , test "reports an error when an expression has unmatched open paren" <|
            \_ ->
                Math.Strings.evaluateExpression "(1 + 2 * 5"
                    |> Expect.err
        , test "reports an error when an expression has unmatched close paren" <|
            \_ ->
                Math.Strings.evaluateExpression "1 + 2) * 5"
                    |> Expect.err
        , test "reports an error when an expression has not enough values for the number operators" <|
            \_ ->
                Math.Strings.evaluateExpression "1 + 2 *"
                    |> Expect.err
        , test "reports an error when an expression has too many values for the number operators" <|
            \_ ->
                Math.Strings.evaluateExpression "1 + 2 3"
                    |> Expect.err
        , test "reports an error when evaluating an empty string" <|
            \_ ->
                Math.Strings.evaluateExpression ""
                    |> Expect.err
        ]
