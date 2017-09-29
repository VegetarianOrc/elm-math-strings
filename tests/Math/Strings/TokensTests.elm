module Math.Strings.TokensTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, tuple, char)
import Test exposing (..)
import Math.Strings.Tokens as Tokens exposing (Token(..))


operatorFuzzer =
    [ '(', ')', '+', '-', '*', '/' ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


suite : Test
suite =
    describe "Tokens"
        [ describe "parseToken"
            [ test "interprests spaces as whitespace tokens" <|
                \_ ->
                    Tokens.parseToken ' '
                        |> Expect.equal (Ok Whitespace)
            , fuzz (Fuzz.intRange 0 9) "interprets numbers as digit tokens" <|
                \i ->
                    let
                        iAsChar =
                            toString i
                                |> String.uncons
                                |> Maybe.map (\( h, t ) -> h)
                                |> Maybe.withDefault '0'
                    in
                        Tokens.parseToken iAsChar
                            |> Expect.equal (Ok (Digit iAsChar))
            , fuzz operatorFuzzer "interprets operators as Operators" <|
                \c ->
                    Tokens.parseToken c
                        |> \token ->
                            case token of
                                Ok (Op _) ->
                                    Expect.pass

                                _ ->
                                    (String.fromChar c)
                                        ++ " is not an operator"
                                        |> Expect.fail
            ]
        ]
