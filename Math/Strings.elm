module Math.Strings exposing (evaluateExpression)

{-| A math expression evaluator written in Elm.

    Evaluates parentheses, exponentials, addition, subtraction, multiplication, and integer division from a String representation.

#Evaluating expressions
@docs evaluateExpression

-}

import Stack exposing (Stack)
import Char
import Debug
import Math.Strings.Operators as Operators exposing (Operator(..))
import Math.Strings.Tokens exposing (..)
import Math.Strings.EvaluationState exposing (..)


{-| Evaluate a math expression

    evaluateExpression "5 * (10 / (10 - 8)) + 3" == Ok 28
    evaluateExpression "2^10" == Ok 1024
    evaluateExpression "invalid" == Err "'i' is not a supported Operator."
    evaluateExpression "6 + 4) / 5" == Err "Unmatched CloseParen at chararcter #6."
    evaluateExpression "6 + 4 / " == Err "Too few values for the operators."
    evaluateExpression "5 / (1 - 1)" == Err "Cannot divide by 0."

-}
evaluateExpression : String -> Result String Int
evaluateExpression expr =
    evaluate initialState expr


{-| Given a condition, recursively apply the top Operator to the top two values until the condition returns True.
-}
applyUntil : (Operator -> Bool) -> EvaluationState -> Result String EvaluationState
applyUntil cond state =
    case Stack.top state.operators of
        Just nextOp ->
            if cond nextOp then
                --if the condition says to stop applying
                Ok state
            else
                let
                    ( maybeB, tmpVals ) =
                        Stack.pop state.values

                    ( maybeA, newVals ) =
                        Stack.pop tmpVals

                    newValue =
                        Maybe.map2 (Operators.applyOperator nextOp) maybeB maybeA
                in
                    case newValue of
                        Just resultVal ->
                            let
                                ( _, newOperators ) =
                                    Stack.pop state.operators

                                updateState : Int -> EvaluationState
                                updateState value =
                                    { state | values = (Stack.push value newVals), operators = newOperators }
                            in
                                resultVal
                                    |> Result.map updateState
                                    |> Result.andThen (applyUntil cond)

                        Nothing ->
                            Err "Too few values for the operators"

        Nothing ->
            Ok state


{-| Update the EvaluationState token by token until the expression is empty. Once the string is empty, process remaining Operators in the EvaluationState.
-}
evaluate : EvaluationState -> String -> Result String Int
evaluate state expr =
    let
        vals =
            state.values

        runningVal =
            state.runningValue
    in
        case String.uncons expr of
            Nothing ->
                saveRunningVal state
                    |> Result.andThen (applyUntil (\x -> False))
                    |> Result.map (\s -> Stack.pop s.values)
                    |> Result.andThen
                        (\( maybeFinal, remaining ) ->
                            case maybeFinal of
                                Just finalValue ->
                                    case Stack.top remaining of
                                        Nothing ->
                                            Ok finalValue

                                        Just _ ->
                                            Err "Too few operators."

                                Nothing ->
                                    Err "Evaluation error: no final value."
                        )

            Just ( nextChar, exprTail ) ->
                parseToken nextChar
                    |> Result.andThen (processToken state)
                    |> Result.andThen (\newState -> evaluate newState exprTail)


{-| Update the EvaluationState by processing the Token
-}
processToken : EvaluationState -> Token -> Result String EvaluationState
processToken state token =
    let
        newState =
            incrementNumProcessed state
    in
        case token of
            Whitespace ->
                saveRunningVal newState

            Digit digitAsChar ->
                let
                    digitAsString =
                        String.fromChar digitAsChar

                    newRunning =
                        newState.runningValue
                            |> Maybe.map (\s -> String.append s digitAsString)
                            |> Maybe.withDefault digitAsString
                in
                    Ok { newState | runningValue = Just newRunning }

            Op operator ->
                saveRunningVal newState
                    |> Result.andThen (processOperator operator)


{-| Handle the Operator according to the current EvaluationState. There are three cases here:

1.  The Operator is '(' -> push to Operator stack
2.  The Operator is ')' -> apply all Operators on the stack until we reach the matching '(', then pop the '(' off the Operator stack
3.  The Operator is any other -> apply the Operators on top of the stack while they have a higher precedence than the new Operator, then push the new Operator on the Operator stack.

-}
processOperator : Operator -> EvaluationState -> Result String EvaluationState
processOperator op state =
    case op of
        OpenParen ->
            Ok (addOperator OpenParen state)

        CloseParen ->
            if state.nestedLevel < 1 then
                -- no matching open paren, err
                Err ("Unmatched CloseParen at chararcter #" ++ (toString state.numProcessed))
            else
                --process until we find the OpenParen
                applyUntil (\o -> o == OpenParen) state
                    |> Result.map popOperator

        _ ->
            --op is a valid operator. apply while it has precendence
            applyUntil (Operators.hasPrecedence op) state
                |> Result.map (addOperator op)
