module Math.Strings.EvaluationState exposing (..)

{-| A type to track the state of an expression evaluation.
-}

import Math.Strings.Operators exposing (Operator(..))
import Stack exposing (Stack)


type alias EvaluationState =
    { values : Stack Int
    , numProcessed : Int
    , nestedLevel : Int
    , operators : Stack Operator
    , runningValue : Maybe String
    }


initialState : EvaluationState
initialState =
    { values = Stack.initialise
    , numProcessed = 0
    , nestedLevel = 0
    , operators = Stack.initialise
    , runningValue = Nothing
    }


incrementNumProcessed : EvaluationState -> EvaluationState
incrementNumProcessed state =
    { state | numProcessed = state.numProcessed + 1 }


{-| Attempts to convert the value stored in runningValue to an int.
If successful, adds the new value to the values stack.
-}
saveRunningVal : EvaluationState -> Result String EvaluationState
saveRunningVal state =
    case state.runningValue of
        Just sVal ->
            String.toInt sVal
                |> Result.map (\i -> Stack.push i state.values)
                |> Result.map (\newValues -> { state | values = newValues, runningValue = Nothing })

        Nothing ->
            Ok state


addOperator : Operator -> EvaluationState -> EvaluationState
addOperator op state =
    let
        newNestedLevel =
            case op of
                OpenParen ->
                    state.nestedLevel + 1

                _ ->
                    state.nestedLevel
    in
        { state | operators = Stack.push op state.operators, nestedLevel = newNestedLevel }


popOperator : EvaluationState -> EvaluationState
popOperator state =
    let
        ( lastOp, newOperators ) =
            Stack.pop state.operators

        newNestedLevel =
            case lastOp of
                Just OpenParen ->
                    state.nestedLevel - 1

                _ ->
                    state.nestedLevel
    in
        { state | operators = newOperators, nestedLevel = newNestedLevel }
