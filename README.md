A math expression evaluator written in Elm.    

Evaluates parentheses, exponentials, addition, subtraction, multiplication, and integer division from a String representation.

```
    import Math.Strings exposing (evaluateExpression)

    evaluateExpression "5 * (10 / (10 - 8)) + 3" == Ok 28
    evaluateExpression "2^10" == Ok 1024
    evaluateExpression "invalid" == Err "'i' is not a supported Operator."
    evaluateExpression "6 + 4) / 5" == Err "Unmatched CloseParen at chararcter #6."
    evaluateExpression "6 + 4 / " == Err "Too few values for the operators."
    evaluateExpression "5 / (1 - 1)" == Err "Cannot divide by 0."
```
