# Ocaml Lexer / Parser
## Usage
```
dune exec CalculatorCompiler -- -i "<input_string>"
```
Where "<input_string>" can feature any mathematical equation that features
the symbols
```
+ -> Binary Addition
- -> Binary subtraction, Unary Minus
* -> Binary Multiplication
/ -> Binary Division
^ -> Power operator
sin, cos, tan -> Unary trig operations
! -> Unary power operation
(
)
```

## Design
The design of the parser is based on a LR Shift Reduce parser using action and goto tables
