module Tokens = struct
    type trig_T = SIN
                | COS
                | TAN

    type token_T = ADD_OP
                 | SUB_OP
                 | POW_OP
                 | TRIG_OP of trig_T
                 | FACT_OP
                 | NUMBER of string
                 | EOF

    let string_of_trig = function
        | SIN -> "SIN"
        | COS -> "COS"
        | TAN -> "TAN"

    let string_of_token = function
        | ADD_OP -> "<ADD_OP>"
        | SUB_OP -> "<SUB_OP>"
        | POW_OP -> "<POW_OP>"
        | TRIG_OP x -> Printf.sprintf "<TRIG_OP, %s>" (string_of_trig x)
        | FACT_OP -> "<FACT_OP>"
        | NUMBER x -> Printf.sprintf "<NUMBER, %s>" (x)
        | EOF -> "<EOF>"
end
