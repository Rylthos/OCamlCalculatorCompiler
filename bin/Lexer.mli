module Lexer : sig
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

    val string_of_trig : trig_T -> string
    val string_of_token : token_T -> string

    val lexer : string -> token_T list
end
