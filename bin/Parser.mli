open Lexer

module Parser : sig
    val generate_parse_tree : Lexer.token_T list -> unit
end
