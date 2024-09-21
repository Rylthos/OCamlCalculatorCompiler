open Tokens

module Lexer : sig
    val lexer : string -> Tokens.token_T list
end
