open Lexer

module Parser : sig
    type tree_T = Leaf of Lexer.token_T
                | SingleNode of Lexer.token_T * tree_T
                | DoubleNode of Lexer.token_T * tree_T * tree_T

    val string_of_tree : tree_T -> string

    val generate_parse_tree : Lexer.token_T list -> tree_T
end
