open Tokens

module Parser : sig
    type tree_T = Leaf of Tokens.token_T
                | SingleNode of Tokens.token_T * tree_T
                | DoubleNode of Tokens.token_T * tree_T * tree_T

    val string_of_tree : tree_T -> string

    val generate_parse_tree : Tokens.token_T list -> tree_T
end
