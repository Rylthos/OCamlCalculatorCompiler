open Lexer
open Parser

let input_string = ref ""
let usage_msg = "CalculatorCompiler -i <input>"
let anon_fun _ = ()

let speclist =
    [("-i", Arg.Set_string input_string, "What is the input that should be parsed")]

let () =
    Arg.parse speclist anon_fun usage_msg;
    Printf.printf "%s\n" !input_string;
    let tokens = Lexer.lexer !input_string in
    Parser.generate_parse_tree tokens
