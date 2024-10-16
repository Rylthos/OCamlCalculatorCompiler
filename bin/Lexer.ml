open Tokens

module Lexer = struct
    let lex_input_str = ref ""
    let error_occured = ref false
    let current_character = ref 0

    let head = ref 0
    let forward = ref 0

    let number_regex =
        Re.seq [
            Re.start;
            (Re.Perl.re "([1-9][0-9]*|0)(\\.([0-9]*[1-9]|0)?)?(e[+-]?([1-9][0-9]*|0))?");
            Re.stop;
        ] |> Re.compile

    let unexpected_symbol = fun () ->
        Printf.printf "Unexpected symbol encountered: Character: %d\n%s\n%*s^\n" !current_character !lex_input_str !current_character " ";
        error_occured := true

    let unexpected_character c =
        Printf.printf "Unexpected character encountered %c: Character: %d\n%s\n%*s^\n" c !current_character !lex_input_str !current_character " ";
        error_occured := true

    let increment_forward i =
        forward := !forward + i;
        current_character := !current_character + i

    let get_offset = fun () -> !head + !forward

    let lex_trig str believed =
        let offset = get_offset () in
        try
            if Char.equal (String.get str (offset + 0)) (String.get believed 0) &&
               Char.equal (String.get str (offset + 1)) (String.get believed 1) &&
               Char.equal (String.get str (offset + 2)) (String.get believed 2) then (
                   true
            ) else (
                unexpected_symbol (); false
            )
        with Invalid_argument _ -> unexpected_symbol (); false

    let lex_number str =
        let continue_loop = ref true in
        let error = ref false in
        let get_substr = fun () ->
            String.sub str !head (!forward + 1)
        in
        while (get_offset ()) < (String.length str) && (!continue_loop && not !error) do
            (*
                Assume that head -> forward is a valid number
                forward + 1 is next character
            *)
            let current_char = String.get str (get_offset ()) in
            match current_char with
            | '.' -> (* Decimal *)
                increment_forward 1
            | 'e' -> (* Exponent. Possible +- *)
                (
                    increment_forward 1;
                    let next_char = String.get str (get_offset ()) in
                    match next_char with
                    | '+' -> increment_forward 1
                    | '-' -> increment_forward 1
                    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> ()
                    | _ -> unexpected_symbol (); error := true
                )
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                    if Re.execp number_regex (get_substr ()) then (
                        increment_forward 1
                    ) else (
                        unexpected_symbol (); error := true
                    )
            | _ -> continue_loop := false
        done;
        if not !error then
            let final_str = String.sub str !head !forward in
            Some final_str (* Needs to be converted into a number *)
        else
            None

    let lex_string str =
        head := 0;
        forward := 0;
        let outputs = ref [] in
        let add_symbol_to_output sym =
            outputs := sym :: !outputs;
            head := get_offset ();
            forward := 0
        in

        while (get_offset ()) < (String.length str) && not !error_occured do
            match (String.get str (get_offset ())) with
            | ' ' -> (* Whitespace *)
                increment_forward 1
            | '+' -> increment_forward 1; add_symbol_to_output Tokens.ADD_OP
            | '-' -> increment_forward 1; add_symbol_to_output Tokens.SUB_OP
            | '*' -> increment_forward 1; add_symbol_to_output Tokens.MULT_OP
            | '/' -> increment_forward 1; add_symbol_to_output Tokens.DIV_OP
            | '^' -> increment_forward 1; add_symbol_to_output Tokens.POW_OP
            | '!' -> increment_forward 1; add_symbol_to_output Tokens.FACT_OP
            | '(' -> increment_forward 1; add_symbol_to_output Tokens.L_PAREN
            | ')' -> increment_forward 1; add_symbol_to_output Tokens.R_PAREN
            | 's' -> if lex_trig str "sin" then (
                         increment_forward 3; add_symbol_to_output (Tokens.TRIG_OP Tokens.SIN)
                     )
            | 'c' -> if lex_trig str "cos" then (
                         increment_forward 3; add_symbol_to_output (Tokens.TRIG_OP Tokens.COS)
                     )
            | 't' -> if lex_trig str "tan" then (
                         increment_forward 3; add_symbol_to_output (Tokens.TRIG_OP Tokens.TAN)
                     )
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                    let output = lex_number str in
                    (
                        match output with
                        | Some n -> add_symbol_to_output (Tokens.NUMBER n)
                        | None -> ()
                    )
            | c -> unexpected_character c
        done;
        current_character := !current_character + 1;
        List.rev !outputs

    let lexer input_string =
        lex_input_str := String.trim input_string;
        let split = String.split_on_char ' ' !lex_input_str in
        let tokens = List.map lex_string split in
        let x = List.fold_left (fun x y -> x @ y) [] tokens in
        (x @ [EOF])
end
