let number_regex =
    Re.seq [
        Re.start;
        (Re.Perl.re "([1-9][0-9]*|0)(\\.([0-9]*[1-9]|0)?)?(e[+-]?([1-9][0-9]*|0))?");
        Re.stop;
        ] |> Re.compile;;


type trig_T = SIN
            | COS
            | TAN

type tokens = ADD_OP
             | SUB_OP
             | POW_OP
             | TRIG_OP of trig_T
             | FACT_OP
             | NUMBER of string

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


let input_string = ref ""

let usage_msg = "CalculatorCompiler -i <input>"
let anon_fun _ = ()

let speclist =
    [("-i", Arg.Set_string input_string, "What is the input that should be parsed")]

let error = ref false
let character = ref 0
let lexer_string = ref ""

let unexpected_symbol = fun () ->
    Printf.printf "Unexpected Symbol occured\n%s\n%*s^\n" !lexer_string !character " ";
    error := true

let lexer str =
    lexer_string := String.trim str;
    let arr = String.split_on_char ' ' !lexer_string in
    let lex_string s =
        let head = ref 0 in
        let forward = ref 0 in
        let outputs = ref [] in
        let get_offset = fun () -> !head + !forward in
        let increment_forward x = forward := !forward + x; character := !character + x in
        let add_symbol s = outputs := s :: !outputs; head := get_offset(); forward := 0  in

        let check_for_trig target =
            try
                if Char.equal (String.get s (get_offset () + 0)) (String.get target 0) &&
                   Char.equal (String.get s (get_offset () + 1)) (String.get target 1) &&
                   Char.equal (String.get s (get_offset () + 2)) (String.get target 2) then
                       true
                else
                    let _ = 1 in unexpected_symbol (); false
            with Invalid_argument _ -> unexpected_symbol (); false
    in

    let get_digit = fun () ->
        let valid = ref true in
        let sub_str = ref (String.sub s (!head) (!forward + 1)) in
        let last_char = fun () -> String.get !sub_str ((String.length !sub_str) - 1) in
        while (Re.execp number_regex !sub_str) && !valid do
            increment_forward 1;
            try
                sub_str := (String.sub s (!head) (!forward + 1));
                if (last_char () = 'e') then (
                    increment_forward 1;
                    sub_str := (String.sub s (!head) (!forward + 1));
                    if (last_char () = '+' || last_char () = '-') then (
                        increment_forward 1;
                        sub_str := (String.sub s (!head) (!forward + 1))
                    ) else ()
                ) else ()
            with _ -> valid := false
        done;
        sub_str := (String.sub s (!head) (!forward));
        add_symbol (NUMBER !sub_str)
    in
    while (get_offset ()) < (String.length s) && not !error do
        match (String.get s (get_offset ())) with
        | ' ' -> increment_forward 1;
        | '+' -> increment_forward 1; add_symbol ADD_OP
        | '-' -> increment_forward 1; add_symbol SUB_OP
        | '^' -> increment_forward 1; add_symbol POW_OP
        | 's' -> if check_for_trig "sin" then (increment_forward 3; add_symbol (TRIG_OP SIN))
        | 'c' -> if check_for_trig "cos" then (increment_forward 3; add_symbol (TRIG_OP COS))
        | 't' -> if check_for_trig "tan" then (increment_forward 3; add_symbol (TRIG_OP TAN))
        | '!' -> increment_forward 1; add_symbol FACT_OP
        |  _  -> get_digit () (* CHECK FOR DIGIT *)
    done;
    character := !character + 1;
    List.rev !outputs
    in
  let tokens = List.map lex_string arr in
  if !error then
      exit 1
            else
                List.iter (fun x -> List.iter (fun y -> Printf.printf "%s\n" (string_of_token y)) x) tokens;
  ()

    let () =
        Arg.parse speclist anon_fun usage_msg;
        Printf.printf "%s\n" !input_string;
        lexer !input_string
