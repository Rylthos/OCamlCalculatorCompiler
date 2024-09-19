open Lexer

module Parser = struct
    type parser_action_T = SHIFT of int
                         | REDUCE of int
                         | ACCEPT
                         | ERROR

    type non_terminal_T  = E
                         | F
                         | T
                         | G
                         | H

    exception Reduce_error

    let action_tbl = Hashtbl.create 100
    let goto_tbl = Hashtbl.create 100

    let parser_stack = ref []

    let add_action_entry state token action =
        Hashtbl.add action_tbl (state, token) action

    let add_goto_entry current_state terminal target_state =
        Hashtbl.add goto_tbl (current_state, terminal) target_state

    let push_stack s =
        parser_stack := s :: !parser_stack

    let top_stack = fun () ->
        List.hd (!parser_stack)

    let rec pop_stack num =
        if num = 0 then (
            ()
        ) else (
            parser_stack := (List.tl !parser_stack);
            pop_stack (num - 1)
        )


    let reduce state =
        let add_non_terminal non_terminal =
            let x = top_stack () in
            push_stack (Hashtbl.find goto_tbl (x, non_terminal))
        in
        Printf.printf "Reduce: ";
        match state with
        |  1 -> pop_stack 1; add_non_terminal E; Printf.printf ("E -> F\n");
        |  2 -> pop_stack 3; add_non_terminal E; Printf.printf ("E -> E + F\n")
        |  3 -> pop_stack 3; add_non_terminal E; Printf.printf ("E -> E - F\n")
        |  4 -> pop_stack 3; add_non_terminal F; Printf.printf ("F -> T ^ F\n")
        |  5 -> pop_stack 1; add_non_terminal F; Printf.printf ("F -> T\n")
        |  6 -> pop_stack 2; add_non_terminal T; Printf.printf ("T -> trig T\n")
        |  7 -> pop_stack 2; add_non_terminal T; Printf.printf ("T -> - T\n")
        |  8 -> pop_stack 1; add_non_terminal T; Printf.printf ("T- > G\n")
        |  9 -> pop_stack 2; add_non_terminal G; Printf.printf ("G -> G !\n")
        | 10 -> pop_stack 1; add_non_terminal G; Printf.printf ("G -> H\n")
        | 11 -> pop_stack 1; add_non_terminal H; Printf.printf ("H -> number\n")
        | _ -> raise Reduce_error

    let token_to_generic token =
        match token with
        | Lexer.ADD_OP | Lexer.SUB_OP | Lexer.FACT_OP | Lexer.POW_OP | Lexer.EOF -> token
        | Lexer.TRIG_OP _ -> Lexer.TRIG_OP SIN
        | Lexer.NUMBER _ -> Lexer.NUMBER ""

    let generate_action_table = fun () ->
        add_action_entry 0 (Lexer.NUMBER "")   (SHIFT 8);
        add_action_entry 0 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 0 (Lexer.SUB_OP)      (SHIFT 8);
        add_action_entry 0 (Lexer.POW_OP)      (ERROR);
        add_action_entry 0 (Lexer.TRIG_OP SIN) (SHIFT 8);
        add_action_entry 0 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 0 (Lexer.EOF)         (ERROR);

        add_action_entry 1 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 1 (Lexer.ADD_OP)      (SHIFT 9);
        add_action_entry 1 (Lexer.SUB_OP)      (SHIFT 10);
        add_action_entry 1 (Lexer.POW_OP)      (ERROR);
        add_action_entry 1 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 1 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 1 (Lexer.EOF)         (ACCEPT);

        add_action_entry 2 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 2 (Lexer.ADD_OP)      (REDUCE 1);
        add_action_entry 2 (Lexer.SUB_OP)      (REDUCE 1);
        add_action_entry 2 (Lexer.POW_OP)      (ERROR);
        add_action_entry 2 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 2 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 2 (Lexer.EOF)         (REDUCE 1);

        add_action_entry 3 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 3 (Lexer.ADD_OP)      (REDUCE 5);
        add_action_entry 3 (Lexer.SUB_OP)      (REDUCE 5);
        add_action_entry 3 (Lexer.POW_OP)      (SHIFT 11);
        add_action_entry 3 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 3 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 3 (Lexer.EOF)         (REDUCE 5);

        add_action_entry 4 (Lexer.NUMBER "")   (SHIFT 5);
        add_action_entry 4 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 4 (Lexer.SUB_OP)      (SHIFT 5);
        add_action_entry 4 (Lexer.POW_OP)      (ERROR);
        add_action_entry 4 (Lexer.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 4 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 4 (Lexer.EOF)         (ERROR);

        add_action_entry 5 (Lexer.NUMBER "")   (SHIFT 8);
        add_action_entry 5 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 5 (Lexer.SUB_OP)      (SHIFT 5);
        add_action_entry 5 (Lexer.POW_OP)      (ERROR);
        add_action_entry 5 (Lexer.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 5 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 5 (Lexer.EOF)         (ERROR);

        add_action_entry 6 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 6 (Lexer.ADD_OP)      (REDUCE 8);
        add_action_entry 6 (Lexer.SUB_OP)      (REDUCE 8);
        add_action_entry 6 (Lexer.POW_OP)      (REDUCE 8);
        add_action_entry 6 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 6 (Lexer.FACT_OP)     (SHIFT 14);
        add_action_entry 6 (Lexer.EOF)         (REDUCE 8);

        add_action_entry 7 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 7 (Lexer.ADD_OP)      (REDUCE 10);
        add_action_entry 7 (Lexer.SUB_OP)      (REDUCE 10);
        add_action_entry 7 (Lexer.POW_OP)      (REDUCE 10);
        add_action_entry 7 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 7 (Lexer.FACT_OP)     (REDUCE 10);
        add_action_entry 7 (Lexer.EOF)         (REDUCE 10);

        add_action_entry 8 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 8 (Lexer.ADD_OP)      (REDUCE 11);
        add_action_entry 8 (Lexer.SUB_OP)      (REDUCE 11);
        add_action_entry 8 (Lexer.POW_OP)      (REDUCE 11);
        add_action_entry 8 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 8 (Lexer.FACT_OP)     (REDUCE 11);
        add_action_entry 8 (Lexer.EOF)         (REDUCE 11);

        add_action_entry 9 (Lexer.NUMBER "")   (SHIFT 8);
        add_action_entry 9 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 9 (Lexer.SUB_OP)      (SHIFT 5);
        add_action_entry 9 (Lexer.POW_OP)      (ERROR);
        add_action_entry 9 (Lexer.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 9 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 9 (Lexer.EOF)         (ERROR);

        add_action_entry 10 (Lexer.NUMBER "")   (SHIFT 8);
        add_action_entry 10 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 10 (Lexer.SUB_OP)      (SHIFT 5);
        add_action_entry 10 (Lexer.POW_OP)      (ERROR);
        add_action_entry 10 (Lexer.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 10 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 10 (Lexer.EOF)         (ERROR);

        add_action_entry 11 (Lexer.NUMBER "")   (SHIFT 8);
        add_action_entry 11 (Lexer.ADD_OP)      (ERROR);
        add_action_entry 11 (Lexer.SUB_OP)      (SHIFT 5);
        add_action_entry 11 (Lexer.POW_OP)      (ERROR);
        add_action_entry 11 (Lexer.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 11 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 11 (Lexer.EOF)         (ERROR);

        add_action_entry 12 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 12 (Lexer.ADD_OP)      (REDUCE 6);
        add_action_entry 12 (Lexer.SUB_OP)      (REDUCE 6);
        add_action_entry 12 (Lexer.POW_OP)      (REDUCE 6);
        add_action_entry 12 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 12 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 12 (Lexer.EOF)         (REDUCE 6);

        add_action_entry 13 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 13 (Lexer.ADD_OP)      (REDUCE 7);
        add_action_entry 13 (Lexer.SUB_OP)      (REDUCE 7);
        add_action_entry 13 (Lexer.POW_OP)      (REDUCE 7);
        add_action_entry 13 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 13 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 13 (Lexer.EOF)         (REDUCE 7);

        add_action_entry 14 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 14 (Lexer.ADD_OP)      (REDUCE 9);
        add_action_entry 14 (Lexer.SUB_OP)      (REDUCE 9);
        add_action_entry 14 (Lexer.POW_OP)      (REDUCE 9);
        add_action_entry 14 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 14 (Lexer.FACT_OP)     (REDUCE 9);
        add_action_entry 14 (Lexer.EOF)         (REDUCE 9);

        add_action_entry 15 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 15 (Lexer.ADD_OP)      (REDUCE 2);
        add_action_entry 15 (Lexer.SUB_OP)      (REDUCE 2);
        add_action_entry 15 (Lexer.POW_OP)      (ERROR);
        add_action_entry 15 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 15 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 15 (Lexer.EOF)         (REDUCE 2);

        add_action_entry 16 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 16 (Lexer.ADD_OP)      (REDUCE 3);
        add_action_entry 16 (Lexer.SUB_OP)      (REDUCE 3);
        add_action_entry 16 (Lexer.POW_OP)      (ERROR);
        add_action_entry 16 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 16 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 16 (Lexer.EOF)         (REDUCE 3);

        add_action_entry 17 (Lexer.NUMBER "")   (ERROR);
        add_action_entry 17 (Lexer.ADD_OP)      (REDUCE 4);
        add_action_entry 17 (Lexer.SUB_OP)      (REDUCE 4);
        add_action_entry 17 (Lexer.POW_OP)      (ERROR);
        add_action_entry 17 (Lexer.TRIG_OP SIN) (ERROR);
        add_action_entry 17 (Lexer.FACT_OP)     (ERROR);
        add_action_entry 17 (Lexer.EOF)         (REDUCE 4);

        ()

    let generate_goto_table = fun () ->
        add_goto_entry 0 E   1;
        add_goto_entry 0 F   2;
        add_goto_entry 0 T   3;
        add_goto_entry 0 G   6;
        add_goto_entry 0 H   7;

        add_goto_entry 1 E  (-1);
        add_goto_entry 1 F  (-1);
        add_goto_entry 1 T  (-1);
        add_goto_entry 1 G  (-1);
        add_goto_entry 1 H  (-1);

        add_goto_entry 2 E  (-1);
        add_goto_entry 2 F  (-1);
        add_goto_entry 2 T  (-1);
        add_goto_entry 2 G  (-1);
        add_goto_entry 2 H  (-1);

        add_goto_entry 3 E  (-1);
        add_goto_entry 3 F  (-1);
        add_goto_entry 3 T  (-1);
        add_goto_entry 3 G  (-1);
        add_goto_entry 3 H  (-1);

        add_goto_entry 4 E  (-1);
        add_goto_entry 4 F  (-1);
        add_goto_entry 4 T  12;
        add_goto_entry 4 G   6;
        add_goto_entry 4 H   7;

        add_goto_entry 5 E  (-1);
        add_goto_entry 5 F  (-1);
        add_goto_entry 5 T  13;
        add_goto_entry 5 G   6;
        add_goto_entry 5 H   7;

        add_goto_entry 6 E  (-1);
        add_goto_entry 6 F  (-1);
        add_goto_entry 6 T  (-1);
        add_goto_entry 6 G  (-1);
        add_goto_entry 6 H  (-1);

        add_goto_entry 7 E  (-1);
        add_goto_entry 7 F  (-1);
        add_goto_entry 7 T  (-1);
        add_goto_entry 7 G  (-1);
        add_goto_entry 7 H  (-1);

        add_goto_entry 8 E  (-1);
        add_goto_entry 8 F  (-1);
        add_goto_entry 8 T  (-1);
        add_goto_entry 8 G  (-1);
        add_goto_entry 8 H  (-1);

        add_goto_entry 9 E  (-1);
        add_goto_entry 9 F  15;
        add_goto_entry 9 T   3;
        add_goto_entry 9 G   6;
        add_goto_entry 9 H   7;

        add_goto_entry 10 E  (-1);
        add_goto_entry 10 F  16;
        add_goto_entry 10 T   3;
        add_goto_entry 10 G   6;
        add_goto_entry 10 H   7;

        add_goto_entry 11 E  (-1);
        add_goto_entry 11 F  17;
        add_goto_entry 11 T   3;
        add_goto_entry 11 G   6;
        add_goto_entry 11 H   7;

        add_goto_entry 12 E  (-1);
        add_goto_entry 12 F  (-1);
        add_goto_entry 12 T  (-1);
        add_goto_entry 12 G  (-1);
        add_goto_entry 12 H  (-1);

        add_goto_entry 13 E  (-1);
        add_goto_entry 13 F  (-1);
        add_goto_entry 13 T  (-1);
        add_goto_entry 13 G  (-1);
        add_goto_entry 13 H  (-1);

        add_goto_entry 14 E  (-1);
        add_goto_entry 14 F  (-1);
        add_goto_entry 14 T  (-1);
        add_goto_entry 14 G  (-1);
        add_goto_entry 14 H  (-1);

        add_goto_entry 15 E  (-1);
        add_goto_entry 15 F  (-1);
        add_goto_entry 15 T  (-1);
        add_goto_entry 15 G  (-1);
        add_goto_entry 15 H  (-1);

        add_goto_entry 16 E  (-1);
        add_goto_entry 16 F  (-1);
        add_goto_entry 16 T  (-1);
        add_goto_entry 16 G  (-1);
        add_goto_entry 16 H  (-1);

        add_goto_entry 17 E  (-1);
        add_goto_entry 17 F  (-1);
        add_goto_entry 17 T  (-1);
        add_goto_entry 17 G  (-1);
        add_goto_entry 17 H  (-1);

        ()

    let generate_tables = fun () ->
        generate_action_table ();
        generate_goto_table ()

    let generate_parse_tree list =
        generate_tables();
        parser_stack := [0];

        let inputs = ref list in
        let next_symbol = fun () ->
            let sym = List.hd (!inputs) in
            inputs := List.tl (!inputs);
            sym
        in

        let continue_loop = ref true in
        let a = ref (next_symbol ()) in
        while !continue_loop do
            let state = top_stack () in
            (* Printf.printf "Find action_tbl %d %s\n" state (Lexer.string_of_token !a); *)
            let action = Hashtbl.find action_tbl (state, token_to_generic !a) in
            match action with
            | SHIFT t ->
                (* Printf.printf "Shift <%s, %d>\n" (Lexer.string_of_token !a) t; *)
                push_stack t;
                a := next_symbol ()
            | REDUCE s ->
                    (* Printf.printf "Reduce <%s, %d>\n" (Lexer.string_of_token !a) s; *)
                reduce s
            | ACCEPT ->
                continue_loop := false
            | ERROR ->
                continue_loop := false; Printf.printf "Error\n"
        done

end
