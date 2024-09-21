open Tokens

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

    type tree_T = Leaf of Tokens.token_T
                | SingleNode of Tokens.token_T * tree_T
                | DoubleNode of Tokens.token_T * tree_T * tree_T

    exception Reduce_error
    exception Parse_error

    let action_tbl = Hashtbl.create 100
    let goto_tbl = Hashtbl.create 100

    let parser_stack = ref []
    let tree_stack = ref []

    let rec string_of_tree tree =
        match tree with
        | Leaf t -> Tokens.string_of_token t
        | SingleNode (h, l) -> Printf.sprintf "(%s, %s)" (Tokens.string_of_token h) (string_of_tree l)
        | DoubleNode (h, l, r) -> Printf.sprintf "(%s, %s, %s)" (Tokens.string_of_token h) (string_of_tree l) (string_of_tree r)

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

    let rec drop lst num =
        if num = 0 then lst
        else
            drop (List.tl lst) (num - 1)

    let reduce state (inputs : Tokens.token_T list ref) =
        let add_non_terminal non_terminal =
            let x = top_stack () in
            push_stack (Hashtbl.find goto_tbl (x, non_terminal))
        in

        (* Printf.printf "Reduce: "; *)
        match state with
        |  1 -> pop_stack 1; add_non_terminal E;
            (* Printf.printf ("E -> F\n"); *)

        |  2 -> pop_stack 3; add_non_terminal E;
            (* Printf.printf ("E -> E + F\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.ADD_OP, left, right)) :: (drop !tree_stack 2);
            inputs := List.tl !inputs;

        |  3 -> pop_stack 3; add_non_terminal E;
            (* Printf.printf ("E -> E - F\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.SUB_OP, left, right)) :: (drop !tree_stack 2);

        |  4 -> pop_stack 3; add_non_terminal F;
            (* Printf.printf ("F -> T ^ F\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.POW_OP, left, right)) :: (drop !tree_stack 2);

        |  5 -> pop_stack 1; add_non_terminal F;
            (* Printf.printf ("F -> T\n") *)

        |  6 -> pop_stack 2; add_non_terminal T;
            (* Printf.printf ("T -> trig T\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (List.nth !inputs 0, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs;

        |  7 -> pop_stack 2; add_non_terminal T;
            (* Printf.printf ("T -> - T\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (Tokens.SUB_OP, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs

        |  8 -> pop_stack 1; add_non_terminal T;
            (* Printf.printf ("T -> G\n") *)

        |  9 -> pop_stack 2; add_non_terminal G;
            (* Printf.printf ("G -> G !\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (Tokens.FACT_OP, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs

        | 10 -> pop_stack 1; add_non_terminal G;
            (* Printf.printf ("G -> H\n") *)

        | 11 -> pop_stack 1; add_non_terminal H;
            (* Printf.printf ("H -> number\n"); *)
            tree_stack := (Leaf (List.nth !inputs 0)) :: !tree_stack;
            inputs := List.tl !inputs

        | _ -> raise Reduce_error

    let token_to_generic token =
        match token with
        | Tokens.ADD_OP | Tokens.SUB_OP | Tokens.FACT_OP | Tokens.POW_OP | Tokens.EOF -> token
        | Tokens.TRIG_OP _ -> Tokens.TRIG_OP SIN
        | Tokens.NUMBER _ -> Tokens.NUMBER ""

    let generate_action_table = fun () ->
        add_action_entry 0 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 0 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 0 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 0 (Tokens.POW_OP)      (ERROR);
        add_action_entry 0 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 0 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 0 (Tokens.EOF)         (ERROR);

        add_action_entry 1 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 1 (Tokens.ADD_OP)      (SHIFT 9);
        add_action_entry 1 (Tokens.SUB_OP)      (SHIFT 10);
        add_action_entry 1 (Tokens.POW_OP)      (ERROR);
        add_action_entry 1 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 1 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 1 (Tokens.EOF)         (ACCEPT);

        add_action_entry 2 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 2 (Tokens.ADD_OP)      (REDUCE 1);
        add_action_entry 2 (Tokens.SUB_OP)      (REDUCE 1);
        add_action_entry 2 (Tokens.POW_OP)      (ERROR);
        add_action_entry 2 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 2 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 2 (Tokens.EOF)         (REDUCE 1);

        add_action_entry 3 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 3 (Tokens.ADD_OP)      (REDUCE 5);
        add_action_entry 3 (Tokens.SUB_OP)      (REDUCE 5);
        add_action_entry 3 (Tokens.POW_OP)      (SHIFT 11);
        add_action_entry 3 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 3 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 3 (Tokens.EOF)         (REDUCE 5);

        add_action_entry 4 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 4 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 4 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 4 (Tokens.POW_OP)      (ERROR);
        add_action_entry 4 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 4 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 4 (Tokens.EOF)         (ERROR);

        add_action_entry 5 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 5 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 5 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 5 (Tokens.POW_OP)      (ERROR);
        add_action_entry 5 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 5 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 5 (Tokens.EOF)         (ERROR);

        add_action_entry 6 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 6 (Tokens.ADD_OP)      (REDUCE 8);
        add_action_entry 6 (Tokens.SUB_OP)      (REDUCE 8);
        add_action_entry 6 (Tokens.POW_OP)      (REDUCE 8);
        add_action_entry 6 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 6 (Tokens.FACT_OP)     (SHIFT 14);
        add_action_entry 6 (Tokens.EOF)         (REDUCE 8);

        add_action_entry 7 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 7 (Tokens.ADD_OP)      (REDUCE 10);
        add_action_entry 7 (Tokens.SUB_OP)      (REDUCE 10);
        add_action_entry 7 (Tokens.POW_OP)      (REDUCE 10);
        add_action_entry 7 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 7 (Tokens.FACT_OP)     (REDUCE 10);
        add_action_entry 7 (Tokens.EOF)         (REDUCE 10);

        add_action_entry 8 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 8 (Tokens.ADD_OP)      (REDUCE 11);
        add_action_entry 8 (Tokens.SUB_OP)      (REDUCE 11);
        add_action_entry 8 (Tokens.POW_OP)      (REDUCE 11);
        add_action_entry 8 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 8 (Tokens.FACT_OP)     (REDUCE 11);
        add_action_entry 8 (Tokens.EOF)         (REDUCE 11);

        add_action_entry 9 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 9 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 9 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 9 (Tokens.POW_OP)      (ERROR);
        add_action_entry 9 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 9 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 9 (Tokens.EOF)         (ERROR);

        add_action_entry 10 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 10 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 10 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 10 (Tokens.POW_OP)      (ERROR);
        add_action_entry 10 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 10 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 10 (Tokens.EOF)         (ERROR);

        add_action_entry 11 (Tokens.NUMBER "")   (SHIFT 8);
        add_action_entry 11 (Tokens.ADD_OP)      (ERROR);
        add_action_entry 11 (Tokens.SUB_OP)      (SHIFT 5);
        add_action_entry 11 (Tokens.POW_OP)      (ERROR);
        add_action_entry 11 (Tokens.TRIG_OP SIN) (SHIFT 4);
        add_action_entry 11 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 11 (Tokens.EOF)         (ERROR);

        add_action_entry 12 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 12 (Tokens.ADD_OP)      (REDUCE 6);
        add_action_entry 12 (Tokens.SUB_OP)      (REDUCE 6);
        add_action_entry 12 (Tokens.POW_OP)      (REDUCE 6);
        add_action_entry 12 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 12 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 12 (Tokens.EOF)         (REDUCE 6);

        add_action_entry 13 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 13 (Tokens.ADD_OP)      (REDUCE 7);
        add_action_entry 13 (Tokens.SUB_OP)      (REDUCE 7);
        add_action_entry 13 (Tokens.POW_OP)      (REDUCE 7);
        add_action_entry 13 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 13 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 13 (Tokens.EOF)         (REDUCE 7);

        add_action_entry 14 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 14 (Tokens.ADD_OP)      (REDUCE 9);
        add_action_entry 14 (Tokens.SUB_OP)      (REDUCE 9);
        add_action_entry 14 (Tokens.POW_OP)      (REDUCE 9);
        add_action_entry 14 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 14 (Tokens.FACT_OP)     (REDUCE 9);
        add_action_entry 14 (Tokens.EOF)         (REDUCE 9);

        add_action_entry 15 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 15 (Tokens.ADD_OP)      (REDUCE 2);
        add_action_entry 15 (Tokens.SUB_OP)      (REDUCE 2);
        add_action_entry 15 (Tokens.POW_OP)      (ERROR);
        add_action_entry 15 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 15 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 15 (Tokens.EOF)         (REDUCE 2);

        add_action_entry 16 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 16 (Tokens.ADD_OP)      (REDUCE 3);
        add_action_entry 16 (Tokens.SUB_OP)      (REDUCE 3);
        add_action_entry 16 (Tokens.POW_OP)      (ERROR);
        add_action_entry 16 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 16 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 16 (Tokens.EOF)         (REDUCE 3);

        add_action_entry 17 (Tokens.NUMBER "")   (ERROR);
        add_action_entry 17 (Tokens.ADD_OP)      (REDUCE 4);
        add_action_entry 17 (Tokens.SUB_OP)      (REDUCE 4);
        add_action_entry 17 (Tokens.POW_OP)      (ERROR);
        add_action_entry 17 (Tokens.TRIG_OP SIN) (ERROR);
        add_action_entry 17 (Tokens.FACT_OP)     (ERROR);
        add_action_entry 17 (Tokens.EOF)         (REDUCE 4);

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
        parser_stack := [0]; (* Initial State *)

        let inputs = ref list in
        let cached_inputs = ref [] in
        let next_symbol = fun () ->
            let sym = List.hd (!inputs) in
            inputs := List.tl (!inputs);
            sym
        in

        let continue_loop = ref true in
        let error = ref false in
        let a = ref (next_symbol ()) in
        while !continue_loop do
            let state = top_stack () in
            (* Printf.printf "Find action_tbl %d %s\n" state (Tokens.string_of_token !a); *)
            let action = Hashtbl.find action_tbl (state, token_to_generic !a) in
            match action with
            | SHIFT t ->
                (* Printf.printf "Shift <%s, %d>\n" (Tokens.string_of_token !a) t; *)
                push_stack t;
                cached_inputs := !a :: !cached_inputs;
                a := next_symbol ()
            | REDUCE s ->
                (* Printf.printf "Reduce <%s, %d>\n" (Tokens.string_of_token !a) s; *)
                (* (Printf.printf "B Inputs: <"); List.iter (fun x -> Printf.printf "%s, " (Tokens.string_of_token x)) !cached_inputs; Printf.printf ">\n"; *)
                reduce s cached_inputs;
                (* (Printf.printf "A Inputs: <"); List.iter (fun x -> Printf.printf "%s, " (Tokens.string_of_token x)) !cached_inputs; Printf.printf ">\n" *)
            | ACCEPT ->
                continue_loop := false;
            | ERROR ->
                continue_loop := false; error := true;
        done;
        if not !error then (
            Printf.printf "Parsed Successfully\n";
            List.hd !tree_stack
        ) else (
            Printf.printf "An error occured\n";
            raise Parse_error
        )

end
