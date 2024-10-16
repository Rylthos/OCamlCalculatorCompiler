open Tokens

module Parser = struct
    type parser_action_T = SHIFT of int
                         | REDUCE of int

                         | ACCEPT
                         | ERROR

    type non_terminal_T  = E
                         | F
                         | G
                         | H
                         | I
                         | J

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

        match state with
        |  1 -> pop_stack 3; add_non_terminal E;
            (* Printf.printf ("E -> E + F\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.ADD_OP, left, right)) :: (drop !tree_stack 2);
            inputs := List.tl !inputs;

        |  2 -> pop_stack 3; add_non_terminal E;
            (* Printf.printf ("E -> E - F\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.SUB_OP, left, right)) :: (drop !tree_stack 2);
            inputs := List.tl !inputs;

        |  3 -> pop_stack 1; add_non_terminal E;
            (* Printf.printf ("E -> F\n"); *)

        |  4 -> pop_stack 3; add_non_terminal F;
            (* Printf.printf ("F -> F * G\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.MULT_OP, left, right)) :: (drop !tree_stack 2);

        |  5 -> pop_stack 3; add_non_terminal F;
            (* Printf.printf ("F -> F / G\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.DIV_OP, left, right)) :: (drop !tree_stack 2);

        |  6 -> pop_stack 1; add_non_terminal F;
            (* Printf.printf ("F -> G\n") *)

        |  7 -> pop_stack 3; add_non_terminal G;
            (* Printf.printf ("G -> H ^ G\n"); *)
            let left = List.nth !tree_stack 1 in
            let right = List.nth !tree_stack 0 in
            tree_stack := (DoubleNode (Tokens.POW_OP, left, right)) :: (drop !tree_stack 2);

        |  8 -> pop_stack 1; add_non_terminal G;
            (* Printf.printf ("G -> H\n") *)

        |  9 -> pop_stack 2; add_non_terminal H;
            (* Printf.printf ("H -> trig H\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (List.nth !inputs 0, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs;

        | 10 -> pop_stack 2; add_non_terminal H;
            (* Printf.printf ("H -> - H\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (Tokens.SUB_OP, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs

        | 11 -> pop_stack 1; add_non_terminal H;
            (* Printf.printf ("H -> I\n") *)

        | 12 -> pop_stack 2; add_non_terminal I;
            (* Printf.printf ("I -> I !\n"); *)
            let head = List.hd !tree_stack in
            tree_stack := (SingleNode (Tokens.FACT_OP, head)) :: (List.tl !tree_stack);
            inputs := List.tl !inputs

        | 13 -> pop_stack 1; add_non_terminal I;
            (* Printf.printf ("I -> J\n"); *)

        | 14 -> pop_stack 3; add_non_terminal J;
            (* Printf.printf ("J -> ( E )\n"); *)
            inputs := (List.hd (drop !inputs 1)) :: (drop !inputs 3)

        | 15 -> pop_stack 1; add_non_terminal J;
            (* Printf.printf ("J -> number\n"); *)
            tree_stack := (Leaf (List.nth !inputs 0)) :: !tree_stack;

        | _ -> raise Reduce_error

    let token_to_generic token =
        match token with
        | Tokens.ADD_OP | Tokens.SUB_OP
        | Tokens.MULT_OP| Tokens.DIV_OP
        | Tokens.L_PAREN | Tokens.R_PAREN
        | Tokens.FACT_OP | Tokens.POW_OP | Tokens.EOF -> token
        | Tokens.TRIG_OP _ -> Tokens.TRIG_OP SIN
        | Tokens.NUMBER _ -> Tokens.NUMBER ""

    let generate_action_table = fun () ->
        add_action_entry 0 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 0 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 0 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 0 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 0 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 0 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 0 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 0 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 0 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 0 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 0 (Tokens.EOF)         (ERROR    );

        add_action_entry 1 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 1 (Tokens.ADD_OP)      (SHIFT  11);
        add_action_entry 1 (Tokens.SUB_OP)      (SHIFT  12);
        add_action_entry 1 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 1 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 1 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 1 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 1 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 1 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 1 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 1 (Tokens.EOF)         (ACCEPT   );

        add_action_entry 2 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 2 (Tokens.ADD_OP)      (REDUCE  3);
        add_action_entry 2 (Tokens.SUB_OP)      (REDUCE  3);
        add_action_entry 2 (Tokens.MULT_OP)     (SHIFT  13);
        add_action_entry 2 (Tokens.DIV_OP)      (SHIFT  14);
        add_action_entry 2 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 2 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 2 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 2 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 2 (Tokens.R_PAREN)     (REDUCE  3);
        add_action_entry 2 (Tokens.EOF)         (REDUCE  3);

        add_action_entry 3 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 3 (Tokens.ADD_OP)      (REDUCE  3);
        add_action_entry 3 (Tokens.SUB_OP)      (REDUCE  3);
        add_action_entry 3 (Tokens.MULT_OP)     (REDUCE  6);
        add_action_entry 3 (Tokens.DIV_OP)      (REDUCE  6);
        add_action_entry 3 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 3 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 3 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 3 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 3 (Tokens.R_PAREN)     (REDUCE  6);
        add_action_entry 3 (Tokens.EOF)         (REDUCE  6);

        add_action_entry 4 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 4 (Tokens.ADD_OP)      (REDUCE  8);
        add_action_entry 4 (Tokens.SUB_OP)      (REDUCE  8);
        add_action_entry 4 (Tokens.MULT_OP)     (REDUCE  8);
        add_action_entry 4 (Tokens.DIV_OP)      (REDUCE  8);
        add_action_entry 4 (Tokens.POW_OP)      (SHIFT  15);
        add_action_entry 4 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 4 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 4 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 4 (Tokens.R_PAREN)     (REDUCE  8);
        add_action_entry 4 (Tokens.EOF)         (REDUCE  8);

        add_action_entry 5 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 5 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 5 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 5 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 5 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 5 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 5 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 5 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 5 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 5 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 5 (Tokens.EOF)         (ERROR    );

        add_action_entry 6 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 6 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 6 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 6 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 6 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 6 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 6 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 6 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 6 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 6 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 6 (Tokens.EOF)         (ERROR    );

        add_action_entry 7 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 7 (Tokens.ADD_OP)      (REDUCE 11);
        add_action_entry 7 (Tokens.SUB_OP)      (REDUCE 11);
        add_action_entry 7 (Tokens.MULT_OP)     (REDUCE 11);
        add_action_entry 7 (Tokens.DIV_OP)      (REDUCE 11);
        add_action_entry 7 (Tokens.POW_OP)      (REDUCE 11);
        add_action_entry 7 (Tokens.FACT_OP)     (SHIFT  18);
        add_action_entry 7 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 7 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 7 (Tokens.R_PAREN)     (REDUCE 11);
        add_action_entry 7 (Tokens.EOF)         (REDUCE 11);

        add_action_entry 8 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 8 (Tokens.ADD_OP)      (REDUCE 13);
        add_action_entry 8 (Tokens.SUB_OP)      (REDUCE 13);
        add_action_entry 8 (Tokens.MULT_OP)     (REDUCE 13);
        add_action_entry 8 (Tokens.DIV_OP)      (REDUCE 13);
        add_action_entry 8 (Tokens.POW_OP)      (REDUCE 13);
        add_action_entry 8 (Tokens.FACT_OP)     (REDUCE 13);
        add_action_entry 8 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 8 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 8 (Tokens.R_PAREN)     (REDUCE 13);
        add_action_entry 8 (Tokens.EOF)         (REDUCE 13);

        add_action_entry 9 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 9 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 9 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 9 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 9 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 9 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 9 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 9 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 9 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 9 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 9 (Tokens.EOF)         (ERROR    );

        add_action_entry 10 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 10 (Tokens.ADD_OP)      (REDUCE 15);
        add_action_entry 10 (Tokens.SUB_OP)      (REDUCE 15);
        add_action_entry 10 (Tokens.MULT_OP)     (REDUCE 15);
        add_action_entry 10 (Tokens.DIV_OP)      (REDUCE 15);
        add_action_entry 10 (Tokens.POW_OP)      (REDUCE 15);
        add_action_entry 10 (Tokens.FACT_OP)     (REDUCE 15);
        add_action_entry 10 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 10 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 10 (Tokens.R_PAREN)     (REDUCE 15);
        add_action_entry 10 (Tokens.EOF)         (REDUCE 15);

        add_action_entry 11 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 11 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 11 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 11 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 11 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 11 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 11 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 11 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 11 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 11 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 11 (Tokens.EOF)         (ERROR    );

        add_action_entry 12 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 12 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 12 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 12 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 12 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 12 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 12 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 12 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 12 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 12 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 12 (Tokens.EOF)         (ERROR    );

        add_action_entry 13 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 13 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 13 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 13 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 13 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 13 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 13 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 13 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 13 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 13 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 13 (Tokens.EOF)         (ERROR    );

        add_action_entry 14 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 14 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 14 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 14 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 14 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 14 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 14 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 14 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 14 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 14 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 14 (Tokens.EOF)         (ERROR    );

        add_action_entry 15 (Tokens.NUMBER "")   (SHIFT  10);
        add_action_entry 15 (Tokens.ADD_OP)      (ERROR    );
        add_action_entry 15 (Tokens.SUB_OP)      (SHIFT   6);
        add_action_entry 15 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 15 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 15 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 15 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 15 (Tokens.TRIG_OP SIN) (SHIFT   5);
        add_action_entry 15 (Tokens.L_PAREN)     (SHIFT   9);
        add_action_entry 15 (Tokens.R_PAREN)     (ERROR    );
        add_action_entry 15 (Tokens.EOF)         (ERROR    );

        add_action_entry 16 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 16 (Tokens.ADD_OP)      (REDUCE  9);
        add_action_entry 16 (Tokens.SUB_OP)      (REDUCE  9);
        add_action_entry 16 (Tokens.MULT_OP)     (REDUCE  9);
        add_action_entry 16 (Tokens.DIV_OP)      (REDUCE  9);
        add_action_entry 16 (Tokens.POW_OP)      (REDUCE  9);
        add_action_entry 16 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 16 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 16 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 16 (Tokens.R_PAREN)     (REDUCE  9);
        add_action_entry 16 (Tokens.EOF)         (REDUCE  9);

        add_action_entry 17 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 17 (Tokens.ADD_OP)      (REDUCE 10);
        add_action_entry 17 (Tokens.SUB_OP)      (REDUCE 10);
        add_action_entry 17 (Tokens.MULT_OP)     (REDUCE 10);
        add_action_entry 17 (Tokens.DIV_OP)      (REDUCE 10);
        add_action_entry 17 (Tokens.POW_OP)      (REDUCE 10);
        add_action_entry 17 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 17 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 17 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 17 (Tokens.R_PAREN)     (REDUCE 10);
        add_action_entry 17 (Tokens.EOF)         (REDUCE 10);

        add_action_entry 18 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 18 (Tokens.ADD_OP)      (REDUCE 12);
        add_action_entry 18 (Tokens.SUB_OP)      (REDUCE 12);
        add_action_entry 18 (Tokens.MULT_OP)     (REDUCE 12);
        add_action_entry 18 (Tokens.DIV_OP)      (REDUCE 12);
        add_action_entry 18 (Tokens.POW_OP)      (REDUCE 12);
        add_action_entry 18 (Tokens.FACT_OP)     (REDUCE 12);
        add_action_entry 18 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 18 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 18 (Tokens.R_PAREN)     (REDUCE 12);
        add_action_entry 18 (Tokens.EOF)         (REDUCE 12);

        add_action_entry 19 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 19 (Tokens.ADD_OP)      (SHIFT  11);
        add_action_entry 19 (Tokens.SUB_OP)      (SHIFT  12);
        add_action_entry 19 (Tokens.MULT_OP)     (ERROR    );
        add_action_entry 19 (Tokens.DIV_OP)      (ERROR    );
        add_action_entry 19 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 19 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 19 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 19 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 19 (Tokens.R_PAREN)     (SHIFT  25);
        add_action_entry 19 (Tokens.EOF)         (ERROR    );

        add_action_entry 20 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 20 (Tokens.ADD_OP)      (REDUCE  1);
        add_action_entry 20 (Tokens.SUB_OP)      (REDUCE  1);
        add_action_entry 20 (Tokens.MULT_OP)     (SHIFT  13);
        add_action_entry 20 (Tokens.DIV_OP)      (SHIFT  14);
        add_action_entry 20 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 20 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 20 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 20 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 20 (Tokens.R_PAREN)     (REDUCE  1);
        add_action_entry 20 (Tokens.EOF)         (REDUCE  1);

        add_action_entry 21 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 21 (Tokens.ADD_OP)      (REDUCE  2);
        add_action_entry 21 (Tokens.SUB_OP)      (REDUCE  2);
        add_action_entry 21 (Tokens.MULT_OP)     (SHIFT  13);
        add_action_entry 21 (Tokens.DIV_OP)      (SHIFT  14);
        add_action_entry 21 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 21 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 21 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 21 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 21 (Tokens.R_PAREN)     (REDUCE  2);
        add_action_entry 21 (Tokens.EOF)         (REDUCE  2);

        add_action_entry 22 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 22 (Tokens.ADD_OP)      (REDUCE  4);
        add_action_entry 22 (Tokens.SUB_OP)      (REDUCE  4);
        add_action_entry 22 (Tokens.MULT_OP)     (REDUCE  4);
        add_action_entry 22 (Tokens.DIV_OP)      (REDUCE  4);
        add_action_entry 22 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 22 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 22 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 22 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 22 (Tokens.R_PAREN)     (REDUCE  4);
        add_action_entry 22 (Tokens.EOF)         (REDUCE  4);

        add_action_entry 23 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 23 (Tokens.ADD_OP)      (REDUCE  5);
        add_action_entry 23 (Tokens.SUB_OP)      (REDUCE  5);
        add_action_entry 23 (Tokens.MULT_OP)     (REDUCE  5);
        add_action_entry 23 (Tokens.DIV_OP)      (REDUCE  5);
        add_action_entry 23 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 23 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 23 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 23 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 23 (Tokens.R_PAREN)     (REDUCE  5);
        add_action_entry 23 (Tokens.EOF)         (REDUCE  5);

        add_action_entry 24 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 24 (Tokens.ADD_OP)      (REDUCE  7);
        add_action_entry 24 (Tokens.SUB_OP)      (REDUCE  7);
        add_action_entry 24 (Tokens.MULT_OP)     (REDUCE  7);
        add_action_entry 24 (Tokens.DIV_OP)      (REDUCE  7);
        add_action_entry 24 (Tokens.POW_OP)      (ERROR    );
        add_action_entry 24 (Tokens.FACT_OP)     (ERROR    );
        add_action_entry 24 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 24 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 24 (Tokens.R_PAREN)     (REDUCE  7);
        add_action_entry 24 (Tokens.EOF)         (REDUCE  7);

        add_action_entry 25 (Tokens.NUMBER "")   (ERROR    );
        add_action_entry 25 (Tokens.ADD_OP)      (REDUCE 14);
        add_action_entry 25 (Tokens.SUB_OP)      (REDUCE 14);
        add_action_entry 25 (Tokens.MULT_OP)     (REDUCE 14);
        add_action_entry 25 (Tokens.DIV_OP)      (REDUCE 14);
        add_action_entry 25 (Tokens.POW_OP)      (REDUCE 14);
        add_action_entry 25 (Tokens.FACT_OP)     (REDUCE 14);
        add_action_entry 25 (Tokens.TRIG_OP SIN) (ERROR    );
        add_action_entry 25 (Tokens.L_PAREN)     (ERROR    );
        add_action_entry 25 (Tokens.R_PAREN)     (REDUCE 14);
        add_action_entry 25 (Tokens.EOF)         (REDUCE 14);

        ()

    let generate_goto_table = fun () ->
        add_goto_entry 0 E 1;
        add_goto_entry 0 F 2;
        add_goto_entry 0 G 3;
        add_goto_entry 0 H 4;
        add_goto_entry 0 I 7;
        add_goto_entry 0 J 8;

        add_goto_entry 1 E (-1);
        add_goto_entry 1 F (-1);
        add_goto_entry 1 G (-1);
        add_goto_entry 1 H (-1);
        add_goto_entry 1 I (-1);
        add_goto_entry 1 J (-1);

        add_goto_entry 2 E (-1);
        add_goto_entry 2 F (-1);
        add_goto_entry 2 G (-1);
        add_goto_entry 2 H (-1);
        add_goto_entry 2 I (-1);
        add_goto_entry 2 J (-1);

        add_goto_entry 3 E (-1);
        add_goto_entry 3 F (-1);
        add_goto_entry 3 G (-1);
        add_goto_entry 3 H (-1);
        add_goto_entry 3 I (-1);
        add_goto_entry 3 J (-1);

        add_goto_entry 4 E (-1);
        add_goto_entry 4 F (-1);
        add_goto_entry 4 G (-1);
        add_goto_entry 4 H (-1);
        add_goto_entry 4 I (-1);
        add_goto_entry 4 J (-1);

        add_goto_entry 5 E (-1);
        add_goto_entry 5 F (-1);
        add_goto_entry 5 G (-1);
        add_goto_entry 5 H 16;
        add_goto_entry 5 I 7;
        add_goto_entry 5 J 8;

        add_goto_entry 6 E (-1);
        add_goto_entry 6 F (-1);
        add_goto_entry 6 G (-1);
        add_goto_entry 6 H 17;
        add_goto_entry 6 I 7;
        add_goto_entry 6 J 8;

        add_goto_entry 7 E (-1);
        add_goto_entry 7 F (-1);
        add_goto_entry 7 G (-1);
        add_goto_entry 7 H (-1);
        add_goto_entry 7 I (-1);
        add_goto_entry 7 J (-1);

        add_goto_entry 8 E (-1);
        add_goto_entry 8 F (-1);
        add_goto_entry 8 G (-1);
        add_goto_entry 8 H (-1);
        add_goto_entry 8 I (-1);
        add_goto_entry 8 J (-1);

        add_goto_entry 9 E 19;
        add_goto_entry 9 F 2;
        add_goto_entry 9 G 3;
        add_goto_entry 9 H 4;
        add_goto_entry 9 I 7;
        add_goto_entry 9 J 8;

        add_goto_entry 10 E (-1);
        add_goto_entry 10 F (-1);
        add_goto_entry 10 G (-1);
        add_goto_entry 10 H (-1);
        add_goto_entry 10 I (-1);
        add_goto_entry 10 J (-1);

        add_goto_entry 11 E (-1);
        add_goto_entry 11 F 20;
        add_goto_entry 11 G 3;
        add_goto_entry 11 H 4;
        add_goto_entry 11 I 7;
        add_goto_entry 11 J 8;

        add_goto_entry 12 E (-1);
        add_goto_entry 12 F 21;
        add_goto_entry 12 G 3;
        add_goto_entry 12 H 4;
        add_goto_entry 12 I 7;
        add_goto_entry 12 J 8;

        add_goto_entry 13 E (-1);
        add_goto_entry 13 F (-1);
        add_goto_entry 13 G 22;
        add_goto_entry 13 H 4;
        add_goto_entry 13 I 7;
        add_goto_entry 13 J 8;

        add_goto_entry 14 E (-1);
        add_goto_entry 14 F (-1);
        add_goto_entry 14 G 23;
        add_goto_entry 14 H 4;
        add_goto_entry 14 I 7;
        add_goto_entry 14 J 8;

        add_goto_entry 15 E (-1);
        add_goto_entry 15 F (-1);
        add_goto_entry 15 G 24;
        add_goto_entry 15 H 4;
        add_goto_entry 15 I 7;
        add_goto_entry 15 J 8;

        add_goto_entry 16 E (-1);
        add_goto_entry 16 F (-1);
        add_goto_entry 16 G (-1);
        add_goto_entry 16 H (-1);
        add_goto_entry 16 I (-1);
        add_goto_entry 16 J (-1);

        add_goto_entry 17 E (-1);
        add_goto_entry 17 F (-1);
        add_goto_entry 17 G (-1);
        add_goto_entry 17 H (-1);
        add_goto_entry 17 I (-1);
        add_goto_entry 17 J (-1);

        add_goto_entry 18 E (-1);
        add_goto_entry 18 F (-1);
        add_goto_entry 18 G (-1);
        add_goto_entry 18 H (-1);
        add_goto_entry 18 I (-1);
        add_goto_entry 18 J (-1);

        add_goto_entry 19 E (-1);
        add_goto_entry 19 F (-1);
        add_goto_entry 19 G (-1);
        add_goto_entry 19 H (-1);
        add_goto_entry 19 I (-1);
        add_goto_entry 19 J (-1);

        add_goto_entry 20 E (-1);
        add_goto_entry 20 F (-1);
        add_goto_entry 20 G (-1);
        add_goto_entry 20 H (-1);
        add_goto_entry 20 I (-1);
        add_goto_entry 20 J (-1);

        add_goto_entry 21 E (-1);
        add_goto_entry 21 F (-1);
        add_goto_entry 21 G (-1);
        add_goto_entry 21 H (-1);
        add_goto_entry 21 I (-1);
        add_goto_entry 21 J (-1);

        add_goto_entry 22 E (-1);
        add_goto_entry 22 F (-1);
        add_goto_entry 22 G (-1);
        add_goto_entry 22 H (-1);
        add_goto_entry 22 I (-1);
        add_goto_entry 22 J (-1);

        add_goto_entry 23 E (-1);
        add_goto_entry 23 F (-1);
        add_goto_entry 23 G (-1);
        add_goto_entry 23 H (-1);
        add_goto_entry 23 I (-1);
        add_goto_entry 23 J (-1);

        add_goto_entry 24 E (-1);
        add_goto_entry 24 F (-1);
        add_goto_entry 24 G (-1);
        add_goto_entry 24 H (-1);
        add_goto_entry 24 I (-1);
        add_goto_entry 24 J (-1);

        add_goto_entry 25 E (-1);
        add_goto_entry 25 F (-1);
        add_goto_entry 25 G (-1);
        add_goto_entry 25 H (-1);
        add_goto_entry 25 I (-1);
        add_goto_entry 25 J (-1);

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
            let action = Hashtbl.find action_tbl (state, token_to_generic !a) in
            match action with
            | SHIFT t ->
                push_stack t;
                cached_inputs := !a :: !cached_inputs;
                a := next_symbol ()
            | REDUCE s ->
                reduce s cached_inputs;
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
