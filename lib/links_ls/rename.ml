(* open Links_lsp.Common *)
open Lsp
open Global
open Document_state
open Links_core.SourceCode

type position =
  { line : int
  ; col : int
  }

type range =
  | Before
  | After
  | Inside

(* TODO: optimise this to end after we have went past the pos *)
let is_within_range start_pos end_pos pos =
  let is_after_start =
    pos.line > start_pos.line || (pos.line = start_pos.line && pos.col >= start_pos.col)
  in
  let is_before_end =
    pos.line < end_pos.line || (pos.line = end_pos.line && pos.col <= end_pos.col - 1)
  in
  match is_after_start, is_before_end with
  | true, true -> Inside
  | true, false -> Before
  | false, true -> After
  | false, false -> Before
;;

let calc_pos ?(debug_info = "") str pos known_line =
  let len = String.length str in
  if pos < 0 || pos >= len
  then invalid_arg ("Position out of bounds: " ^ debug_info ^ " " ^ string_of_int pos);
  let rec find_line_start i line =
    if i >= len || line = known_line
    then i
    else if str.[i] = '\n'
    then find_line_start (i + 1) (line + 1)
    else find_line_start i (line + 1)
  in
  let start_index = find_line_start 0 1 in
  (* Now find the column number from the start_index *)
  let rec aux i col =
    if i = pos
    then { line = known_line; col }
    else if i >= len
    then { line = known_line; col } (* In case position is at the end *)
    else (
      let next_col = if str.[i] = '\n' then 1 else col + 1 in
      aux (i + 1) next_col)
  in
  aux start_index 1
;;

let get_real_position ?(name = "") pos content =
  let start = Position.start pos in
  let finish = Position.finish pos in
  ( calc_pos ~debug_info:name content start.pos_cnum start.pos_lnum
  , calc_pos ~debug_info:name content finish.pos_cnum finish.pos_lnum )
;;

(* Tuple in the form ('_, start_pos, end_pos) representing the *)
type _ renamable =
  | Phrase :
      (Links_core.Sugartypes.phrase * position * position)
      -> Links_core.Sugartypes.phrase renamable
  | Binding :
      (Links_core.Sugartypes.binding * position * position)
      -> Links_core.Sugartypes.binding renamable
  | Pattern :
      (Links_core.Sugartypes.Pattern.t * position * position)
      -> Links_core.Sugartypes.Pattern.t renamable

and any_t = Anything : 'a renamable -> any_t

class prepare_rename_traversal content =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super
    val mutable phrase_positions : any_t list = []
    method get_phrase_positions = phrase_positions
    method add_phrase_position i = phrase_positions <- i :: phrase_positions

    method is_inside pos =
      let pos = { line = pos.line + 1; col = pos.col + 1 } in
      let rec aux positions =
        match positions with
        | x :: xs ->
          let range, s, f =
            match x with
            | Anything (Phrase (_, start, finish)) ->
              is_within_range start finish pos, start, finish
            | Anything (Binding (_, start, finish)) ->
              is_within_range start finish pos, start, finish
            | Anything (Pattern (_, start, finish)) ->
              is_within_range start finish pos, start, finish
          in
          (match range with
           | Inside -> Some (s, f)
           | _ -> aux xs)
        | [] -> None
      in
      aux self#get_phrase_positions

    (* method printer = log_to_file *)
    method printer = print_endline

    method pp_phrase_positions () =
      let open Links_core.Sugartypes in
      let pp n s e =
        self#printer
          (n
           ^ ": ("
           ^ string_of_int s.line
           ^ ", "
           ^ string_of_int s.col
           ^ ") - ("
           ^ string_of_int e.line
           ^ ", "
           ^ string_of_int e.col
           ^ ")")
      in
      let pp_each (Anything item) =
        match item with
        | Phrase (p, start_pos, end_pos) ->
          (match WithPos.node p with
           | Var n -> pp n start_pos end_pos
           | _ -> ())
        | Binding (b, start_pos, end_pos) ->
          (match WithPos.node b with
           | Fun f -> pp (Binder.to_name f.fun_binder) start_pos end_pos
           | _ -> ())
        | Pattern (p, start, finish) ->
          (match p with
           | Variable v -> pp (Binder.to_name v) start finish
           | _ -> ())
      in
      List.iter pp_each (List.rev phrase_positions)

    method! phrase p =
      (match p.node with
       | Var _ ->
         let start, finish = get_real_position p.pos content in
         self#add_phrase_position (Anything (Phrase (p, start, finish)))
       | _ -> ());
      super#phrase p

    method! binding b =
      (match b.node with
       | Fun f ->
         let start, finish = get_real_position f.fun_binder.pos content in
         self#add_phrase_position (Anything (Binding (b, start, finish)))
       | _ -> ());
      super#binding b

    method! pattern p =
      (match p.node with
       | Variable _ ->
         let start, finish = get_real_position p.pos content in
         self#add_phrase_position (Anything (Pattern (p.node, start, finish)))
       | _ -> ());
      super#pattern p

    method! name n = super#name n
    method! binder n = super#binder n
    method! phrasenode p = super#phrasenode p
  end

(* TODO: Make return result for some failure cases *)
let prepare_rename (p : Types.PrepareRenameParams.t) =
  let doc = get_document p.textDocument.uri in
  let content =
    match doc with
    | None -> ""
    | Some v -> v.content
  in
  let ast_foldr = new prepare_rename_traversal content in
  let ast = Linxer.Phases.Parse.string (get_init_context ()) content in
  let _ = ast_foldr#program ast.program_ in
  let r = ast_foldr#is_inside { line = p.position.line; col = p.position.character } in
  match r with
  | Some (s, f) ->
    let start = Types.Position.create ~character:(s.col - 1) ~line:(s.line - 1) in
    let finish = Types.Position.create ~character:(f.col - 1) ~line:(f.line - 1) in
    let range = Types.Range.create ~end_:finish ~start in
    Types.Range.yojson_of_t range
  | None -> `Null
;;

(* let pos, s1, s2 = source_code#lookup (p.position.line, p.position.character) in *)

(** TESTS *)

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let rec find_root dir =
  let parent = Filename.dirname dir in
  if parent = dir
  then failwith "Reached filesystem root without finding _build"
  else if Filename.basename dir = "_build"
  then parent (* Found the _build dir, return its parent *)
  else find_root parent
;;

let testing_dir () = find_root (Sys.getcwd ()) ^ "/test/test_programs"

let%expect_test "prepare_rename_function" =
  let file1 = testing_dir () ^ "/silly-progress.links" in
  let file2 = testing_dir () ^ "/date.links" in
  let ast1 = Links_core.Loader.load (get_init_context ()) file1 in
  let ast2 = Links_core.Loader.load (get_init_context ()) file2 in
  let ast1_walker = new prepare_rename_traversal (read_whole_file file1) in
  let ast2_walker = new prepare_rename_traversal (read_whole_file file2) in
  let _ = ast1_walker#program ast1.program_ in
  let _ = ast2_walker#program ast2.program_ in
  let char_before_function_signature = { line = 0; col = 3 } in
  let first_char_function_signature = { line = 0; col = 4 } in
  let last_char_function_signature = { line = 0; col = 14 } in
  let char_after_function_signature = { line = 0; col = 15 } in
  let print_is_inside pos =
    match ast1_walker#is_inside pos with
    | Some _ -> print_endline "Can rename"
    | None -> print_endline "Cannot rename"
  in
  print_is_inside char_before_function_signature;
  print_is_inside first_char_function_signature;
  print_is_inside last_char_function_signature;
  print_is_inside char_after_function_signature;
  [%expect {|
    Cannot rename
    Can rename
    Can rename
    Cannot rename |}];
  (* let positions_to_test = [ { line = 0; col = 3 }, false; { line = 0; col = 4 }, true ] in *)
  (* let test_positions positions_to_test = *)
  (*   let aux *)
  ()
;;

(* let%expect_test "prepare_rename4" = *)
(*   let _filepath = *)
(*     "/home/brandon/doc/uni/5th_year/diss/links/examples/silly-progress.links" *)
(*     (1* "/home/brandon/doc/uni/5th_year/diss/links/examples/date.links" *1) *)
(*   in *)
(*   (1* let testing = new prepare_rename_traversal content in *1) *)
(*   let ast = Links_core.Loader.load (get_init_context ()) _filepath in *)
(*   (1* let _ = testing#program ast.program_ in *1) *)
(*   (1* (); *1) *)
(*   (1* testing#pp_phrase_positions (); *1) *)
(*   print_endline (Links_core.Sugartypes.show_program ast.program_); *)
(*   [%expect {| |}] *)
(* ;; *)

(* let%expect_test "prepare_rename_2" = *)
(*   let ast = Linxer.Phases.Parse.string (get_init_context ()) content in *)
(*   print_endline (Links_core.Sugartypes.show_program ast.program_); *)
(*   [%expect {| Testing something: 1 |}] *)
(* ;; *)
