open Links_lsp.Common
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

    method printer = log_to_file
    (* method printer = print_endline *)

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
  let r =
    ast_foldr#is_inside { line = p.position.line + 1; col = p.position.character + 1 }
  in
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

let content2 =
  {|
fun g() client {
  println("!")
}

fun f() server {
  <b l:onmousedown="{g()}">world</b>
}

page
  <html>
    <body>
      <b l:onmousedown="{replaceNode(f(), getNodeById("hole"))}">hello</b>
      <div><div id="hole">to be replaced</div></div>
    </body>
  </html>
    |}
;;

let content =
  {|
fun computeStep(count) server {
 count+1
}

fun showProgress(count, total) client {
 var percent = 100.0 *. intToFloat(count) /. intToFloat(total);
 replaceNode(
        <div id="bar"
             style="width:{floatToString(percent)}%;
                    background-color: black">|</div>,
        getNodeById("bar")
 )
}

fun compute(count, total) client {
 if (count < total) {
  showProgress(count, total);
  compute(computeStep(count), total)
 } else "done counting to " ^^ intToString(total)
}

fun showAnswer(answer) client {
 replaceNode(
    <div id="bar">{stringToXml(answer)}</div>,
        getNodeById("bar")
    )
}

page
 <html>
  <body>
   <form l:onsubmit="{showAnswer(compute(0, stringToInt(n)))}">
    <input type="text" l:name="n"/>
    <input type="submit"/>
   </form>
   <div id="bar"/>
  </body>
 </html>|}
;;

(* let%expect_test "prepare_rename" = *)
(*   let testing = new prepare_rename_traversal content in *)
(*   let ast = Linxer.Phases.Parse.string (get_init_context ()) content in *)
(*   let _ = testing#program ast.program_ in *)
(*   (); *)
(*   (1* testing#pp_phrase_positions (); *1) *)
(*   (1* print_endline (Links_core.Sugartypes.show_program ast.program_); *1) *)
(*   [%expect {| |}] *)
(* ;; *)

let read_whole_file filename =
  (* open_in_bin works correctly on Unix and Windows *)
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
;;

let%expect_test "prepare_rename3" =
  let _filepath =
    "/home/brandon/doc/uni/5th_year/diss/links/examples/silly-progress.links"
    (* "/home/brandon/doc/uni/5th_year/diss/links/examples/date.links" *)
  in
  let testing = new prepare_rename_traversal (read_whole_file _filepath) in
  let ast = Links_core.Loader.load (get_init_context ()) _filepath in
  let _ = testing#program ast.program_ in
  ();
  testing#pp_phrase_positions ();
  (* print_endline (Links_core.Sugartypes.show_program ast.program_); *)
  print_endline (string_of_bool (testing#is_inside { line = 1; col = 16 }));
  let test =
    is_within_range { line = 1; col = 5 } { line = 1; col = 16 } { line = 1; col = 8 }
  in
  (match test with
   | Inside -> print_endline "Inside"
   | After -> print_endline "After"
   | Before -> print_endline "Before");
  [%expect {| |}]
;;

let%expect_test "prepare_rename4" =
  let _filepath =
    "/home/brandon/doc/uni/5th_year/diss/links/examples/silly-progress.links"
    (* "/home/brandon/doc/uni/5th_year/diss/links/examples/date.links" *)
  in
  (* let testing = new prepare_rename_traversal content in *)
  let ast = Links_core.Loader.load (get_init_context ()) _filepath in
  (* let _ = testing#program ast.program_ in *)
  (* (); *)
  (* testing#pp_phrase_positions (); *)
  print_endline (Links_core.Sugartypes.show_program ast.program_);
  [%expect {| |}]
;;

(* let%expect_test "prepare_rename_2" = *)
(*   let ast = Linxer.Phases.Parse.string (get_init_context ()) content in *)
(*   print_endline (Links_core.Sugartypes.show_program ast.program_); *)
(*   [%expect {| Testing something: 1 |}] *)
(* ;; *)
