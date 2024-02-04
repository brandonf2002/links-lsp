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

let is_within_range start_pos end_pos pos =
  let is_after_start =
    pos.line > start_pos.line || (pos.line = start_pos.line && pos.col >= start_pos.col)
  in
  let is_before_end =
    pos.line < end_pos.line || (pos.line = end_pos.line && pos.col <= end_pos.col)
  in
  match is_after_start, is_before_end with
  | true, true -> Inside
  | true, false -> Before
  | false, true -> After
  | false, false -> Before
;;

let char_to_line_col str pos known_line =
  let len = String.length str in
  if pos < 0 || pos >= len then invalid_arg "Position out of bounds";
  (* Find the starting index of the known line *)
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

(* Tuple in the form ('_, start_pos, end_pos) representing the *)
type _ renamable =
  | Phrase :
      (Links_core.Sugartypes.phrase * position * position)
      -> Links_core.Sugartypes.phrase renamable
  | Binding :
      (Links_core.Sugartypes.binding * position * position)
      -> Links_core.Sugartypes.binding renamable

and any_t = Anything : 'a renamable -> any_t

class prepare_rename_traversal content =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super
    val mutable phrase_positions : any_t list = []
    method get_phrase_positions = phrase_positions
    method add_phrase_position i = phrase_positions <- i :: phrase_positions

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
      in
      List.iter pp_each (List.rev phrase_positions)

    (* method! name n = *)
    (*   print_endline n; *)
    (*   super#string n *)

    method! phrasenode p =
      match p with
      | _ -> super#phrasenode p

    method! phrase p =
      match p.node with
      | Var _ ->
        let start = Position.start p.pos in
        let finish = Position.finish p.pos in
        let start_pos = char_to_line_col content start.pos_cnum start.pos_lnum in
        let finish_pos = char_to_line_col content finish.pos_cnum finish.pos_lnum in
        self#add_phrase_position (Anything (Phrase (p, start_pos, finish_pos)));
        (* self#add_phrase_position (start_pos, finish_pos); *)
        super#phrase p
      | _ -> super#phrase p

    method! binding b =
      let open Links_core.Sugartypes in
      (match b.node with
       | Fun f ->
         (* let name = Binder.to_name f.fun_binder in *)
         (* self#printer name; *)
         let start = Position.start f.fun_binder.pos in
         let finish = Position.finish f.fun_binder.pos in
         (* self#printer (Position.show b.pos); *)
         (* self#printer (Position.show f.fun_binder.pos); *)
         (* self#printer ""; *)
         let start_pos = char_to_line_col content start.pos_cnum start.pos_lnum in
         let finish_pos = char_to_line_col content finish.pos_cnum finish.pos_lnum in
         self#add_phrase_position (Anything (Binding (b, start_pos, finish_pos)));
         ()
       | _ -> ());
      super#binding b

    method! name n = super#name n

    method! binder n =
      (* let open Links_core.Sugartypes in *)
      (* self#printer (Binder.to_name n); *)
      (* self#printer (Position.show n.pos); *)
      super#binder n
  end

(* TODO: Make return result for some failure cases *)
let prepare_rename (p : Types.PrepareRenameParams.t) =
  "Testing something: " ^ string_of_int p.position.line |> log_to_file;
  "Testing something: " ^ string_of_int p.position.character |> log_to_file;
  (* get current docs *)
  let doc = get_document p.textDocument.uri in
  let content =
    match doc with
    | None -> ""
    | Some v -> v.content
  in
  (* let _, _ = Links_core.Parse.parse_string Links_core.Parse.program content in *)
  (* let _ = Linxer.Phases.evaluate_string (get_init_context ()) content in *)
  let testing = new prepare_rename_traversal content in
  let ast = Linxer.Phases.Parse.string (get_init_context ()) content in
  let _ = testing#program ast.program_ in
  ();
  testing#pp_phrase_positions ();
  (* let pos, s1, s2 = source_code#lookup (p.position.line, p.position.character) in *)
  `String ""
;;

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

let%expect_test "prepare_rename3" =
  let _filepath =
    "/home/brandon/doc/uni/5th_year/diss/links/examples/silly-progress.links"
  in
  let testing = new prepare_rename_traversal content in
  let ast = Links_core.Loader.load (get_init_context ()) _filepath in
  let _ = testing#program ast.program_ in
  ();
  testing#pp_phrase_positions ();
  print_endline (Links_core.Sugartypes.show_program ast.program_);
  [%expect {| |}]
;;

(* let%expect_test "prepare_rename_2" = *)
(*   let ast = Linxer.Phases.Parse.string (get_init_context ()) content in *)
(*   print_endline (Links_core.Sugartypes.show_program ast.program_); *)
(*   [%expect {| Testing something: 1 |}] *)
(* ;; *)
