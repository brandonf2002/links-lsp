open Lsp
open Document_state
open Links_lsp.Common
open Global
module StringSet = Set.Make (String)

class prepare_rename_traversal =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super
    val mutable phrase_positions = StringSet.empty
    method get_phrase_positions = phrase_positions
    method add_phrase_position i = phrase_positions <- StringSet.add i phrase_positions

    (* method printer = log_to_file *)
    method printer = print_endline

    method! name n =
      let _ = self#add_phrase_position n in
      super#name n

    method! binder n = super#binder n
    method! phrasenode p = super#phrasenode p
  end

let complation (r : Types.CompletionParams.t) =
  let line, col = r.position.line, r.position.character in
  log_to_file (string_of_int line ^ " " ^ string_of_int col);
  let doc = get_document r.textDocument.uri in
  let ast =
    match doc with
    | None -> None
    | Some v -> v.ast
  in
  match ast with
  | None -> `Null
  | Some a ->
    let ast_foldr = new prepare_rename_traversal in
    let _ = ast_foldr#program a.program_ in
    let item_list =
      StringSet.fold
        (fun s acc -> Types.CompletionItem.create ~label:s () :: acc)
        ast_foldr#get_phrase_positions
        []
    in
    let ret = Types.CompletionList.create ~isIncomplete:false ~items:item_list () in
    Types.CompletionList.yojson_of_t ret
;;

(* TESTS *)

let rec find_root dir =
  let parent = Filename.dirname dir in
  if parent = dir
  then failwith "Reached filesystem root without finding _build"
  else if Filename.basename dir = "_build"
  then parent (* Found the _build dir, return its parent *)
  else find_root parent
;;

let testing_dir () = find_root (Sys.getcwd ()) ^ "/test/test_programs"

let%expect_test "add to completion set" =
  let file1 = testing_dir () ^ "/silly-progress.links" in
  let _ = Links_core.Loader.load (get_init_context ()) file1 in
  let ast1_walker = new prepare_rename_traversal in
  ast1_walker#printer
    (string_of_bool (StringSet.is_empty ast1_walker#get_phrase_positions));
  [%expect {| true |}];
  let _ = ast1_walker#add_phrase_position "Hello" in
  ast1_walker#printer
    (string_of_bool (StringSet.is_empty ast1_walker#get_phrase_positions));
  [%expect {| false |}];
  ()
;;
