open Lsp
open Document_state
open Links_lsp.Common
open Global

type item_info = { kind : Types.CompletionItemKind.t }

module ItemTable = Hashtbl.Make (struct
    type t = String.t

    let equal = String.equal
    let hash = String.hash
  end)

let items = ItemTable.create 10
let remove_item i = ItemTable.remove i
let get_item i = ItemTable.find_opt i

let rec add_item name ?(kind = Types.CompletionItemKind.Text) =
  match get_item items name with
  | Some i ->
    remove_item items name;
    add_item name ~kind
  | None -> ItemTable.add items name { kind }
;;

module StringSet = Set.Make (String)

class prepare_rename_traversal =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super
    val mutable completion_items = StringSet.empty
    method get_completion_items = completion_items
    method add_completion_item i = completion_items <- StringSet.add i completion_items

    (* method printer = log_to_file *)
    method printer = print_endline

    method! binding b =
      let open Links_core.Sugartypes in
      (match b.node with
       | Fun f ->
         add_item (Binder.to_name f.fun_binder) ~kind:Types.CompletionItemKind.Function
       | _ -> ());
      super#binding b

    method! pattern p =
      let open Links_core.Sugartypes in
      (match p.node with
       | Variable v -> add_item (Binder.to_name v) ~kind:Types.CompletionItemKind.Variable
       | _ -> ());
      super#pattern p

    method! name n =
      (match get_item items n with
       | Some _ -> ()
       | None -> add_item n ~kind:Types.CompletionItemKind.Text);
      super#name n

    method! binder n = super#binder n
    method! phrasenode p = super#phrasenode p
  end

let complation (r : Types.CompletionParams.t) =
  let ast_foldr = new prepare_rename_traversal in
  let context =
    Links_core.Context.
      { empty with
        name_environment = Links_core.Lib.nenv
      ; typing_environment = Links_core.Lib.typing_env
      }
  in
  let filename =
    Links_core.Utility.val_of (Links_core.Settings.get Linxer.prelude_file)
  in
  let result = Linxer.Phases.Parse.run context filename in
  let _ = ast_foldr#program result.program_ in
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
    let _ = ast_foldr#program a.program_ in
    let item_list =
      ItemTable.fold
        (fun s info acc -> Types.CompletionItem.create ~label:s ~kind:info.kind () :: acc)
        items
        []
    in
    let item_list =
      List.fold_right
        (fun n acc ->
          Types.CompletionItem.create
            ~label:(fst n)
            ~kind:Types.CompletionItemKind.Keyword
            ()
          :: acc)
        Links_core.Lexer.keywords
        item_list
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
    (string_of_bool (StringSet.is_empty ast1_walker#get_completion_items));
  [%expect {| true |}];
  let _ = ast1_walker#add_completion_item "Hello" in
  ast1_walker#printer
    (string_of_bool (StringSet.is_empty ast1_walker#get_completion_items));
  [%expect {| false |}];
  ()
;;

let%expect_test "Kind Info" =
  let file1 = testing_dir () ^ "/silly-progress.links" in
  let ast = Links_core.Loader.load (get_init_context ()) file1 in
  let ast1_walker = new prepare_rename_traversal in
  let _ = ast1_walker#program ast.program_ in
  [%expect {| |}];
  ()
;;
