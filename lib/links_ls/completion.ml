open Lsp
open Document_state
open Links_lsp.Common
open Global

type item_info =
  { kind : Types.CompletionItemKind.t
  ; detail : string
  }

module ItemTable = Hashtbl.Make (struct
    type t = String.t

    let equal = String.equal
    let hash = String.hash
  end)

type item_table_with_temp_list =
  { table : item_info ItemTable.t
  ; mutable temporary_items : string list
  }

let item_table = { table = ItemTable.create 100; temporary_items = [] }

let rec add_item ?(kind = Types.CompletionItemKind.Text) ?(detail = "") name is_temp_item =
  match ItemTable.find_opt item_table.table name with
  | Some _ ->
    ItemTable.remove item_table.table name;
    add_item name is_temp_item ~kind ~detail
  | None ->
    ItemTable.add item_table.table name { kind; detail };
    if is_temp_item then item_table.temporary_items <- name :: item_table.temporary_items
;;

let remove_temp_items () =
  List.iter (fun key -> ItemTable.remove item_table.table key) item_table.temporary_items;
  item_table.temporary_items <- []
;;

module StringSet = Set.Make (String)

class completion_traversal is_temp_item =
  object (_ : 'self_type)
    inherit Links_core.SugarTraversals.fold as super

    (* method printer = log_to_file *)
    method printer = print_endline

    method! binding b =
      let open Links_core.Sugartypes in
      (match b.node with
       | Fun f ->
         add_item
           (Binder.to_name f.fun_binder)
           is_temp_item
           ~kind:Types.CompletionItemKind.Function
           ~detail:(Links_core.Types.string_of_datatype (Binder.to_type f.fun_binder))
       | _ -> ());
      super#binding b

    method! pattern p =
      let open Links_core.Sugartypes in
      (match p.node with
       | Variable v ->
         add_item
           (Binder.to_name v)
           is_temp_item
           ~kind:Types.CompletionItemKind.Variable
           ~detail:(Links_core.Types.string_of_datatype (Binder.to_type v))
       | _ -> ());
      super#pattern p

    method! name n = super#name n
    method! binder n = super#binder n
    method! phrasenode p = super#phrasenode p
  end

let get_lib_functions () =
  let types =
    Links_core.Env.String.fold
      (fun str t acc -> (str, t) :: acc)
      Links_core.Lib.type_env
      []
  in
  let _ =
    List.iter
      (fun (s, t) ->
        match t with
        | Links_core.Types.ForAll (_, tt) ->
          (match tt with
           | Links_core.Types.Var _ ->
             add_item s false ~kind:Types.CompletionItemKind.Variable
           | Links_core.Types.Function _ ->
             add_item
               s
               false
               ~kind:Types.CompletionItemKind.Function
               ~detail:(Links_core.Types.string_of_datatype t)
           | _ -> ())
          (* add_item s ~kind:Types.CompletionItemKind.Variable *)
        | Links_core.Types.Primitive _ ->
          add_item
            s
            false
            ~kind:Types.CompletionItemKind.Constant
            ~detail:(Links_core.Types.string_of_datatype t)
        | _ -> ())
      types
  in
  ()
;;

let init_item_table () =
  let ast_foldr = new completion_traversal false in
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
  let result = Linxer.Phases.Desugar.run result in
  let _ = ast_foldr#program result.program in
  get_lib_functions ();
  List.iter
    (fun n -> add_item (fst n) false ~kind:Types.CompletionItemKind.Keyword)
    Links_core.Lexer.keywords
;;

let complation (r : Types.CompletionParams.t) =
  let ast_foldr = new completion_traversal true in
  let doc = get_document r.textDocument.uri in
  let ast =
    match doc with
    | None -> None
    | Some v -> v.ast
  in
  match ast with
  | None -> `Null
  | Some a ->
    (* let desugared_ast = Linxer.Phases.Desugar.run a in *)
    let _ = ast_foldr#program a.program_ in
    let item_list =
      ItemTable.fold
        (fun s info acc ->
          Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail ()
          :: acc)
        item_table.table
        []
    in
    remove_temp_items ();
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

let%expect_test "Kind Info" =
  (* let file1 = testing_dir () ^ "/date.links" in *)
  let file1 =
    "/home/brandon/doc/uni/5th_year/diss/links/examples/sessions/linear_if.links"
  in
  let ast = Links_core.Loader.load (get_init_context ()) file1 in
  (* let ast = Linxer.Phases.Desugar.run ast in *)
  (* let _ = print_endline (Links_core.Sugartypes.show_program ast.program) in *)
  (* Print all of the types and their str using Types.string_of_datatype *)
  (* get_lib_functions (); *)
  (* let _ = *)
  (*   List.iter *)
  (*     (fun (s, t) -> print_endline (s ^ ":---- " ^ Types.string_of_datatype t)) *)
  (*     types *)
  (* in *)
  (* let ast1_walker = new prepare_rename_traversal in *)
  (* let _ = ast1_walker#program ast.program in *)
  print_endline (Links_core.Sugartypes.show_program ast.program_);
  (* let item_list = *)
  (*   ItemTable.fold *)
  (*     (fun s info acc -> *)
  (*       Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail () :: acc) *)
  (*     items *)
  (*     [] *)
  (* in *)
  [%expect {| |}];
  ()
;;
