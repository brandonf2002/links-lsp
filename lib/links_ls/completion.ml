open Lsp
open Document_state
open Links_core.SourceCode
open Links_lsp.Common

(* open Links_lsp.Common *)
(* open Global *)

type position =
  { line : int
  ; col : int
  }

let string_of_position p = "(" ^ string_of_int p.line ^ ", " ^ string_of_int p.col ^ ")"

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

let item_table = { table = ItemTable.create 1000; temporary_items = [] }

let rec add_item ?(kind = Types.CompletionItemKind.Text) ?(detail = "") ~is_temp_item name
  =
  match ItemTable.find_opt item_table.table name with
  | Some _ ->
    ItemTable.remove item_table.table name;
    add_item name ~is_temp_item ~kind ~detail
  | None ->
    ItemTable.add item_table.table name { kind; detail };
    if is_temp_item then item_table.temporary_items <- name :: item_table.temporary_items
;;

let remove_temp_items () =
  List.iter (fun key -> ItemTable.remove item_table.table key) item_table.temporary_items;
  item_table.temporary_items <- []
;;

let untyped_tbl = Hashtbl.create 1000

class completion_traversal ~is_temp_item ~has_types =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super

    (* method printer = log_to_file *)
    method printer = print_endline

    method add_item ?kind ?detail name =
      match has_types with
      | true ->
        if Hashtbl.mem untyped_tbl name then add_item ?kind ?detail name ~is_temp_item
      | false -> Hashtbl.add untyped_tbl name ()

    method! binding b =
      let open Links_core.Sugartypes in
      (match b.node with
       | Fun f ->
         self#add_item
           (Binder.to_name f.fun_binder)
           ~kind:Types.CompletionItemKind.Function
           ~detail:(Links_core.Types.string_of_datatype (Binder.to_type f.fun_binder))
       | Funs f ->
         (match f with
          | x :: _ ->
            self#add_item
              (Binder.to_name x.node.rec_binder)
              ~kind:Types.CompletionItemKind.Function
              ~detail:
                (Links_core.Types.string_of_datatype (Binder.to_type x.node.rec_binder))
          | _ -> ())
       | _ -> ());
      super#binding b

    method! pattern p =
      let open Links_core.Sugartypes in
      (match p.node with
       | Variable v ->
         self#add_item
           (Binder.to_name v)
           ~kind:Types.CompletionItemKind.Variable
           ~detail:(Links_core.Types.string_of_datatype (Binder.to_type v))
       | _ -> ());
      super#pattern p

    method! program p = super#program p
    method! name n = super#name n
    method! binder n = super#binder n
    method! phrasenode p = super#phrasenode p
  end

class completion_traversal_content ~is_temp_item ~has_types ~content ~position =
  object (self : 'self_type)
    inherit Links_core.SugarTraversals.fold as super

    (* method printer = log_to_file *)
    method printer = print_endline

    method add_item ?kind ?detail name =
      (* if not (item_position == After) *)
      (* then ( *)
      match has_types with
      | true ->
        if Hashtbl.mem untyped_tbl name then add_item ?kind ?detail name ~is_temp_item
      | false -> Hashtbl.add untyped_tbl name ()
    (* else log_to_file "WOWO" *)

    method! binding b =
      let open Links_core.Sugartypes in
      (match b.node with
       | Fun f ->
         if not has_types
         then (
           let start, finish = get_real_position f.fun_binder.pos content in
           if not (is_within_range start finish position == After)
           then
             self#add_item
               ~kind:Types.CompletionItemKind.Function
               ~detail:(Links_core.Types.string_of_datatype (Binder.to_type f.fun_binder))
               (Binder.to_name f.fun_binder))
         else
           self#add_item
             ~kind:Types.CompletionItemKind.Function
             ~detail:(Links_core.Types.string_of_datatype (Binder.to_type f.fun_binder))
             (Binder.to_name f.fun_binder)
       | Funs f ->
         (match f with
          | x :: _ ->
            if not has_types
            then (
              let start, finish = get_real_position x.node.rec_binder.pos content in
              if not (is_within_range start finish position == After)
              then
                self#add_item
                  ~kind:Types.CompletionItemKind.Function
                  ~detail:
                    (Links_core.Types.string_of_datatype
                       (Binder.to_type x.node.rec_binder))
                  (Binder.to_name x.node.rec_binder))
            else
              self#add_item
                ~kind:Types.CompletionItemKind.Function
                ~detail:
                  (Links_core.Types.string_of_datatype (Binder.to_type x.node.rec_binder))
                (Binder.to_name x.node.rec_binder)
          | _ -> ())
       | _ -> ());
      super#binding b

    method! pattern p =
      let open Links_core.Sugartypes in
      (match p.node with
       | Variable v ->
         if not has_types
         then (
           let start, finish = get_real_position p.pos content in
           if not (is_within_range start finish position == After)
           then
             self#add_item
               ~kind:Types.CompletionItemKind.Variable
               ~detail:(Links_core.Types.string_of_datatype (Binder.to_type v))
               (Binder.to_name v))
         else
           self#add_item
             ~kind:Types.CompletionItemKind.Variable
             ~detail:(Links_core.Types.string_of_datatype (Binder.to_type v))
             (Binder.to_name v)
       | _ -> ());
      super#pattern p

    method! program p = super#program p
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
             add_item s ~is_temp_item:false ~kind:Types.CompletionItemKind.Variable
           | Links_core.Types.Function _ ->
             add_item
               s
               ~is_temp_item:false
               ~kind:Types.CompletionItemKind.Function
               ~detail:(Links_core.Types.string_of_datatype t)
           | _ -> ())
          (* add_item s ~kind:Types.CompletionItemKind.Variable *)
        | Links_core.Types.Primitive _ ->
          add_item
            s
            ~is_temp_item:false
            ~kind:Types.CompletionItemKind.Constant
            ~detail:(Links_core.Types.string_of_datatype t)
        | _ -> ())
      types
  in
  ()
;;

let init_item_table () =
  let ast_foldr = new completion_traversal ~is_temp_item:false ~has_types:false in
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
  let ast_foldr = new completion_traversal ~is_temp_item:false ~has_types:true in
  let result = Linxer.Phases.Desugar.run result in
  let _ = ast_foldr#program result.program in
  get_lib_functions ();
  Hashtbl.clear untyped_tbl;
  List.iter
    (fun n -> add_item (fst n) ~is_temp_item:false ~kind:Types.CompletionItemKind.Keyword)
    Links_core.Lexer.keywords
;;

let complation (r : Types.CompletionParams.t) =
  let doc = get_document r.textDocument.uri in
  let position = { line = r.position.line; col = r.position.character } in
  let parsed_ast, desugared_ast =
    match doc with
    | None -> None, None
    | Some v -> v.parsed_ast, v.desugared_ast
  in
  let content =
    match doc with
    | None -> ""
    | Some v -> v.content
  in
  match parsed_ast, desugared_ast with
  | None, _ ->
    let item_list =
      ItemTable.fold
        (fun s info acc ->
          Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail ()
          :: acc)
        item_table.table
        []
    in
    let ret = Types.CompletionList.create ~isIncomplete:false ~items:item_list () in
    Types.CompletionList.yojson_of_t ret
  | _, None ->
    let item_list =
      ItemTable.fold
        (fun s info acc ->
          Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail ()
          :: acc)
        item_table.table
        []
    in
    let ret = Types.CompletionList.create ~isIncomplete:false ~items:item_list () in
    Types.CompletionList.yojson_of_t ret
  | Some a, Some b ->
    let ast_foldr =
      new completion_traversal_content
        ~is_temp_item:true
        ~has_types:false
        ~position
        ~content
    in
    let _ = ast_foldr#program a.program_ in
    let ast_foldr =
      new completion_traversal_content
        ~is_temp_item:true
        ~has_types:true
        ~position
        ~content
    in
    let _ = ast_foldr#program b.program in
    let item_list =
      ItemTable.fold
        (fun s info acc ->
          Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail ()
          :: acc)
        item_table.table
        []
    in
    remove_temp_items ();
    Hashtbl.clear untyped_tbl;
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

(* let%expect_test "Kind Info" = *)
(*   init_item_table (); *)
(*   (1* let file1 = testing_dir () ^ "/silly-progress.links" in *1) *)
(*   let file1 = *)
(*     "/home/brandon/doc/uni/5th_year/diss/links/examples/sessions/linear_if.links" *)
(*   in *)
(*   let a = Links_core.Loader.load (get_init_context ()) file1 in *)
(*   let ast_foldr = new completion_traversal ~is_temp_item:true ~has_types:false in *)
(*   let _ = ast_foldr#program a.program_ in *)
(*   print_endline (Links_core.Sugartypes.show_program a.program_); *)
(*   let a = Linxer.Phases.Desugar.run a in *)
(*   let ast_foldr = new completion_traversal ~is_temp_item:true ~has_types:true in *)
(*   let _ = ast_foldr#program a.program in *)
(*   let _ = *)
(*     ItemTable.fold *)
(*       (fun s info acc -> *)
(*         print_endline (s ^ " : " ^ info.detail); *)
(*         Types.CompletionItem.create ~label:s ~kind:info.kind ~detail:info.detail () :: acc) *)
(*       item_table.table *)
(*       [] *)
(*   in *)
(*   [%expect {| |}]; *)
(*   () *)
(* ;; *)
