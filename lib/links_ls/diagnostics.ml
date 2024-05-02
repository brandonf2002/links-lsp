(* TEMP
   TODO: Move to separate file *)
open Lexing
open Links_lsp.Common
open Links_core.Utility
open Links_core.SourceCode

type position =
  { line : int
  ; col : int
  }

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

let pos_prefix ?pos line =
  let prefix =
    match pos with
    | Some pos -> Printf.sprintf "%s:%d" pos.pos_fname pos.pos_lnum
    | None -> "***"
  in
  Printf.sprintf "%s: %s " prefix line
;;

let prefix_lines prefix s =
  prefix ^ Str.global_replace (Str.regexp "\n") ("\n" ^ prefix) s
;;

type synerrspec =
  { filename : string
  ; linespec : string
  ; message : string
  ; linetext : string
  ; marker : string
  }

let extract_column_from_marker marker =
  let len = String.length marker in
  let rec aux i = if i = len || marker.[i] = '^' then i else aux (i + 1) in
  aux 0
;;

let format_exception = function
  | Links_core.Errors.RichSyntaxError s ->
    log_to_file (Printf.sprintf "RichSyntaxError: %s\n" s.filename);
    log_to_file (Printf.sprintf "RichSyntaxError: %s\n" s.linespec);
    log_to_file (Printf.sprintf "RichSyntaxError: %s\n" s.message);
    log_to_file (Printf.sprintf "RichSyntaxError: %s\n" s.linetext);
    log_to_file (Printf.sprintf "RichSyntaxError: %s\n" s.marker);
    let col = extract_column_from_marker s.marker in
    Printf.sprintf
      "%d,%d,%d,%d,Parse error: %s"
      (int_of_string s.linespec)
      col
      (int_of_string s.linespec)
      (col + 5)
      s.message
  | Links_core.Errors.Type_error (pos, s) ->
    let _, expr = Position.resolve_start_expr pos in
    Printf.sprintf
      "%d,%d,%d,%d,Type error: %s\nIn expression: %s"
      (Position.start pos).pos_lnum
      (Position.start pos).pos_cnum
      (Position.finish pos).pos_lnum
      (Position.finish pos).pos_cnum
      s
      expr
  | Links_core.Errors.ModuleError (s, pos) ->
    let message = Printf.sprintf "Module Error: %s" s in
    (match pos with
     | None -> Printf.sprintf "%d,%d,%d,%d,%s" 0 0 1 0 message
     | Some pos ->
       Printf.sprintf
         "%d,%d,%d,%d,%s"
         (Position.start pos).pos_lnum
         (Position.start pos).pos_cnum
         (Position.finish pos).pos_lnum
         (Position.finish pos).pos_cnum
         message)
  | e ->
    log_to_file ("Wrong error: " ^ Links_core.Errors.format_exception e);
    Links_core.Errors.format_exception e
;;

(* END TEMP *)

open Common

module ItemTable = Hashtbl.Make (struct
    type t = String.t

    let equal = String.equal
    let hash = String.hash
  end)

let diagnostics = ItemTable.create 10
let clear_diagnostics uri = ItemTable.remove diagnostics (Lsp.Uri.to_string uri)

let nearest_of_chars str start chars =
  let len = String.length str in
  let positions =
    List.filter_map
      (fun c ->
        try Some (String.index_from str start c) with
        | Not_found -> None)
      chars
  in
  let positions = List.filter (fun pos -> pos >= start) positions in
  match positions with
  | [] -> len (* If no characters are found, return the end of the string *)
  | _ -> List.fold_left min len positions
;;

let extract_string_and_number (s : string) : string * int =
  let prefix1 = "<string>:" in
  let prefix2 = "***: Parse error: <string>:" in
  log_to_file (Printf.sprintf "Extracting string and number from: %s" s);
  if String.starts_with ~prefix:prefix1 s
  then (
    let num_start = String.length prefix1 in
    let num_end = String.index_from s num_start ':' in
    let str_start = num_end + 2 in
    let num_str = String.sub s num_start (num_end - num_start) in
    let num = int_of_string num_str in
    let modified_str = String.sub s str_start (String.length s - str_start) in
    modified_str, num)
  else if String.starts_with ~prefix:prefix2 s
  then (
    log_to_file "Extracting string and number from prefix2";
    let modified_str =
      String.sub s (String.length prefix2) (String.length s - String.length prefix2)
    in
    let num_start = 0 in
    let num_end = nearest_of_chars modified_str num_start [ ' '; '\n' ] in
    let num_str = String.sub modified_str num_start (num_end - num_start) in
    log_to_file (Printf.sprintf "num_str: %s" num_str);
    let num = int_of_string (String.trim num_str) in
    (* remove the number from the string *)
    let modified_str =
      String.sub modified_str (num_end + 1) (String.length modified_str - num_end - 1)
    in
    let modified_str = "Parse error: " ^ String.trim modified_str in
    modified_str, num)
  else s, 0
;;

let parse_format (input : string) : int * int * int * int * string =
  match String.split_on_char ',' input with
  | [ line1; col1; line2; col2; message ] ->
    let line1 = int_of_string line1 in
    let col1 = int_of_string col1 in
    let line2 = int_of_string line2 in
    let col2 = int_of_string col2 in
    let message = String.sub message 0 (String.length message) in
    line1, col1, line2, col2, message
  | _ -> failwith "Invalid input format"
;;

let add_diagnostic uri error =
  let error_string = format_exception error in
  let uri_string = Lsp.Uri.to_string uri in
  let d_list =
    match ItemTable.find_opt diagnostics uri_string with
    | Some d -> d
    | None -> []
  in
  let line1, col1, line2, col2, message = parse_format error_string in
  let range =
    Lsp.Types.Range.create
      ~start:(Lsp.Types.Position.create ~line:(line1 - 1) ~character:(col1 - 1))
      ~end_:(Lsp.Types.Position.create ~line:(line2 - 1) ~character:(col2 - 1))
  in
  let new_d = Lsp.Types.Diagnostic.create ~message ~range () in
  ItemTable.replace diagnostics uri_string (new_d :: d_list)
;;

let get_diagnotics uri =
  match ItemTable.find_opt diagnostics (Lsp.Uri.to_string uri) with
  | Some d -> d
  | None -> []
;;

let diagnostic_notification uri _err channel =
  let notif =
    Lsp.Types.PublishDiagnosticsParams.create ~diagnostics:(get_diagnotics uri) ~uri ()
  in
  write_message_notif
    channel
    (Lsp.Server_notification.to_jsonrpc
       (Lsp.Server_notification.PublishDiagnostics notif));
  ()
;;
