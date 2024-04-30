(* TEMP
   TODO: Move to separate file *)
open Lexing
open Links_lsp.Common
open Links_core.Utility
open Links_core.SourceCode

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

let format_exception = function
  | Links_core.Errors.RichSyntaxError s ->
    pos_prefix
      ("Parse error: "
       ^ s.filename
       ^ ":"
       ^ s.linespec
       ^ "\n"
       ^ s.message
       ^ "\n"
       ^ prefix_lines "  " s.linetext
       ^ "\n"
       ^ "   "
       ^ s.marker)
  | Links_core.Errors.Type_error (pos, s) ->
    let pos, expr = Position.resolve_start_expr pos in
    pos_prefix ~pos (Printf.sprintf "Type error: %s\nIn expression: %s.\n" s expr)
  (* | Links_core.Errors.ModuleError (s, pos) -> *)
  (*   pos_prefix ~pos (Printf.sprintf "Module error: %s\n" s) *)
  | e ->
    log_to_file ("Wrong error: " ^ Links_core.Errors.format_exception e);
    failwith ""
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

let add_diagnostic uri error =
  (* log_to_file "TESTING ADD DIAGNOSTIC"; *)
  (* log_to_file (format_exception error); *)
  (* log_to_file "Hello"; *)
  (* log_to_file (Links_core.Errors.format_exception error); *)
  (* log_to_file "TESTING ADD DIAGNOSTIC"; *)
  let error_string = Links_core.Errors.format_exception error in
  let uri_string = Lsp.Uri.to_string uri in
  let d_list =
    match ItemTable.find_opt diagnostics uri_string with
    | Some d -> d
    | None -> []
  in
  let error_string, line_number = extract_string_and_number error_string in
  let range =
    Lsp.Types.Range.create
      ~start:(Lsp.Types.Position.create ~line:(line_number - 1) ~character:0)
      ~end_:(Lsp.Types.Position.create ~line:(line_number - 1) ~character:3)
  in
  let new_d = Lsp.Types.Diagnostic.create ~message:error_string ~range () in
  ItemTable.replace diagnostics uri_string (new_d :: d_list)
;;

let get_diagnotics uri =
  (* ItemTable.iter (fun k v -> log_to_file (Printf.sprintf "Key: %s" k)) diagnostics; *)
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
