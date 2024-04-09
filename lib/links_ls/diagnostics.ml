(* TEMP
   TODO: Move to separate file *)
(* END TEMP *)

open Common
open Links_lsp.Common

module ItemTable = Hashtbl.Make (struct
    type t = String.t

    let equal = String.equal
    let hash = String.hash
  end)

let diagnostics = ItemTable.create 10
let clear_diagnostics uri = ItemTable.remove diagnostics (Lsp.Uri.to_string uri)

let extract_string_and_number (s : string) : string * int =
  let prefix1 = "<string>:" in
  let prefix2 = "***: Parse error: <string>:" in
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
    let modified_str =
      String.sub s (String.length prefix2) (String.length s - String.length prefix2)
    in
    let num_start = 0 in
    let num_end = String.index_from modified_str num_start ' ' in
    let num_str = String.sub modified_str num_start (num_end - num_start) in
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
  let error_string = Links_core.Errors.format_exception error in
  let uri_string = Lsp.Uri.to_string uri in
  let d_list =
    match ItemTable.find_opt diagnostics uri_string with
    | Some d -> d
    | None -> []
  in
  log_to_file error_string;
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
  log_to_file "Hello testing 1";
  ItemTable.iter (fun k v -> log_to_file (Printf.sprintf "Key: %s" k)) diagnostics;
  log_to_file "Hello testing 2";
  match ItemTable.find_opt diagnostics (Lsp.Uri.to_string uri) with
  | Some d -> d
  | None -> []
;;

let diagnostic_notification uri err channel =
  log_to_file "Hello 1";
  let notif =
    Lsp.Types.PublishDiagnosticsParams.create ~diagnostics:(get_diagnotics uri) ~uri ()
  in
  log_to_file "Hello 2";
  write_message_notif
    channel
    (Lsp.Server_notification.to_jsonrpc
       (Lsp.Server_notification.PublishDiagnostics notif));
  ()
;;
