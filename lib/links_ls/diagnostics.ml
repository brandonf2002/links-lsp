open Common
open Links_lsp.Common

module ItemTable = Hashtbl.Make (struct
    type t = String.t

    let equal = String.equal
    let hash = String.hash
  end)

let diagnostics = ItemTable.create 10
let clear_diagnostics uri = ItemTable.remove diagnostics (Lsp.Uri.to_string uri)

let add_diagnostic uri error =
  let error_string = Links_core.Errors.format_exception error in
  let uri_string = Lsp.Uri.to_string uri in
  let d_list =
    match ItemTable.find_opt diagnostics uri_string with
    | Some d -> d
    | None -> []
  in
  let range =
    Lsp.Types.Range.create
      ~start:(Lsp.Types.Position.create ~line:0 ~character:0)
      ~end_:(Lsp.Types.Position.create ~line:0 ~character:3)
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
