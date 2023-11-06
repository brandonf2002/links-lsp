open Links_lsp.Common
open Lsp

open Document_state

let did_open (p : Client_notification.t) =
  (match p with 
  | TextDocumentDidOpen p -> (
    add_document {
      uri = p.textDocument.uri; 
      version = p.textDocument.version; 
      language_id = p.textDocument.languageId; 
      content = p.textDocument.text
    }
  )
  | _ -> failwith "Unreachable");
  log_to_file (format_documents ())

let get_text (change_event : Lsp.Types.TextDocumentContentChangeEvent.t) = change_event.text

let did_change (p : Client_notification.t) =
  let rec do_all f lst =
    match lst with
    | [] -> ()
    | x :: xs -> f x; do_all f xs in
  (match p with 
  | TextDocumentDidChange p -> (
    let changes = p.contentChanges in
    let uri = p.textDocument.uri in
    let version = p.textDocument.version in
    do_all (fun x -> update_document uri (get_text x) version) changes;
  )
  | _ -> failwith "Unreachable");
  log_to_file (format_documents ())

let did_close (p : Client_notification.t) =
  (match p with 
  | TextDocumentDidClose p -> (
    remove_document p.textDocument.uri
  )
  | _ -> failwith "Unreachable");
  log_to_file (format_documents ())
