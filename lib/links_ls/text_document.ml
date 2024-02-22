open Links_lsp.Common
open Lsp
open Global
open Document_state

let did_open (p : Client_notification.t) =
  match p with
  | TextDocumentDidOpen p ->
    let parsed_ast =
      try Some (Linxer.Phases.Parse.string (get_init_context ()) p.textDocument.text) with
      | _ -> None
    in
    let desugared_ast =
      try
        match parsed_ast with
        | Some ast -> Some (Linxer.Phases.Desugar.run ast)
        | _ -> None
      with
      | _ -> None
    in
    add_document
      { uri = p.textDocument.uri
      ; version = p.textDocument.version
      ; language_id = p.textDocument.languageId
      ; content = p.textDocument.text
      ; parsed_ast
      ; desugared_ast
      }
  | _ -> failwith "Unreachable"
;;

(* log_to_file (format_documents () ^ "\n\n"); *)
(* log_to_file (parse_doc_string () ^ "\n\n") *)

let get_text (change_event : Lsp.Types.TextDocumentContentChangeEvent.t) =
  change_event.text
;;

let did_change (p : Client_notification.t) =
  let rec do_all f lst =
    match lst with
    | [] -> ()
    | x :: xs ->
      f x;
      do_all f xs
  in
  match p with
  | TextDocumentDidChange p ->
    let changes = p.contentChanges in
    let uri = p.textDocument.uri in
    let version = p.textDocument.version in
    do_all (fun x -> update_document uri (get_text x) version) changes
  | _ -> failwith "Unreachable"
;;

(* log_to_file (parse_doc_string () ^ "\n\n") *)
(* log_to_file (format_documents () ^ "\n\n"); *)

let did_close (p : Client_notification.t) =
  (match p with
   | TextDocumentDidClose p -> remove_document p.textDocument.uri
   | _ -> failwith "Unreachable");
  log_to_file (format_documents () ^ "\n\n")
;;

(* let prep_rename channel (r : 'a Client_request.t) id = *)
(*   let params = (match r with *)
(*   | TextDocumentPrepareRename r -> r) in *)

(* (1* let uri = params.textDocument.uri in *1) *)
(* (1* let doc = get_document uri in *1) *)

(* write_message channel (Response.error id Response.Error.Code.InternalError) *)
