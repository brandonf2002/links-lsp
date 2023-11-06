open Links_lsp.Common
open Jsonrpc2.Jsonrpc
open Common

let server_not_initialzed ?(id=(`Int 0)) () = 
  get_error_response ServerNotInitialized "Sernver not initialized" id

let do_initialize channel (r : Request.t) = 
  let open Lsp.Types in
  let open Jsonrpc2.Jsonrpc in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"links-lsp" ~version:"0.1" ()
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true
         ~change:TextDocumentSyncKind.Full
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let capabilities = ServerCapabilities.create ~textDocumentSync () in
  let init_result = InitializeResult.create ~capabilities ~serverInfo () in
  let msg = Response.ok r.id (InitializeResult.yojson_of_t init_result) in
  write_message channel msg

let rec initialize channel =
  let packet = read_message channel in
  match packet with
  | Request r -> (match r.method_ with
    | "initialize" -> do_initialize channel r
    | _ -> write_message channel (server_not_initialzed ()); initialize channel)
  | Notification n -> (match n.method_ with
    | "exit" -> exit 0
    | _ -> write_message channel (server_not_initialzed ()); initialize channel)
  | _ -> write_message channel (server_not_initialzed ()); initialize channel

let did_open (n : Jsonrpc2.Jsonrpc.Notification.t) =
  let params = Yojson.Safe.to_string (Notification.yojson_of_t n) in
  log_to_file params

let handle_notification (n : Notification.t) = 
  log_to_file n.method_;
  match n.method_ with
  | "exit" -> exit 0
  | "textDocument/didOpen" -> did_open n
  | "textDocument/didChange" -> log_to_file "didChange"
  | "textDocument/didClose" -> log_to_file "didClose"
  | _ -> prerr_endline "Not imlemented yet"

let handle_request (r : Request.t) = 
  log_to_file r.method_;
  match r.method_ with
  | _ -> prerr_endline "Not imlemented yet"

let rec main_loop channel = 
  let packet = read_message channel in

  (match packet with
  | Request r -> handle_request r
  | Notification r -> handle_notification r
  | Response _ -> failwith "Response"
  | Batch_response _ -> failwith "Batch_response"
  | Batch_call _ -> failwith "Batch_call");

  main_loop channel

let run channel = 
  initialize channel;
  main_loop channel;
