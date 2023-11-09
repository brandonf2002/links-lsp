open Links_lsp.Common
open Jsonrpc2.Jsonrpc
open Common
open Text_document

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
         (* ~change:TextDocumentSyncKind.Incremental *)
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
  (match packet with
  | Request r -> (match r.method_ with
    | "initialize" -> do_initialize channel r
    | _ -> write_message channel (server_not_initialzed ()); initialize channel)
  | Notification n -> (match n.method_ with
    | "exit" -> exit 0
    | _ -> write_message channel (server_not_initialzed ()); initialize channel)
  | _ -> write_message channel (server_not_initialzed ()); initialize channel);
  let packet = read_message channel in

  match packet with
  | Notification n -> (match n.method_ with
    | "exit" -> exit 0
    | "initialized" -> ()
    | _ -> exit 1)
  | _ -> exit 1


let handle_notification (n : Notification.t) = 
  let open Lsp.Client_notification in
  let params = of_jsonrpc n in
  log_to_file n.method_;
  match params with
  | Ok p -> (match n.method_ with
    | "exit" -> exit 0
    | "textDocument/didOpen" -> did_open p
    | "textDocument/didChange" -> did_change p
    | "textDocument/didClose" -> did_close p
    | _ -> prerr_endline "Not imlemented yet (Notif)"; log_to_file ("Not implimented: " ^ n.method_))
  | Error e -> "Error: " ^ e |> log_to_file;
  ()

let handle_request (r : Request.t) = 
  match r.method_ with
  | _ -> prerr_endline "Not imlemented yet (Request)"

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
