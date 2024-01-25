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
  let renameProvider =
    `RenameOptions (RenameOptions.create ~prepareProvider:true ())
  in
  let capabilities = ServerCapabilities.create ~textDocumentSync ~renameProvider () in
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

(* Just keeping this here as we might want to do more complex things with the shutdown process later to kill/save things *)
let shutdown id = 
  Response.ok id (`Null)

let handle_notification (n : Notification.t) = 
  let open Lsp.Client_notification in
  let params = of_jsonrpc n in
  match params with
  | Ok p -> (match n.method_ with
    | "exit" -> exit 0
    | "textDocument/didOpen" -> did_open p
    | "textDocument/didChange" -> did_change p
    | "textDocument/didClose" -> did_close p
    | _ -> prerr_endline "Not imlemented yet (Notif)"; log_to_file ("Not implimented: " ^ n.method_))
  | Error e -> "Error: " ^ e |> log_to_file;
  ()

let default_fail_response ?(id=(`Int 0)) ?(error="Invalid Request") () = 
  get_error_response InvalidRequest error id

let handle_request channel (r : Request.t) = 
  let open Lsp.Client_request in
  (match of_jsonrpc r with
  | Error e -> default_fail_response ~id:r.id ~error:e ()
  | Ok p -> match p with
    | E (Shutdown) -> shutdown r.id
    | E (TextDocumentPrepareRename _params) -> default_fail_response ~id:r.id () ~error:"Coming from the match!!!!"
    | _ -> default_fail_response ~id:r.id () ~error:"Hello world!") 

  |> write_message channel
  (* match params with *)
  (* | Ok _ -> write_message channel (match r.method_ with *)
  (*   (1* | "textDocument/prepareRename" -> prep_rename channel p r.id (2* "Not imlemented yet WOOOOOO!" |> log_to_file *2) *1) *)
  (*   (1* | _ -> "Not imlemented yet (Request) " ^ r.method_ |> log_to_file) *1) *)
  (*   | "shutdown" -> Response.ok r.id (`Bool true) *)
  (*   | _ -> default_fail_response ~id:r.id ()) *)
  (* | Error e -> "Error: " ^ e |> log_to_file; *)
  (* () *)

let rec main_loop channel = 
  let packet = read_message channel in

  (match packet with
  | Request r -> handle_request channel r
  | Notification r -> handle_notification r
  | Response _ -> failwith "Response"
  | Batch_response _ -> failwith "Batch_response"
  | Batch_call _ -> failwith "Batch_call");

  main_loop channel

let run channel = 
  initialize channel;
  main_loop channel;
