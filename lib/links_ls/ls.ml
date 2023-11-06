open Communication_channel
open Links_lsp.Common
open Jsonrpc2.Jsonrpc

let add_content_length_header str =
  let content_length = String.length str in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" content_length in
  header ^ str

let read_message channel = 
  let msg = Yojson.Safe.from_string (Channel.read_message !channel) in
  Packet.t_of_yojson msg

let write_message channel msg = 
  Channel.write_message !channel (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)

let server_not_initialzed ?(id=(`Int 0)) () = 
  let error = Response.Error.make ~code:Response.Error.Code.ServerNotInitialized ~message:"Server not initialized" () in
  Response.error id error

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

let rec main_loop channel = 
  let packet = read_message channel in

  (match packet with
  | Request _ -> log_to_file "Request"
  | Notification r -> handle_notification r
  | Response _ -> failwith "Response"
  | Batch_response _ -> failwith "Batch_response"
  | Batch_call _ -> failwith "Batch_call");

  main_loop channel

let run channel = 
  initialize channel;
  main_loop channel;
  (* let rec loop () = *) 
  (*   let msg = read channel in *)
  (*   match msg with *)
  (*   | None -> () *)
  (*   | Some msg -> *) 
  (*     let _ = process msg in *)
  (*     loop () *)
