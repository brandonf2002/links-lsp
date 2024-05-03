open Links_lsp.Common
open Jsonrpc2.Jsonrpc
open Common
open Text_document

(*  *)
open Rename
open Completion
open Diagnostics

let server_not_initialzed ?(id = `Int 0) () =
  get_error_response ServerNotInitialized "Sernver not initialized" id
;;

let do_initialize channel (r : Request.t) =
  let open Lsp.Types in
  let open Jsonrpc2.Jsonrpc in
  let open Lsp.Import in
  let serverInfo =
    InitializeResult.create_serverInfo ~name:"links-lsp" ~version:"0.1" ()
  in
  let textDocumentSync =
    `TextDocumentSyncOptions
      (TextDocumentSyncOptions.create
         ~openClose:true (* ~change:TextDocumentSyncKind.Incremental *)
         ~change:TextDocumentSyncKind.Full
         ~willSave:false
         ~save:(`SaveOptions (SaveOptions.create ~includeText:false ()))
         ~willSaveWaitUntil:false
         ())
  in
  let completionProvider =
    CompletionOptions.create ~triggerCharacters:[ "." ] ~resolveProvider:false ()
  in
  let renameProvider = `RenameOptions (RenameOptions.create ~prepareProvider:true ()) in
  (* let semanticTokensProvider = *)
  (*   let full = `Full (SemanticTokensOptions.create_full ~delta:false ()) in *)
  (*   `SemanticTokensOptions *)
  (*     (SemanticTokensOptions.create ~legend:Highlighting.legend ~full ()) *)
  (* in *)
  let capabilities =
    ServerCapabilities.create
      ~textDocumentSync
      ~renameProvider
      ~completionProvider
      (* ~semanticTokensProvider *)
      ()
  in
  let init_result = InitializeResult.create ~capabilities ~serverInfo () in
  let msg = Response.ok r.id (InitializeResult.yojson_of_t init_result) in
  write_message channel msg
;;

let rec initialize channel =
  let packet = read_message channel in
  (match packet with
   | Request r ->
     (match r.method_ with
      | "initialize" -> do_initialize channel r
      | _ ->
        write_message channel (server_not_initialzed ());
        initialize channel)
   | Notification n ->
     (match n.method_ with
      | "exit" -> exit 0
      | _ ->
        write_message channel (server_not_initialzed ());
        initialize channel)
   | _ ->
     write_message channel (server_not_initialzed ());
     initialize channel);
  let packet = read_message channel in
  match packet with
  | Notification n ->
    (match n.method_ with
     | "exit" -> exit 0
     | "initialized" -> ()
     | _ -> exit 1)
  | _ -> exit 1
;;

(* Just keeping this here as we might want to do more complex things with the shutdown process later to kill/save things *)
let shutdown () = `Null

let handle_notification channel (n : Notification.t) =
  let open Lsp.Client_notification in
  let params = of_jsonrpc n in
  match params with
  | Ok p ->
    (match n.method_ with
     | "exit" -> exit 0
     | "textDocument/didOpen" ->
       let uri = did_open p in
       diagnostic_notification uri "hello" channel
     | "textDocument/didChange" ->
       let uri = did_change p in
       diagnostic_notification uri "hello" channel
     | "textDocument/didClose" -> did_close p
     | _ ->
       prerr_endline "Not imlemented yet (Notif)"
       (* log_to_file ("Not implimented: " ^ n.method_)) *))
  | Error e ->
    (* "Error: " ^ e |> log_to_file; *)
    ()
;;

let default_fail_response ?(error = "Invalid Request") () =
  Response.Error.make ~code:InvalidRequest ~message:error ()
;;

let handle_request channel (r : Request.t) =
  let open Lsp.Client_request in
  let result =
    match of_jsonrpc r with
    | Error e -> Result.error (default_fail_response ~error:e ())
    | Ok p ->
      (match p with
       (* TODO -> Change these functions to return the Result type of these *)
       (*         This will make it look cleaner and means we can return error from rename *)
       | E Shutdown -> Result.ok (shutdown ())
       | E (TextDocumentPrepareRename p) -> Result.ok (prepare_rename p)
       | E (TextDocumentRename p) -> Result.ok (rename p)
       | E (TextDocumentCompletion p) -> Result.ok (complation p)
       (* | E (TextDocumentHover p) -> Result.ok (Hover.hover p) *)
       | _ -> Result.error (default_fail_response ~error:"Hello world!" ()))
  in
  write_message channel { id = r.id; result }
;;

let rec main_loop channel =
  let packet = read_message channel in
  (match packet with
   | Request r -> handle_request channel r
   | Notification r -> handle_notification channel r
   | Response _ -> failwith "Response"
   | Batch_response _ -> failwith "Batch_response"
   | Batch_call _ -> failwith "Batch_call");
  main_loop channel
;;

let precompute_data () = Completion.init_item_table ()

let run channel =
  initialize channel;
  precompute_data ();
  main_loop channel
;;
