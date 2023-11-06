open Links_lsp.Common
open Communication_channel

open Jsonrpc
open Jsonrpc2.Jsonrpc

open Links_ls.Ls

(* The Argument Parsing *) 

let method_ref = ref Channel.Stdio
let option_counter = ref 0 (* Number of communication method options specified (Fail if more than 1) *)
let client_pid_ref = ref None  (* Optional client process ID *)

let check_multiple_options () =
  incr option_counter;
  if !option_counter > 1 then
    begin
      Printf.eprintf "Error: Multiple communication methods specified.\n";
      exit 1
    end

let specs =
  [
    "--stdio", Arg.Unit (fun () -> check_multiple_options (); method_ref := Channel.Stdio), " Use stdio";
    "--pipe", Arg.String (fun s -> check_multiple_options (); method_ref := (Channel.Pipe s)), " Use named pipes";
    "--socket", Arg.Int (fun i -> check_multiple_options (); method_ref := (Channel.Socket i)), " Use TCP/IP socket";
    "--clientProcessId", Arg.String (fun s -> client_pid_ref := Some s), " Store the client's process ID"
  ]

(* Main *)

let default_json_value () =
  let json_str = {|{
    "capabilities": {},
    "serverInfo": {
        "name": "links-lsp",
        "version": "0.1"
    }
  }|}
  in
  Yojson.Safe.from_string json_str

  let default_response ?(jsonrpc: Types.jsonrpc="2.0") ?result ?error ~id () =
  let open Jsonrpc.Types in
  {
    jsonrpc;
    result;
    error;
    id;
  }

let add_content_length_header str =
  let content_length = String.length str in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" content_length in
  header ^ str

let handle_request r = 
  let error = Response.Error.make ~code:(Response.Error.Code.RequestFailed) ~message:"Request" () in
  let msg = Response.error (`Int 1) error in
  Channel.write_message !method_ref (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)

let did_open (n : Jsonrpc2.Jsonrpc.Notification.t) =
  let open Jsonrpc2.Jsonrpc in
  let params = Yojson.Safe.to_string (Notification.yojson_of_t n) in
  log_to_file params


let handle_notification (n : Jsonrpc2.Jsonrpc.Notification.t) = 
  (* let open Lsp.Types in *)
  let open Jsonrpc2.Jsonrpc in
  log_to_file n.method_;
  (match n.method_ with
  | "exit" -> exit 0
  | "textDocument/didOpen" -> did_open n
  | "textDocument/didChange" -> log_to_file "didChange"
  | "textDocument/didClose" -> log_to_file "didClose"
  | _ -> prerr_endline "Not imlemented yet");
  let error = Response.Error.make ~code:(Response.Error.Code.RequestFailed) ~message:"Notification" () in
  let msg = Response.error (`Int 1) error in
  Channel.write_message !method_ref (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)

let handle_response r = 
  let error = Response.Error.make ~code:(Response.Error.Code.RequestFailed) ~message:"Response" () in
  let msg = Response.error (`Int 1) error in
  Channel.write_message !method_ref (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)

let do_initialize method_ref r = 
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
  let msg = Response.ok r (InitializeResult.yojson_of_t init_result) in

  (* append_to_file (Response.yojson_of_t msg |> Yojson.Safe.to_string) "/home/brandon/LSP_test"; *)

  Channel.write_message !method_ref (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header);

  let msg = Yojson.Safe.from_string (Channel.read_message !method_ref) in
  let packet = Packet.t_of_yojson msg in
  match packet with
  | Notification n -> (match n.method_ with
    | "exit" -> exit 0
    | "initialized" -> ()
    | _ -> prerr_endline "Not initialized (Notification)"
  )
  | _ -> append_to_file "Not Notification\n" "/home/brandon/LSP_test"

let send_server_not_initialzed method_ref id = 
  let open Jsonrpc2.Jsonrpc in
  let error = Response.Error.make ~code:Response.Error.Code.ServerNotInitialized ~message:"Server not initialized" () in
  let msg = Response.error id error in
  Channel.write_message !method_ref (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)

let initialize method_ref =
  let msg = Yojson.Safe.from_string (Channel.read_message !method_ref) in
  let packet = Packet.t_of_yojson msg in
  match packet with
  | Request r -> (
    match r.method_ with
    | "initialize" -> do_initialize method_ref r.id
    | _ -> send_server_not_initialzed method_ref r.id
  )
  | Notification n -> (match n.method_ with
    | "exit" -> exit 0
    | _ -> prerr_endline "Not initialize (Notification)")
  | _ -> prerr_endline "Not initialize (Other)"

let main_loop method_ref =
  while true do
    let msg = Yojson.Safe.from_string (Channel.read_message !method_ref) in
    let packet = Packet.t_of_yojson msg in
    (match packet with
    | Request r -> handle_request r
    | Response r -> handle_response r
    | Notification n -> handle_notification n
    | _ -> failwith "Not implemented yet");
    ()
  done

let _ =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  write_to_file "Hello world\n\n" "/home/brandon/LSP_test";

  run method_ref;


  (* initialize method_ref; *)

  (* main_loop method_ref; *)

  (* let init_req = (Channel.read_message !method_ref) in *)

  (* let request = (Option.get (Types.Message.str_to_t init_req)) in *)

  (* append_to_file (Types.Message.pretty_print request) "/home/brandon/LSP_test"; *)

  (* let test2 = (match request with *)
  (* | Request r -> handle_request r *)
  (* (1* | Notification n -> handle_notification n *1) *)
  (* | _ -> None) in *)
  (* let test2 = (Option.get test2) in *)
  (* let test = default_response ?result:(Some (default_json_value ())) ~id:(test2.id) () in *)

  (* append_to_file (Types.Message.pretty_print (Types.Message.Response test)) "/home/brandon/LSP_test"; *)

  (* let test3 = add_content_length_header (Types.Message.t_to_str (Types.Message.Response test)) in *)

  (* append_to_file test3 "/home/brandon/LSP_test"; *)

  (* print_string test3; *)
  (* flush stdout; *)

  (* let init_req = (Channel.read_message !method_ref) in *)
  (* let request = (Option.get (Types.Message.str_to_t init_req)) in *)

  (* append_to_file (Types.Message.pretty_print request) "/home/brandon/LSP_test"; *)

  (* let init_req = (Channel.read_message !method_ref) in *)
  (* let request = (Option.get (Types.Message.str_to_t init_req)) in *)

  (* append_to_file (Types.Message.pretty_print request) "/home/brandon/LSP_test"; *)

  (* let test = *) 

  exit 0
