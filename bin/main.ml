open Links_lsp.Common
open Communication_channel

(* open Jsonrpc.Types *)

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

(* type jsonrpc = string *)
(* type id = Yojson.Safe.t *)
(* type params = Yojson.Safe.t *)
(* type result = Yojson.Safe.t *)
(* type error = { *)
(*   code: int; *)
(*   message: string; *)
(*   data: Yojson.Safe.t option; *)
(* } *)

(* type request = { *)
(*   jsonrpc: jsonrpc; *)
(*   method_name: string; *)
(*   params: params option; *)
(*   id: id; *)
(* } *)

(* type response = { *)
(*   jsonrpc: jsonrpc; *)
(*   result: result option; *)
(*   error: error option; *)
(*   id: id; *)
(* } *)

(* Main *)

let () =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  write_to_file "Hello world\n\n" "/home/brandon/LSP_test";
  append_to_file (Channel.read_message !method_ref) "/home/brandon/LSP_test";
  (* let req = { *)
  (*   jsonrpc = "2.0"; *)
  (*   method_name = "subtract"; *)
  (*   params = Some (`List [`Int 42; `Int 23]); *)
  (*   id = `Int 1; *)
  (* } in *)
  (* () *)
