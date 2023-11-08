open Links_lsp.Common
open Communication_channel

open Links_ls.Ls

open Links_core

(* The Argument Parsing *) 

let channel = ref Channel.Stdio
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
    "--stdio", Arg.Unit (fun () -> check_multiple_options (); channel := Channel.Stdio), " Use stdio";
    "--pipe", Arg.String (fun s -> check_multiple_options (); channel := (Channel.Pipe s)), " Use named pipes";
    "--socket", Arg.Int (fun i -> check_multiple_options (); channel := (Channel.Socket i)), " Use TCP/IP socket";
    "--clientProcessId", Arg.String (fun s -> client_pid_ref := Some s), " Store the client's process ID"
  ]

(* Main *)

let _ =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  write_to_file "Hello world\n\n" "/home/brandon/LSP_test";

  log_to_file "Starting server\n";

  let phr = Sugartypes.Constant (CommonTypes.Constant.Int 42) in
  log_to_file (Sugartypes.show_phrasenode phr);

  run channel;

  exit 0
