type file = string
type tcp_stream = int

type communication_channel =
  | Stdio
  | Pipe of file
  | Socket of tcp_stream

let describe_channel ch =
  match ch with
  | Stdio -> "Standard I/O"
  | Pipe f -> "Pipe with file: " ^ f  (* Assuming f can be converted to string *)
  | Socket s -> "Socket with stream: " ^ (string_of_int s)  (* Assuming s can be converted to string *)

let method_ref = ref Stdio (* Default communication method *)
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
    "--stdio", Arg.Unit (fun () -> check_multiple_options (); method_ref := Stdio), " Use stdio";
    "--pipe", Arg.String (fun s -> check_multiple_options (); method_ref := (Pipe s)), " Use named pipes";
    "--socket", Arg.Int (fun i -> check_multiple_options (); method_ref := (Socket i)), " Use TCP/IP socket";
    "--clientProcessId", Arg.String (fun s -> client_pid_ref := Some s), " Store the client's process ID"
  ]

let () =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  
  (match !client_pid_ref with
  | Some pid -> Printf.printf "Client Process ID: %s\n" pid
  | None -> ());

  describe_channel !method_ref |> print_endline;

  (* match !method_ref with *)
  (* | Stdio -> print_endline "Using stdio" *)
  (* | (Pipe s) -> Printf.printf "Using named pipes: %s\n" s *)
  (* | (Socket i) -> Printf.printf "Using TCP/IP socket on port: %d\n" i *)
  (* | None -> print_endline "Using stdio (default)" *)
