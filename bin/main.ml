(* The communication_channel stuff *)

type file = string
type tcp_stream = int

type communication_channel =
  | Stdio
  | Pipe of file
  | Socket of tcp_stream

let describe_channel (ch : communication_channel) =
    match ch with
  | Stdio -> "Standard I/O"
  | Pipe f -> "Pipe with file: " ^ f  (* Assuming f can be converted to string *)
  | Socket s -> "Socket with stream: " ^ (string_of_int s)  (* Assuming s can be converted to string *)

type header_info = {
  content_length : int;
  content_type : string option;
}

exception InvalidHeader

let read_line () =
  try Some (input_line stdin) with End_of_file -> None

let parse_header () : header_info =
  let rec aux length ctype = 
    match read_line () with
    | Some "" -> { content_length = length; content_type = ctype }  (* Empty line indicates end of headers *)
    | Some line ->
      (match String.split_on_char ':' line with
       | "Content-Length" :: value :: [] -> aux (int_of_string (String.trim value)) ctype
       | "Content-Type" :: value :: [] -> aux length (Some (String.trim value))
       | _ -> aux length ctype)
    | None -> raise InvalidHeader
  in aux 0 None

let read_message_stdio () : string =
  let header = parse_header () in
  let buffer = Bytes.create header.content_length in
  really_input stdin buffer 0 header.content_length;
  Bytes.to_string buffer

let read_message ch : string =
  match ch with
  | Stdio -> read_message_stdio ()
  | Pipe f -> failwith "Not implemented"
  | Socket s -> failwith "Not implemented"

let write_message ch : string =
  match ch with
  | Stdio -> failwith "Not implemented"
  | Pipe f -> failwith "Not implemented"
  | Socket s -> failwith "Not implemented"

(* The Argument Parsing *) 

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

(* Main *)

let () =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  
  (match !client_pid_ref with
  | Some pid -> Printf.printf "Client Process ID: %s\n" pid
  | None -> ());

  describe_channel !method_ref |> print_endline;

  match !method_ref with
  | Stdio -> read_message_stdio () |> print_endline
  | (Pipe s) -> failwith "Not implemented"
  | (Socket i) -> failwith "Not implemented"
