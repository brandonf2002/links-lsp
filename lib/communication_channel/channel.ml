open Links_lsp.Header_parser

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

let read_message_stdio () : string =
  let header = parse_header () in
  append_to_file "DONE" "/home/brandon/LSP_test";
  let buffer = Bytes.create header.content_length in
  really_input stdin buffer 0 header.content_length;
  Bytes.to_string buffer

(* TODO Change from using printf to something a bit more efficient *)
let write_message_stdio msg =
  let header = {
    content_length = String.length msg;
    content_type = None;
  } in
  Printf.printf "Content-Length: %d\r\n\r\n%s" header.content_length msg

let read_message ch : string =
  match ch with
  | Stdio -> read_message_stdio ()
  | Pipe f -> failwith "Not implemented"
  | Socket s -> failwith "Not implemented"

let write_message ch msg =
  match ch with
  | Stdio -> write_message_stdio msg
  | Pipe f -> failwith "Not implemented"
  | Socket s -> failwith "Not implemented"
