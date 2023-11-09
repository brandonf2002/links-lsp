open Stdio
open Links_lsp.Common

type file = string
type tcp_stream = int

type t =
  | Stdio
  | Pipe of file
  | Socket of tcp_stream

let describe_channel (ch : t) =
    match ch with
  | Stdio -> "Standard I/O"
  | Pipe f -> "Pipe with file: " ^ f  (* Assuming f can be converted to string *)
  | Socket s -> "Socket with stream: " ^ (string_of_int s)  (* Assuming s can be converted to string *)

let read_message ch : string =
  let msg = match ch with
  | Stdio -> read_message_stdio ()
  | Pipe _ -> failwith "Not implemented"
  | Socket _ -> failwith "Not implemented" in
  append_to_file ("Reading from channel: \n" ^ msg ^ "\n\n") "/home/brandon/LSP_test";
  msg

let write_message ch msg =
  append_to_file ("Writing to channel: \n" ^ msg ^ "\n\n") "/home/brandon/LSP_test";
  match ch with
  | Stdio -> write_message_stdio msg
  | Pipe _ -> failwith "Not implemented"
  | Socket _ -> failwith "Not implemented"
