open Stdio

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
  match ch with
  | Stdio -> read_message_stdio ()
  | Pipe _ -> failwith "Not implemented"
  | Socket _ -> failwith "Not implemented"

let write_message ch msg =
  match ch with
  | Stdio -> write_message_stdio msg
  | Pipe _ -> failwith "Not implemented"
  | Socket _ -> failwith "Not implemented"
