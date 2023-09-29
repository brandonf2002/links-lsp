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
