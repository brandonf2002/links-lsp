(* Type definitions for file and tcp_stream. *)
type file = string
type tcp_stream = int

(* Type definition for the communication channel. *)
type t =
  | Stdio
  | Pipe of file
  | Socket of tcp_stream

(* Function to describe a communication channel. *)
val describe_channel : t -> string
val read_message : t -> string
val write_message : t -> string -> unit
