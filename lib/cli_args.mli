(* Expose the parsing function and refs that main will use *)
open CommunicationChannel

val parse_args : unit -> unit
val method_ref : CommunicationChannel.t ref
val client_pid_ref : string option ref
