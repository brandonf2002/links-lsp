type t = {
  uri: string;
  content: string;
}

val add_document : t -> unit

val get_document : string -> t option

val remove_document : string -> unit

val update_document : string -> string -> unit
