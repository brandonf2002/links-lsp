type t = {
  uri: string;
  version: int;
  content: string;
  language_id: string;
}

val add_document : t -> unit

val get_document : string -> t option

val remove_document : string -> unit

val update_document : string -> string -> int -> string -> unit
