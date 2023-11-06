type t = {
  uri: Lsp.Types.DocumentUri.t;
  version: int;
  content: string;
  language_id: string;
}

val add_document : t -> unit

val get_document : Lsp.Types.DocumentUri.t -> t option

val remove_document : Lsp.Types.DocumentUri.t -> unit

val update_document : Lsp.Types.DocumentUri.t -> string -> int -> unit
(* val update_document : Lsp.Types.DocumentUri.t -> Lsp.Types.TextDocumentContentChangeEvent.t -> int -> unit *)

val format_documents : unit -> string
