type t =
  { uri : Lsp.Types.DocumentUri.t
  ; version : int
  ; content : string
  ; language_id : string
  ; ast : Links_core.Sugartypes.program Links_core.Loader.result option
  }

val add_document : t -> unit
val get_document : Lsp.Types.DocumentUri.t -> t option
val remove_document : Lsp.Types.DocumentUri.t -> unit
val update_document : Lsp.Types.DocumentUri.t -> string -> int -> unit
(* val update_document : Lsp.Types.DocumentUri.t -> Lsp.Types.TextDocumentContentChangeEvent.t -> int -> unit *)

val format_documents : unit -> string
val parse_doc_ast : unit -> Links_core.Sugartypes.program Links_core.Loader.result
val parse_doc_string : unit -> string
