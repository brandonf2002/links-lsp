type t =
  { uri : Lsp.Types.DocumentUri.t
  ; version : int
  ; content : string
  ; language_id : string
  ; parsed_ast : Links_core.Sugartypes.program Links_core.Loader.result option
  ; desugared_ast : Links_core.Sugartypes.program Links_core.Frontend.result option
  }

val add_document : t -> unit
val get_document : Lsp.Types.DocumentUri.t -> t option
val remove_document : Lsp.Types.DocumentUri.t -> unit

val update_document
  :  ?language_id:string
  -> Lsp.Types.DocumentUri.t
  -> string
  -> int
  -> unit
(* val update_document : Lsp.Types.DocumentUri.t -> Lsp.Types.TextDocumentContentChangeEvent.t -> int -> unit *)

val format_documents : unit -> string
val parse_doc_ast : unit -> Links_core.Sugartypes.program Links_core.Loader.result
val parse_doc_string : unit -> string
