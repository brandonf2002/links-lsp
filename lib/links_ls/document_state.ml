open Global

type t =
  { uri : Lsp.Types.DocumentUri.t
  ; version : int
  ; content : string
  ; language_id : string
  ; parsed_ast : Links_core.Sugartypes.program Links_core.Loader.result option
  ; desugared_ast : Links_core.Sugartypes.program Links_core.Frontend.result option
  }

module DocumentTable = Hashtbl.Make (struct
    type t = Lsp.Types.DocumentUri.t

    let equal = Lsp.Types.DocumentUri.equal
    let hash = Lsp.Types.DocumentUri.hash
  end)

let current_uri : Lsp.Types.DocumentUri.t option ref = ref None
let documents : t DocumentTable.t = DocumentTable.create 10

let add_document doc =
  current_uri := Some doc.uri;
  DocumentTable.add documents doc.uri doc
;;

let update_document uri new_content new_version =
  current_uri := Some uri;
  let parsed_ast =
    try Some (Linxer.Phases.Parse.string (get_init_context ()) new_content) with
    | _ -> None
  in
  let desugared_ast =
    try
      match parsed_ast with
      | Some ast -> Some (Linxer.Phases.Desugar.run ast)
      | _ -> None
    with
    | _ -> None
  in
  (* TODO: This is horrible, it's just trying to keep some ASTs for
     completion when the document is updated to an unparsable state *)
  match DocumentTable.find_opt documents uri with
  | Some doc ->
    (match parsed_ast with
     | Some _ ->
       (match desugared_ast with
        | Some _ ->
          DocumentTable.replace
            documents
            uri
            { doc with
              content = new_content
            ; version = new_version
            ; parsed_ast
            ; desugared_ast
            }
        | _ ->
          DocumentTable.replace
            documents
            uri
            { doc with content = new_content; version = new_version; parsed_ast })
     | _ ->
       DocumentTable.replace
         documents
         uri
         { doc with content = new_content; version = new_version })
  | None -> failwith "Document not found"
;;

let remove_document uri = DocumentTable.remove documents uri
let get_document uri = DocumentTable.find_opt documents uri

(* let gen_ast uri = *)

let parse_doc_ast () : Links_core.Sugartypes.program Links_core.Loader.result =
  let x = Linxer.Phases.initialise () in
  let doc = DocumentTable.find documents (Option.get !current_uri) in
  Linxer.Phases.Parse.string x doc.content
;;

let parse_doc_string () : string =
  let x = Linxer.Phases.initialise () in
  let doc = DocumentTable.find documents (Option.get !current_uri) in
  try
    let context = Linxer.Phases.Parse.string x doc.content in
    let y = context.program_ in
    Links_core.Sugartypes.show_program y
  with
  | e -> Links_core.Errors.format_exception e
;;

let format_documents () : string =
  let buffer = Buffer.create 256 in
  (* Use a buffer for efficient string concatenation *)
  (* Iterate over all documents *)
  DocumentTable.iter
    (fun _uri doc ->
      Buffer.add_string
        buffer
        (Format.sprintf
           "URI: %s\nVersion: %d\nLanguage ID: %s\nContent:\n%s\n\n"
           (Lsp.Types.DocumentUri.to_string doc.uri)
           doc.version
           doc.language_id
           doc.content))
    documents;
  Buffer.contents buffer
;;
