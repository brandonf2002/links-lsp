type t = {
  uri: Lsp.Types.DocumentUri.t;
  version: int;
  content: string;
  language_id: string;
}

module DocumentTable = Hashtbl.Make(struct
  type t = Lsp.Types.DocumentUri.t
  let equal = Lsp.Types.DocumentUri.equal
  let hash = Lsp.Types.DocumentUri.hash
end)

let documents : t DocumentTable.t = DocumentTable.create 10

let add_document doc =
  DocumentTable.add documents doc.uri doc

let update_document uri new_content new_version =
  match DocumentTable.find_opt documents uri with
  | Some doc -> DocumentTable.replace documents uri { doc with content = new_content; version = new_version }
  | None -> failwith "Document not found"

let remove_document uri =
  DocumentTable.remove documents uri

let get_document uri =
  DocumentTable.find_opt documents uri

let format_documents () : string =
  let buffer = Buffer.create 256 in (* Use a buffer for efficient string concatenation *)
  (* Iterate over all documents *)
  DocumentTable.iter (fun _uri doc ->
    Buffer.add_string buffer (Format.sprintf "URI: %s\nVersion: %d\nLanguage ID: %s\nContent:\n%s\n\n"
      (Lsp.Types.DocumentUri.to_string doc.uri) doc.version doc.language_id doc.content)
  ) documents;
  Buffer.contents buffer
