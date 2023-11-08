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

let current_uri : Lsp.Types.DocumentUri.t option ref = ref None

let documents : t DocumentTable.t = DocumentTable.create 10

let add_document doc =
  current_uri := Some doc.uri;
  DocumentTable.add documents doc.uri doc

let update_document uri new_content new_version =
  match DocumentTable.find_opt documents uri with
  | Some doc -> DocumentTable.replace documents uri { doc with content = new_content; version = new_version }
  | None -> failwith "Document not found"

let remove_document uri =
  DocumentTable.remove documents uri

let get_document uri =
  DocumentTable.find_opt documents uri

let parse_doc () : string = 
  Links_lsp.Common.log_to_file "Hello 1";
  let x = Linxer.Phases.initialise () in
  Links_lsp.Common.log_to_file "Hello 2";
  let cur_uri = Option.get !current_uri in
  Links_lsp.Common.log_to_file "Hello 2";
  let doc_str = Lsp.Types.DocumentUri.to_string (cur_uri) in
  Links_lsp.Common.log_to_file doc_str;
  Links_lsp.Common.log_to_file (String.sub doc_str 7 (String.length doc_str - 7));
  let context = Linxer.Phases.whole_program x doc_str in
  Links_lsp.Common.log_to_file "Hello 3";
  let y = context.program in
  Links_lsp.Common.log_to_file "Hello 4";
  Links_core.Sugartypes.show_program y

let format_documents () : string =
  let buffer = Buffer.create 256 in (* Use a buffer for efficient string concatenation *)
  (* Iterate over all documents *)
  DocumentTable.iter (fun _uri doc ->
    Buffer.add_string buffer (Format.sprintf "URI: %s\nVersion: %d\nLanguage ID: %s\nContent:\n%s\n\n"
      (Lsp.Types.DocumentUri.to_string doc.uri) doc.version doc.language_id doc.content)
  ) documents;
  Buffer.contents buffer
