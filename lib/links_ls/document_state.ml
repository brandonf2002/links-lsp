type t = {
  uri: string;
  version: int;
  content: string;
  language_id: string;
}

module DocumentTable = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let documents : t DocumentTable.t = DocumentTable.create 10

let add_document doc =
  DocumentTable.add documents doc.uri doc

let update_document uri new_content new_version new_language_id =
  match DocumentTable.find_opt documents uri with
  | Some doc -> DocumentTable.replace documents uri { doc with content = new_content; version = new_version; language_id = new_language_id }
  | None -> failwith "Document not found"

let remove_document uri =
  DocumentTable.remove documents uri

let get_document uri =
  DocumentTable.find_opt documents uri
