type t = {
  uri: string;
  content: string;
}

module DocumentTable = Hashtbl.Make(struct
  type t = string
  let equal = String.equal
  let hash = Hashtbl.hash
end)

let documents : t DocumentTable.t = DocumentTable.create 10

let add_document doc =
  DocumentTable.add documents doc.uri doc

let update_document uri new_content =
  match DocumentTable.find_opt documents uri with
  | Some doc -> DocumentTable.replace documents uri { doc with content = new_content; }
  | None -> failwith "Document not found"

let remove_document uri =
  DocumentTable.remove documents uri

let get_document uri =
  DocumentTable.find_opt documents uri
