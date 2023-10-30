open Import

let meth_ = "$/cancelRequest"

let t_of_yojson json =
  match json with
  | `Assoc fields -> Json.field_exn fields "id" Jsonrpc2.Jsonrpc.Id.t_of_yojson
  | _ -> Json.error "invalid id" json

let yojson_of_t id = `Assoc [ ("id", Jsonrpc2.Jsonrpc.Id.yojson_of_t id) ]
