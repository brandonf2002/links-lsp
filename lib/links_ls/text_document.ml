open Links_lsp.Common
open Jsonrpc2.Jsonrpc

let did_open (n : Jsonrpc2.Jsonrpc.Notification.t) =
  let params = Yojson.Safe.to_string (Notification.yojson_of_t n) in
  log_to_file params

let did_change (n : Jsonrpc2.Jsonrpc.Notification.t) =
  let params = Yojson.Safe.to_string (Notification.yojson_of_t n) in
  log_to_file params
