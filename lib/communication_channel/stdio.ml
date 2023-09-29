open Links_lsp.Header_parser
open Links_lsp.Common

let read_message_stdio () : string =
  let header = parse_header () in
  append_to_file "DONE" "/home/brandon/LSP_test";
  let buffer = Bytes.create header.content_length in
  really_input stdin buffer 0 header.content_length;
  Bytes.to_string buffer

(* TODO Change from using printf to something a bit more efficient *)
let write_message_stdio msg =
  let header = {
    content_length = String.length msg;
    content_type = None;
  } in
  Printf.printf "Content-Length: %d\r\n\r\n%s" header.content_length msg
