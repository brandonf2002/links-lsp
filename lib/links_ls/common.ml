open Communication_channel
open Jsonrpc2.Jsonrpc

let add_content_length_header str =
  let content_length = String.length str in
  let header = Printf.sprintf "Content-Length: %d\r\n\r\n" content_length in
  header ^ str
;;

let read_message channel =
  let msg = Yojson.Safe.from_string (Channel.read_message !channel) in
  Packet.t_of_yojson msg
;;

let write_message channel msg =
  Channel.write_message
    !channel
    (Response.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)
;;

let write_message_notif channel msg =
  Channel.write_message
    !channel
    (Notification.yojson_of_t msg |> Yojson.Safe.to_string |> add_content_length_header)
;;

let get_error_response code message id =
  let error = Response.Error.make ~code ~message () in
  Response.error id error
;;
