type header_info = {
  content_length : int;
  content_type : string option;
}

(* let write_to_file content file_name = *)
(*   let oc = open_out file_name in *)
(*   output_string oc content; *)
(*   close_out oc *)

(* let append_to_file content file_name = *)
(*   let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 file_name in *)
(*   output_string oc (content ^ "\n"); *)
(*   close_out oc *)

exception InvalidHeader

let read_line () =
  try Some (input_line stdin) with End_of_file -> None

let parse_header () : header_info =
  let rec aux length ctype = 
    match read_line () with
    | Some line -> (match String.trim line with
      | "" -> 
          { content_length = length; content_type = ctype }  (* Empty line indicates end of headers *)
      | line ->
        (match String.split_on_char ':' line with
         | "Content-Length" :: value :: [] -> aux (int_of_string (String.trim value)) ctype
         | "Content-Type" :: value :: [] -> aux length (Some (String.trim value))
         | _ -> aux length ctype));
    | None -> raise InvalidHeader
  in aux 0 None
