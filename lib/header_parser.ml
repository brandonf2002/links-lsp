type header_info = {
  content_length : int;
  content_type : string option;
}

exception InvalidHeader

let read_line () =
  try Some (input_line stdin) with End_of_file -> None

let parse_header () : header_info =
  let rec aux length ctype = 
    match read_line () with
    | Some line -> (match String.trim line with
      | "" -> { content_length = length; content_type = ctype }
      | line -> (match String.split_on_char ':' line with
         | "Content-Length" :: value :: [] -> aux (int_of_string (String.trim value)) ctype
         | "Content-Type" :: value :: [] -> aux length (Some (String.trim value))
         | _ -> aux length ctype));
    | None -> raise InvalidHeader
  in aux 0 None
