(* The communication_channel stuff *)

open Links_lsp.Common
open Links_lsp.Header_parser

open Communication_channel

type file = string
type tcp_stream = int

(* type communication_channel = *)
(*   | Stdio *)
(*   | Pipe of file *)
(*   | Socket of tcp_stream *)

(* let describe_channel (ch : communication_channel) = *)
(*     match ch with *)
(*   | Stdio -> "Standard I/O" *)
(*   | Pipe f -> "Pipe with file: " ^ f  (1* Assuming f can be converted to string *1) *)
(*   | Socket s -> "Socket with stream: " ^ (string_of_int s)  (1* Assuming s can be converted to string *1) *)

(* type header_info = { *)
(*   content_length : int; *)
(*   content_type : string option; *)
(* } *)

(* let write_to_file content file_name = *)
(*   let oc = open_out file_name in *)
(*   output_string oc content; *)
(*   close_out oc *)

(* let append_to_file content file_name = *)
(*   let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 file_name in *)
(*   output_string oc (content ^ "\n"); *)
(*   close_out oc *)

(* exception InvalidHeader *)

(* let read_line () = *)
(*   try Some (input_line stdin) with End_of_file -> None *)

(* let parse_header () : header_info = *)
(*   let rec aux length ctype = *) 
(*     match read_line () with *)
(*     | Some line -> (match String.trim line with *)
(*       | "" -> *) 
(*           { content_length = length; content_type = ctype }  (1* Empty line indicates end of headers *1) *)
(*       | line -> *)
(*         (match String.split_on_char ':' line with *)
(*          | "Content-Length" :: value :: [] -> aux (int_of_string (String.trim value)) ctype *)
(*          | "Content-Type" :: value :: [] -> aux length (Some (String.trim value)) *)
(*          | _ -> aux length ctype)); *)
(*     | None -> raise InvalidHeader *)
(*   in aux 0 None *)

(* let read_message_stdio () : string = *)
(*   let header = parse_header () in *)
(*   append_to_file "DONE" "/home/brandon/LSP_test"; *)
(*   let buffer = Bytes.create header.content_length in *)
(*   really_input stdin buffer 0 header.content_length; *)
(*   Bytes.to_string buffer *)

(* (1* TODO Change from using printf to something a bit more efficient *1) *)
(* let write_message_stdio msg = *)
(*   let header = { *)
(*     content_length = String.length msg; *)
(*     content_type = None; *)
(*   } in *)
(*   Printf.printf "Content-Length: %d\r\n\r\n%s" header.content_length msg *)

(* let read_message ch : string = *)
(*   match ch with *)
(*   | Stdio -> read_message_stdio () *)
(*   | Pipe f -> failwith "Not implemented" *)
(*   | Socket s -> failwith "Not implemented" *)

(* let write_message ch msg = *)
(*   match ch with *)
(*   | Stdio -> write_message_stdio msg *)
(*   | Pipe f -> failwith "Not implemented" *)
(*   | Socket s -> failwith "Not implemented" *)

(* The Argument Parsing *) 

let method_ref = ref Stdio (* Default communication method *)
let option_counter = ref 0 (* Number of communication method options specified (Fail if more than 1) *)
let client_pid_ref = ref None  (* Optional client process ID *)

let check_multiple_options () =
  incr option_counter;
  if !option_counter > 1 then
    begin
      Printf.eprintf "Error: Multiple communication methods specified.\n";
      exit 1
    end

let specs =
  [
    "--stdio", Arg.Unit (fun () -> check_multiple_options (); method_ref := Stdio), " Use stdio";
    "--pipe", Arg.String (fun s -> check_multiple_options (); method_ref := (Pipe s)), " Use named pipes";
    "--socket", Arg.Int (fun i -> check_multiple_options (); method_ref := (Socket i)), " Use TCP/IP socket";
    "--clientProcessId", Arg.String (fun s -> client_pid_ref := Some s), " Store the client's process ID"
  ]

(* Main *)

let () =
  Arg.parse specs (fun _ -> ()) "Usage: links_lsp [options]";
  write_to_file "Hello world\n\n" "/home/brandon/LSP_test";

  (match !client_pid_ref with
  | Some pid -> Printf.printf "Client Process ID: %s\n" pid
  | None -> ());

  describe_channel !method_ref |> print_endline;

  (match !method_ref with
  (* | Stdio -> read_message_stdio () |> print_endline *)
  | Stdio -> append_to_file (read_message_stdio ()) "/home/brandon/LSP_test"
  | (Pipe s) -> failwith "Not implemented"
  | (Socket i) -> failwith "Not implemented");
