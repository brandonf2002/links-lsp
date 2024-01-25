(* NOTE: This should be replaced with some logging info and actually now I 
   think about it a logging module *)

let write_to_file content file_name =
  let oc = open_out file_name in
  output_string oc content;
  close_out oc

let append_to_file content file_name =
  let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o666 file_name in
  output_string oc (content ^ "\n");
  close_out oc

let log_to_file content = append_to_file content "/home/brandon/LSP_test"
