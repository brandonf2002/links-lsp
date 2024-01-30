open Links_lsp.Common
open Lsp

(* open Global *)

open Document_state

(* TODO: Make return result for some failure cases *)
let prepare_rename (p : Types.PrepareRenameParams.t) =
  "Testing something: " ^ string_of_int p.position.line |> log_to_file;
  "Testing something: " ^ string_of_int p.position.character |> log_to_file;
  (* get current docs *)
  let doc = get_document p.textDocument.uri in
  let _ =
    match doc with
    | None -> ""
    | Some v -> v.content
  in
  (* let (_, _) = Links_core.Parse.parse_string Links_core.Parse.program content in *)
  (* let _ = Linxer.Phases.evaluate_string (get_init_context ()) content in *)
  (* let (pos, s1, s2) = source_code#lookup (p.position.line, p.position.character) in *)
  `Null
;;
