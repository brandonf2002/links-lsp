let init_context = ref None

let get_init_context () =
  match !init_context with
  | None -> (
    let context = Linxer.Phases.initialise () in
    init_context := Some context;
    context
  )
  | Some c -> c
