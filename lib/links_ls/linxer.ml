open Links_core
open Utility

(** Name of the file containing the prelude code. *)
let prelude_file =
  Settings.(option ~default: (Some Linkspath.prelude) "prelude"
            |> synopsis "The Links prelude source file"
            |> to_string from_string_option
            |> convert (Sys.expand ->- some)
            |> sync)

let typecheck_only
  = Settings.(flag "typecheck_only"
              |> synopsis "Only type check Links modules (development)"
              |> convert parse_bool
              |> sync)

module Phases = struct
  module Parse = struct
    let run : Context.t -> string -> Sugartypes.program Loader.result
      = Loader.load

    let string : Context.t -> string -> Sugartypes.program Loader.result
      = fun context source_code ->
      let program, pos_context =
        Parse.parse_string
          ~pp:(from_option "" (Settings.get Parse.pp))
          Parse.program
          source_code
      in
      let context' = Context.({ context with source_code = pos_context }) in
      Loader.({ program_ = program; context = context' })

    let interactive : string -> Context.t -> Sugartypes.sentence Loader.result
      = fun ps1 context ->
      let program, pos_context =
        Parse.Readline.parse ps1
      in
      let context' = Context.({ context with source_code = pos_context }) in
      Loader.({ program_ = program; context = context' })
  end

  module Desugar = struct
    let run : Sugartypes.program Loader.result -> Sugartypes.program Frontend.result
      = fun Loader.({ context; program_ }) ->
      Frontend.program context program_

    let interactive : Sugartypes.sentence Loader.result -> Sugartypes.sentence Frontend.result
      = fun Loader.({ context; program_ }) ->
      Frontend.interactive context program_
  end

  module Compile = struct
    module IR = struct
      let run : Sugartypes.program Frontend.result -> Sugartoir.result
        = fun Frontend.({ context; datatype; program }) ->
        Sugartoir.program context datatype program
    end
  end


  let dump_lib : out_channel -> unit
    = fun oc ->
    Printf.fprintf oc "lib.ml mappings:\n%!";
    Env.String.iter
      (fun name var ->
        let (datatype : string) =
          Types.string_of_datatype
            (Env.String.find name Lib.typing_env.Types.var_env)
        in
        Printf.fprintf oc " %d -> %s : %s\n%!" var name datatype)
      Lib.nenv

  (* Loads the prelude, and returns the 'initial' compilation context. *)
  let initialise : unit -> Context.t
    = fun () ->
    let context = Context.({ empty with name_environment = Lib.nenv; typing_environment = Lib.typing_env })
    in
    let filename = val_of (Settings.get prelude_file) in
    let result =
      Parse.run context filename
      |> Desugar.run
      |> (fun result ->
        let context = result.Frontend.context in
        let venv =
          Var.varify_env (Lib.nenv, Lib.typing_env.Types.var_env)
        in
        let context' = Context.({ context with variable_environment = venv }) in
        context') in
    result

        (* Compile.IR.run Frontend.({ result with context = context' })) *)
    (* in *)
    (* let context', _, _ = Evaluate.run result in *)
    (* let nenv = Context.name_environment context' in *)
    (* let tenv = Context.typing_environment context' in *)
    (* let venv = Var.varify_env (nenv, tenv.Types.var_env) in *)
    (* (1* Prepare the webserver. *1) *)
    (* Webserver.set_prelude (fst result.Backend.program); *)
    (* (1* Return the 'initial' compiler context. *1) *)
    (* Context.({ context' with variable_environment = venv }) *)

  let whole_program : Context.t -> string -> Links_core.Sugartypes.program Links_core.Frontend.result
    = fun initial_context filename ->
    (* Process source file (and its dependencies. *)
    Parse.run initial_context filename
      |> Desugar.run

  (* let evaluate_string : Context.t -> string -> Links_core.Sugartypes.program Links_core.Frontend.result *)
  let evaluate_string : Context.t -> string -> Links_core.Sugartypes.program Loader.result
    = fun initial_context source_code ->
    Parse.string initial_context source_code
    (* |> Desugar.run *)
end
