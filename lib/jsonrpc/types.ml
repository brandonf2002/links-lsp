module ErrorCode = struct
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    (* the codes below are LSP specific *)
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestFailed
    | ServerCancelled
    | ContentModified
    | RequestCancelled
    (* all other codes are custom *)
    | Other of int

  let of_int = function
    | -32700 -> ParseError
    | -32600 -> InvalidRequest
    | -32601 -> MethodNotFound
    | -32602 -> InvalidParams
    | -32603 -> InternalError
    | -32099 -> ServerErrorStart
    | -32000 -> ServerErrorEnd
    | -32002 -> ServerNotInitialized
    | -32001 -> UnknownErrorCode
    | -32800 -> RequestCancelled
    | -32801 -> ContentModified
    | -32802 -> ServerCancelled
    | -32803 -> RequestFailed
    | code -> Other code

  let to_int = function
    | ParseError -> -32700
    | InvalidRequest -> -32600
    | MethodNotFound -> -32601
    | InvalidParams -> -32602
    | InternalError -> -32603
    | ServerErrorStart -> -32099
    | ServerErrorEnd -> -32000
    | ServerNotInitialized -> -32002
    | UnknownErrorCode -> -32001
    | RequestCancelled -> -32800
    | ContentModified -> -32801
    | ServerCancelled -> -32802
    | RequestFailed -> -32803
    | Other code -> code

  type error = {
    code: t;
    message: string;
    data: Yojson.Safe.t option;
  }
end


let pretty_print_error_code code =
  match code with
  | ErrorCode.ParseError -> "ParseError"
  | ErrorCode.InvalidRequest -> "InvalidRequest"
  | ErrorCode.MethodNotFound -> "MethodNotFound"
  | ErrorCode.InvalidParams -> "InvalidParams"
  | ErrorCode.InternalError -> "InternalError"
  | ErrorCode.ServerErrorStart -> "ServerErrorStart"
  | ErrorCode.ServerErrorEnd -> "ServerErrorEnd"
  | ErrorCode.ServerNotInitialized -> "ServerNotInitialized"
  | ErrorCode.UnknownErrorCode -> "UnknownErrorCode"
  | ErrorCode.RequestFailed -> "RequestFailed"
  | ErrorCode.ServerCancelled -> "ServerCancelled"
  | ErrorCode.ContentModified -> "ContentModified"
  | ErrorCode.RequestCancelled -> "RequestCancelled"
  | ErrorCode.Other n -> Printf.sprintf "Other(%d)" n

type jsonrpc = string
type id = Yojson.Safe.t
type params = Yojson.Safe.t
type result = Yojson.Safe.t

type request = {
  jsonrpc: jsonrpc;
  method_name: string;
  params: params option;
  id: id;
}

type notification = {
  jsonrpc: jsonrpc;
  method_name: string;
  params: params option;
}

type response = {
  jsonrpc: jsonrpc;
  result: result option;
  error: ErrorCode.error option;
  id: id;
}

module Message = struct
  type t = 
    | Request of request
    | Notification of notification
    | Response of response

  (* val str_to_t : msg:string -> t *)
  (* TODO: This never returns None, which seems like an issue honestly *)
  let str_to_t msg = 
    let open Yojson.Safe.Util in
    let json = Yojson.Safe.from_string msg in
    let jsonrpc = json |> member "jsonrpc" |> to_string in
    (* TODO: Validate jsonrpc *)
    let id = json |> member "id" in
    match id with
    | `Null ->
      let method_name = json |> member "method" |> to_string in
      let params = json |> member "params" in
      (match params with
      | `Null -> Some(Notification { jsonrpc; method_name; params = None })
      | _ -> Some(Notification { jsonrpc; method_name; params = Some params }))
    (* TODO: ID should be restricted to ints, strings and bools *)
    | _ -> 
      let method_name = json |> member "method" in
      match method_name with
      | `Null -> 
        let result = json |> member "result" in
        let result = match result with
          | `Null -> None
          | _ -> Some(result) in
        let error = json |> member "error" in
        let error = match error with
          | `Null -> None
          | _ -> 
            let code = json |> member "error" |> member "code" |> to_int in
            let message = json |> member "error" |> member "message" |> to_string in
            Some(ErrorCode.{ code = ErrorCode.of_int code; message; data = None })
        in
        let id = json |> member "id" in
        Some(Response { jsonrpc; result; error; id })
      | _ -> 
          let method_name = to_string method_name in
          let params = json |> member "params" in
          let params = match params with
            | `Null -> None
            | _ -> Some(params) in
          Some (Request { jsonrpc; method_name; params; id })

  (* val t_to_str : msg:t -> string *)
  let t_to_str msg : string =
    let json_obj = match msg with
      | Request req ->
        `Assoc [
          ("jsonrpc", `String req.jsonrpc);
          ("method", `String req.method_name);
          ("id", req.id);
          ("params", match req.params with Some p -> p | None -> `Null)
        ]
      | Notification notif ->
        `Assoc [
          ("jsonrpc", `String notif.jsonrpc);
          ("method", `String notif.method_name);
          ("params", match notif.params with Some p -> p | None -> `Null)
        ]
      | Response res ->
        `Assoc [
          ("jsonrpc", `String res.jsonrpc);
          ("id", res.id);
          ("result", match res.result with Some r -> r | None -> `Null);
          ("error", match res.error with
                    | Some e -> `Assoc [("code", `Int (ErrorCode.to_int e.ErrorCode.code));
                                        ("message", `String e.ErrorCode.message)]
                    | None -> `Null)
        ]
    in
    Yojson.Safe.to_string json_obj

  let pretty_print (msg: t) : string =
    match msg with
    | Request req ->
      Printf.sprintf "Request {\n  jsonrpc: %s,\n  method_name: %s,\n  params: %s,\n  id: %s\n}"
        req.jsonrpc
        req.method_name
        (Option.value ~default:"None" (Option.map Yojson.Safe.to_string req.params))
        (Yojson.Safe.to_string req.id)
    | Notification notif ->
      Printf.sprintf "Notification {\n  jsonrpc: %s,\n  method_name: %s,\n  params: %s\n}"
        notif.jsonrpc
        notif.method_name
        (Option.value ~default:"None" (Option.map Yojson.Safe.to_string notif.params))
    | Response res ->
      let error_str = match res.error with
        | Some err -> pretty_print_error_code err.ErrorCode.code  (* Assuming ErrorCode.error has a field named code of type ErrorCode.t *)
        | None -> "None"
      in
      Printf.sprintf "Response {\n  jsonrpc: %s,\n  result: %s,\n  error: %s,\n  id: %s\n}"
        res.jsonrpc
        (Option.value ~default:"None" (Option.map Yojson.Safe.to_string res.result))
        error_str
        (Yojson.Safe.to_string res.id)
end
