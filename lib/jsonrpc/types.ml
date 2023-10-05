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
end
