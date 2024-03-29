module ErrorCode : sig
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestFailed
    | ServerCancelled
    | ContentModified
    | RequestCancelled
    | Other of int

  type error =
    { code : t
    ; message : string
    ; data : Yojson.Safe.t option
    }
end

val pretty_print_error_code : ErrorCode.t -> string

type jsonrpc = string
type id
type params
type result = Yojson.Safe.t

type request =
  { jsonrpc : jsonrpc
  ; method_name : string
  ; params : params option
  ; id : id
  }

type notification =
  { jsonrpc : jsonrpc
  ; method_name : string
  ; params : params option
  }

type response =
  { jsonrpc : jsonrpc
  ; result : result option
  ; error : ErrorCode.error option
  ; id : id
  }

module Message : sig
  type t =
    | Request of request
    | Notification of notification
    | Response of response

  val str_to_t : string -> t option
  val t_to_str : t -> string
  val pretty_print : t -> string
end
