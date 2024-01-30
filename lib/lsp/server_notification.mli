open Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of LogMessageParams.t
  | LogTrace of LogTraceParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc2.Jsonrpc.Id.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | UnknownNotification of Jsonrpc2.Jsonrpc.Notification.t

val to_jsonrpc : t -> Jsonrpc2.Jsonrpc.Notification.t
val of_jsonrpc : Jsonrpc2.Jsonrpc.Notification.t -> (t, string) Result.t
