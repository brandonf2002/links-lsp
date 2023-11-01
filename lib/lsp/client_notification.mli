open! Import
open Types

type t =
  | TextDocumentDidOpen of DidOpenTextDocumentParams.t
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | DidChangeWatchedFiles of DidChangeWatchedFilesParams.t
  | DidCreateFiles of CreateFilesParams.t
  | DidDeleteFiles of DeleteFilesParams.t
  | DidRenameFiles of RenameFilesParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFoldersParams.t
  | ChangeConfiguration of DidChangeConfigurationParams.t
  | Initialized
  | Exit
  | CancelRequest of Jsonrpc2.Jsonrpc.Id.t
  | WorkDoneProgressCancel of WorkDoneProgressCancelParams.t
  | SetTrace of SetTraceParams.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | UnknownNotification of Jsonrpc2.Jsonrpc.Notification.t

val of_jsonrpc : Jsonrpc2.Jsonrpc.Notification.t -> (t, string) result

val to_jsonrpc : t -> Jsonrpc2.Jsonrpc.Notification.t
