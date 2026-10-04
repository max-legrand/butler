type command_spec =
  | Command_string of string
  | Command_args of string list

type service_schema =
  { name : string
  ; command : command_spec
  ; cwd : string option
  ; run_as_shell : bool
  ; color : string option
  ; dependencies : string list option
  ; watchlist : string list option
  }

type version =
  { major : int
  ; minor : int
  ; patch : int
  }

type state =
  | Pending
  | Ready
  | Running
  | Succeeded
  | Failed
  | Blocked

type node =
  { service : service_schema
  ; deps : int list
  ; mutable dependents : int list
  ; mutable state : state
  }

type graph =
  { nodes : node array
  ; lookup : (string, int) Hashtbl.t
  }

let ( let* ) = Result.bind
