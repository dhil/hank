type cmd_args =
  { verbose: bool
  ; version: bool
  ; sources: string list
  ; frank_mode: bool
  ; flex_only: bool
  }

val args : cmd_args
