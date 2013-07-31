
type compile_options = {
  project : string;
  src : (string * string) list ;
  output : string
}

type compile_result = {
  initial_proj : string;
  stdout : string ;
  exec : string ;
  bytecode : string;
  code: int
}

val compile : (compile_result -> unit) ->  compile_options -> unit
