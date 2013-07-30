
type compile_options = {
  src : (string * string) list ;
  output : string
}

type compile_result = {
  stdout : string ;
  exec : string ;
  bytecode : string;
  code: int
}

val compile : (compile_result -> unit) ->  compile_options -> unit
