
type bytecode = string

type compile_options = {
  src : (string * string) list ;
  output : string
}

val compile : (bytecode * string -> unit) ->  compile_options -> unit
