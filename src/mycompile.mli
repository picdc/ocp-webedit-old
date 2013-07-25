
type compile_options = {
  src : (string * string) list ;
  output : string
}

val compile : compile_options -> unit
