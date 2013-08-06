
type conftype = Compile of string
type conf = (string * string) list

type compile_conf = {
  files : string list ;
  output : string
}
