
type conf = (string * string) list

val split : string -> char -> string list

exception Bad_conf_file
exception Unknown_conf_var of string

val parse_conf : string -> conf
val parse_compile_conf : conf -> Conftypes.compile_conf

val generate_conf : conf -> string
val generate_compile_conf : Conftypes.compile_conf -> conf
