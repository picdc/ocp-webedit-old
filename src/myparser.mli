
open Conftypes

val split : string -> char -> string list

exception Bad_conf_file
exception Unknown_conf_var of string

val parse_to_conf : string -> conf
val parse_to_compile_conf : conf -> compile_conf

val generate_of_conf : conf -> string
val generate_of_compile_conf : compile_conf -> conf
