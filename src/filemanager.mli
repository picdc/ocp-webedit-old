
type project = {
  name : string ;
  mutable opened : bool;
  mutable files : string list
} 
type file = {
  id : int; (* Must be unique for all files *)
  mutable project: string;
  mutable filename: string;
  mutable is_open: bool
}

exception Bad_project_name of string
exception Bad_file_name of string * string
exception File_not_found of string * string
exception Project_not_found of string
exception Project_closed of string
exception Workspace_already_open

val is_project_opened : string -> bool
val is_file_opened : project:string -> filename:string -> bool
val file_exists : project:string -> filename:string -> bool
val project_exists : string -> bool

val get_file : int -> file
val get_id : project:string -> filename:string -> int

val open_workspace : callback:(string list -> unit) -> unit
val open_project : (file list -> unit) -> string -> unit
val open_file : callback:(int -> string -> unit) -> project:string ->
  filename:string -> unit

val create_project : (string -> unit) -> string -> unit
val create_file : (file -> unit) -> (string * string) -> unit

val rename_file : (file -> unit) -> (int * string) -> unit
val rename_project : ((string * string) -> unit) -> (string * string)
  -> unit
val save_file : (unit -> unit) -> (int * string) -> unit

val delete_file : (file -> unit) -> int -> unit
val delete_project : (string -> unit) -> string -> unit
