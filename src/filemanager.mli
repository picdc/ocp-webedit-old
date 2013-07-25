
type compile_opts = {
  mutable fids : int list;
  mutable output : string
}

type project = {
  name : string ;
  mutable opened : bool;
  mutable files : string list;
  compile_opts : compile_opts
} 

type file = {
  id : int; (* Must be unique for all files *)
  mutable project: string;
  mutable filename: string;
  mutable is_open: bool;
  mutable is_unsaved: bool
}

exception Bad_project_name of string
exception Bad_file_name of string * string
exception File_not_found of int
exception File_not_found2 of string * string
exception Project_not_found of string
exception Project_closed of string
exception Workspace_already_opened
exception Workspace_closed

val is_project_opened : string -> bool
val is_file_opened : project:string -> filename:string -> bool
val file_exists : project:string -> filename:string -> bool
val project_exists : string -> bool

val get_current_file : unit -> int option
val get_nb_files_opened : unit -> int
val get_prev_opened_file : unit -> file option
val get_file : int -> file
val get_id : project:string -> filename:string -> int

val open_workspace : (string list -> unit) -> unit -> unit
val close_workspace : (unit -> unit) -> unit -> unit
val open_project : (string * file list -> unit) -> string -> unit
val open_file : (file * string -> unit) -> string * string -> unit
(* Ferme le fichier puis appel les callback
   avec le fichier qui vient d'être fermé *)
val close_file : (file -> unit) -> int -> unit

val create_project : (string -> unit) -> string -> unit
val create_file : (file -> unit) -> (string * string) -> unit
val rename_file : (file -> unit) -> (int * string) -> unit
val rename_project : (string * string -> unit) -> string * string -> unit
val save_file : (file -> unit) -> int -> unit
val import_file : (file-> unit) -> (string * string * string) -> unit
val unsaved_file : (file -> unit) -> int -> unit
val switch_file : (int option * int -> unit) -> int -> unit 
(* Supprime le fichier puis appel les callback
   avec le fichier qui vient d'être supprimé *)
val delete_file : (file -> unit) -> int -> unit
val delete_project : (string -> unit) -> string -> unit
