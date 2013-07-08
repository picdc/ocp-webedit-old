
val get_list_of_projects :
  callback:(string list -> unit) -> unit

val get_list_of_files :
  callback:(string list -> unit) -> string -> unit

val get_content_of_file :
  callback:(string -> unit) -> project:string -> filename:string -> unit

val create_project :
  callback:(unit -> unit) -> string -> unit

val create_file :
  callback:(unit -> unit) -> project:string -> filename:string -> unit

val save_file :
  callback:(unit -> unit) -> project:string -> filename:string ->
  content:string -> unit

val import_file :
  callback:(unit -> unit) -> project:string -> filename:string ->
  content:string -> unit

val rename_file :
  callback:(unit -> unit) -> project:string -> filename:string -> 
  new_name:string -> unit

val rename_project :
  callback:(unit -> unit) -> project:string -> new_name:string -> unit

val delete_project :
  callback:(unit -> unit) -> project:string -> unit

val delete_file :
  callback:(unit -> unit) -> project:string -> filename:string -> unit
