
class ['a, 'b] event : (('a -> unit) -> 'b -> unit) -> object
  method add_event : ('a -> unit) -> unit
  method trigger : 'b -> unit
end


val create_file : (Filemanager.file , string * string) event
val create_project : (string, string) event

val rename_file : (Filemanager.file , int * string) event
val rename_project : (string * string, string * string) event

val open_project : (Filemanager.file list, string) event
val open_file : (Filemanager.file * string, string * string) event
val close_file : (Filemanager.file, int) event 

val save_file : (Filemanager.file, int) event
val unsaved_file : (Filemanager.file, int) event
val switch_file : (int option * int, int) event

val delete_file : (Filemanager.file, int) event
val delete_project : (string, string) event
