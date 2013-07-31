
class ['a, 'b] event : (('a -> unit) -> 'b -> unit) -> object
  method add_event : ('a -> unit) -> unit
  method trigger : 'b -> unit
end


val open_workspace : (string list, unit) event
val close_workspace : (unit, unit) event

val create_file : (Filemanager.file , string * string) event
val create_project : (string, string) event

val rename_file : (Filemanager.file , int * string) event
val rename_project : (string * string, string * string) event

val open_project : (string * Filemanager.file list, string) event
val open_file : (Filemanager.file * string, string * string) event
val close_file : (Filemanager.file, int) event 

val save_file : (Filemanager.file, int) event
val import_file : (Filemanager.file, string * string * string) event
val unsaved_file : (Filemanager.file, int) event
val switch_file : (int option * int, int) event

val delete_file : (Filemanager.file, int) event
val delete_project : (string, string) event


val save_conf : (Conftypes.conftype * Conftypes.conf,
                 Conftypes.conftype * Conftypes.conf) event
val compile : (Mycompile.compile_result, string) event

val go_to_next_error : 
  (Filemanager.file * string, string * Errors_format.err_format) event
