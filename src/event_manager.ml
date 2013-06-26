
class ['a, 'b] event act = object
  val action = (act: ('a -> unit) -> 'b -> unit)
  val mutable events = []

  method add_event e = events <- e::events
  method trigger args = 
    let f s = List.iter (fun f -> f s) events in
    act f args
end



let create_file = new event Filemanager.create_file
let create_project = new event Filemanager.create_project
let rename_file = new event Filemanager.rename_file
let rename_project = new event Filemanager.rename_project
let open_project = new event Filemanager.open_project
let open_file = new event Filemanager.open_file
let close_file = new event Filemanager.close_file 
let save_file = new event Filemanager.save_file
let unsaved_file = new event Filemanager.unsaved_file
let switch_file = new event Filemanager.switch_file
let delete_project = new event Filemanager.delete_project
let delete_file = new event Filemanager.delete_file
