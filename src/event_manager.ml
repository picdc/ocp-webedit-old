
class ['a, 'b] event act = object
  val action = (act: ('a -> unit) -> 'b -> unit)
  val mutable events = []

  method add_event e = events <- e::events
  method trigger args = 
    let f s = List.iter (fun f -> f s) events in
    act f args
end


let open_workspace = new event Filemanager.open_workspace
let close_workspace = new event Filemanager.close_workspace
let create_project = new event Filemanager.create_project
let rename_file = new event Filemanager.rename_file
let rename_project = new event Filemanager.rename_project
let open_project = new event Filemanager.open_project
let switch_file = new event Filemanager.switch_file

let open_and_switch_action callback (project, filename) =
  let callback args =
    callback args;
    let id = Filemanager.get_id ~project ~filename in
    switch_file#trigger id in
  Filemanager.open_file callback (project, filename)

let create_and_switch_action callback (project, filename) =
  let callback args =
    callback args;
    let id = Filemanager.get_id ~project ~filename in
    switch_file#trigger id in
  Filemanager.create_file callback (project, filename)


let open_file = new event open_and_switch_action
let create_file = new event create_and_switch_action

let close_and_switch_action callback file =
  let callback args =
    callback args;
    let prev_file_opt = Filemanager.get_prev_opened_file () in
    match prev_file_opt with
    | Some prev_file -> 
(      Ace_utils.console_debug prev_file;
      switch_file#trigger prev_file.Filemanager.id)
    | None -> ()
  in
  Filemanager.close_file callback file

let close_file = new event close_and_switch_action 
let save_file = new event Filemanager.save_file


let import_and_switch_action callback (project, filename, content) =
  let callback args =
    callback args;
    let id = Filemanager.get_id ~project ~filename in
    switch_file#trigger id
  in
  Filemanager.import_file callback (project, filename, content)


let import_file = new event import_and_switch_action
let unsaved_file = new event Filemanager.unsaved_file
let delete_project = new event Filemanager.delete_project

let delete_and_switch_action callback file =
  let callback args =
    callback args;
    let prev_file_opt = Filemanager.get_prev_opened_file () in
    match prev_file_opt with
    | Some prev_file -> switch_file#trigger prev_file.Filemanager.id
    | None -> ()
  in
  Filemanager.delete_file callback file

let delete_file = new event delete_and_switch_action
