
open Dom_html

module H = Hashtbl

let focused_file = ref None

let right_clic_dialog_file =
  let lstr = [ "Save file" ; "Delete file" ] in
  let handler_save_file = handler (fun _ ->
    match !focused_file with
    | None -> assert false
    | Some file_id ->
      let content = Tabs.get_content_tab file_id in
      match content with
      | None -> Js._true
      | Some content -> 
	Event_manager.save_file#trigger (file_id, content);
	Js._true) in
  let handler_delete_file = handler (fun _ ->
    match !focused_file with
    | None -> assert false
    | Some file_id ->
      Event_manager.delete_file#trigger file_id;
      Js._true) in
  let lhandler = [ handler_save_file ; handler_delete_file ] in
  Dialog.Right_clic_dialog.create lstr lhandler

let add_file container file =
  let id, project, filename =
    file.Filemanager.id,
    file.Filemanager.project,
    file.Filemanager.filename
  in
  let li = createLi document in
  let is_open = ref false in

  li##className <- Js.string "side_class_file_name";
  li##id <- Js.string (Format.sprintf "side_file_%d" id);
  li##innerHTML <- Js.string filename;
  li##onclick <- handler (fun _ -> 
    if not !is_open then 
      (let callback _ content =
	 Tabs.add_tab id filename content;
	 Tabs.change_tab id
       in
       Filemanager.open_file ~callback ~project ~filename;
       is_open := true)
    else
      begin
	if Tabs.exist_tab id then Tabs.change_tab id
	else 
	  let callback _ content =
	    Tabs.add_tab id filename content;
	    Tabs.change_tab id
	  in
	  Filemanager.open_file ~callback ~project ~filename
      end;
    Js._true);
  let hand = handler (fun ev ->
    let ev = Js.Opt.get (Dom_html.CoerceTo.mouseEvent ev) (fun () ->
      failwith "fail on coerceTo mouseEvent TAG:#48978") in
    focused_file := Some id;
    let x = ev##clientX in
    let y = ev##clientY in
    Dialog.Right_clic_dialog.show right_clic_dialog_file x y;
    Js._false
  ) in
  Ace_utils.make_event_oncontextmenu li hand;

  Dom.appendChild container li

let rename_file container filename =
  container##innerHTML <- (Js.string filename)



let focused_project = ref None


let create_file project filename =
  Event_manager.create_file#trigger (project, filename)

let handler_rename_project () = handler ( fun _ ->
  match !focused_project with
  | None -> assert false
  | Some project ->
    let f new_name =
      Event_manager.rename_project#trigger (project, new_name) in
    Dialog.Prompt_dialog.prompt "Choose new project name :" project f;
    Js._true)

let right_clic_dialog_opened_project =
  let lstr = [ "Create new file" ; "Rename project" ] in
  let handler_new_file = handler (fun _ ->
    match !focused_project with
    | None -> assert false
    | Some project ->
      Dialog.Prompt_dialog.prompt "Choose file name :" "untitled.ml"
	(create_file project);
      Js._true)
  in		 
  let lhandler = [ handler_new_file ; handler_rename_project () ] in
  Dialog.Right_clic_dialog.create lstr lhandler

let right_clic_dialog_closed_project =
  let lstr = [ "Open project" ; "Rename project" ] in
  let handler_open_project = handler (fun _ ->
    match !focused_project with
    | None -> assert false
    | Some project ->
      Event_manager.open_project#trigger project;
      Js._true)
  in		 
  let lhandler = [ handler_open_project ; handler_rename_project () ] in
  Dialog.Right_clic_dialog.create lstr lhandler

let add_project container title =
  let li = createLi document in
  let ul = createUl document in
  let span = createSpan document in
  let is_shown = ref false in

  li##className <- Js.string "side_class_project_item";
  ul##className <- Js.string "side_class_file_list";
  ul##id <- Js.string ("side_project_"^title);
  span##className <- Js.string "side_class_project_name";
  span##innerHTML <- Js.string title;
  span##id <- Js.string ("side_project_"^title^"_title");
  span##onclick <- handler (fun ev ->
    if not (Filemanager.is_project_opened title) then
      (Event_manager.open_project#trigger title;
       is_shown := true)
    else if not !is_shown then
      (ul##style##display <- Js.string "";
       is_shown := true)
    else
      (ul##style##display <- Js.string "none";
       is_shown := false);
    Js._true);
  let hand = handler (fun ev ->
    let ev = Js.Opt.get (Dom_html.CoerceTo.mouseEvent ev) (fun () ->
      failwith "fail on coerceTo mouseEvent TAG:#48977") in
    focused_project := Some title;
    let x = ev##clientX in
    let y = ev##clientY in
    if Filemanager.is_project_opened title then
      Dialog.Right_clic_dialog.show right_clic_dialog_opened_project x y
    else Dialog.Right_clic_dialog.show right_clic_dialog_closed_project x y;
    Js._false
  ) in
  Ace_utils.make_event_oncontextmenu span hand;
  
  Dom.appendChild li span;
  Dom.appendChild li ul;
  Dom.appendChild container li




let make_sidepanel () =
  let div = createDiv document in
  let sideprojects = createUl document in
  div##id <- Js.string "sidepanel";

  (* Le bouton de création de project *)
  let button_create_project = createButton document in
  button_create_project##innerHTML <- Js.string "Create new project";
  button_create_project##onclick <- handler (fun _ -> 
    let f s = Event_manager.create_project#trigger s in
    Dialog.Prompt_dialog.prompt "Choose new project's name :"
      "new_project" f;
    Js._true);

  (* Les projets *)
  sideprojects##id <- Js.string "side_projects";
  let callback ls =
    List.iter (fun el -> add_project sideprojects el) ls in
  Filemanager.open_workspace ~callback; 
  



  let callback_rename_file file =
    let id, project, filename =
      file.Filemanager.id,
      file.Filemanager.project,
      file.Filemanager.filename in
    let id_container = Format.sprintf "side_file_%d" id in
    let container = Ace_utils.get_element_by_id id_container in
    rename_file container filename
  in

  let callback_create_project project =
    add_project sideprojects project
  in

  let callback_create_file file =
    let project = file.Filemanager.project in
    let id_container = "side_project_"^project in
    let container = Ace_utils.get_element_by_id id_container in
    add_file container file
  in

  let callback_open_project files =
    let project = (List.hd files).Filemanager.project in
    let id_container = "side_project_"^project in
    let container = Ace_utils.get_element_by_id id_container in
    List.iter (fun file ->
      add_file container file) files
  in

  let callback_delete_file file =
    let id_c_project = "side_project_"^file.Filemanager.project in
    let c_project = Ace_utils.get_element_by_id id_c_project in
    let id_c_file = Format.sprintf "side_file_%d" file.Filemanager.id in
    let c_file = Ace_utils.get_element_by_id id_c_file in
    Dom.removeChild c_project c_file
  in

  let callback_rename_project (name, new_name) =
    let id_c_project = "side_project_"^name in
    let c_project = Ace_utils.get_element_by_id id_c_project in
    c_project##id <- Js.string ("side_project_"^new_name);
    let id_c_title = "side_project_"^name^"_title" in
    let c_title = Ace_utils.get_element_by_id id_c_title in
    c_title##innerHTML <- Js.string new_name
  in

  Event_manager.create_file#add_event callback_create_file;
  Event_manager.create_project#add_event callback_create_project;
  Event_manager.rename_file#add_event callback_rename_file;
  Event_manager.open_project#add_event callback_open_project;
  Event_manager.delete_file#add_event callback_delete_file;
  Event_manager.rename_project#add_event callback_rename_project;

  Dom.appendChild div button_create_project;
  Dom.appendChild div sideprojects;
  div
    
