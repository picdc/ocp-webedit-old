
open Dom_html
open Ace_utils

type state =
| List_of_projects
| List_of_files of string

let state = ref List_of_projects

let reload_centerpanel () =
  let container = get_element_by_id "centerpanel" in
  let container_project = get_element_by_id "center_project_list" in
  let list_container_files =
    query_selector_all container ".center_file_list" in
  match !state with
  | List_of_projects ->
    container_project##style##display <- Js.string "";
    List.iter (fun el -> el##style##display <- Js.string "none")
      list_container_files
  | List_of_files project ->
    let container_this_project =
      get_element_by_id ("center_file_list_of_"^project) in
    container_project##style##display <- Js.string "none";
    List.iter (fun el -> el##style##display <- Js.string "none")
      list_container_files;
    container_this_project##style##display <- Js.string ""


let add_item_file container file =
  let div = createDiv document in
  let icon = createImg document in
  let id, filename, project =
    file.Filemanager.id,
    file.Filemanager.filename,
    file.Filemanager.project
  in
  icon##alt <- Js.string "-";
  icon##src <- Js.string "./icons/file_medium.png";
  div##id <- Js.string (Format.sprintf "center_file_num%d" id);
  div##className <- Js.string "center_file";
  div##innerHTML <- Js.string filename;
  div##onclick <- handler (fun _ ->
    Event_manager.open_file#trigger (project, filename);
    Js._true);
  insert_first div icon;
  Dom.appendChild container div


let add_item_project container container_pl project =
  let div = createDiv document in
  let icons = createSpan document in
  let ic_o = createImg document in
  let ic_c = createImg document in
  let div_file_list = createDiv document in
  let title = createH1 document in
  let button_ret = createButton document in
  icons##className <- Js.string "center_project_icons";
  ic_o##alt <- Js.string "[O]";
  ic_o##src <- Js.string "./icons/dir_opened_big.png";
  ic_o##className <- Js.string "icon_dir_opened";
  ic_o##style##display <- Js.string "none";
  ic_c##alt <- Js.string "[C]";
  ic_c##src <- Js.string "./icons/dir_closed_big.png";
  ic_c##className <- Js.string "icon_dir_closed";
  title##innerHTML <- Js.string project;
  button_ret##innerHTML <- Js.string "Return to projects list";
  div##id <- Js.string ("center_project_"^project);
  div_file_list##id <- Js.string ("center_file_list_of_"^project);
  div##className <- Js.string "center_project";
  div_file_list##className <- Js.string "center_file_list";
  div##innerHTML <- Js.string project;
  div_file_list##style##display <- Js.string "none";
  button_ret##onclick <- handler (fun _ ->
    state := List_of_projects;
    reload_centerpanel ();
    Js._true);
  div##onclick <- handler (fun _ ->
    if not (Filemanager.is_project_opened project) then
      Event_manager.open_project#trigger project;
    state := List_of_files project;
    reload_centerpanel ();
    Js._true);
  Dom.appendChild icons ic_o;
  Dom.appendChild icons ic_c;
  insert_first div icons;
  Dom.appendChild div_file_list button_ret;
  Dom.appendChild div_file_list title; 
  Dom.appendChild container div_file_list;
  Dom.appendChild container_pl div

let make_centerpanel () =
  let div = createDiv document in
  let title = createH1 document in
  let div_project_list = createDiv document in
  title##innerHTML <- Js.string "List of your projects";
  div##id <- Js.string "centerpanel";
  div_project_list##id <- Js.string "center_project_list";
  Dom.appendChild div_project_list title;
  Dom.appendChild div div_project_list;
  div

let _ =
  let callback_open_workspace ls =
    let c1 = get_element_by_id "centerpanel" in
    let c2 = get_element_by_id "center_project_list" in
    List.iter (fun s -> add_item_project c1 c2 s) ls
  in

  let callback_open_project (project, files) =
    let container = Ace_utils.get_element_by_id (
      "center_file_list_of_"^project) in
    let container_project = get_element_by_id ("center_project_"^project)in
    let ic_o = query_selector container_project ".icon_dir_opened" in
    let ic_c = query_selector container_project ".icon_dir_closed" in
    ic_c##style##display <- Js.string "none";
    ic_o##style##display <- Js.string "";
    List.iter (fun file -> add_item_file container file) files
  in

  Event_manager.open_workspace#add_event callback_open_workspace;
  Event_manager.open_project#add_event callback_open_project
