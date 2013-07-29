
open Dom_html
open Ace_utils

module H = Hashtbl

let focused_file = ref None
let project_shown = H.create 19

let get_class_filename file is_active =
  let l = "side_file_name" in
  let l =
    if file.Filemanager.is_unsaved then l^" side_file_unsaved"
    else l
  in
  let l =
    if is_active then l^" side_file_active"
    else l
  in
  Js.string l


let reload_icons_project project =
  let container = query_selector
    (get_element_by_id ("side_project_"^project^"_title"))
    ".side_project_icons"
  in
  let ic_p = Ace_utils.query_selector container ".side_icon_plus" in
  let ic_m = Ace_utils.query_selector container ".side_icon_minus" in
  let ic_o = Ace_utils.query_selector container ".side_icon_open" in
  let ic_c = Ace_utils.query_selector container ".side_icon_close" in
  if H.find project_shown project then
    (ic_m##style##display <- Js.string "";
     ic_p##style##display <- Js.string "none")
  else
    (ic_m##style##display <- Js.string "none";
     ic_p##style##display <- Js.string "");
  if Filemanager.is_project_opened project then
    (ic_c##style##display <- Js.string "none";
     ic_o##style##display <- Js.string "")
  else
    (ic_c##style##display <- Js.string "";
     ic_o##style##display <- Js.string "none")


let right_clic_dialog_file =
  let lstr = [ "Save file" ; "Delete file" ] in
  let handler_save_file = handler (fun _ ->
    match !focused_file with
    | None -> assert false
    | Some file_id ->
      Event_manager.save_file#trigger file_id;
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
  let id, filename =
    file.Filemanager.id,
    file.Filemanager.filename
  in
  let li = createLi document in
  let icons = createImg document in
  let text = createSpan document in

  li##id <- Js.string (Format.sprintf "side_file_%d" id);
  li##className <- get_class_filename file false;
  icons##alt <- Js.string "-";
  icons##src <- Js.string "icons/file_little.png";
  icons##className <- Js.string "side_file_icons";
  text##className <- Js.string "side_file_name_text";
  text##innerHTML <- Js.string filename;
  li##onclick <- handler (fun _ -> 
    let project, filename, is_open =
      file.Filemanager.project,		(* Obligé de refresh ici *)
      file.Filemanager.filename,	(* au cas où il y a eu du chgmt *)
      file.Filemanager.is_open in
    if is_open then Event_manager.switch_file#trigger id
    else Event_manager.open_file#trigger (project, filename);
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
  make_event_oncontextmenu li hand;

  Dom.appendChild li icons;
  Dom.appendChild li text;
  Dom.appendChild container li


let focused_project = ref None


let get_class_project_name is_active =
  let l = "side_project_name" in
  let l =
    if is_active then l^" side_project_name_active"
    else l
  in
  Js.string l

let create_file project filename =
  Event_manager.create_file#trigger (project, filename)

let handler_open_project title = handler (fun _ ->
  let ul = get_element_by_id ("side_project_"^title) in
  if not (Filemanager.is_project_opened title) then
    Event_manager.open_project#trigger title
  else if not (H.find project_shown title) then
    (ul##style##display <- Js.string "";
     H.replace project_shown title true)
  else
    (ul##style##display <- Js.string "none";
     H.replace project_shown title false);
  reload_icons_project title;
  Js._true)

let handler_rename_project () = handler (fun _ ->
  match !focused_project with
  | None -> assert false
  | Some project ->
    let f new_name =
      Event_manager.rename_project#trigger (project, new_name) in
    Dialog.Prompt_dialog.prompt "Choose new project name :" project f;
    Js._true)

let handler_delete_project () = handler (fun _ ->
  match !focused_project with
  | None -> assert false
  | Some project ->
      Event_manager.delete_project#trigger project;
      Js._true)

let handler_compile_project () = handler (fun _ ->
  match !focused_project with
    | None -> assert false
    | Some project ->
        Event_manager.compile#trigger project;
        Js._true
)

let handler_compileopts_project () = handler (fun _ ->
  match !focused_project with
    | None -> assert false
    | Some project ->
        let conf = Filemanager.get_project_conf project in
        let default_files = String.concat " " conf.Conftypes.files in
        let callback files =
          let files = Myparser.split files ' ' in
          let callback output =
            let compile_conf = { Conftypes.files ; Conftypes.output } in
            let conf = Myparser.generate_of_compile_conf compile_conf in
            Event_manager.save_conf#trigger (Conftypes.Compile(project), conf)
          in
          Dialog.Prompt_dialog.prompt "Output's name"
            conf.Conftypes.output callback
        in
        Dialog.Prompt_dialog.prompt "Compile file order (file1.ml file2.ml ...)"
          default_files callback;
        Js._true)

let handler_export_project () = handler (fun _ ->
  match !focused_project with
    | None -> assert false
    | Some project ->
        (* Request.export_project (fun _ -> ()) project; *)
        let i = get_element_by_id "project-name-form" in
        let i = coerceTo_input i in
        i##value <- Js.string project;
        let s = get_element_by_id "export-submit-form" in
        let s = coerceTo_input s in
        s##click();
        Js._true)

let handler_import_file () =
  handler (fun _ ->
    let i = get_element_by_id "input_file_import" in
    let i = Ace_utils.coerceTo_input i in
    i##click ();
    Js._true)

let right_clic_dialog_opened_project =
  let lstr = [ "Create new file" ; 
               "Compile project";
               "Change compile options" ;
               "Rename project" ; 
               "Delete project"; 
               "Export project";
               "Import file" ] in
  let handler_new_file = handler (fun _ ->
    match !focused_project with
    | None -> assert false
    | Some project ->
        Dialog.Prompt_dialog.prompt "Choose file name :" "untitled.ml"
	  (create_file project);
        Js._true)
  in		 
  let lhandler = [ handler_new_file ;
                   handler_compile_project ();
                   handler_compileopts_project ();
                   handler_rename_project ();
		   handler_delete_project ();
                   handler_export_project ();
                   handler_import_file () ] in
  Dialog.Right_clic_dialog.create lstr lhandler

let right_clic_dialog_closed_project =
  let lstr = [ "Open project" ; "Rename project" ; "Delete project" ] in
  let handler_open_project = handler (fun _ ->
    match !focused_project with
    | None -> assert false
    | Some project ->
      Event_manager.open_project#trigger project;
      Js._true)
  in		 
  let lhandler = [ handler_open_project ; handler_rename_project ();
		   handler_delete_project () ] in
  Dialog.Right_clic_dialog.create lstr lhandler

let add_project container title =
  H.add project_shown title false;
  let li = createLi document in
  let ul = createUl document in
  let name = createDiv document in
  let text = createSpan document in
  let icons = createSpan document in
  let ic_p = createImg document in
  let ic_m = createImg document in
  let ic_o = createImg document in
  let ic_c = createImg document in

  li##id <- Js.string ("side_container_project_"^title);
  li##className <- Js.string "side_project";
  ul##className <- Js.string "side_file_list";
  ul##id <- Js.string ("side_project_"^title);
  icons##className <- Js.string "side_project_icons";
  ic_p##src <- Js.string "./icons/plus.png";
  ic_m##src <- Js.string "./icons/minus.png";
  ic_p##alt <- Js.string "+";
  ic_m##alt <- Js.string "-";
  ic_o##src <- Js.string "./icons/dir_opened_little.png";
  ic_o##alt <- Js.string "[O]";
  ic_c##src <- Js.string "./icons/dir_closed_little.png";
  ic_c##alt <- Js.string "[C]";
  ic_p##className <- Js.string "side_icon_plus";
  ic_m##className <- Js.string "side_icon_minus";
  ic_o##className <- Js.string "side_icon_open";
  ic_c##className <- Js.string "side_icon_close";
  name##className <- get_class_project_name false;
  name##id <- Js.string ("side_project_"^title^"_title");
  name##onclick <- handler_open_project title;
  text##className <- Js.string "side_project_name_text";
  text##innerHTML <- Js.string title;
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
  make_event_oncontextmenu name hand;
  Dom.appendChild icons ic_p;
  Dom.appendChild icons ic_m;
  Dom.appendChild icons ic_o;
  Dom.appendChild icons ic_c;
  Dom.appendChild name icons;
  Dom.appendChild name text;
  Dom.appendChild li name;
  Dom.appendChild li ul;
  Dom.appendChild container li;
  reload_icons_project title 




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

  sideprojects##id <- Js.string "side_projects";

  Dom.appendChild div button_create_project;
  Dom.appendChild div sideprojects;
  div
  




let _ =

  (* Création de l'input caché qui permet l'importation des fichiers *)
  let button = createInput
    ~name:(Js.string "importFileButton")
    ~_type:(Js.string "file")
    document
  in
  (* button##setAttribute(Js.string "multiple", Js.string "multiple"); *)
  button##style##display <- Js.string "none";
  button##id <- Js.string "input_file_import";
  button##onchange <- handler (fun _ ->
    begin
      match Js.Optdef.to_option button##files with
      | None -> ()
      | Some fl ->
  	for i=0 to fl##length do
  	  match Js.Opt.to_option fl##item(i) with
  	  | None -> ()
  	  | Some f ->
  	    begin
  	      let reader = jsnew File.fileReader () in
  	      reader##onload <- Dom.handler (fun _ ->
  		let s =
  		  match Js.Opt.to_option
  		    (File.CoerceTo.string (reader##result)) with
  		    | None -> assert false
  		    | Some str -> str
  		in
                let project = match !focused_project with
                  | None -> assert false
                  | Some p -> p
                in
                let filename, content = 
                  (Js.to_string f##name, Js.to_string s) 
                in
                Event_manager.import_file#trigger (project, filename, content);
  		Js._false);
  	      reader##readAsText (( f :> (File.blob Js.t)));
  	    end
  	done
    end;
    Js._true
  ); 
  Dom.appendChild document##body button;

  (* Creation du formulaire pour exporter les projets
     Solution temporaire *)

  let form = createForm document in
  let i = createInput
    ~_type:(Js.string "text")
    ~name:(Js.string "project")
    document
  in
  i##id <- Js.string "project-name-form";
  let submit = createInput
    ~_type:(Js.string "submit")
    ~name:(Js.string "export-submit")
    document
  in
  submit##id <- Js.string "export-submit-form";
  form##_method <- Js.string "post";
  form##action <- Js.string "export";
  form##style##display <- Js.string "none";
  
  Dom.appendChild form i;
  Dom.appendChild form submit;
  Dom.appendChild document##body form;

  let callback_open_workspace ls =
    let sideprojects =
      query_selector global_conf.container "#side_projects" in
    List.iter (fun el -> add_project sideprojects el) ls
  in

  let callback_close_workspace () =
    focused_file := None;
    focused_project := None;
    H.reset project_shown;
    let sideprojects =
      query_selector global_conf.container "#side_projects" in
    let cl = Dom.list_of_nodeList sideprojects##childNodes in
    List.iter (fun el -> Dom.removeChild sideprojects el) cl
  in

  let callback_rename_file file =
    let id, filename = file.Filemanager.id, file.Filemanager.filename in
    let container = query_selector
      (get_element_by_id (Format.sprintf "side_file_%d" id))
      ".side_file_name_text"
    in
    container##innerHTML <- (Js.string filename)
  in

  let callback_create_project project =
    let sideprojects = get_element_by_id "side_projects" in
    add_project sideprojects project;
    H.replace project_shown project true;
    reload_icons_project project
  in

  let callback_close_file file =
    let id_c_file = Format.sprintf "side_file_%d" file.Filemanager.id in
    let c_file = get_element_by_id id_c_file in
    c_file##className <- get_class_filename file false;
    let id_c_project = Format.sprintf "side_project_%s_title"
      file.Filemanager.project in
    let c_project = get_element_by_id id_c_project in
    c_project##className <- get_class_project_name false
  in

  let callback_create_file file =
    let project = file.Filemanager.project in
    let id_container = "side_project_"^project in
    let container = get_element_by_id id_container in
    add_file container file
  in

  let callback_open_project (project, files) =
    let id_container = "side_project_"^project in
    let container = get_element_by_id id_container in
    H.replace project_shown project true;
    reload_icons_project project;
    List.iter (fun file ->
      add_file container file) files
  in

  let callback_delete_file file =
    callback_close_file file;
    let id_c_file = Format.sprintf "side_file_%d" file.Filemanager.id in
    let c_file = get_element_by_id id_c_file in
    remove_node c_file
  in

  let callback_rename_project (name, new_name) =
    let b = H.find project_shown name in
    H.remove project_shown name;
    H.add project_shown new_name b;
    let id_c_project = "side_project_"^name in
    let c_project = get_element_by_id id_c_project in
    c_project##id <- Js.string ("side_project_"^new_name);
    let id_c_title = "side_project_"^name^"_title" in
    let c_title = get_element_by_id id_c_title in
    c_title##id <- Js.string ("side_project_"^new_name^"_title");
    c_title##onclick <- handler_open_project new_name;
    let c_title_text = query_selector c_title ".side_project_name_text" in
    c_title_text##innerHTML <- Js.string new_name
  in

  let callback_delete_project project =
    let sideprojects = get_element_by_id "side_projects" in
    let id_c_project = "side_container_project_"^project in
    let c_project = get_element_by_id id_c_project in
    Dom.removeChild sideprojects c_project
  in

  let callback_save_and_unsaved_file file =
    let file_id = file.Filemanager.id in
    let b = match Filemanager.get_current_file () with
      | Some id when id = file_id -> true
      | _ -> false
    in
    let id_c_file = Format.sprintf "side_file_%d" file_id in
    let c_file = get_element_by_id id_c_file in
    c_file##className <- get_class_filename file b
  in
  
  let callback_switch_file (old_id, new_id) =
    let newfile = Filemanager.get_file new_id in
    let id_c_newfile = Format.sprintf "side_file_%d" new_id in
    let c_newfile = get_element_by_id id_c_newfile in
    c_newfile##className <- get_class_filename newfile true;
    let id_c_newproject = Format.sprintf "side_project_%s_title"
      newfile.Filemanager.project in
    let c_newproject = get_element_by_id id_c_newproject in
    c_newproject##className <- get_class_project_name true; 
    match old_id with
    | Some old_id ->
      let oldfile = Filemanager.get_file old_id in
      let id_c_oldfile = Format.sprintf "side_file_%d" old_id in
      let c_oldfile = get_element_by_id id_c_oldfile in
      c_oldfile##className <- get_class_filename oldfile false;
      if oldfile.Filemanager.project <> newfile.Filemanager.project then
	let id_c_oldproject = Format.sprintf "side_project_%s_title"
	  oldfile.Filemanager.project in
	let c_oldproject = get_element_by_id id_c_oldproject in
	c_oldproject##className <- get_class_project_name false 
    | None -> ()
  in

    

  Event_manager.open_workspace#add_event callback_open_workspace;
  Event_manager.close_workspace#add_event callback_close_workspace;
  Event_manager.create_file#add_event callback_create_file;
  Event_manager.create_project#add_event callback_create_project;
  Event_manager.rename_file#add_event callback_rename_file;
  Event_manager.open_project#add_event callback_open_project;
  Event_manager.close_file#add_event callback_close_file;
  Event_manager.delete_file#add_event callback_delete_file;
  Event_manager.save_file#add_event callback_save_and_unsaved_file;
  Event_manager.unsaved_file#add_event callback_save_and_unsaved_file;
  Event_manager.switch_file#add_event callback_switch_file;
  Event_manager.rename_project#add_event callback_rename_project;
  Event_manager.delete_project#add_event callback_delete_project;

  (* Importer un fichier revient simplement à l'ajouter dans le sidepanel *)
  Event_manager.import_file#add_event callback_create_file
    
