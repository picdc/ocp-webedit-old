
(* WARNING : Quand on est pas logger, attention raise Workspace_closed

 *)

open Conftypes


type project = {
  name : string ;
  mutable opened : bool;
  mutable files : string list;
  mutable compile_opts : compile_conf
} 

let default_compile_opts = { Conftypes.files = [] ; output = "a.out" }

type file = {
  id : int;
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

module H = Hashtbl

let id = ref 0
let current_file = ref None
let existing_projects = H.create 19
let existing_files = H.create 19
let nb_files_opened = ref 0

let opened_file_order = ref []

let file_content = H.create 19


let get_current_file () =
  !current_file

(* Pour retrouver la correspondance entre id <-> fichier *)
let get_file id =
  try H.find existing_files id
  with Not_found -> raise (File_not_found id)

let get_file2 ~project ~filename =
  let f = H.fold (fun k v acc ->
    if v.project = project && v.filename = filename then Some v
    else acc) existing_files None
  in
  match f with
  | None -> raise (File_not_found2 (project,filename))
  | Some f -> f

let get_id ~project ~filename =
  let f = get_file2 ~project ~filename in
  f.id



(* Fonctions internes *)
let set_project_conf project conf =
  (H.find existing_projects project).compile_opts <- conf

let get_project_conf project =
  (H.find existing_projects project).compile_opts

let project_exists name =
  H.mem existing_projects name

let file_exists ~project ~filename =
  let p = H.find existing_projects project in
  List.mem filename p.files

let is_file_opened ~project ~filename =
  H.fold (fun k v acc ->
    if v.project = project && v.filename = filename then v.is_open
    else acc) existing_files false

let is_project_opened project =
  try (H.find existing_projects project).opened
  with Not_found -> raise (Project_not_found project)

let add_project name compile_opts =
  let project = { name ; opened = false ; files = [] ; compile_opts } in
  H.add existing_projects name project

let add_new_project name =
  let project = { name ; opened = true ; files = [] ;
                  compile_opts = default_compile_opts } in
  H.add existing_projects name project

let add_file file =
  H.add existing_files file.id file

let add_file_to_project project filename =
  let p = H.find existing_projects project in
  p.files <- filename :: p.files

let get_nb_files_opened () = !nb_files_opened

let get_prev_opened_file () = 
  match !opened_file_order with
  | [] -> None
  | i::_ -> Some (get_file i)

let get_content id =
  try
    let es = H.find file_content id in
    Some (es##getDocument()##getValue())
  with Not_found -> None








(* Fonctions pour utiliser le filemanager *)
let workspace_opened = ref false
let open_workspace callback () =
  if not !workspace_opened then
    (* - On émet la requête pour avoir la liste des dossier
       Pour chaque dossier de la liste :
         - On émet une requête pour récupérer le .conf
         - Quand on reçoit le .conf du project, on ajoute ce project dans la
           liste
         - Si le project ajouté était le dernier de la liste,
           on appelle alors les callbacks qui étaient prévus quand on obtient
           la liste des dossiers
    *)
    (let callback ls =
       let projects_unloaded = ref (List.length ls) in
       let callback_onload_project project strconf =
         let conf = Myparser.parse_to_compile_conf
           (Myparser.parse_to_conf strconf) in
         add_project project conf;
         decr projects_unloaded;
         if !projects_unloaded <= 0 then
           callback ls
       in
       List.iter (fun project -> 
         Request.load_conf ~callback:(callback_onload_project project)
            ~name:".webuild" ~project:(Some project) ())
         ls in
     Request.get_list_of_projects ~callback;
     workspace_opened := true)
  else raise Workspace_already_opened

let close_workspace callback () =
  if !workspace_opened then
    (workspace_opened := false;
     id := 0;
     current_file := None;
     H.reset file_content;
     H.reset existing_projects;
     H.reset existing_files;
     callback ())
  else raise Workspace_closed


let open_project callback project =
 if not (is_project_opened project) then
   let callback lstr = 
     let p = H.find existing_projects project in
     p.opened <- true;
     p.files <- lstr;
     let files = List.fold_left (fun acc f ->
       let i = !id in
       let file = { id = i ; project ; filename = f ;
		    is_open = false ; is_unsaved = false } in
       add_file file;
       incr id;
       file::acc
     ) [] lstr in
     callback (project, files)
   in
   Request.get_list_of_files ~callback project
       

let open_file callback (project, filename) =
  let file = get_file2 ~project ~filename in
  if not file.is_open then
    let callback str =
      file.is_open <- true;
      incr nb_files_opened;
      opened_file_order := file.id::!opened_file_order;
      let es = Ace.createEditSession str "ace/mode/ocaml" in
      H.add file_content file.id es;
      callback (file, str)
    in
    Request.get_content_of_file ~callback ~project ~filename

let close_file callback id =
  let file = get_file id in
  file.is_open <- false;
  decr nb_files_opened;
  opened_file_order := List.filter (fun el -> el <> id) !opened_file_order;
  file.is_unsaved <- false;
  H.remove file_content id;
  begin 
    match !current_file with
    | Some i when id = i -> current_file := None
    | _ -> ()
  end;
  callback file

let create_project callback project =
  (* 
     - On émet une requête pour créer un nouveau dossier pour le project
     - Quand c'est fait, on émet une requête pour créer le .conf du project
     - Quand c'est fait, on ajoute ce project au filemanager et on appelle les
       callback prévus à la création du project
  *)
  if not (project_exists project) then
    let callback () =
      let content = Myparser.generate_of_conf
        (Myparser.generate_of_compile_conf default_compile_opts) in
      let name = ".webuild" in
      let callback () =
        add_new_project project;
        callback project
      in
      let project = Some project in
      Request.save_conf ~callback ~name ~project ~content
    in
    Request.create_project callback project 
  else raise (Bad_project_name project)


let create_file callback (project, filename) =
  let _ = try Filename.chop_extension filename 
    with _ -> 
      raise (Invalid_argument "Filename must contain an extension .ml or .mli")
  in
  
  if not (Filename.check_suffix filename "ml" 
          || Filename.check_suffix filename  "mli") then
    raise (Invalid_argument 
         ("Extension impossible, only .ml or .mli allowed"));
  
  if is_project_opened project then
    if not (file_exists ~project ~filename) then
      let callback () =
	let i = !id in
	let file = { id = i ; project ; filename ;
		     is_open = true ; is_unsaved = false } in
	add_file file;
	incr id;
	incr nb_files_opened;
	opened_file_order := i::!opened_file_order;
	add_file_to_project project filename;
	let es = Ace.createEditSession "" "ace/mode/ocaml" in
	H.add file_content i es;
	callback file
      in
      Request.create_file ~callback ~project ~filename
    else raise (Bad_file_name (project, filename))
  else raise (Project_closed project)



let rename_project callback (name, new_name) =
  if name <> new_name then
    if project_exists name then
      if not (project_exists new_name) then
	let callback () =
	  let project = H.find existing_projects name in
	  let rproject = { project with name = new_name } in
	  H.remove existing_projects name;
	  H.add existing_projects new_name rproject;
	  H.iter (fun _ f ->
	    if f.project = name then f.project <- new_name) existing_files;
	  callback (name, new_name)
	in
	Request.rename_project callback name new_name
      else raise (Bad_project_name new_name)
    else raise (Project_not_found name)

let rename_file callback (id, new_name) =
  let file = get_file id in
  if file.filename <> new_name then
    if not (file_exists ~project:file.project ~filename:new_name) then
      let callback () =
	let project = H.find existing_projects file.project in
	let new_files = List.fold_left (fun acc el ->
	  if el = file.filename then new_name::acc
	  else el::acc
	) [] project.files in
	project.files <- new_files;
	file.filename <- new_name;
	callback file
      in
      Request.rename_file ~callback ~project:file.project 
	~filename:file.filename ~new_name
    else raise (Bad_file_name (file.project, new_name))


let save_file callback id =
  let file = get_file id in
  let content = get_content id in
  let content = 
    match content with
      | Some s -> Js.to_string s
      | None -> ""
  in
  let callback () =
    file.is_unsaved <- false;
    callback file in
  Request.save_file ~callback ~project:file.project ~filename:file.filename
    ~content

let import_file callback (project, file, content) =
  let rec rename file cpt =
    let f = file ^ (string_of_int cpt) in
    if file_exists ~project ~filename:f then
      rename file (cpt+1)
    else
      f
  in
  let filename =
    if file_exists ~project ~filename:file then
      rename file 1
    else
      file
  in
  let callback () =
    let i = !id in
    let file = { id = i ; project ; filename ;
		 is_open = true ; is_unsaved = false } in
    add_file file;
    incr id;
    incr nb_files_opened;
    opened_file_order := i::!opened_file_order;
    add_file_to_project project filename;
    let es = Ace.createEditSession "" "ace/mode/ocaml" in
    es##getDocument()##setValue(Js.string content);
    H.add file_content i es;
    callback (file)
  in
  Request.import_file ~callback ~project ~filename ~content

let unsaved_file callback id =
  let file = get_file id in
  if not file.is_unsaved then
    (file.is_unsaved <- true;
     callback file)


let switch_file callback new_id =
  let old_file = !current_file in
  let do_it () =
    opened_file_order := new_id::(List.filter (fun el -> el <> new_id)
				    !opened_file_order);
    current_file := Some new_id;
    let es = try H.find file_content new_id
      with _ -> failwith "Filemanager : file_content Not Found" in
    (Ace_utils.editor())##setSession(es);
    callback (old_file, new_id)
  in 
  match old_file with
  | None -> do_it ()
  | Some id when id <> new_id -> do_it ()
  | _ -> ()


let delete_file callback id =
  let file = get_file id in
  let callback () =
    if file.is_open then
      (decr nb_files_opened;
       opened_file_order := List.filter (fun el -> el <> id)
	 !opened_file_order;
       match !current_file with
       | Some i when i = id -> current_file := None
       | _ -> ());
    H.remove existing_files id;
    H.remove file_content id;
    let project = H.find existing_projects file.project in
    let new_files = List.fold_left (fun acc el ->
      if el = file.filename then acc
      else el::acc
    ) [] project.files in
    project.files <- new_files;
    callback file
  in
  Request.delete_file ~callback ~project:file.project ~filename:file.filename
  

let delete_project callback name =
  if project_exists name then
    let callback () =
      H.remove existing_projects name;
      H.iter (fun k v -> 
	if v.project = name then
	  (* ???? #trigger delete_file ???? *)
	  H.remove existing_files k) existing_files;
      callback name
    in
    Request.delete_project ~callback ~project:name
  else raise (Project_not_found name)



let save_conf callback (conftype, conf) =
  let project = match conftype with
    | Compile p -> Some p
    | _ -> None in
  let name = match conftype with
    | Compile _ -> ".webuild" in
  let content = Myparser.generate_of_conf conf in
  let callback () =
    (match conftype with
      | Compile p ->
          let conf = Myparser.parse_to_compile_conf conf in
          set_project_conf p conf
      | _ -> ());
    callback (conftype, conf)
  in
  Request.save_conf ~callback ~name ~project ~content


let compile callback project =
  let cconf = get_project_conf project in
  let src = List.rev (List.fold_left (fun acc filename ->
    try
      let file = get_file2 ~project ~filename in
      let content = match get_content file.id with
        | Some s -> Js.to_string s
        | None -> "" in
      (filename, content)::acc
    with File_not_found2 (_,_) -> acc
  ) [] cconf.Conftypes.files) in
  let cconf = Mycompile.({ src ; output = cconf.Conftypes.output }) in
  Mycompile.compile callback cconf
