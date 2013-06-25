
type project = {
  name : string ;
  mutable opened : bool;
  mutable files : string list
} 
type file = {
  id : int;
  mutable project: string;
  mutable filename: string;
  mutable is_open: bool
}

exception Bad_project_name of string
exception Bad_file_name of string * string
exception File_not_found of int
exception File_not_found2 of string * string
exception Project_not_found of string
exception Project_closed of string
exception Workspace_already_open

module H = Hashtbl

let id = ref 0
let existing_projects = H.create 19
let existing_files = H.create 19


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

let add_project name =
  let project = { name ; opened = false ; files = [] } in
  H.add existing_projects name project

let add_new_project name =
  let project = { name ; opened = true ; files = [] } in
  H.add existing_projects name project

let add_file file =
  H.add existing_files file.id file

let add_file_to_project project filename =
  let p = H.find existing_projects project in
  p.files <- filename :: p.files





(* Fonctions pour utiliser le filemanager *)
let open_workspace =
  let already_open = ref false in
  fun ~callback ->
    if not !already_open then
      (let callback ls =
	 List.iter (fun el -> add_project el) ls;
	 callback ls in
       Request.get_list_of_projects ~callback;
       already_open := true)
    else raise Workspace_already_open


let open_project callback project =
 if not (is_project_opened project) then
   let callback lstr = 
     let p = H.find existing_projects project in
     p.opened <- true;
     p.files <- lstr;
     let files = List.fold_left (fun acc f ->
       let i = !id in
       let file = { id = i ; project ; filename = f ; is_open = false } in
       add_file file;
       incr id;
       file::acc
     ) [] lstr in
     callback files
   in
   Request.get_list_of_files ~callback project
       

let open_file callback (project, filename) =
  let file = get_file2 ~project ~filename in
  if not file.is_open then
    let callback str =
      file.is_open <- true;
      callback (file, str)
    in
    Request.get_content_of_file ~callback ~project ~filename

let close_file callback id =
  let file = get_file id in
  file.is_open <- false;
  callback file


let create_project callback project =
  if not (project_exists project) then
    let callback () =
      add_new_project project;
      callback project
    in
    Request.create_project callback project 
  else raise (Bad_project_name project)


let create_file callback (project, filename) =
  if is_project_opened project then
    if not (file_exists ~project ~filename) then
      let callback () =
	let i = !id in
	let file = { id = i ; project ; filename ; is_open = true } in
	add_file file;
	incr id;
	add_file_to_project project filename;
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


let save_file callback (id, content) =
  let file = get_file id in
  Request.save_file ~callback ~project:file.project ~filename:file.filename
    ~content


let delete_file callback id =
  let file = get_file id in
  let callback () =
    H.remove existing_files id;
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
	if v.project = name then H.remove existing_files k) existing_files;
      callback name
    in
    Request.delete_project ~callback ~project:name
  else raise (Project_not_found name)
