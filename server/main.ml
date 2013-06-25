

exception Empty_cgi_argument
exception Bad_cgi_argument
exception Fail_shell_call


(* let ppath = "/home/dmaison/ace-ocaml/data" *)
let ppath = Format.sprintf "%s/ace-ocaml/data" (Sys.getenv "HOME")

let get_argument (cgi: Netcgi.cgi_activation) name =
  if cgi#argument_exists name then begin
    let value = cgi#argument_value name in
    if value <> "" then value
    else raise Empty_cgi_argument end
  else raise Bad_cgi_argument



let print_string str (cgi: Netcgi.cgi_activation) =
  (* cgi#set_header ~content_type:"plain/text" (); (\* TELECHARGE Oo? *\) *)
  cgi#out_channel#output_string str;
  cgi#out_channel#commit_work ()



let project_function () =
  let path = Format.sprintf "%s/common_user" ppath in
  let res = Shell.cmd "ls" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_list_function project =
  let path = Format.sprintf "%s/common_user/%s"ppath  project in
  let res = Shell.cmd "ls" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_load_function project file =
  let path = Format.sprintf "%s/common_user/%s/%s" ppath project file in
  let res = Shell.cmd "cat" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call

let create_function dirname =
  let path = Format.sprintf "%s/common_user/%s" ppath dirname in
  let res = Shell.cmd "mkdir" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_create_function project file =
  let path = Format.sprintf "%s/common_user/%s/%s"
    ppath project file in
  let res = Shell.cmd "touch" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call

let project_save_function project file content =
  let path = Format.sprintf "%s/common_user/%s/%s" ppath project file in
  let res = Shell.cmd "echo" [ content ] in
  let stdout = Shell.to_file ~append:false path in
  try
    Shell.call ~stdout [ res ]
  with _ -> raise Fail_shell_call


let rename_function project new_name =
  let path = Format.sprintf "%s/common_user/" ppath in
  let res = Shell.cmd "mv" [ path^project; path^new_name ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call


let project_rename_function project file new_name =
  let path = Format.sprintf "%s/common_user/%s/" ppath project in
  let res = Shell.cmd "mv" [ path^file; path^new_name ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let delete_function project =
  let path = Format.sprintf "%s/common_user/%s" ppath project in
  let res = Shell.cmd "rm" [ "-r" ; path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let project_delete_function project file =
  let path = Format.sprintf "%s/common_user/%s/%s" ppath project file in
  let res = Shell.cmd "rm" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let empty_dyn_service = 
  { Nethttpd_services.dyn_handler = (fun _ _ -> ());
    dyn_activation = Nethttpd_services.std_activation
      `Std_activation_buffered;
    dyn_uri = None;
    dyn_translator = (fun _ -> "");
    dyn_accept_all_conditionals=false; }

let project_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let res = project_function () in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_list_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let res = project_list_function project in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }


let project_load_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let res = project_load_function project file in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let create_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "name" in
	  let _ = create_function project in
	  print_string "Project created successfully" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_create_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "name" in
	  let _ = project_create_function project file in
	  print_string "File created successfully" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_save_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let content = get_argument cgi "content" in
	  project_save_function project file content;
	  print_string "Saved" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let rename_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let new_name = get_argument cgi "newname" in
	  rename_function project new_name;
	  print_string "Renamed" cgi
	with _ -> print_string "Error !" cgi
      ); }

let project_rename_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let new_name = get_argument cgi "newname" in
	  project_rename_function project file new_name;
	  print_string "Renamed" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let delete_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  delete_function project;
	  print_string "Deleted" cgi
	with _ -> print_string "Error !" cgi
      ); }

let project_delete_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  project_delete_function project file;
	  print_string "Deleted" cgi
	with _ -> print_string "Error !" cgi
      ); }


let my_factory =
  Nethttpd_plex.nethttpd_factory
    ~name:"ace-edit_processor"
    ~handlers: [ "project_service", project_service ;
		 "project_list_service", project_list_service ;
		 "project_load_service", project_load_service;
		 "project_create_service", project_create_service;
		 "create_service", create_service;
		 "project_save_service", project_save_service;
                 "rename_service", rename_service;
                 "project_rename_service", project_rename_service;
		 "delete_service", delete_service;
		 "project_delete_service", project_delete_service ]

    ()

let main() =
  (* Create a parser for the standard Netplex command-line arguments: *)
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  (* Parse the command-line arguments: *)
  Arg.parse
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";

  (* Select multi-processing: *)
  let parallelizer = Netplex_mp.mp() in  

  (* Start the Netplex system: *)
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ my_factory ]
    cmdline_cfg

let _ =
  Netsys_signal.init ();
  main()
