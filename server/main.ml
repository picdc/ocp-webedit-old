

exception Empty_cgi_argument
exception Bad_cgi_argument
exception Fail_shell_call

module H = Hashtbl

let logged_users = H.create 127

(* let ppath = "/home/dmaison/ace-ocaml/data" *)
let ppath = Format.sprintf "%s/ocp-webedit/data" (Sys.getenv "HOME")

let email_to_dirname str =
  let pos_at = String.index str '@' in
  let add_pre_at = String.sub str 0 pos_at in
  let add_post_at = String.sub str (pos_at+1)
    (String.length str - pos_at - 1) in
  add_pre_at^"_at_"^add_post_at

let create_user_directory user =
  let default_dirname = "hello_project" in
  let default_filename = "hello_world.ml" in
  let default_content = "let _ = \n  print_endline \"Hello world !\"" in

  let user_dirname = email_to_dirname user in

  let dirpath = Format.sprintf "%s/%s" ppath user_dirname in
  let projectpath = Format.sprintf "%s/%s" dirpath default_dirname in
  let filepath = Format.sprintf "%s/%s" projectpath default_filename in

  let stdout = Shell.to_file ~append:false filepath in
  let cmd1 = Shell.cmd "mkdir" [ dirpath ] in
  let cmd2 = Shell.cmd "mkdir" [ projectpath ] in
  let cmd3 = Shell.cmd "echo" [ default_content ] in
  try
    Shell.call [ cmd1 ];
    Shell.call [ cmd2 ];
    Shell.call ~stdout [ cmd3 ];
  with _ -> raise Fail_shell_call

let user_exists user =
  let user_dirname = email_to_dirname user in
  let cmd1 = Shell.cmd "ls" [ ppath ] in
  let cmd2 = Shell.cmd "grep" [ "-c" ; "-w" ; user_dirname ] in
  let b = Buffer.create 503 in

  try
    Shell.call ~stdout:(Shell.to_buffer b) [ cmd1 ; cmd2 ];
    let s = String.trim (Buffer.contents b) in
    let n = int_of_string s in
    n = 1
  with
      _ -> false

exception Wrong_assertion_key
exception User_not_found


let verify_logged_user user key =
  Format.printf "Verifying cookie@.";
  if not (H.mem logged_users user) then
    () (* /!\ temporary *) 
  else
    let stored_key = H.find logged_users user in
    if stored_key <> key then
      raise Wrong_assertion_key

let get_argument (cgi: Netcgi.cgi_activation) name =
  if cgi#argument_exists name then begin
    let value = cgi#argument_value name in
    if value <> "" then value
    else raise Empty_cgi_argument end
  else raise Bad_cgi_argument

let get_cookie (cgi: Netcgi.cgi_activation) name =
  let cgi = cgi#environment in
  let c = cgi#cookies in
  try
    let res = List.find (fun co -> (Netcgi.Cookie.name co) = name) c in
    Netcgi.Cookie.value res
  with
    | Not_found -> failwith ("Cookie with name " ^ name ^ "doesn't exists")

let print_cookies (cgi: Netcgi.cgi_activation) =
  let cgi = cgi#environment in
  let c = cgi#cookies in
  Format.printf "Let's print the list of cookies received : @.";
  List.iter (fun co -> 
    let name, value = (Netcgi.Cookie.name co), (Netcgi.Cookie.value co) in
    Format.printf "%s=%s@." name value) 
    c

let print_string str (cgi: Netcgi.cgi_activation) =
  (* cgi#set_header ~content_type:"plain/text" (); (\* TELECHARGE Oo? *\) *)
  cgi#out_channel#output_string str;
  cgi#out_channel#commit_work ()

let send_file absolute_path file (cgi: Netcgi.cgi_activation) =
  cgi#set_header ~content_type:"application/octet-stream" ~filename:file ();
  let f = open_in absolute_path in
  let ic = new Netchannels.input_channel f in
  cgi#out_channel#output_channel ic;
  cgi#out_channel#commit_work ()
  
let send_cookies cookies (cgi: Netcgi.cgi_activation) =
  cgi#set_header ~set_cookies:cookies ~cache:`No_cache ();
  cgi#out_channel#output_string "Authentified successfully";
  cgi#out_channel#commit_work ()

exception Request_failed of string

let parse_persona_response r =
  let open Yojson.Basic in
      let res = from_string r in
      let status = Util.member "status" res in
      if (Util.to_string status) = "okay" then
        Util.to_string (Util.member "email" res)
      else
        let reason = Util.member "reason" res in
        raise (Request_failed (to_string reason))


(** Project management functions **)

let login_function assertion =
  let open Http_client.Convenience in
  Format.printf "Verifying assertion@.";
  let data = [("assertion", assertion); 
              ("audience", "http://localhost:4444")]  in
  let req = http_post_message
    "https://verifier.login.persona.org/verify" data in
  while not (req#is_served) do 
    Format.printf "Waiting@." done;
  let body = req#response_body in
  let user = parse_persona_response body#value in
  
  H.replace logged_users user assertion;
  (* In case the user doesn't exists *)
  if not (user_exists user) then
    begin
      Format.printf "User doesn't exists@.";
      create_user_directory user
    end;
  user


let project_function user =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s" ppath user in
  let res = Shell.cmd "ls" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_list_function user project =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s" ppath user project in
  let res = Shell.cmd "ls" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_load_function user project file =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s/%s" ppath user project file in
  let res = Shell.cmd "cat" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call

let create_function user dirname =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s" ppath user dirname in
  let res = Shell.cmd "mkdir" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call


let project_create_function user project file =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s/%s"
    ppath user project file in
  let res = Shell.cmd "touch" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ];
    Buffer.contents b
  with _ -> raise Fail_shell_call

let project_save_function user project file content =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s/%s" ppath user project file in
  let res = Shell.cmd "echo" [ content ] in
  let stdout = Shell.to_file ~append:false path in
  try
    Shell.call ~stdout [ res ]
  with _ -> raise Fail_shell_call

let project_import_function user project file content =
  ignore (project_create_function user project file);
  project_save_function user project file content

let rename_function user project new_name =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/" ppath user in
  let res = Shell.cmd "mv" [ path^project; path^new_name ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call


let project_rename_function user project file new_name =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s/" ppath user project in
  let res = Shell.cmd "mv" [ path^file; path^new_name ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let delete_function user project =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s" ppath user project in
  let res = Shell.cmd "rm" [ "-r" ; path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let project_delete_function user project file =
  let user = email_to_dirname user in
  let path = Format.sprintf "%s/%s/%s/%s" ppath user project file in
  let res = Shell.cmd "rm" [ path ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ res ]
  with _ -> raise Fail_shell_call

let export_function user project =
  let user = email_to_dirname user in
  let proj_path = Format.sprintf "%s/%s/%s" ppath user project in
  let filename = Format.sprintf "%s.tar.gz" proj_path in
  let res = Shell.cmd "tar" [ "-zcf"; filename; proj_path ] in
  Format.printf "tar -zcf %s %s@." filename proj_path;
  Shell.call [ res ];
  filename, Format.sprintf "%s.tar.gz" project


(* let export_function user project = *)
(*   let user = email_to_dirname user in *)
(*   let user_path = Format.sprintf "%s/%s" ppath user in *)
(*   let filename = Format.sprintf "%s/%s.tar.gz" user_path project in *)

(*   Format.printf "tar -zcf %s %s@." filename project; *)
(*   let res = Shell.cmd "tar" [ "-zcf"; filename; "-C"; user_path; project ] in *)
(*   Shell.call [ res ]; *)
(*   filename, Format.sprintf "%s.tar.gz" project *)

let empty_dyn_service = 
  { Nethttpd_services.dyn_handler = (fun _ _ -> ());
    dyn_activation = Nethttpd_services.std_activation
      `Std_activation_buffered;
    dyn_uri = None;
    dyn_translator = (fun _ -> "");
    dyn_accept_all_conditionals=false; }


let login_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let key = get_argument cgi "assertion" in
          let user = login_function key in
          let u = Nethttp.Cookie.make "user" user in
          let k = Nethttp.Cookie.make "key" key in
          send_cookies [u; k] cgi
	with
	  _ -> Format.printf "OUps@."; print_string "Error !" cgi
      ); }

let logout_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          H.remove logged_users user;
          Format.printf "Logged out@.";
          print_string "Logged_out" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }


let project_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let res = project_function user in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_list_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let res = project_list_function user project in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }


let project_load_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let res = project_load_function user project file in
	  print_string res cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let create_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "name" in
	  let _ = create_function user project in
	  print_string "Project created successfully" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_create_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "name" in
	  let _ = project_create_function user project file in
	  print_string "File created successfully" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_save_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let content = get_argument cgi "content" in
	  project_save_function project user file content;
	  print_string "Saved" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let project_import_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let content = get_argument cgi "content" in
	  project_import_function user project file content;
	  print_string "Imported" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let rename_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let new_name = get_argument cgi "newname" in
	  rename_function user project new_name;
	  print_string "Renamed" cgi
	with _ -> print_string "Error !" cgi
      ); }

let project_rename_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  let new_name = get_argument cgi "newname" in
	  project_rename_function user project file new_name;
	  print_string "Renamed" cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let delete_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  delete_function user project;
	  print_string "Deleted" cgi
	with _ -> print_string "Error !" cgi
      ); }

let project_delete_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let file = get_argument cgi "file" in
	  project_delete_function user project file;
	  print_string "Deleted" cgi
	with _ -> print_string "Error !" cgi
      ); }

let export_service = 
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	(* try *)
          let user = get_cookie cgi "user" in
          let key = get_cookie cgi "key" in
          verify_logged_user user key;
	  let project = get_argument cgi "project" in
	  let abs_f, f = export_function user project in
	  send_file abs_f f cgi
	(* with _ -> print_string "Error !" cgi *)
      ); }
  


let my_factory =
  Nethttpd_plex.nethttpd_factory
    ~name:"ace-edit_processor"
    ~handlers: [
      "login_service", login_service;
      "logout_service", logout_service;
      "project_service", project_service ;
      "project_list_service", project_list_service ;
      "project_load_service", project_load_service;
      "project_create_service", project_create_service;
      "create_service", create_service;
      "project_save_service", project_save_service;
      "project_import_service", project_import_service;
      "rename_service", rename_service;
      "project_rename_service", project_rename_service;
      "delete_service", delete_service;
      "project_delete_service", project_delete_service;
      "export_service", export_service 
    ]

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
  (* Enables SSL for Http_client.Convenience *)
  Ssl.init();
  Http_client.Convenience.configure_pipeline
    (fun p ->
      let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
      let tct = Https_client.https_transport_channel_type ctx in
      p # configure_transport Http_client.https_cb_id tct
    );

  Netsys_signal.init ();
  main()
