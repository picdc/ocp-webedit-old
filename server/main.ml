

exception Empty_cgi_argument
exception Bad_cgi_argument
exception Fail_shell_call


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
  let cmd2 = Shell.cmd "grep" [ "-c" ; user_dirname ] in
  let b = Buffer.create 503 in
  try
    Shell.call ~stdout:(Shell.to_buffer b) [ cmd1 ; cmd2 ];
    let n = int_of_string (Buffer.contents b) in
    n = 1
  with 
    Shell.Subprocess_error | Shell_sys.Fatal_error ->
      raise Fail_shell_call
  | Failure _ -> false
  | _ -> failwith "Ne doit jamais arriver"

let get_argument (cgi: Netcgi.cgi_activation) name =
  if cgi#argument_exists name then begin
    let value = cgi#argument_value name in
    if value <> "" then value
    else raise Empty_cgi_argument end
  else raise Bad_cgi_argument

let get_cookie (cgi: Netcgi.cgi_activation) name =
  let cgi = cgi#environment in
  cgi#cookie name

let print_cookies (cgi: Netcgi.cgi_activation) =
  let cgi = cgi#environment in
  let c = cgi#cookies in
  Format.printf "Let's print the list of cookies received : @.";
  List.iter (fun co -> Format.printf "name=%s@." (Netcgi.Cookie.name co)) c

let print_string str (cgi: Netcgi.cgi_activation) =
  (* cgi#set_header ~content_type:"plain/text" (); (\* TELECHARGE Oo? *\) *)
  cgi#out_channel#output_string str;
  cgi#out_channel#commit_work ()
  
let send_cookies cookies (cgi: Netcgi.cgi_activation) =
  cgi#set_header ~set_cookies:cookies ();
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
  parse_persona_response body#value


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


let login_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
          let key = get_argument cgi "assertion" in
          let user = login_function key in
          let c = Nethttp.Cookie.make "user" user in
          send_cookies [c] cgi
	with
	  _ -> print_string "Error !" cgi
      ); }

let logout_service =
  { empty_dyn_service with
    Nethttpd_services.dyn_handler =
      (fun _ cgi -> 
	try
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
	  let res = project_function () in
          print_cookies cgi;
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
    ~handlers: [
      "login_service", login_service;
      "logout_service", logout_service;
      "project_service", project_service ;
      "project_list_service", project_list_service ;
      "project_load_service", project_load_service;
      "project_create_service", project_create_service;
      "create_service", create_service;
      "project_save_service", project_save_service;
      "rename_service", rename_service;
      "project_rename_service", project_rename_service;
      "delete_service", delete_service;
      "project_delete_service", project_delete_service 
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
