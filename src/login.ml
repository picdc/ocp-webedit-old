
exception Bad_request_url of string

open Dom_html
open Ace_utils

let pull_request_with_failure ~callback ~callback_failure ~meth ~url ~asyn ~msg =
  let req = XmlHttpRequest.create () in
  req##_open(Js.string meth, Js.string url, Js.bool asyn);
  req##setRequestHeader(Js.string "Content-Type",
			Js.string "application/x-www-form-urlencoded");
  let f () = 
    match req##readyState with
    | XmlHttpRequest.DONE -> if req##status = 200 then
        callback (req)
      else
        callback_failure (req##responseText)
    | _ -> ()
  in
  req##onreadystatechange <- Js.wrap_callback f;
  req##send(Js.some (Js.string msg))


let logout () =
  let open Js.Unsafe in
      ignore (fun_call (variable "navigator.id.logout") [||])


let verify_assertion ~callback assertion =
  let assertion = Js.to_string assertion in
  let callback json =
    (* Main.make_editor () *)
    Event_manager.open_workspace#trigger ();
    callback json
  in
  let callback_failure json =
    console_debug json;
    logout ();
    console_log "Login failure"
  in
  let msg = 
    Format.sprintf "assertion=%s" assertion
  in 
  pull_request_with_failure ~callback
    ~callback_failure
    ~meth:"POST"
    ~url:"/login"
    ~asyn:true 
    ~msg

let onlogout () =
  Event_manager.close_workspace#trigger ();
  console_log "Logged out"

let _ =

  let signin = createSpan document in
  signin##id <- Js.string "signin";
  signin##innerHTML <- Js.string "Sign Up / Sign In";
  signin##onclick <- handler (fun _ -> 
    console_debug "Trying to log !";
    ignore 
      (Js.Unsafe.fun_call (Js.Unsafe.variable "navigator.id.request") [||]);
    Js._true
  );


  let signout = createSpan document in
  signout##id <- Js.string "signout";  
  signout##innerHTML <- Js.string "Sign Out";
  signout##onclick <- handler (fun _ ->
    logout ();
    Js._true
  );

  let b = document##body in
  Dom.appendChild b signin;
  Dom.appendChild b signout;

  (Js.Unsafe.coerce Dom_html.window)##verifyAssertion <- 
    Js.wrap_callback (verify_assertion ~callback:(fun _ -> ()));
  (Js.Unsafe.coerce Dom_html.window)##onlogoutFunction <- 
    Js.wrap_callback onlogout
  
  
