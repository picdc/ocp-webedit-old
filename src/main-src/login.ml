
exception Bad_request_url of string

open Dom_html
open Myutils

let pull_request_with_failure ~callback ~callback_failure ~meth ~url ~asyn ~msg =
  let req = XmlHttpRequest.create () in
  let url = "api/" ^ url in
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



let onclick_signin = handler (fun _ -> Persona.request (); Js._true)

let onclick_signout = handler (fun _ -> Persona.logout (); Js._true)


let verify_assertion ~callback assertion =
  let assertion = Js.to_string assertion in
  let callback json =
    (* Main.make_editor () *)
    Eventmanager.open_workspace#trigger ();
    callback json
  in
  let callback_failure json =
    Persona.logout ();
    console (Js.string "Login failure")
  in
  let msg = 
    Format.sprintf "assertion=%s" assertion
  in
  let signin = get_element_by_id "signin" in
  let signout = get_element_by_id "signout" in
  signin##className <- Js.string "sign-button sign-disabled";
  signin##onclick <- handler (fun _ -> Js._true);
  signout##className <- Js.string "sign-button";
  signout##onclick <- onclick_signout;
  pull_request_with_failure ~callback
    ~callback_failure
    ~meth:"POST"
    ~url:"/login"
    ~asyn:true 
    ~msg

let onlogout () =
  Eventmanager.close_workspace#trigger ();
  let signin = get_element_by_id "signin" in
  let signout = get_element_by_id "signout" in
  signin##className <- Js.string "sign-button";
  signin##onclick <- onclick_signin;
  signout##className <- Js.string "sign-button sign-disabled";
  signout##onclick <- handler (fun _ -> Js._true);
  console (Js.string "Logged out")

let main () =
  let signin = createSpan document in
  signin##id <- Js.string "signin";
  signin##innerHTML <- Js.string "Sign Up / Sign In";
  signin##className <- Js.string "sign-button";
  signin##onclick <- onclick_signin;

  let signout = createSpan document in
  signout##id <- Js.string "signout";  
  signout##innerHTML <- Js.string "Sign Out";
  signout##className <- Js.string "sign-button sign-disabled";
  (* signout##onclick <- onclick_signout; *)

  let b = document##body in
  Dom.appendChild b signin;
  Dom.appendChild b signout;
 
  (Js.Unsafe.coerce Dom_html.window)##verifyAssertion <- 
    Js.wrap_callback (verify_assertion ~callback:(fun _ -> ()));
  (Js.Unsafe.coerce Dom_html.window)##onlogoutFunction <- 
    Js.wrap_callback onlogout;

  Myutils.console "Calling watcher";
  Persona.watch ~onlogin:(verify_assertion ~callback:(fun _ -> ()))
    ~onlogout ()
