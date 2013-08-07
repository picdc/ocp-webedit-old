
(** Id **)
open Js.Unsafe


let watch ?(loggedUser=Js.string "") ?(onready=(fun _ -> ())) 
    ~onlogin ~onlogout () =
  Myutils.console "watch !";
  let id = variable "navigator.id" in
  let w = obj [| "loggedUser", inject loggedUser; 
                 "onlogin", inject onlogin;
                 "onlogout", inject onlogout; 
                 "onready", inject onready |] 
  in
  meth_call id "watch" [| inject w |]
    
    
(* val logout : unit -> unit Js.meth *)
      
let logout _ =
  let id = variable "navigator.id" in
  meth_call id "logout" [||]

let request ?(backgroundColor=Js.string "") ?(oncancel=(fun () -> ())) 
    ?(privacyPolicy=Js.string "")
    ?(returnTo=Js.string "") 
    ?(siteLogo=Js.string "") 
    ?(siteName=Js.string "") 
    ?(termsOfService=Js.string "") () =
  let _s = Js.string in
  let i_v v = inject (variable (Js.to_string v)) in
  let id = variable "navigator.id" in
  let r = obj [| "backgroundColor", i_v backgroundColor;
                 "oncancel", inject oncancel;
                 "privacyPolicy", i_v privacyPolicy;
                 "returnTo", i_v returnTo;
                 "siteLogo", i_v siteLogo;
                 "siteName", i_v siteName;
                 "termsOfService", i_v termsOfService |]
  in
  ignore (meth_call id "request" [| inject r |])
