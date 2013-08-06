
val watch : ?loggedUser:(Js.js_string Js.t) -> ?onready:(unit -> unit) 
  -> onlogin:(Js.js_string Js.t -> unit) -> onlogout:(unit -> unit) -> unit -> unit 
      
val logout : unit -> unit
    
val request : ?backgroundColor:Js.js_string Js.t
  -> ?oncancel:(unit -> unit)
  -> ?privacyPolicy:Js.js_string Js.t
  -> ?returnTo:Js.js_string Js.t
  -> ?siteLogo:Js.js_string Js.t
  -> ?siteName:Js.js_string Js.t
  -> ?termsOfService:Js.js_string Js.t
  -> unit
  -> unit
