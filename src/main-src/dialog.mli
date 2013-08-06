module Right_clic_dialog : sig

  type t
  type action = (Dom_html.element Js.t, Dom_html.mouseEvent Js.t)
           Dom_html.event_listener

  val show : t -> int -> int -> unit
  val hide : t -> unit 
  val create : string list -> action list -> t

  val hide_all : unit -> unit
   
end


module Prompt_dialog : sig

  val prompt : title:string -> labels:string list -> names:string list ->
    defaults:string list -> callback:((string * string) list -> unit) -> unit 

end
