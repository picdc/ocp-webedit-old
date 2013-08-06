
type toplevel_worker_res = { eval : string ; output : string }

val execute : string -> (toplevel_worker_res -> unit) -> unit

val evaluate_input : unit -> unit
val evaluate_selection : unit -> unit 
val reset_toplevel : unit -> unit

val make_output : unit -> Dom_html.element Js.t
val make_toplevel : unit -> Dom_html.element Js.t
