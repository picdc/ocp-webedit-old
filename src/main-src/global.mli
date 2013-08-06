
type config = { mutable editor : Ace.editor Js.t option ;
		mutable container : Dom_html.element Js.t }

val global_conf : config

val editor : unit -> Ace.editor Js.t
val init_editor : Dom_html.element Js.t -> unit
