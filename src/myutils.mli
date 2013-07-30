
(* Utils for Blob Objects *)
type blob
val string_to_blob : string -> blob

(* Utils for Debug *)
val alert : string -> unit
val console : 'a -> unit

(* Utils for Dom's operation *)
val get_element_by_id : string -> Dom_html.element Js.t
val query_selector : Dom_html.element Js.t -> string -> Dom_html.element Js.t
val query_selector_all : Dom_html.element Js.t -> string -> Dom_html.element Js.t list
val insert_first : #Dom.node Js.t -> #Dom.node Js.t -> unit
val remove_node : #Dom.node Js.t -> unit
val appendChilds : #Dom.node Js.t -> #Dom.node Js.t list -> unit

(* Unsafe coerce of Dom's nodes *)
val coerceTo_input : Dom_html.element Js.t -> Dom_html.inputElement Js.t
val coerceTo_button : Dom_html.element Js.t -> Dom_html.buttonElement Js.t
val coerceTo_textarea : Dom_html.element Js.t -> Dom_html.textAreaElement Js.t

(* Utils for Ace Editor *)
val get_lines : int -> int -> string

(* Utils for Dom's events (not referenced) *)
type handler = (Dom_html.element Js.t, Dom_html.event Js.t)
           Dom_html.event_listener
val make_event_oncontextmenu : Dom_html.element Js.t -> handler -> unit
val make_event_onblur : Dom_html.element Js.t -> handler -> unit


