(**
   Some utils functions : 
   - Functions
   - Some Js bindings to Ace API
**)

type editSession
type range
type acetoken

type handler = (Dom_html.element Js.t, Dom_html.event Js.t)
           Dom_html.event_listener

(* class type tabs_widget = object *)
(*   inherit Dom_html.element  *)
(*   method get_tab : int -> Dom_html.element Js.t *)
(*   method set_current_tab : int -> unit *)
(*   method set_tabs_style : string -> unit *)
(*   method set_contents_style : string -> unit *)
(*   method set_tab_active_style : string -> unit *)
(*   method set_tab_noactive_style : string -> unit *)
(* end *)


val optionnal_widget : Dom_html.element Js.t -> bool -> Dom_html.element Js.t
val tabs_widget : string list -> Dom_html.element Js.t list -> int ->
  ?style_active:string -> ?style_noactive:string -> 
  ?style_tabs:string -> ?style_content:string -> unit
  -> Dom_html.element Js.t


val alert : string -> unit
val console_log : string -> unit
val console_debug : 'a -> unit

val get_element_by_id : string -> Dom_html.element Js.t

val coerceTo_input : Dom_html.element Js.t -> Dom_html.inputElement Js.t
val coerceTo_textarea : Dom_html.element Js.t -> Dom_html.textAreaElement Js.t
val split : string -> string -> string list

val make_event_oncontextmenu : Dom_html.element Js.t -> handler -> unit
val make_event_onblur : Dom_html.element Js.t -> handler -> unit



val enable_editor : unit -> unit
val disable_editor : unit -> unit

val create_edit_session : string -> editSession
val change_edit_session : editSession -> unit
val get_editor_value : unit -> string
val set_editor_value : string -> unit
val get_editsession_content : editSession -> string

val get_line : int -> string
val get_lines : int -> int -> string
val get_tab_size : unit -> int
val make_range : int -> int -> int -> int -> range
val replace : range -> string -> unit
val get_selection_range : unit -> range
val get_text_range : range -> string

val get_tokens : int -> acetoken array

module AceToken : sig
  val get_value : acetoken -> string
  val get_type : acetoken -> string
end



