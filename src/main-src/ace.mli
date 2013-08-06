
type tokenarray

class type position = object
  method row : int Js.readonly_prop
  method column : int Js.readonly_prop
end


(* TO COMPLETE *) class type range = object
  method start : position Js.t Js.readonly_prop
  method _end : position Js.t Js.readonly_prop
end

(* DISGUSTING ! (but jsnew & constr don't work) *)
val range : int -> int -> int -> int -> range Js.t

(* TO COMPLETE *) class type document = object
  method getLine : int -> Js.js_string Js.t Js.meth
  method getLines : int -> int -> Js.string_array Js.t Js.meth
  method getTextRange : range Js.t -> Js.js_string Js.t Js.meth
  method getValue : Js.js_string Js.t Js.meth
  method replace : range Js.t -> Js.js_string Js.t -> unit Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
end

(* TO COMPLETE *) class type editSession = object
  method getDocument : document Js.t Js.meth
  method getTabSize : int Js.meth
  (* method getTokens : int -> tokenarray *)
end


class type selection = object
  method selectLine : unit Js.meth
  method selectTo : int -> int -> unit Js.meth
  method setSelectionRange : range Js.t -> bool Js.t -> unit Js.meth
end

(* TO COMPLETE *) class type editor = object
  method destroy : unit Js.meth
  method getSelection : selection Js.t Js.meth
  method getSelectionRange : range Js.t Js.meth
  method getSession : editSession Js.t Js.meth
  method getValue : Js.js_string Js.t Js.meth
  method moveCursorTo : int -> int -> unit Js.meth
  method removeLines : unit Js.meth
  method selectAll : unit Js.meth
  method setReadOnly : bool Js.t -> unit Js.meth 
  method setSession : editSession Js.t -> unit Js.meth
  method setTheme : Js.js_string Js.t -> unit Js.meth
  method setValue : Js.js_string Js.t -> unit Js.meth
end

(* TO COMPLETE *) val edit : Dom_html.element Js.t -> editor Js.t
(* TO COMPLETE *) val createEditSession : string -> string -> editSession Js.t

(* CAN BE IMPROVED ? *) val require : string -> unit
