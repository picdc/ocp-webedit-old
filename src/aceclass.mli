
(* type editor *)
(* type editSession *)
(* type document *)
(* type range *)
(* type token *)
type t5

class range : t5 -> object 
  (* method range : int -> int -> int -> int -> range *)
end

val new_range : int -> int -> int -> int -> range

type t4

class token : t4 -> object
  method value : unit -> string
  method _type : unit -> string
end

type t3

class document : t3 -> object
  method getLine : int -> string
  method getLines : int -> int -> string array
  method getTextRange : range -> string
  method getValue : unit -> string
  method replace : range -> string -> unit
  method setValue : string -> unit
end

type t2

class editSession : t2 -> object
  method getDocument : unit -> document
  method getTabSize : unit -> int
  method getTokens : int -> token array
end

type t1

class editor : t1 -> object
  method getSelectionRange : unit -> range
  method getSession : unit -> editSession
  method getValue : unit -> string
  method removeLines : unit -> unit
  method selectAll : unit -> unit
  method setReadOnly : bool -> unit
  method setSession : editSession -> unit
  method setValue : string -> unit
end

val edit : string -> editor
val createEditSession : string -> string -> editSession
val require : string -> unit

