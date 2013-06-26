
type editor
type editSession
type document
type range
type token

module Range : sig
  val range : int -> int -> int -> int -> range
end

module Token : sig
  val value : token -> string
  val _type : token -> string
end

module Document : sig
  val getLine : document -> int -> string
  val getLines : document -> int -> int -> string array
  val getTextRange : document -> range -> string
  val getValue : document -> string
  val replace : document -> range -> string -> unit
  val setValue : document -> string -> unit
end

module EditSession : sig
  val getDocument : editSession -> document
  val getTabSize : editSession -> int
  val getTokens : editSession -> int -> token array  
end

module Editor : sig
  val getSelectionRange : editor -> range
  val getSession : editor -> editSession
  val getValue : editor -> string
  val removeLines : editor -> unit
  val selectAll : editor -> unit
  val setReadOnly : editor -> bool -> unit
  val setSession : editor -> editSession -> unit
  val setValue : editor -> string -> unit
end

val edit : string -> editor
val createEditSession : text:string -> mode:string -> editSession
val require : string -> unit

