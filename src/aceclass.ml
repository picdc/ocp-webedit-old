
open Js.Unsafe

(* type editor *)
(* type editSession *)
(* type document *)
(* type range *)
(* type token *)

type t5

class range t = object
    
  val js_t = (t: t5)

  (* method range startRow startColumn endRow endColumn = *)
  (*   fun_call (variable "new Range") *)
  (*     [| inject startRow ; inject startColumn ; *)
  (* 	 inject endRow ; inject endColumn |] *)

end

let new_range startRow startColumn endRow endColumn =
  new range (fun_call (variable "new Range") 
       [| inject startRow ; inject startColumn ; 
   	 inject endRow ; inject endColumn |])

type t4

class token t = object
    
  val js_t = (t: t4)

  method value () = Js.to_string (get js_t "value")
  method _type () = Js.to_string (get js_t "type")

end

type t3

class document t = object

  val js_t = (t: t3)

  method getLine (row: int) =
    Js.to_string (meth_call js_t "getLine" [| inject row |])

  method getLines (rowStart: int) (rowEnd: int) =
    let res = Js.to_array (meth_call js_t "getLines"
		    [| inject rowStart; inject rowEnd |]) in
    Array.map (fun s -> Js.to_string s) res

  method getTextRange (range: range) =
    Js.to_string (meth_call js_t "getTextRange" [| inject range |])

  method getValue () =
    Js.to_string (meth_call js_t "getValue" [||])

  method replace (range: range) (text: string) =
    ignore (meth_call js_t "replace"
	      [| inject range ; inject (Js.string text) |])

  method setValue (value: string) =
    ignore (meth_call js_t "setValue" [| inject (Js.string value) |])

end


type t2

class editSession t = object

  val js_t = (t : t2)

  val mutable document =
    let t = meth_call t "getDocument" [||] in
    new document t

  method getDocument () = document

  method getTabSize () : int = 
    meth_call js_t "getTabSize" [||]

  method getTokens (row: int) : token array =
    Js.to_array (meth_call js_t "getTokens" [| inject row |])
  
end

type t1

class editor t = object 

  val js_t = (t: t1)

  val mutable session =
    let t = meth_call t "getSession" [||] in
    new editSession t

  method getSelectionRange () =
    new range (meth_call js_t "getSelectionRange" [||])

  method getSession () = session

  method getValue () = Js.to_string (meth_call js_t "getValue" [||])

  method removeLines () =
    ignore (meth_call js_t "removeLines" [||])
    
  method selectAll () =
    ignore (meth_call js_t "selectAll" [||])
 
  method setReadOnly readOnly =
    ignore (meth_call js_t "setReadOnly" [| inject (Js.bool readOnly) |])

  method setSession new_session = session <- new_session

  method setValue value =
    ignore (meth_call js_t "setValue" [| inject (Js.string value) |])

end


let edit el =
  new editor (fun_call (variable "ace.edit") [| inject (Js.string el) |])

let createEditSession text mode = 
  new editSession (fun_call (variable "ace.createEditSession")
    [| inject (Js.string text) ; inject (Js.string mode) |])

(* UNSAFE (enfin plus que les autres) *)
let require moduleName =
  let fileName = String.uncapitalize moduleName in
  let str = Format.sprintf "ace.require(\"./%s\").%s;"
    fileName moduleName in
  Firebug.console##log(Js.string str);
  let r = eval_string str in
  Firebug.console##debug(r)

