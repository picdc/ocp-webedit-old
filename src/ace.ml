
open Js.Unsafe

type editor
type editSession
type document
type range
type token

module Range = struct
    
  let range startRow startColumn endRow endColumn =
    fun_call (variable "new Range")
      [| inject startRow ; inject startColumn ; 
	 inject endRow ; inject endColumn |]

end

module Token = struct
  let value token = Js.to_string (get token "value")
  let _type token = Js.to_string (get token "type")
end

module Document = struct
  let getLine document row =
    Js.to_string (meth_call document "getLine" [| inject row |])

  let getLines document rowStart rowEnd =
    let res = Js.to_array (meth_call document "getLines"
		    [| inject rowStart; inject rowEnd |]) in
    Array.map (fun s -> Js.to_string s) res

  let getTextRange document range =
    Js.to_string (meth_call document "getTextRange" [| inject range |])

  let getValue document =
    Js.to_string (meth_call document "getValue" [||])

  let replace document range text =
    ignore (meth_call document "replace"
	      [| inject range ; inject (Js.string text) |])

  let setValue document value =
    ignore (meth_call document "setValue" [| inject (Js.string value) |])

end

module EditSession = struct

  let getDocument editSession =
    meth_call editSession "getDocument" [||]

  let getTabSize editSession =
    meth_call editSession "getTabSize" [||]

  let getTokens editSession row =
    Js.to_array (meth_call editSession "getTokens" [| inject row |])
  
end


module Editor = struct

  let getSelectionRange editor =
    meth_call editor "getSelectionRange" [||]

  let getSession editor =
    meth_call editor "getSession" [||]

  let getValue editor =
    Js.to_string (meth_call editor "getValue" [||])

  let removeLines editor =
    ignore (meth_call editor "removeLines" [||])
    
  let selectAll editor =
    ignore (meth_call editor "selectAll" [||])
 
  let setReadOnly editor readOnly =
    ignore (meth_call editor "setReadOnly" [| inject (Js.bool readOnly) |])

  let setSession editor session =
    ignore (meth_call editor "setSession" [| inject session |])

  let setValue editor value =
    ignore (meth_call editor "setValue" [| inject (Js.string value) |])

end


let edit el =
  fun_call (variable "ace.edit") [| inject (Js.string el) |]

let createEditSession ~text ~mode = 
  fun_call (variable "ace.createEditSession")
    [| inject (Js.string text) ; inject (Js.string mode) |]

(* UNSAFE (enfin plus que les autres) *)
let require moduleName =
  let fileName = String.uncapitalize moduleName in
  let str = Format.sprintf "var %s = ace.require(\"./%s\").%s;"
    moduleName fileName moduleName in
  ignore (eval_string str)
