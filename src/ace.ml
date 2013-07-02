
open Js.Unsafe

type editor
type editSession
type document
type range
type token

(* type delta_action = InsertText | InsertLines | RemoveText | RemoveLines *)
(* type delta = { action : delta_action ; range : range ; text : string } *)

module Range = struct
    
  let range startRow startColumn endRow endColumn =
    fun_call (variable "new Range")
      [| inject startRow ; inject startColumn ; 
	 inject endRow ; inject endColumn |]

  let getStart range =
    let start = get range "start" in
    let row = get start "row" in
    let column = get start "column" in
    row, column

  let getEnd range =
    let end_ = get range "end" in
    let row = get end_ "row" in
    let column = get end_ "column" in
    row, column


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
 
  let replace editSession range text =
    ignore (meth_call editSession "replace "
	      [| inject range ; inject (Js.string text) |])

end


module Editor = struct

  let getSelectionRange editor =
    meth_call editor "getSelectionRange" [||]

  let getSession editor =
    meth_call editor "getSession" [||]

  let getValue editor =
    Js.to_string (meth_call editor "getValue" [||])

  (* let onChange editor f = *)
  (*   let f o =  *)
  (*     let data = get o "data" in *)
  (*     let action = match Js.to_string (get data "action") with *)
  (* 	| "insertText" -> InsertText   | "removeText" -> RemoveText *)
  (* 	| "removeLines" -> RemoveLines | "insertLines" -> InsertLines *)
  (* 	| s -> failwith  *)
  (* 	  ("onChange match action : pattern not exhaustive : "^s) in *)
  (*     let range = get data "range" in *)
  (*     let text = try Js.to_string (get data "text") with _ -> "" in *)
  (*     Firebug.console##log(Js.string (text^"--")); *)
  (*     let delta = { action ; range ; text } in *)
  (*     f delta *)
  (*   in *)
  (*   ignore (meth_call editor "on" *)
  (* 	      [| inject (Js.string "change");  *)
  (* 		 inject f |]) *)

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
  let str = Format.sprintf "ace.require(\"./%s\").%s;"
    fileName moduleName in
  Firebug.console##log(Js.string str);
  let r = eval_string str in
  Firebug.console##debug(r)
  
