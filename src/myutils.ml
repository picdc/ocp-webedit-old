
open Dom_html
open Js
open Unsafe


(* Utils for Blob Objects *)

open Typed_array

type blob

let string_to_blob s =
  let uint8Array : (int -> uint8Array Js.t) Js.constr = 
    variable "this.Uint8Array" in
  let a = jsnew uint8Array(String.length s) in
  for i = 0 to (String.length s) - 1 do set a i (Char.code s.[i]) done;
  let a = [| a##buffer |] in 
  fun_call (variable "new Blob")
    [| inject a ; 
       inject (variable "{type: \"application/octet-stream;charset=utf-8\"}") |]
   
    

 
(* Utils for Debug *)

let alert str = window##alert(string str)

let console o = Firebug.console##log(o)





(* Utils for Dom's operation *)

let get_element_by_id id =
  Opt.get (document##getElementById (string id))
    (fun () -> failwith ("fail get_element_by_id : "^id))

let query_selector el query =
  Opt.get el##querySelector(string query)
    (fun () -> 
      console (string "Failure QuerySelector on #Dom.element :");
      console el;
      failwith ("QuerySelector fail with : "^query))

let query_selector_all el query =
  Dom.list_of_nodeList el##querySelectorAll(string query)

let insert_first n c =
  let p = n##firstChild in
  Dom.insertBefore n c p

let remove_node c =
  let p = c##parentNode in
  let p = Js.Opt.get p (fun _ -> failwith "Echec remove_node") in
  Dom.removeChild p c

let appendChilds n cl =
  List.iter (fun c -> Dom.appendChild n c) cl



(* Unsafe coerce of Dom's nodes *)

let coerceTo_input el =
  match Opt.to_option (Dom_html.CoerceTo.input el) with
  | Some s -> s
  | None -> failwith "coerco_input failed"

let coerceTo_button el =
  match Opt.to_option (Dom_html.CoerceTo.button el) with
  | Some s -> s
  | None -> failwith "coerco_button failed"

let coerceTo_textarea el =
  match Opt.to_option (Dom_html.CoerceTo.textarea el) with
  | Some s -> s
  | None -> failwith "coerco_textarea failed"






(* Utils for Ace Editor *)

let get_lines row_start row_end =
  let doc = (Global.editor())##getSession()##getDocument() in
  let res = to_array (str_array (doc##getLines(row_start, row_end))) in
  let res = List.fold_right (fun str acc -> (to_string str)::acc)
    (Array.to_list res) [] in
  String.concat "\n" res



(* Utils for Dom's events (not referenced) *)

type handler = (element Js.t, event Js.t) event_listener

let make_event_oncontextmenu el handler =
  ignore (addEventListener el (Event.make "contextmenu") handler _true)

let make_event_onblur el handler =
  ignore (addEventListener el (Event.make "blur") handler _true)

