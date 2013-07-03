
open Dom_html

type handler = (Dom_html.element Js.t, Dom_html.event Js.t)
           Dom_html.event_listener

let _editor = ref (Obj.magic 5) (* O_O WHAT???? *)

let init_editor el = _editor := Ace.edit el
let editor () = !_editor

(* class tabs_widget = object (self) *)
(*   inherit Dom_html.element *)

(*   val id = id *)
(*   val titles = tl *)
(*   val contents = cl *)
(*   val styles = createStyle document *)
(*   val mutable style_tabs = "border-bottom: 1px solid black;" *)
(*   val mutable style_contents = "border: 1px solid black; *)
(*    border-top: none;" *)
(*   val mutable style_tab_active = "border: 1px solid black; *)
(*    border-bottom: 1px solid white;" *)
(*   val mutable style_tab_noactive = "border: 1px solid black;" *)
(*   val mutable current_tab = 0 *)
  
(*   method init () = *)
(*     let div_titles = create Div in *)
(*     div_titles##className <- Js.string (id^"_class_tabs"); *)
(*     Dom.appendChild self style; *)
(*     Dom.appendChild self div_titles; *)
(*     ignore (List.fold_left2 (fun num element title  -> *)
(*       let div_tab = createDiv document in *)
(*       let span_title = createSpan document in *)
(*       span_title##className <- Js.string (id^"_class_noactive"); *)
(*       div_tab##style##display <- Js.string "none"; *)
(*       div_tab##className <- Js.string (id^"_class_content"); *)
(*       span_title##innerHTML <- Js.string title; *)
(*       span_title##onclick <- handler (fun _ -> self#set_current_tab num; *)
(* 	Js._true); *)
(*       Dom.appendChild div_tab element; *)
(*       Dom.appendChild div_titles span_title; *)
(*       Dom.appendChild self div_tab; *)
(*       num+1 *)
(*     ) 0 element_list title_list ) *)

(*   method get_tab i = List.nth titles i *)
  
(*   method set_current_tab i =  *)
(*     let old_tab = List.nth titles current_tab in *)
(*     let old_content = List.nth contents current_tab in *)
(*     old_tab##className <- Js.string (id^"_class_noactive"); *)
(*     old_content##style##display <- Js.string "none"; *)
(*     let new_tab = List.nth titles i in *)
(*     let new_content = List.nth contents i in *)
(*     new_tab##className <- Js.string (id^"_class_active"); *)
(*     new_content##style##display <- Js.string ""; *)
(*     current_tab <- i *)

(*   method update_styles () = *)
(*     let s = Format.sprintf ".%s_class_tab_active { %s } *)
(*      .%s_class_tab_noactive { %s } *)
(*      .%s_class_tabs { %s } *)
(*      .%s_class_contents { %s }" *)
(*       id style_active id style_tab_noactive *)
(*       id style_tabs id style_tab_contents in *)
(*     styles##innerHTML <- Js.string s *)

(*   method set_tabs_style s = style_tabs <- s; self#update_styles () *)
(*   method set_contents_style s = style_contents <- s; self#update_styles () *)
(*   method set_tab_active_style s = *)
(*     style_tab_active <- s; self#update_styles () *)
(*   method set_tab_noactive_style s = *)
(*     style_tab_noactive <- s; self#update_styles ()  *)

(* end *)

(* Bindings des fonctions JS utiles *)

let alert str =
  Dom_html.window##alert(Js.string str)

let console_log str =
  Firebug.console##log(Js.string str)

let console_debug o =
  Firebug.console##debug(o)

let get_element_by_id id =
  Js.Opt.get (document##getElementById (Js.string id))
    (fun () -> failwith ("fail get_element_by_id : "^id))

let coerceTo_input el =
  match Js.Opt.to_option (Dom_html.CoerceTo.input el) with
  | Some s -> s
  | None -> failwith "coerco_input failed"

let coerceTo_textarea el =
  match Js.Opt.to_option (Dom_html.CoerceTo.textarea el) with
  | Some s -> s
  | None -> failwith "coerco_textarea failed"
    

let split str del =
  let str = Js.string str in
  let astr = Js.str_array (str##split(Js.string del)) in
  let lstr = Array.to_list (Js.to_array astr) in
  List.rev (List.fold_left (fun acc el ->
    let new_el = Js.to_string el in
    new_el::acc) [] lstr)
  
   



(* Création de widgets spécifiques *)

let optionnal_widget_count = ref 0
let optionnal_widget element init_display =
  let id = 
    let i = Js.to_string element##id in
    if i = "" then
      (let i = Format.sprintf "optionnal_widget_%d"
	 !optionnal_widget_count in
       incr optionnal_widget_count;
       i)
    else i
  in
  element##id <- Js.string id;

  let div = createDiv document in
  let button = createButton document in
  let button_text, display_text =
    if init_display then "-", "" else "+", "none" in
  
  element##style##display <- Js.string display_text;
  button##style##cssFloat <- Js.string "right";
  button##style##fontSize <- Js.string "8px";
  button##style##width <- Js.string "15px";
  button##style##height <- Js.string "15px";
  button##innerHTML <- Js.string button_text;
  button##onclick <- handler (fun _ ->
    let t = Js.to_string element##style##display in
    if t = "" then 
      (element##style##display <- Js.string "none";
       button##innerHTML <- Js.string "+")
    else (element##style##display <- Js.string "";
       button##innerHTML <- Js.string "-");
    Js._true);
  Dom.appendChild div button;
  Dom.appendChild div element;
  div





let tabs_widget_count = ref 0
let tabs_default_style_active = 
  "border: 1px solid black;
   border-bottom: 1px solid white;"
let tabs_default_style_noactive =
  "border: 1px solid black;"
let tabs_default_style_tabs =
  "border-bottom: 1px solid black;"
let tabs_default_style_content =
  "border: 1px solid black;
   border-top: none;"
let tabs_widget title_list element_list init_num_tab
    ?(style_active=tabs_default_style_active)
    ?(style_noactive=tabs_default_style_noactive)
    ?(style_tabs=tabs_default_style_tabs)
    ?(style_content=tabs_default_style_content) ()
    =
  let style = createStyle document in
  let div = createDiv document in
  let div_titles = createDiv document in
  let id_tabs = Format.sprintf "tabs_widget_%d" !tabs_widget_count in
  let letnum = createInput ~_type:(Js.string "hidden") document in
  letnum##value <- Js.string "0";
  div_titles##className <- Js.string (id_tabs^"_class_tabs");

  let style_innerHTML = Format.sprintf
    ".%s_class_active { %s }
     .%s_class_noactive { %s }
     .%s_class_tabs { %s }
     .%s_class_content { %s }"
    id_tabs style_active id_tabs style_noactive
    id_tabs style_tabs id_tabs style_content in
  style##innerHTML <- Js.string style_innerHTML;

  Dom.appendChild div style;
  Dom.appendChild div letnum;
  Dom.appendChild div div_titles;
  
  ignore (List.fold_left2 (fun num element title ->
    let div_tab = createDiv document in
    let span_title = createSpan document in

    let id_tab = Format.sprintf "%s_tab_%d" id_tabs num in

    if num = init_num_tab then
      (letnum##value <- Js.string (string_of_int num);
       span_title##className <- Js.string (id_tabs^"_class_active");
       div_tab##style##display <- Js.string "")
    else
      (span_title##className <- Js.string (id_tabs^"_class_noactive");
       div_tab##style##display <- Js.string "none");

    div_tab##id <- Js.string (id_tab^"_content");
    div_tab##className <- Js.string (id_tabs^"_class_content");
    span_title##id <- Js.string (id_tab^"_title");
    span_title##innerHTML <- Js.string title;
    span_title##onclick <- handler (fun _ ->
      let oldnum_active = int_of_string (Js.to_string letnum##value) in
      let oldid_active = Format.sprintf "%s_tab_%d" id_tabs oldnum_active in
      let oldtab_active = get_element_by_id (oldid_active^"_title") in
      let oldcontent_active = get_element_by_id (oldid_active^"_content") in
      oldtab_active##className <- Js.string (id_tabs^"_class_noactive");
      oldcontent_active##style##display <- Js.string "none";
      span_title##className <- Js.string (id_tabs^"_class_active");
      div_tab##style##display <- Js.string "";

      letnum##value <- Js.string (string_of_int num);
      Js._true
    );

    Dom.appendChild div_tab element;
    Dom.appendChild div_titles span_title;
    Dom.appendChild div div_tab;
    num+1
  ) 0 element_list title_list );

  incr tabs_widget_count;
  div







(* Bindings des fonctions Ace *)

(* type editSession *)
(* type range *)
(* type acetoken *)

let enable_editor () =
  (!_editor)##setReadOnly(Js.bool false)
  (* Js.Unsafe.fun_call (Js.Unsafe.variable "editor.setReadOnly") *)
  (*   [| Js.Unsafe.inject Js._false |] *)

let disable_editor () =
  (!_editor)##selectAll();
  (!_editor)##removeLines();
  (!_editor)##setReadOnly(Js.bool true)
  (* ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "editor.selectAll") *)
  (*   [| Js.Unsafe.inject () |]); *)
  (* ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "editor.removeLines") *)
  (*   [| Js.Unsafe.inject () |]); *)
  (* Js.Unsafe.fun_call (Js.Unsafe.variable "editor.setReadOnly") *)
  (*   [| Js.Unsafe.inject Js._true |] *)

(* let create_edit_session (content: string) : editSession = *)
(*   let text = Js.Unsafe.inject (Js.string content) in *)
(*   let mode = Js.Unsafe.inject (Js.string "ace/mode/ocaml") in *)
(*   Js.Unsafe.fun_call (Js.Unsafe.variable "ace.createEditSession") *)
(*     [| text;mode |] *)

(* let change_edit_session (es : editSession) = *)
(*   ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "editor.setSession") *)
(* 	    [| Js.Unsafe.inject es |]) *)

(* let get_editor_value () = *)
(*   let res = Js.Unsafe.fun_call *)
(*     (Js.Unsafe.variable *)
(*        "editor.getSession().getDocument().getValue") *)
(*     [| Js.Unsafe.inject () |] in  *)
(*   Js.to_string res *)

(* let set_editor_value str = *)
(*   ignore (Js.Unsafe.fun_call *)
(* 	    (Js.Unsafe.variable *)
(* 	       "editor.getSession().getDocument().setValue") *)
(* 	    [| Js.Unsafe.inject str |]) *)

(* let get_editsession_content es = *)
(*   Js.to_string (Js.Unsafe.meth_call es "getDocument().getValue" *)
(*   		  [| Js.Unsafe.inject () |]) *)

(* let get_line (row: int) : string = *)
(*   Js.to_string (Js.Unsafe.fun_call *)
(* 		  (Js.Unsafe.variable *)
(* 		     "editor.getSession().getDocument().getLine") *)
(* 		  [| Js.Unsafe.inject row |]) *)

let get_lines row_start row_end =
  let res = Js.to_array (Js.str_array (
    (!_editor)##getSession()##getDocument()##getLines(row_start, row_end)))
  in
  let res = List.fold_right (fun str acc -> (Js.to_string str)::acc)
    (Array.to_list res) [] in
  String.concat "\n" res

(* let get_tab_size () = *)
(*   Js.Unsafe.fun_call *)
(*     (Js.Unsafe.variable "editor.getSession().getTabSize") [||] *)

(* let make_range startRow startColumn endRow endColumn : range = *)
(*   Js.Unsafe.fun_call *)
(*     (Js.Unsafe.variable "new Range") *)
(*     [| Js.Unsafe.inject startRow ; *)
(*        Js.Unsafe.inject startColumn ; *)
(*        Js.Unsafe.inject endRow ; *)
(*        Js.Unsafe.inject endColumn |] *)


(* let replace (range: range) (text: string) : unit = *)
(*   ignore (Js.Unsafe.fun_call *)
(* 	    (Js.Unsafe.variable  *)
(* 	       "editor.getSession().getDocument().replace") *)
(* 	    [| Js.Unsafe.inject range ; *)
(* 	       Js.Unsafe.inject (Js.string text) |]) *)


(* let get_selection_range () = *)
(*   Js.Unsafe.fun_call *)
(*     (Js.Unsafe.variable "editor.getSelectionRange") *)
(*     [||] *)


(* let get_text_range r = *)
(*   Js.to_string (Js.Unsafe.fun_call *)
(* 		  (Js.Unsafe.variable *)
(* 		     "editor.getSession().getDocument().getTextRange") *)
(* 		     [| Js.Unsafe.inject r |]) *)


(* let get_tokens row : acetoken array = *)
(*   Js.to_array (Js.Unsafe.fun_call *)
(* 		 (Js.Unsafe.variable *)
(* 		    "editor.getSession().getTokens") *)
(* 		 [| Js.Unsafe.inject row |]) *)

let make_event_oncontextmenu el handler =
  ignore (Dom_html.addEventListener el
	    (Dom_html.Event.make "contextmenu")
	    handler Js._true)

let make_event_onblur el handler =
  ignore (Dom_html.addEventListener el
	    (Dom_html.Event.make "blur")
	    handler Js._true)


(* module AceToken = struct *)
(*   type t = acetoken *)

(*   let get_value (token: t) : string =  *)
(*     Js.to_string (Js.Unsafe.get token "value") *)

(*   let get_type (token: t) : string = *)
(*     Js.to_string (Js.Unsafe.get token "type") *)
(* end *)
