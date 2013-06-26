(* XXX START HACK *)
let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

class type global_data = object
  method toc : (string * string) list Js.readonly_prop
  method compile : (string -> string) Js.writeonly_prop
end

external global_data : unit -> global_data Js.t = "caml_get_global_data"

let g = global_data ()

let _ =
  let toc = g##toc in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Buffer.contents b
  in
  g##compile <- compile (* XXX HACK! *)
(* XXX END HACK *)


open Dom_html

let textarea_line_size = 14

let new_prompt () : Dom_html.element Js.t =
  let el = createDiv document in
  el##className <- Js.string "toplvl_prompt";
  el##innerHTML <- Js.string "#";
  el

let print_input (str: string) =
  let output = Ace_utils.get_element_by_id "toplvl_area_output" in
  let div = createDiv document in
  let prompt = new_prompt () in
  let text = createDiv document in
  text##className <- Js.string "toplvl_input_text";
  text##innerHTML <- Js.string str;
  Dom.appendChild div prompt;
  Dom.appendChild div text;
  Dom.appendChild output div

let print_output (str: string) =
  let output = Ace_utils.get_element_by_id "toplvl_area_output" in
  let div = createDiv document in
  (* let regexp = jsnew Js.regExp_withFlags(Js.string "\\n", Js.string "g") in *)
  (* let js_str = (Js.string str)##replace(regexp, Js.string "<br />") in *)
  div##className <- Js.string "toplvl_output_text";
  div##innerHTML <- Js.string str;
  Dom.appendChild output div
  
exception End_of_input

let execute (str: string) =
  let ppf =
    let b = Buffer.create 80 in
    Format.make_formatter
      (fun s i l -> Buffer.add_substring b s i l)
      (fun () -> print_output (Buffer.contents b); Buffer.clear b)
  in
  let str = match str with
    | "" -> "" 
    | _ -> str ^ ";;" in
  Ace_utils.console_log str;
  let lb = Lexing.from_string str in
  try
    let phr = 
      try !Toploop.parse_toplevel_phrase lb
      with End_of_file -> raise End_of_input
    in
    Ace_utils.console_debug phr;
    ignore(Toploop.execute_phrase true ppf phr)
      (* (!Toploop.parse_use_file lb) *)
  with
    End_of_input -> ()
    | exn -> Errors.report_error ppf exn


let evaluate_input () =
  let input = Ace_utils.coerceTo_textarea
    (Ace_utils.get_element_by_id "toplvl_area_input_textarea") in
  let text = Js.to_string input##value in
  print_input text;
  execute text;
  input##value <- Js.string "";
  input##style##height <- Js.string
    (Format.sprintf "%dpx" textarea_line_size)

let evaluate_selection () =
  let doc = Ace.EditSession.getDocument
    (Ace.Editor.getSession (Ace_utils.editor ())) in
  let range = Ace.Editor.getSelectionRange (Ace_utils.editor ()) in
  let text = Ace.Document.getTextRange doc range in
  print_input text;
  execute text

let reset_toplevel () =
  let input = Ace_utils.coerceTo_textarea
    (Ace_utils.get_element_by_id "toplvl_area_input_textarea") in
  let output = Ace_utils.get_element_by_id "toplvl_area_output" in
  input##value <- Js.string "";
  input##style##height <- Js.string
    (Format.sprintf "%dpx" textarea_line_size);
  output##innerHTML <- Js.string "";
  Toploop.initialize_toplevel_env ()
  

let make_output () : Dom_html.element Js.t =
  let console_output = createPre document in
  console_output##id <- Js.string "output";
  console_output

let make_toplevel () : Dom_html.element Js.t =
  let toplevel = createDiv document in
  let toplvl_area = createDiv document in
  let toplvl_buttons = createDiv document in
  let area_output = createPre document in
  let area_input = createDiv document in
  let button_reset = createButton document in
  let button_eval = createButton document in
  let button_eval_select = createButton document in
  let input_prompt = new_prompt () in
  let input_textarea = createTextarea document in


  toplevel##id <- Js.string "toplevel";
  toplvl_area##id <- Js.string "toplvl_area";
  toplvl_area##onclick <- handler (fun _ ->
    input_textarea##focus(); Js._true);
  toplvl_buttons##id <- Js.string "toplvl_buttons";
  area_output##id <- Js.string "toplvl_area_output";
  area_input##id <- Js.string "toplvl_area_input";
  button_eval##id <- Js.string "toplvl_button_eval";
  button_eval_select##id <- Js.string "toplvl_button_eval_select";
  button_reset##id <- Js.string "toplvl_button_reset";
  input_textarea##id <- Js.string "toplvl_area_input_textarea";
  input_textarea##className <- Js.string "toplvl_input_text";
  input_textarea##style##height <-
    Js.string (Format.sprintf "%dpx" textarea_line_size);
  input_textarea##onkeypress <- handler (fun kev ->
    if kev##keyCode == 13 then
      (let hpx = Js.to_string (input_textarea##style##height) in
       let h = String.sub hpx 0 (String.length hpx - 2) in
       let new_h = (int_of_string h) + textarea_line_size in
       let new_hpx = Format.sprintf "%dpx" new_h in
       input_textarea##style##height <- Js.string new_hpx);
    Js._true);

  button_eval##innerHTML <- Js.string "Evaluate";
  button_eval_select##innerHTML <- Js.string "Evaluate Selection";
  button_reset##innerHTML <- Js.string "Reset env.";
  (* button_eval_select##disabled <- Js._true; *)
  button_eval##onclick <- handler (fun _ ->
    evaluate_input (); Js._true);
  button_eval_select##onclick <- handler (fun _ ->
    evaluate_selection (); Js._true);
  button_reset##onclick <- handler (fun _ ->
    reset_toplevel (); Js._true);

  Dom.appendChild area_input input_prompt;
  Dom.appendChild area_input input_textarea;
  Dom.appendChild toplvl_area area_output;
  Dom.appendChild toplvl_area area_input;
  Dom.appendChild toplvl_buttons button_eval;
  Dom.appendChild toplvl_buttons button_eval_select;
  Dom.appendChild toplvl_buttons button_reset;
  Dom.appendChild toplevel toplvl_area;
  Dom.appendChild toplevel toplvl_buttons;

  Toploop.initialize_toplevel_env ();
  toplevel
