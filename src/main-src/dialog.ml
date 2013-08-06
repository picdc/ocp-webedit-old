
open Dom_html

module Right_clic_dialog = struct

  type t = Dom_html.divElement Js.t
  type action = (Dom_html.element Js.t, Dom_html.mouseEvent Js.t)
           Dom_html.event_listener

  let elements = ref [] 

  let hide dialog =
    dialog##style##display <- Js.string "none"

  let create lstr lhandler =
    assert ((List.length lstr) = (List.length lhandler));
    let dialog = createDiv document in
    dialog##className <- Js.string "dialog_right_clic_dialog";
    dialog##style##display <- Js.string "none";
    dialog##style##position <- Js.string "absolute";
    List.iter2 (fun str handler ->
      let span = createSpan document in
      span##innerHTML <- Js.string str;
      span##onclick <- handler;
      span##className <- Js.string "dialog_right_clic_item";
      Dom.appendChild dialog span
    ) lstr lhandler;
    Dom.appendChild document##body dialog;
    elements := dialog::(!elements);
    dialog
   

  let hide_all () =
    List.iter (fun el -> hide el) !elements


  let show dialog x y =
    hide_all ();
    let l = Format.sprintf "%dpx" x in
    let t = Format.sprintf "%dpx" y in
    dialog##style##display <- Js.string "";
    dialog##style##left <- Js.string l;
    dialog##style##top <- Js.string t


end





module Prompt_dialog = struct

  let prompt ~title ~labels ~names ~defaults ~callback =
    assert ((List.length labels) = (List.length defaults));
    assert ((List.length labels) = (List.length names));
    let div = createDiv document in
    let container = createDiv document in
    let background = createDiv document in
    let c_title = createDiv document in
    let c_content = createTable document in
    let c_labels = Array.init (List.length labels)
      (fun _ -> createSpan document) in (* Don't use Array.make ! *)
    let c_inputs = Array.init (List.length defaults)
      (fun _ -> createInput document) in
    let b_cancel = createButton document in
    let b_submit = createButton document in

    div##className <- Js.string "dialog_prompt";
    container##className <- Js.string "dialog_prompt_container";
    background##className <- Js.string "dialog_prompt_background";
    c_title##className <- Js.string "dialog_prompt_title";
    c_title##innerHTML <- Js.string title;
    c_content##className <- Js.string "dialog_prompt_content";

    let hide () =
      Dom.removeChild document##body div;
      Dom.removeChild document##body background in
    let submit () = 
      let args = List.rev_map2 (fun n i -> (n, Js.to_string i##value))
        names (Array.to_list c_inputs) in
      hide ();
      callback args in

    b_submit##innerHTML <- Js.string "Submit";
    b_submit##className <- Js.string "dialog_prompt_button_submit";
    b_submit##onclick <- handler (fun _ -> submit(); Js._true);
    b_cancel##innerHTML <- Js.string "Cancel";
    b_cancel##className <- Js.string "dialog_prompt_button_cancel";
    b_cancel##onclick <- handler (fun _ -> hide(); Js._true);

    ignore (List.fold_left2 (fun i label default ->
      let c_tr = createTr document in
      let c_td1 = createTd document in
      let c_td2 = createTd document in

      c_labels.(i)##innerHTML <- Js.string label;
      c_labels.(i)##className <- Js.string "dialog_prompt_label";
      c_inputs.(i)##value <- Js.string default;
      c_inputs.(i)##className <- Js.string "dialog_prompt_input";

      Dom.appendChild c_td1 c_labels.(i);
      Dom.appendChild c_td2 c_inputs.(i);
      Dom.appendChild c_tr c_td1;
      Dom.appendChild c_tr c_td2;
      Dom.appendChild c_content c_tr;
      i+1
    ) 0 labels defaults);

    Dom.appendChild container c_title;
    Dom.appendChild container c_content;
    Dom.appendChild container b_cancel;
    Dom.appendChild container b_submit;
    Dom.appendChild div container;
    Dom.appendChild document##body background;
    Dom.appendChild document##body div


end
