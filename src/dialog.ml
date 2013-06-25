
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

  let prompt title default f =
    let res = Dom_html.window##prompt(Js.string title, Js.string default) in
    try
      let res = Js.to_string res in
      f res
    with _ -> ()

end
