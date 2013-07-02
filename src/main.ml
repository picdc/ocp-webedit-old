
open Ace_utils
open Indent


let make_bottom_widget () =
  (* Création du widget pour le toplevel *)
  let toplevel = Mytoplevel.make_toplevel () in
  let output = Mytoplevel.make_output () in
  let div = Dom_html.createDiv Dom_html.document in
  let div_titles = Dom_html.createDiv Dom_html.document in
  let curr_tab = ref "toplevel" in

  div##id <- Js.string "bottabs";
  div_titles##id <- Js.string "bottabs_titles";
  Dom.appendChild div div_titles;

  let title_toplevel = Dom_html.createDiv Dom_html.document in
  let title_output = Dom_html.createDiv Dom_html.document in

  title_toplevel##innerHTML <- Js.string "Toplevel";
  title_toplevel##id <- Js.string "bottabs_toplevel_title";
  title_output##innerHTML <- Js.string "Output";
  title_output##id <- Js.string "bottabs_output_title";
  title_toplevel##className <- Js.string "bottabs_tab bottabs_tab_active";
  title_output##className <- Js.string "bottabs_tab bottabs_tab_noactive";
  toplevel##className <- Js.string "bottabs_content";
  toplevel##style##display <- Js.string "";
  output##style##display <- Js.string "none";
  output##className <- Js.string "bottabs_content";

  title_toplevel##onclick <- Dom_html.handler (fun _ ->
    let id_old_tab = Format.sprintf "bottabs_%s_title" !curr_tab in
    let old_tab = Ace_utils.get_element_by_id id_old_tab in
    let old_content = Ace_utils.get_element_by_id !curr_tab in
    old_tab##className <- Js.string "bottabs_tab bottabs_tab_noactive";
    title_toplevel##className <- Js.string "bottabs_tab bottabs_tab_active";
    old_content##style##display <- Js.string "none";
    toplevel##style##display <- Js.string "";
    curr_tab := "toplevel";
    Js._true);
  title_output##onclick <- Dom_html.handler (fun _ ->
    let id_old_tab = Format.sprintf "bottabs_%s_title" !curr_tab in
    let old_tab = Ace_utils.get_element_by_id id_old_tab in
    let old_content = Ace_utils.get_element_by_id !curr_tab in
    old_tab##className <- Js.string "bottabs_tab bottabs_tab_noactive";
    title_output##className <- Js.string "bottabs_tab bottabs_tab_active";
    old_content##style##display <- Js.string "none";
    output##style##display <- Js.string "";
    curr_tab := "output";
    Js._true);

  Dom.appendChild div_titles title_toplevel;
  Dom.appendChild div_titles title_output;
  Dom.appendChild div toplevel;
  Dom.appendChild div output;
  div


let make_editor (container: Dom_html.element Js.t) : unit =
  Ace.require "Range";

  let doc = Dom_html.document in
  let div_main = Dom_html.createDiv doc in
  let div_input = Dom_html.createDiv doc in
  let div_tabs = Dom_html.createDiv doc in
  let div_listtabs = Dom_html.createDiv doc in
  let div_editor = Dom_html.createDiv doc in
  let script_ace_init = Dom_html.createScript doc in
  let css_tabs = Dom_html.createLink doc in
  let css_toplvl = Dom_html.createLink doc in
  let css_main = Dom_html.createLink doc in
  let css_sidepanel = Dom_html.createLink doc in

  div_main##id <- Js.string "divmain";
  div_input##id <- Js.string "input";
  div_tabs##id <- Js.string "tabs";
  div_listtabs##id <- Js.string "listtabs";
  div_editor##id <- Js.string "editor";
  script_ace_init##text <- Js.string
    "var editor = ace.edit(\"editor\");
     editor.setTheme(\"ace/theme/eclipse\");
     editor.getSession().setMode(\"ace/mode/ocaml\");";
  css_tabs##href <- Js.string "./css/tabs.css";
  css_tabs##rel <- Js.string "stylesheet";
  css_tabs##_type <- Js.string "text/css";
  css_toplvl##href <- Js.string "./css/mytoplevel.css";
  css_toplvl##rel <- Js.string "stylesheet";
  css_toplvl##_type <- Js.string "text/css";
  css_main##href <- Js.string "./css/main.css";
  css_main##rel <- Js.string "stylesheet";
  css_main##_type <- Js.string "text/css";
  css_sidepanel##href <- Js.string "./css/sidepanel.css";
  css_sidepanel##rel <- Js.string "stylesheet";
  css_sidepanel##_type <- Js.string "text/css";
  container##style##minWidth <- Js.string "750px";

  let sidepanel = Sidepanel.make_sidepanel () in
  let bottabs = make_bottom_widget () in

  Dom.appendChild container div_input;
  Dom.appendChild container sidepanel;
  Dom.appendChild div_main div_tabs; (* A METTRE DANS TABS.init() ?? *)
  Dom.appendChild div_main div_listtabs;
  Dom.appendChild div_main div_editor;
  Dom.appendChild div_main bottabs;
  Dom.appendChild container div_main;
  Dom.appendChild doc##body script_ace_init;
  Dom.appendChild doc##body css_tabs;
  Dom.appendChild doc##body css_toplvl;
  Dom.appendChild doc##body css_main;
  Dom.appendChild doc##body css_sidepanel;

  doc##body##onclick <- Dom_html.handler (fun _ ->
    Dialog.Right_clic_dialog.hide_all ();
    Js._true);

  Ace_utils.init_editor "editor";
  Tabs.main ();
  disable_editor ()

  (* Modif des breakpoints à la saisie du texte pour l'indentation *)
  (* let f_bkpt_remove delta = *)
  (*   match delta.Ace.action with *)
  (*   | Ace.RemoveText | Ace.RemoveLines ->  *)
  (*     let range = delta.Ace.range in *)
  (*     let startrow, _ = Ace.Range.getStart range in *)
  (*     let endrow, _ = Ace.Range.getEnd range in *)
  (*     for i=startrow to endrow do *)
  (* 	Indent.remove_breakpoints i *)
  (*     done *)
  (*   | Ace.InsertLines | Ace.InsertText -> *)
  (*     let text = delta.Ace.text in *)
  (*     Ace_utils.console_log text *)
  (* in *)
  (* Ace.Editor.onChange (Ace_utils.editor ()) f_bkpt_remove *)


let _ =
  (Js.Unsafe.coerce Dom_html.window)##makeEditor <- Js.wrap_callback
     make_editor
