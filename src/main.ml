
(** AMELIORER DISPLAY SIDEPANEL ET TESTER RENOMMAGE
-> doit changer les id apres un renommage !!! **)

open Ace_utils
open Indent

(* Widget contenant le top-level et l'output *)
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


(* <div> contenant l'éditeur et tous les autres widgets
   Il est gardé en mémoire ici afin de ne pas créer de doublons dans
   le DOM si on ferme et ré-ouvre le workspace *)
let main_content =
   let doc = Dom_html.document in
   let div_global = Dom_html.createDiv doc in
   let div_main = Dom_html.createDiv doc in
   let div_editor = Dom_html.createDiv doc in
   div_main##id <- Js.string "divmain";
   div_editor##id <- Js.string "editor";
   div_global##style##minWidth <- Js.string "750px";
   let sidepanel = Sidepanel.make_sidepanel () in
   let bottabs = make_bottom_widget () in
   let div_tabs = Tabs.make_tabs () in
   Dom.appendChild div_global sidepanel;
   Dom.appendChild div_main div_tabs;
   Dom.appendChild div_main div_editor;
   Dom.appendChild div_main bottabs;
   Dom.appendChild div_global div_main;
   div_global


(* Conteneur principal qui accueillera nos wigdets *)
let main_container = Dom_html.createDiv Dom_html.document
let _ = 
  main_container##id <- Js.string "main_content";
  global_conf.container <- main_content;
  Dom.appendChild Dom_html.document##body main_container 



let _ =
  Ace.require("Range");

  let doc = Dom_html.document in
  let css_tabs = Dom_html.createLink doc in
  let css_toplvl = Dom_html.createLink doc in
  let css_main = Dom_html.createLink doc in
  let css_sidepanel = Dom_html.createLink doc in
  let css_rel = Js.string "stylesheet" in
  let css_type = Js.string "text/css" in
  css_tabs##href <- Js.string "./css/tabs.css";
  css_toplvl##href <- Js.string "./css/mytoplevel.css";
  css_main##href <- Js.string "./css/main.css";
  css_sidepanel##href <- Js.string "./css/sidepanel.css";
  css_tabs##rel <- css_rel;
  css_tabs##_type <- css_type;
  css_toplvl##rel <- css_rel;
  css_toplvl##_type <- css_type;
  css_main##rel <- css_rel;
  css_main##_type <- css_type;
  css_sidepanel##rel <- css_rel;
  css_sidepanel##_type <- css_type;
  Dom.appendChild doc##body css_tabs;
  Dom.appendChild doc##body css_toplvl;
  Dom.appendChild doc##body css_main;
  Dom.appendChild doc##body css_sidepanel;

  doc##body##onclick <- Dom_html.handler (fun _ ->
    Dialog.Right_clic_dialog.hide_all ();
    Js._true);

  let editor = query_selector global_conf.container "#editor"  in
  init_editor editor;
  (Js.Unsafe.coerce Dom_html.window)##editor <- (Ace_utils.editor ());


  (* Fonction callback pour le lancement du workspace *)
  let launch _ =
    Dom.appendChild main_container global_conf.container;
    disable_editor ()
  in

  (* Fonction callback pour la fermeture du workspace *)
  let close () =
    let cl = Dom.list_of_nodeList main_container##childNodes in
    List.iter (fun c -> Dom.removeChild main_container c) cl
  in
  
  Event_manager.open_workspace#add_event launch;
  Event_manager.close_workspace#add_event close
