
open Myutils
open Dom_html


(* Widget contenant l'output du compilateur *)
let make_compilation_widget () =
  let div = createDiv document in
  let comp_out = createPre document in
  let byte_res = createButton document in
  div##id <- Js.string "compilation";
  comp_out##id <- Js.string "compilation_output";
  byte_res##id <- Js.string "bytecode";
  byte_res##innerHTML <- Js.string "Get compiled bytecode";
  byte_res##disabled <- Js._true;
  Dom.appendChild div comp_out;
  Dom.appendChild div byte_res;
  div


(* Widget contenant le top-level et l'output *)
let make_bottom_widget () =
  (* Création du widget pour le toplevel *)
  let toplevel = Mytoplevel.make_toplevel () in
  let output = Mytoplevel.make_output () in
  let compilation = make_compilation_widget () in
  let div = createDiv document in
  let div_titles = createDiv document in
  let curr_tab = ref "toplevel" in

  div##id <- Js.string "bottabs";
  div_titles##id <- Js.string "bottabs_titles";
  Dom.appendChild div div_titles;

  let title_toplevel = createDiv document in
  let title_output = createDiv document in
  let title_compilation = createDiv document in

  title_toplevel##innerHTML <- Js.string "Toplevel";
  title_toplevel##id <- Js.string "bottabs_toplevel_title";
  title_output##innerHTML <- Js.string "Output";
  title_output##id <- Js.string "bottabs_output_title";
  title_compilation##innerHTML <- Js.string "Compilation";
  title_compilation##id <- Js.string "bottabs_compilation_title";
  title_toplevel##className <- Js.string "bottabs_tab bottabs_tab_active";
  title_output##className <- Js.string "bottabs_tab bottabs_tab_noactive";
  title_compilation##className <- Js.string "bottabs_tab bottabs_tab_noactive";
  toplevel##className <- Js.string "bottabs_content";
  toplevel##style##display <- Js.string "";
  output##style##display <- Js.string "none";
  output##className <- Js.string "bottabs_content";
  compilation##className <- Js.string "bottabs_content";
  compilation##style##display <- Js.string "none";

  title_toplevel##onclick <- handler (fun _ ->
    let id_old_tab = Format.sprintf "bottabs_%s_title" !curr_tab in
    let old_tab = get_element_by_id id_old_tab in
    let old_content = get_element_by_id !curr_tab in
    old_tab##className <- Js.string "bottabs_tab bottabs_tab_noactive";
    title_toplevel##className <- Js.string "bottabs_tab bottabs_tab_active";
    old_content##style##display <- Js.string "none";
    toplevel##style##display <- Js.string "";
    curr_tab := "toplevel";
    Js._true);
  title_output##onclick <- handler (fun _ ->
    let id_old_tab = Format.sprintf "bottabs_%s_title" !curr_tab in
    let old_tab = get_element_by_id id_old_tab in
    let old_content = get_element_by_id !curr_tab in
    old_tab##className <- Js.string "bottabs_tab bottabs_tab_noactive";
    title_output##className <- Js.string "bottabs_tab bottabs_tab_active";
    old_content##style##display <- Js.string "none";
    output##style##display <- Js.string "";
    curr_tab := "output";
    Js._true);
  let switch_to_compilation_tab () =
    let id_old_tab = Format.sprintf "bottabs_%s_title" !curr_tab in
    let old_tab = get_element_by_id id_old_tab in
    let old_content = get_element_by_id !curr_tab in
    old_tab##className <- Js.string "bottabs_tab bottabs_tab_noactive";
    title_compilation##className <- Js.string "bottabs_tab bottabs_tab_active";
    old_content##style##display <- Js.string "none";
    compilation##style##display <- Js.string "";
    curr_tab := "compilation"
  in
  title_compilation##onclick <- handler (fun _ ->
    switch_to_compilation_tab ();
    Js._true);

  (* Ajout de l'event pour la compilation *)
  Eventmanager.compile#add_event (fun result ->
    switch_to_compilation_tab ();
    let container = query_selector compilation "#compilation_output" in
    let button = coerceTo_button
      (query_selector compilation "#bytecode") in
    let stdout, errors = 
      Errors_lexer.parse_compile_output result.Mycompile.stdout in
    container##innerHTML <- Js.string stdout;

    let project = result.Mycompile.initial_proj in
    Errors_report.add_errors_reports project errors;
    
    if result.Mycompile.code = 0 then begin
      let blob = string_to_blob result.Mycompile.bytecode in
      button##onclick <- handler (fun _ ->
        ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "saveAs")
                  [| Js.Unsafe.inject blob;
                     Js.Unsafe.inject (Js.string result.Mycompile.exec)|]);
        Js._true);
      button##disabled <- Js._false end
    else button##disabled <- Js._true);

  Dom.appendChild div_titles title_toplevel;
  Dom.appendChild div_titles title_output;
  Dom.appendChild div_titles title_compilation;
  Dom.appendChild div toplevel;
  Dom.appendChild div output;
  Dom.appendChild div compilation;
  div



(* <div> contenant l'éditeur et tous les autres widgets
   Il est gardé en mémoire ici afin de ne pas créer de doublons dans
   le DOM si on ferme et ré-ouvre le workspace *)
let main_content =
  let doc = document in
  let div_global = createDiv doc in
  let div_main = createDiv doc in
  let div_editor = createDiv doc in
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


let main_main () =
  (* Conteneur principal qui accueillera nos wigdets *)
  let main_container = createDiv document in
  main_container##id <- Js.string "main_content";
  Global.(global_conf.container <- main_content);
  Dom.appendChild document##body main_container ;

  Ace.require("Range");

  document##body##onclick <- handler (fun _ ->
    Dialog.Right_clic_dialog.hide_all ();
    Js._true);

  let editor = query_selector Global.(global_conf.container) "#editor"  in
  Global.init_editor editor;
  (Js.Unsafe.coerce window)##editor <- (Global.editor ());


  (* Fonction callback pour le lancement du workspace *)
  let launch _ =
    Dom.appendChild main_container Global.(global_conf.container)
  in

  (* Fonction callback pour la fermeture du workspace *)
  let close () =
    let cl = Dom.list_of_nodeList main_container##childNodes in
    List.iter (fun c -> Dom.removeChild main_container c) cl
  in

  Eventmanager.open_workspace#add_event launch;
  Eventmanager.close_workspace#add_event close




(* Point d'entrée du programme *)

let _ =
  Login.main ();
  Indent.main ();
  Tabs.main ();
  Sidepanel.main ();
  main_main ()
