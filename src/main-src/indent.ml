
(**
   WARNING
   on garde les braekpoints des fichiers remove et close !
**)

(* Table pour stocker les breakpoints d'indentations en fonction
   des fichiers (identifiés par leur id du Filemanager) *)
let all_breakpoints = Hashtbl.create 19

(* Liste des breakpoints d'intentations du fichier courrant *)
let breakpoints = ref []

(* Lors d'un changement à la ligne [row], tous les breakpoints
   après cette ligne sont considérés comme invalides *)
let notify_change_on_row row =
  breakpoints := List.filter (fun i -> i < row) !breakpoints

(* Renvoie le breakpoint valide le plus proche de la ligne [row] *)
let get_best_breakpoint row =
  List.fold_left (fun acc l ->
    if l <= row && l > acc then l else acc) 0 !breakpoints

(* kind pour parser le code en mettant à jour les breakpoints.
   [acc] est la liste des entiers correspondants aux indentations des
   lignes du code en question, par ordre décroissant des index des
   lignes
   Si parmi le texte parcouru, on trouve de nouveaux breakpoints non
   référencés, on les ajoute *)
let kind_ext = IndentPrinter.Extended (fun block elt (line, acc) ->
  let line = match elt with
      IndentPrinter.Newline -> line + 1 | _ -> line in
  (* Mise à jour des breakpoints *)
  if IndentBlock.is_at_top block then
    (let bkpts = !breakpoints in
     if not (List.mem line bkpts) then
	 breakpoints := line::bkpts);
  (* Mise à jour de l'acc pour la liste des indentations *)
  match elt with
  | IndentPrinter.Newline -> line, (IndentBlock.indent block)::acc
  | _-> line, acc
)


(* Renvoie le texte minimum nécessaire à ocp-indent pour indenter
   correctement les lignes [rowstart] à [rowend] grâce aux breakpoints *)
let get_minimum_text rowstart rowend =
  let doc = (Global.editor())##getSession()##getDocument() in
  let start = get_best_breakpoint rowstart in
  let col = String.length (Js.to_string (doc##getLine(rowend))) in
  let range =  Ace.range start 0 rowend col in
  Js.to_string (doc##getTextRange(range))



(* Fonction principale d'appel à ocp-indent :
   [str] est une chaine de caractère qui correspond au code à indenter
   [offset] est là où se situe la 1ere ligne de [str] dans le code
      complet, nécessaire pour la mise à jour des breakpoints
   Renvoie la liste des indentations de chaque ligne avec en premier
   index l'indentation de la dernière ligne. *)
let call_ocp_indent str offset =
  let output = {
    IndentPrinter.
    debug = false;
    config = IndentConfig.default;
    in_lines = (fun _ -> true);
    adaptive = false;
    indent_empty = true;
    kind = kind_ext; } in
  let stream = Nstream.of_string str in
  snd (IndentPrinter.proceed output stream IndentBlock.empty (offset, [0]))


(* Récupère la taille de l'indentation d'une line,
   i.e le nombre d'espace qui compose l'indentation *)
let get_indent_size line =
  let size = String.length line in
  let tab_size = (Global.editor())##getSession()##getTabSize() in
  let rec aux i =
    if i >= size then i
    else
      (let c = String.get line i in
       if c = ' ' then aux (i+1)
       else if c = '\t' then aux (i+tab_size)
       else i) in
  aux 0


(* Récupère l'indentation que devrait avoir la ligne [row]
   de l'éditeur actuel *)
let get_indent_next_line row : Js.js_string Js.t =
  let text = get_minimum_text row row in
  let offset = get_best_breakpoint row in
  let res = List.hd (call_ocp_indent text offset) in
  Js.string (String.make res ' ')



(* Remplace l'indentation de la ligne [row] de l'éditeur actuel
   par [n] espaces *)
let replace_indent row n =
  let doc = (Global.editor())##getSession()##getDocument() in
  let size = get_indent_size (Js.to_string doc##getLine(row)) in
  let range = Ace.range row 0 row size in
  let new_indent = String.make n ' ' in
  doc##replace(range, Js.string new_indent)


(* Indente la ligne [row] de l'éditeur actuel *)
let indent_line row =
  let text = get_minimum_text row row in
  let offset = get_best_breakpoint row in
  let res = List.hd (call_ocp_indent text offset) in
  replace_indent row res
      
(* Indente les lignes depuis [row_start] à [row_end] de l'éditeur
   actuel *)
let indent_region row_start row_end  =
  if row_start = row_end then indent_line row_start
  else
    (let text = get_minimum_text row_start row_end in
     let offset = get_best_breakpoint row_start in
     let res = call_ocp_indent text offset in
     ignore (List.fold_left (fun row n ->
       if row >= row_start then replace_indent row n;
       row - 1) row_end res))



let main () =
  (* Gestions des evenements *)
  let callback_create_file file =
    let id = file.Filemanager.id in
    Hashtbl.add all_breakpoints id [] in
  let callback_open_file (file, content) =
    let id = file.Filemanager.id in
    Hashtbl.add all_breakpoints id [] in
  let callback_close_file file =
    Hashtbl.remove all_breakpoints (file.Filemanager.id) in
  let callback_switch_file (old_id, id) =
    begin
      match old_id with
      | None -> ()
      | Some old_id ->
	Hashtbl.replace all_breakpoints old_id !breakpoints
    end;
    try breakpoints := Hashtbl.find all_breakpoints id
    with _ -> failwith "Not_found in callback_switch_file in indent.ml"
  in
  Eventmanager.import_file#add_event callback_create_file;
  Eventmanager.switch_file#add_event callback_switch_file;
  Eventmanager.open_file#add_event callback_open_file;
  Eventmanager.create_file#add_event callback_create_file;
  Eventmanager.close_file#add_event callback_close_file;
  Eventmanager.go_to_next_error#add_event callback_open_file;

  (* Fonctions accessibles depuis le javascript *)
  (Js.Unsafe.coerce Dom_html.window)##getIndentLine <- Js.wrap_callback
    get_indent_next_line;
  (Js.Unsafe.coerce Dom_html.window)##indentLine <- Js.wrap_callback
    indent_line;
  (Js.Unsafe.coerce Dom_html.window)##indentRegion <- Js.wrap_callback
    indent_region;
  (Js.Unsafe.coerce Dom_html.window)##indentNotifyChange <-
    Js.wrap_callback notify_change_on_row


