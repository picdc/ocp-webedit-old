(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: main.ml 12511 2012-05-30 13:29:48Z lefessan $ *)

open Config
open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Misc.chop_extension_if_any oname

let process_interface_file ppf name =
  Compile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Compile.implementation ppf name opref;
  objfiles := (opref ^ ".cmo") :: !objfiles

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    let opref = output_prefix name in
    Compile.implementation ppf name opref;
    objfiles := (opref ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Compile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmo"
       || Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ext_dll then
    dllibs := name :: !dllibs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The OCaml compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous = process_file ppf;;
let impl = process_implementation_file ppf;;
let intf = process_interface_file ppf;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = ccopts := s :: !ccopts
  let _config = show_config
  let _custom = set custom_runtime
  let _dllib s = dllibs := Misc.rev_split_words s @ !dllibs
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _principal = set principal
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _strict_sequence = set strict_sequence
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unsafe = set fast
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v = print_version_and_library
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dparsetree = set dump_parsetree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous
end)

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None -> fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

(* let main () = *)
(*   let a = Sys.argv in *)
(*   Array.iter (fun s -> print_endline s) a; *)
  
(*   try *)
(*     Arg.parse Options.list anonymous usage; *)
(*     if *)
(*       List.length (List.filter (fun x -> !x) *)
(*                       [make_archive;make_package;compile_only;output_c_object]) *)
(*         > 1 *)
(*     then *)
(*       if !print_types then *)
(*         fatal "Option -i is incompatible with -pack, -a, -output-obj" *)
(*       else *)
(*         fatal "Please specify at most one of -pack, -a, -c, -output-obj"; *)
(*     if !make_archive then begin *)
(*       Compile.init_path(); *)

(*       Bytelibrarian.create_archive ppf  (List.rev !objfiles) *)
(*                                    (extract_output !output_name); *)
(*       Warnings.check_fatal (); *)
(*     end *)
(*     else if !make_package then begin *)
(*       Compile.init_path(); *)
(*       let extracted_output = extract_output !output_name in *)
(*       let revd = List.rev !objfiles in *)
(*       Bytepackager.package_files ppf revd (extracted_output); *)
(*       Warnings.check_fatal (); *)
(*     end *)
(*     else if not !compile_only && !objfiles <> [] then begin *)
(*       let target = *)
(*         if !output_c_object then *)
(*           let s = extract_output !output_name in *)
(*           if (Filename.check_suffix s Config.ext_obj *)
(*             || Filename.check_suffix s Config.ext_dll *)
(*             || Filename.check_suffix s ".c") *)
(*           then s *)
(*           else *)
(*             fatal *)
(*               (Printf.sprintf *)
(*                  "The extension of the output file must be .c, %s or %s" *)
(*                  Config.ext_obj Config.ext_dll *)
(*               ) *)
(*         else *)
(*           default_output !output_name *)
(*       in *)
(*       Compile.init_path(); *)
(*       Bytelink.link ppf (List.rev !objfiles) target; *)
(*       Warnings.check_fatal (); *)
(*     end; *)
(*     exit 0 *)
(*   with x -> *)
(*     Errors.report_error ppf x; *)
(*     exit 2 *)



(* TO BE DELETED *)
(* let dump content = *)
(*   let uriContent = *)
(*     Js.string ("data:application/octet-stream;charset=utf-8," ^ *)
(*            (Js.to_string  content)) in *)
(*   let _ = Dom_html.window##open_(uriContent, Js.string "Try OCaml", Js.null) in *)
(*   Dom_html.window##close () *)


let onmessage f =
  (Js.Unsafe.coerce Dom_html.window)##onmessage <- Js.wrap_callback f

let postMessage msg =
  (Js.Unsafe.coerce Dom_html.window)##postMessage(msg)


(* let my_compile name obj = *)
(*   Clflags.preprocessor := None; *)
(*   Clflags.dump_parsetree := false; *)
(*   Clflags.dump_rawlambda := false; *)
(*   Clflags.dump_lambda := false; *)
(*   Clflags.dump_instr := false; *)
(*   Clflags.custom_runtime := false; *)
(*   Clflags.no_std_include := true; *)

(*   objfiles := obj; *)
(*   process_file ppf name; *)
(*   Compile.init_path(); *)
(*   Bytelink.link ppf (List.rev !objfiles) "toto.byte"; *)
(*   Warnings.check_fatal () *)


(* MUST BE Mycompile.compile_options ; TO BE DELETED *)
type compile_options = {
  src : (string * string) list ;
  output : string
}

type compile_result = {
  stdout : string ;
  exec : string ;
  bytecode : string;
  code: int
}


let add_to_filemanager (name: string) (content: string) : unit =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "add_to_filemanager") 
            [| Js.Unsafe.inject (Js.string name) ;
               Js.Unsafe.inject content |])

let reset_filemanager () : unit =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.variable "reset_filemanager")  [| |])

let get_from_filemanager (name: string) : string =
  Js.Unsafe.fun_call (Js.Unsafe.variable "get_from_filemanager") 
            [| Js.Unsafe.inject (Js.string name) |]


let mycompile_init () =
  (Js.Unsafe.coerce Dom_html.window)##stdout <- Js.string "";
  reset_filemanager ();
  add_to_filemanager "camlheader" "";
  Clflags.preprocessor := None;
  Clflags.dump_parsetree := false;
  Clflags.dump_rawlambda := false;
  Clflags.dump_lambda := false;
  Clflags.dump_instr := false;
  Clflags.custom_runtime := false;
  Clflags.no_std_include := true;
  Bytelink.clear_crc_interfaces ();     (* A REMOVE ? *)
  Bytelink.clear_implementations_defines ();     (* A REMOVE ? *)
  objfiles := []

let objfiles_to_string () =
  match String.concat " " (List.rev !objfiles) with
    | "" -> ""
    | s -> s^" "

(* TO DO WITHOUT UNSAFE *)
let main data =
  let args = Json.unsafe_input data in
  mycompile_init ();
  let start_date = jsnew Js.date_now() in
  print_endline (Format.sprintf "Compilation started at %s@."
                   (Js.to_string start_date##toString()));

  let code =
    try
      List.iter (fun (name, content) ->
        add_to_filemanager name content;
        print_endline (Format.sprintf "ocamlc -c %s%s"
                         (objfiles_to_string()) name);
        process_file ppf name
      ) args.src;
      Compile.init_path();
      print_endline (Format.sprintf "ocamlc -o %s %s"
                       args.output (objfiles_to_string()));
      Bytelink.link ppf (List.rev !objfiles) args.output;
      Warnings.check_fatal ();

      let end_date = jsnew Js.date_now() in
      print_endline (Format.sprintf "@.Compilation <span style=\"color:green; font-weight:bold;\">finished</span> at %s@." 
                       (Js.to_string end_date##toString()));
      0
    with x ->
      Errors.report_error ppf x;
      let end_date = jsnew Js.date_now() in
      print_endline (Format.sprintf "@.Compilation <span style=\"color:red; font-weight:bold;\">exited abnormally</span> with code <span style=\"color:red; font-weight:bold;\">2</span> at %s"
                       (Js.to_string end_date##toString()));
      2 in
  let stdout = Js.to_string (Js.Unsafe.coerce Dom_html.window)##stdout in
  let bytecode = get_from_filemanager args.output in
  let result = { stdout ; exec = args.output; bytecode ; code } in
  let msg = Json.output result in
  postMessage(msg)


let _ =
  onmessage (fun ev -> main ev##data) 

(* TO BE DELETED *)
(* let _ = *)
(*   (Js.Unsafe.coerce Dom_html.window)##mycompile <- Js.wrap_callback *)
(*     my_compile; *)
(*   (Js.Unsafe.coerce Dom_html.window)##dump <- Js.wrap_callback dump *)
(* END TO BE DELETED *)

