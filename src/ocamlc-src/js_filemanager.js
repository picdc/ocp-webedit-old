
//Provides: caml_global_filesystem
var caml_global_filesystem = [0];

function list_mem(l, o) {
    if (l[0]) { return false; }
    else if (l[1] == o) { return true; }
    else if (l[2] == 0) { return false; }
    else { return list_mem(l[2], o); }
} 


//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data, caml_global_filesystem
function caml_sys_open (x, y) {
    console.log("##### caml_sys_open #####"); 
   // console.log("-------------------------caml_sys_open!--------------------------");
    console.debug(x);
    // console.debug(caml_global_filesystem);
    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[x];
    // console.log("f =");
    // console.debug(f); 
    if (v) {
	var s = new MlString (v);
	s.offset = 0;
	return s;
    } else if (f != undefined) {
	return f;
    } else {
	if ( list_mem(y, 3) ) {
	    var s = new MlString ("");
	    caml_global_filesystem[x] = s;
	    s.offset = 0;
	    return s;
	} else
	    caml_raise_sys_error (x + ": no such file or directory");
    }
}

//Provides: caml_sys_file_exists
//Requires: caml_global_data, caml_global_filesystem
function caml_sys_file_exists (x) {
    console.log("##### caml_sys_file_exists #####");
    // console.debug(x);
    var b = (caml_global_data.interfaces[x])?1:0;
    if ( !b ) {
	var s = x.toJsString();
	// console.debug(s);
	return (caml_global_filesystem[s])?1:0;
    } else return b;
}

//Provides: caml_ml_open_descriptor_in
function caml_ml_open_descriptor_in (x) {
    console.log("##### caml_ml_open_descriptor_in #####");
    console.debug(x);
    return x;
}

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) {
    console.log("##### caml_ml_open_descriptor_out #####");
    return x;
}

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
    console.log("##### caml_ml_output #####");
    // console.debug(x);
    if ( x == 1 ) { // stdout
	// console.log(s.toString());
	var o = document.getElementById("output");
	o.appendChild (document.createTextNode(s.toString().slice(p,p+l)))
	;
    } else if ( x == 2 ) {
	// console.debug(s.toJsString());
	var o = document.getElementById("output");
	o.appendChild (document.createTextNode(s.toString().slice(p,p+l)))
    } else {
	// DUMMY !
	console.debug(x);
	console.debug(s);
	// x.string = "pouet";
	var s = new MlString(x.toJsString()+s.toJsString());
	x.string = s.string;
	x.bytes = s.bytes;
	x.fullBytes = s.fullBytes;
	x.len = s.len;
	x.last = s.last;
	x.offset = s.offset;
	console.debug(caml_global_filesystem["toto.cmo"]);
	console.debug(caml_global_filesystem["toto.cmi"]);
	// x[0] = new MlString(x[0].toJsString() + s.toJsString());
    }
    // console.debug(x);
    // console.log("-------------------------caml_ml_output! END---------------------");
    return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
function caml_ml_output_char (x, c) {
    console.log("##### caml_ml_output_char #####");
    return caml_ml_output (x, String.fromCharCode (c), 0, 1);
}

//Provides: caml_output_value
//Requires: caml_ml_output
function caml_output_value (x, v) {
    console.log("##### caml_output_value #####");
    console.debug(x);
    console.debug(v);
    return 0// caml_ml_output (x, v[1], 0, 1)
    ;
}

//Provides: caml_sys_get_argv const
//Requires: MlString
function caml_sys_get_argv () {
    console.log("##### caml_sys_get_argv #####");
    var exec_name = new MlWrappedString("ocamlc");
    var arg1 = new MlWrappedString("toto.ml");
    var argv = [0, exec_name, arg1];
    return [0, exec_name, argv];
}

//Provides: caml_sys_remove
function caml_sys_remove() {
    console.log("##### caml_sys_remove #####");
    // A AMELIORER OU PAS??
    return 0;
}

//Provides: caml_sys_exit
function caml_sys_exit(x) {
    console.log("##### caml_sys_exit #####");
    return x;
}

//Provides: caml_ml_seek_in
function caml_ml_seek_in(x, i) {
    console.log("##### caml_ml_seek_in #####");
    console.debug(x);
    console.debug(i);
    x.offset = i;
    return 0;
}

//Provides: caml_md5_chan
function caml_md5_chan(x) {
    console.log("##### caml_md5_chan #####");
    return x;
}


