
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
    // console.log("-------------------------caml_sys_open!--------------------------");
    // console.debug(x);
    // console.debug(caml_global_filesystem);
 
    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[x];
    console.log("f =");
    console.debug(f); 
    if (v) {
	var s = new MlString (v);
	s.offset = 0;
	return s;
    } else if (f != undefined) {
	return f;
    } else {
	if ( list_mem(y, 3) ) {
	    caml_global_filesystem[x] = new Array("");
	    return caml_global_filesystem[x];
	} else
	    caml_raise_sys_error (x + ": no such file or directory");
    }
}

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) {
    return x;
}

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
    // console.log("-------------------------caml_ml_output!-------------------------");
    // console.debug(x);
    if ( x == 1 ) { // stdout
	var o = document.getElementById("output");
	o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
    } else {
	// DUMMY !
	x[0] = x[0]+s.toJsString();
    }
    // console.debug(x);
    // console.log("-------------------------caml_ml_output! END---------------------");
    return 0;
}