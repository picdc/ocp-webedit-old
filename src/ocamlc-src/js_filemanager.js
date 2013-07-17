// Js_of_ocaml toplevel runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2011 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


//Provides: caml_raise_sys_error
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
    console.log("##### caml_raise_sys_error #####");
    caml_raise_with_string(caml_global_data[2], msg);
}

// var caml_output_val = function (){
//   function Writer () { this.chunk = []; }
//   Writer.prototype = {
//     chunk_idx:20, block_len:0, obj_counter:0, size_32:0, size_64:0,
//     write:function (size, value) {
//       for (var i = size - 8;i >= 0;i -= 8)
//         this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
//     },
//     write_code:function (size, code, value) {
//       this.chunk[this.chunk_idx++] = code;
//       for (var i = size - 8;i >= 0;i -= 8)
//         this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
//     },
//     finalize:function () {
//       this.block_len = this.chunk_idx - 20;
//       this.chunk_idx = 0;
//       this.write (32, 0x8495A6BE);
//       this.write (32, this.block_len);
//       this.write (32, this.obj_counter);
//       this.write (32, this.size_32);
//       this.write (32, this.size_64);
//       return this.chunk;
//     }
//   }
//   return function (v) {
//     var writer = new Writer ();
//     var stack = [];
//     function extern_rec (v) {
//       var cst = caml_marshal_constants;
//       if (v instanceof Array && v[0] === (v[0]|0)) {
//         if (v[0] == 255) {
//           // Int64
//           writer.write (8, cst.CODE_CUSTOM);
//           for (var i = 0; i < 3; i++) writer.write (8, "_j\0".charCodeAt(i));
//           var b = caml_int64_to_bytes (v);
//           for (var i = 0; i < 8; i++) writer.write (8, b[i]);
//           writer.size_32 += 4;
//           writer.size_64 += 3;
//           return;
//         }
//         if (v[0] < 16 && v.length - 1 < 8)
//           writer.write (8, cst.PREFIX_SMALL_BLOCK + v[0] + ((v.length - 1)<<4));
//         else
//           writer.write_code(32, cst.CODE_BLOCK32, (v.length << 10) | v[0]);
//         writer.size_32 += v.length;
//         writer.size_64 += v.length;
//         if (v.length > 1) stack.push (v, 1);
//       } else if (v instanceof MlString) {
//         var len = v.getLen();
//         if (len < 0x20)
//           writer.write (8, cst.PREFIX_SMALL_STRING + len);
//         else if (len < 0x100)
//           writer.write_code (8, cst.CODE_STRING8, len);
//         else
//           writer.write_code (32, cst.CODE_STRING32, len);
//         for (var i = 0;i < len;i++) writer.write (8, v.get(i));
//         writer.size_32 += 1 + (((len + 4) / 4)|0);
//         writer.size_64 += 1 + (((len + 8) / 8)|0);
//       } else {
//         if (v != (v|0)) caml_failwith("output_value: non-serializable value");
//         if (v >= 0 && v < 0x40) {
//           writer.write (8, cst.PREFIX_SMALL_INT + v);
//         } else {
//           if (v >= -(1 << 7) && v < (1 << 7))
//             writer.write_code(8, cst.CODE_INT8, v);
//           else if (v >= -(1 << 15) && v < (1 << 15))
//             writer.write_code(16, cst.CODE_INT16, v);
//           else
//             writer.write_code(32, cst.CODE_INT32, v);
//         }
//       }
//     }
//     extern_rec (v);
//     while (stack.length > 0) {
//       var i = stack.pop ();
//       var v = stack.pop ();
//       if (i + 1 < v.length) stack.push (v, i + 1);
//       extern_rec (v[i]);
//     }
//     writer.finalize ();
//     return writer.chunk;
//   }
// } ();

//Provides: caml_ml_input
//Require: caml_blit_string
function caml_ml_input (f, s, i, l) {
    console.log("##### caml_ml_input #####");
    var l2 = f.getLen() - f.offset;
    if (l2 < l) l = l2;
    caml_blit_string(f, f.offset, s, i, l);
    f.offset += l;
    return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string
function caml_input_value (s) {
    console.log("##### caml_input_value #####");
    caml_marshal_data_size (s, s.offset);
    return caml_input_value_from_string(s, s.offset);
}

//Provides: caml_reify_bytecode
//Requires: caml_global_data
// function caml_reify_bytecode (code, sz) {
//     console.log("##### caml_reify_data #####");
//     return eval(caml_global_data.compile(code).toString());
// }

//Provides: caml_get_global_data
//Requires: caml_global_data
function caml_get_global_data () { 
    console.log("##### caml_get_global_data #####");
    return caml_global_data;
}



// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------
// ça c'est à nous
// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------
// -------------------------------------------------


//Provides: caml_global_filesystem
var caml_global_filesystem = [0];
caml_global_filesystem["std_exit.ml"] = new MlString("let _ = do_at_exit()");


function list_mem(l, o) {
    if (l[0]) { return false; }
    else if (l[1] == o) { return true; }
    else if (l[2] == 0) { return false; }
    else { return list_mem(l[2], o); }
} 

function mlstrdebug(s) {
    var a = s.toArray();

    var s = "";
    for (v in a) {
	var i = a[v];
	if (i >= 33 && i <= 126 ) {
	    s += String.fromCharCode(i);
	} else {
	    s += "\\";
	    if ( i < 10 ) s += "00";
	    else if ( i < 100 ) s += "0";
	    s += i;
	}
    }
    
    console.debug(s);
}


function add_to_output(x, s, p, l) {
    // var s_add = s.bytes.slice(p, l);
    // x = new MlString(x.bytes+s_add);
    // x.bytes = s.bytes;
    // x.fullBytes = s.fullBytes;
    // x.len = s.len;
    // x.last = s.last;
    // x.offset = s.offset;
    console.log("x = ");
    mlstrdebug(x);
    console.log("s = ");
    mlstrdebug(s);
    console.debug("p = "+p);
    console.debug("l = "+l);

    var s = new MlString(s.getBytes().slice(p, l));
    console.log("s apres slice = ");
    mlstrdebug(s);
    var xl = x.getLen();
    var sl = s.getLen();
    var str = caml_create_string(xl+sl|0);
    caml_blit_string(x,0,str,0,xl);
    caml_blit_string(s,0,str,xl,sl);

    x.bytes = str.bytes;
    x.fullBytes = str.bytes;
    x.len = str.len;
    x.last = str.last;
    x.offset = xl;


    console.log("\nResultat :");
    mlstrdebug(x);


    // DEBUG
    var div = document.getElementById("toto.cmi");
    if ( caml_global_filesystem["std_exit.cmi"] ) {
	// var el = document.createElement("li");
	div.innerHTML = caml_global_filesystem["std_exit.cmi"].bytes;
	// div.appendChild(el);
    }
    var div = document.getElementById("toto.cmo");
    if ( caml_global_filesystem["std_exit.cmo"] ) {
    	//var el = document.createElement("li");
    	div.innerHTML = caml_global_filesystem["std_exit.cmo"].bytes;
    	//div.appendChild(el);
    }

    return 0;
}


//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data, caml_global_filesystem
function caml_sys_open (x, y) {
    console.log("##### caml_sys_open #####");
    var id = x.toJsString();
    // console.debug(x);
    // console.debug(id);
    console.log("Open de : "+id);
    
    if ( list_mem(y, 0) ) console.log("RDONLY");
    if ( list_mem(y, 1) ) console.log("WRONLY");

    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[id];

    if (f != undefined) {
	if ( list_mem(y, 4) ) { // Open_trunc
	    var s = new MlString("");
	    s.offset = 0;
	    caml_global_filesystem[id] = s;
	}
	return caml_global_filesystem[id];
    } else if (v) {
	var s = new MlString (v);
	s.offset = 0;
	// console.debug(s);
	return s;
    } else {
	if ( list_mem(y, 3) ) {
	    // console.log("Create !")
	    var s = new MlString("");
	    caml_global_filesystem[id] = s;
	    // console.debug(s);
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
    // console.debug(x);
    return x;
}

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) {
    console.log("##### caml_ml_open_descriptor_out #####");
    // console.debug(x);
    return x;
}

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
    console.log("##### caml_ml_output #####");
    // console.debug(s);

    if ( ! (x instanceof MlString) ) { 
	if ( x == 1 ) { // stdout
	    // console.log(s.toString());
	    var o = document.getElementById("output");
	    o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
	} else if ( x == 2 ) {
	    // console.debug(s.toJsString());
	    var o = document.getElementById("output");
	    o.appendChild (document.createTextNode(s.toString().slice(p,p+l)))
	}
    } else {
	// DUMMY !
	// console.log("x = ");
	// console.debug(x);
	// console.log("s = ");
	// console.debug(s);

	add_to_output(x, s, p, l);
    }

    return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
function caml_ml_output_char (x, c) {
    console.log("##### caml_ml_output_char #####");
    return add_to_output(x, String.fromCharCode(c), 0, 1);
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
function caml_ml_output_int (x, i) {
    console.log("##### caml_ml_output_int #####");
    // console.debug(i);
    var s;
    if (i == undefined) return 0;// s = new MlString(String.fromCharCode(0));
    
    else s = new MlString(i+"");
    return add_to_output(x, s, 0, s.getLen());
}

//Provides: caml_output_value
//Requires: caml_ml_output, caml_output_value_to_string
function caml_output_value (x, v, fl) {
    console.log("##### caml_output_value #####");
    // console.debug(x);

    // console.debug(v);
    var s = new MlStringFromArray(caml_output_val(v));
    console.debug(s);
    var str = new MlString(s.getBytes());
    // console.debug(str);

    return add_to_output(x, str, 0, str.getLen());
}

//Provides: caml_sys_get_argv const
//Requires: MlString
function caml_sys_get_argv () {
    console.log("##### caml_sys_get_argv #####");
    var exec_name = new MlWrappedString("ocamlc");
    var arg1 = new MlWrappedString("toto456.ml");
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
    // console.debug(x);
    // console.debug(i);
    x.offset = i;
    return 0;
}

//Provides: caml_ml_seek_out
function caml_ml_seek_out(x, i) {
    console.log("##### caml_ml_seek_out #####");
    // console.debug(x);
    // console.debug(i);
    x.offset = i;
    return 0;
}

//Provides: caml_md5_chan
function caml_md5_chan(x, l) {
    console.log("##### caml_md5_chan #####");
    console.debug(x);
    console.debug(l);
    var str = x.getBytes();
    var len = str.length;
    console.debug(str);
    console.debug(len);
    var s = caml_md5_string(x, 0, l);

    // var test = new MlString("coucou");
    // var testres = caml_md5_string(test, 0, 6);
    // console.debug(testres);

    console.debug(s);
    return s;
}


//Provides: caml_ml_pos_out
function caml_ml_pos_out(x) {
    console.log("##### caml_ml_pos_out #####");
    return x.offset;
}

//Provides: caml_ml_close_channel
function caml_ml_close_channel(x) {
    console.log("##### caml_ml_pos_out #####");
    return 0;
}
