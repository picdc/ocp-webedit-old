
//Provides: caml_output_val
//Requires: caml_marshal_constants, caml_int64_to_bytes, caml_failwith
var caml_output_val = function (){
  function Writer () { this.chunk = []; }
  Writer.prototype = {
    chunk_idx:20, block_len:0, obj_counter:0, size_32:0, size_64:0,
    write:function (size, value) {
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    write_code:function (size, code, value) {
      this.chunk[this.chunk_idx++] = code;
      for (var i = size - 8;i >= 0;i -= 8)
        this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
    },
    finalize:function () {
      this.block_len = this.chunk_idx - 20;
      this.chunk_idx = 0;
      this.write (32, 0x8495A6BE);
      this.write (32, this.block_len);
      this.write (32, this.obj_counter);
      this.write (32, this.size_32);
      this.write (32, this.size_64);
      return this.chunk;
    }
  }
  return function (v) {
    var writer = new Writer ();
    var stack = [];
    function extern_rec (v) {
      var cst = caml_marshal_constants;
      if (v instanceof Array && v[0] === (v[0]|0)) {
        if (v[0] == 255) {
          // Int64
          writer.write (8, cst.CODE_CUSTOM);
          for (var i = 0; i < 3; i++) writer.write (8, "_j\0".charCodeAt(i));
          var b = caml_int64_to_bytes (v);
          for (var i = 0; i < 8; i++) writer.write (8, b[i]);
          writer.size_32 += 4;
          writer.size_64 += 3;
          return;
        }
        if (v[0] < 16 && v.length - 1 < 8)
          writer.write (8, cst.PREFIX_SMALL_BLOCK + v[0] + ((v.length - 1)<<4));
        else
          writer.write_code(32, cst.CODE_BLOCK32, (v.length << 10) | v[0]);
        writer.size_32 += v.length;
        writer.size_64 += v.length;
        if (v.length > 1) stack.push (v, 1);
      } else if (v instanceof MlString) {
        var len = v.getLen();
        if (len < 0x20)
          writer.write (8, cst.PREFIX_SMALL_STRING + len);
        else if (len < 0x100)
          writer.write_code (8, cst.CODE_STRING8, len);
        else
          writer.write_code (32, cst.CODE_STRING32, len);
        for (var i = 0;i < len;i++) writer.write (8, v.get(i));
        writer.size_32 += 1 + (((len + 4) / 4)|0);
        writer.size_64 += 1 + (((len + 8) / 8)|0);
      } else {
        // if (v != (v|0)) caml_failwith("output_value: non-serializable value");
        if (v >= 0 && v < 0x40) {
          writer.write (8, cst.PREFIX_SMALL_INT + v);
        } else {
          if (v >= -(1 << 7) && v < (1 << 7))
            writer.write_code(8, cst.CODE_INT8, v);
          else if (v >= -(1 << 15) && v < (1 << 15))
            writer.write_code(16, cst.CODE_INT16, v);
          else
            writer.write_code(32, cst.CODE_INT32, v);
        }
      }
    }
    extern_rec (v);
    while (stack.length > 0) {
      var i = stack.pop ();
      var v = stack.pop ();
      if (i + 1 < v.length) stack.push (v, i + 1);
      extern_rec (v[i]);
    }
    writer.finalize ();
    return writer.chunk;
  }
} ();





//Provides: caml_global_filesystem
var caml_global_filesystem = [0];
caml_global_filesystem["std_exit.ml"] = new MlString("let _ = do_at_exit()");


function list_mem(l, o) {
    if (l[0]) { return false; }
    else if (l[1] == o) { return true; }
    else if (l[2] == 0) { return false; }
    else { return list_mem(l[2], o); }
} 


function add_to_output(x, s, p, l) {
    // var s_add = s.bytes.slice(p, l);
    // x = new MlString(x.bytes+s_add);
    // x.bytes = s.bytes;
    // x.fullBytes = s.fullBytes;
    // x.len = s.len;
    // x.last = s.last;
    // x.offset = s.offset;
    var s = new MlString(s.bytes.slice(p, l));
    var xl = x.getLen();
    var sl = s.getLen();
    var str = caml_create_string(xl+sl|0);
    caml_blit_string(x,0,str,0,xl);
    caml_blit_string(s,0,str,xl,sl);

    x.bytes = str.bytes;
    x.fullBytes = str.fullBytes;
    x.len = str.len;
    x.last = str.last;
    x.offset = str.offset;


    // DEBUG
    var div = document.getElementById("toto.cmi");
    if ( caml_global_filesystem["toto.cmi"] ) {
	var el = document.createElement("li");
	el.innerHTML = caml_global_filesystem["toto.cmi"].bytes;
	div.appendChild(el);
    }
    var div = document.getElementById("toto.cmo");
    if ( caml_global_filesystem["toto.cmo"] ) {
	var el = document.createElement("li");
	el.innerHTML = caml_global_filesystem["toto.cmo"].bytes;
	div.appendChild(el);
    }

    return 0;
}

//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data, caml_global_filesystem
function caml_sys_open (x, y) {
    console.log("##### caml_sys_open #####"); 
    console.debug(x);

    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[x];

    if (v) {
	var s = new MlString (v);
	s.offset = 0;
	console.debug(s);
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
    console.debug(x);
    return x;
}

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
    console.log("##### caml_ml_output #####");
    console.debug(x);

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
	console.log("x = ");
	console.debug(x);
	console.log("s = ");
	console.debug(s);

	add_to_output(x, s, p, l);;
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
    console.debug(i);
    var s;
    if (i == undefined) return 0;// s = new MlString(String.fromCharCode(0));
    
    else s = new MlString(i+"");
    return add_to_output(x, s, 0, 1);
}

//Provides: caml_output_value
//Requires: caml_ml_output, caml_output_value_to_string
function caml_output_value (x, v, fl) {
    console.log("##### caml_output_value #####");
    console.debug(x);

    console.debug(v);
    var s = new MlStringFromArray(caml_output_val(v));
    var str = new MlString(s.getBytes());
    console.debug(str);

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
    console.debug(x);
    console.debug(i);
    x.offset = i;
    return 0;
}

//Provides: caml_ml_seek_out
function caml_ml_seek_out(x, i) {
    console.log("##### caml_ml_seek_out #####");
    console.debug(x);
    console.debug(i);
    x.offset = i;
    return 0;
}

//Provides: caml_md5_chan
function caml_md5_chan(x) {
    console.log("##### caml_md5_chan #####");
    console.debug(x);
    return x;
}


//Provides: caml_ml_pos_out
function caml_ml_pos_out(x) {
    console.log("##### caml_ml_pos_out #####");
    return x.offset;
}

