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


//Provides: caml_marshal_data_size mutable
//Requires: caml_failwith
function caml_marshal_data_size (s, ofs) {
    function get32(s,i) {
        return (s.get(i) << 24) | (s.get(i + 1) << 16) |
            (s.get(i + 2) << 8) | s.get(i + 3);
    }
    // console.debug(s);
    // console.debug(s.get(ofs) << 24);
    // console.debug(get32(s, ofs));
    if (get32(s, ofs) != (0x8495A6BE|0))
        caml_failwith("Marshal.data_size: bad object");
    return (get32(s, ofs + 4));
}


//Provides: caml_input_value_from_string mutable
//Requires: caml_failwith, MlStringFromArray, MlString, caml_marshal_constants
//Requires: caml_int64_float_of_bits, caml_int64_of_bytes
var caml_input_value_from_string = function (){
    function ArrayReader (a, i) { this.a = a; this.i = i; }
    ArrayReader.prototype = {
        read8u:function () { return this.a[this.i++]; },
        read8s:function () { return this.a[this.i++] << 24 >> 24; },
        read16u:function () {
            var a = this.a, i = this.i;
            this.i = i + 2;
            return (a[i] << 8) | a[i + 1]
        },
        read16s:function () {
            var a = this.a, i = this.i;
            this.i = i + 2;
            return (a[i] << 24 >> 16) | a[i + 1];
        },
        read32u:function () {
            var a = this.a, i = this.i;
            this.i = i + 4;
            return ((a[i] << 24) | (a[i+1] << 16) | (a[i+2] << 8) | a[i+3]) >>> 0;
        },
        read32s:function () {
            var a = this.a, i = this.i;
            this.i = i + 4;
            return (a[i] << 24) | (a[i+1] << 16) | (a[i+2] << 8) | a[i+3];
        },
        // HACK XXX
        read64s:function () {
            var t = [];
            for (var j=0 ; j<8 ; j++ ) t[j] = this.read8u ();
            var res = caml_int64_of_bytes(t);
            console.debug("res de read64s" + res);
            return res;
        },
        // END HACK
        readstr:function (len) {
            var i = this.i;
            this.i = i + len;
            return new MlStringFromArray(this.a.slice(i, i + len));
        }
    }
    function StringReader (s, i) { this.s = s; this.i = i; }
    StringReader.prototype = {
        read8u:function () { return this.s.charCodeAt(this.i++); },
        read8s:function () { return this.s.charCodeAt(this.i++) << 24 >> 24; },
        read16u:function () {
            var s = this.s, i = this.i;
            this.i = i + 2;
            return (s.charCodeAt(i) << 8) | s.charCodeAt(i + 1)
        },
        read16s:function () {
            var s = this.s, i = this.i;
            this.i = i + 2;
            return (s.charCodeAt(i) << 24 >> 16) | s.charCodeAt(i + 1);
        },
        read32u:function () {
            var s = this.s, i = this.i;
            this.i = i + 4;
            return ((s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
                    (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3)) >>> 0;
        },
        read32s:function () {
            var s = this.s, i = this.i;
            this.i = i + 4;
            return (s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
                (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3);
        },
        readstr:function (len) {
            var i = this.i;
            this.i = i + len;
            return new MlString(this.s.substring(i, i + len));
        }
    }
    function caml_float_of_bytes (a) {
        return caml_int64_float_of_bits (caml_int64_of_bytes (a));
    }

    return function (s, ofs) {
       console.log("\n##### caml_input_value_from_string #####");
       console.debug("s =");
        if ( ofs == undefined ) ofs = 0;
        var reader = s.array?new ArrayReader (s.array, ofs):
            new StringReader (s.getFullBytes(), ofs);
        var magic = reader.read32u ();
        var block_len = reader.read32u ();
        var num_objects = reader.read32u ();
        var size_32 = reader.read32u ();
        var size_64 = reader.read32u ();
        var stack = [];
        var intern_obj_table = (num_objects > 0)?[]:null;
        var obj_counter = 0;
        function intern_rec () {
          //  console.log("start intern rec ");
            var cst = caml_marshal_constants;
            var code = reader.read8u ();
          //  console.log("after read8u");
            if (code >= cst.PREFIX_SMALL_INT) {
             //   console.log("prefix small int");
                if (code >= cst.PREFIX_SMALL_BLOCK) {
                //    console.log("prefix small block");
                    var tag = code & 0xF;
                    var size = (code >> 4) & 0x7;
                    var v = [tag];
                    if (size == 0) return v;
                    if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                    stack.push(v, size);
                    return v;
                } else {
                //    console.log("else prefix small block");
                    return (code & 0x3F);
                }
            } else {
              //  console.log("else prefix small int");
                if (code >= cst.PREFIX_SMALL_STRING) {
                    // console.log("prefix small string");
                    var len = code & 0x1F;
                    var v = reader.readstr (len);
                    if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                    return v;
                } else {
             //       console.log("else prefix small string");
                    if (code == undefined ) code = 0;
                    switch(code) {
                    case cst.CODE_INT8:
                       // console.log("CODE INT 8"); 
                        return reader.read8s ();
                    case cst.CODE_INT16://console.log("CODE INT 16"); 
                        return reader.read16s ();
                    case cst.CODE_INT32://console.log("CODE INT 32"); 
                        return reader.read32s ();
                    case cst.CODE_INT64://console.log("CODE INT 64"); 
                        // HACK XXX
                        var res = reader.read64s ();
                        //console.debug("pouet");
                        return res;
                        // END HACK
                        caml_failwith("input_value: integer too large");
                        break;
                    case cst.CODE_SHARED8://console.log("CODE SHARDE 8 "); 
                        var ofs = reader.read8u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_SHARED16://console.log("CODE SHARDE 16 "); 
                        var ofs = reader.read16u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_SHARED32://console.log("CODE SHARDE 32"); 
                        var ofs = reader.read32u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_BLOCK32:
                       // console.log("CODE BLOCK 32"); 
                        var header = reader.read32u ();
                        var tag = header & 0xFF;
                        var size = header >> 10;
                        var v = [tag];
                        if (size == 0) return v;
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        stack.push(v, size);
                        return v;
                    case cst.CODE_BLOCK64:
                        caml_failwith ("input_value: data block too large");
                        break;
                    case cst.CODE_STRING8:
                       // console.log("CODE STRING 8"); 
                        var len = reader.read8u();
                        var v = reader.readstr (len);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_STRING32:
                      //  console.log("CODE STRING 32"); 
                        var len = reader.read32u();
                        var v = reader.readstr (len);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_LITTLE://console.debug("CODE DOUBLE LITTLE");
                        var t = [];
                        for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
                        var v = caml_float_of_bytes (t);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_BIG://console.debug("CODE DOUBLE BIG");
                        var t = [];
                        for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
                        var v = caml_float_of_bytes (t);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_ARRAY8_LITTLE://console.debug("CODE DOUBLE ARRAY8 LITTLE");
                        var len = reader.read8u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
                            v[i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY8_BIG:// console.debug("CODE DOUBLE ARRAY8 BIG");
                        var len = reader.read8u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            v [i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY32_LITTLE: //console.debug("CODE DOUBLE ARRAY32 LITTLE");
                        var len = reader.read32u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
                            v[i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY32_BIG: console.debug("CODE DOUBLE ARRAY32 BIG");
                        var len = reader.read32u();
                        var v = [0];
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            v [i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_CODEPOINTER:
                    case cst.CODE_INFIXPOINTER:
                        caml_failwith ("input_value: code pointer");
                        break;
                    case cst.CODE_CUSTOM:
                       // console.debug("CODE CUSTOM");
                        var c, s = "";
                        while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
                        switch(s) {
                        case "_j":
                            // Int64
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            var v = caml_int64_of_bytes (t);
                            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                            return v;
                        case "_i":
                            // Int32
                            var v = reader.read32s ();
                            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                            return v;
                        case "_n":
                            switch ( reader.read8u() ) {
                            case 1: 
                                var v = reader.read32s ();
                                if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                                return v;
                            case 2:
                                caml_failwith("input_value: native integer value too large");
                            default:
                                caml_failwith("input_value: ill-formed native integer");
                            }
                        default:
                            caml_failwith("input_value: unknown custom block identifier");
                        }
                    default:
                        caml_failwith ("input_value: ill-formed message");
                    }
                }
            }
        }
       // console.debug("start");
        var res = intern_rec ();
       // console.debug("end");
        while (stack.length > 0) {
            var size = stack.pop();
            var v = stack.pop();
            var d = v.length;
            if (d < size) stack.push(v, size);
            v[d] = intern_rec ();
        }
        s.offset = reader.i;
        return res;
    }
}();


var obj_counter;
var extern_value_area = new Array();

function is_long(v) {
    return (!(v instanceof Array || v instanceof MlString));
}

function is_value_unseen(v) {
    // console.log("function : is_in_value start");
    cf = offset_of_value_seen(v);    
    // console.debug(cf);
    var res = ( cf == null);
    // console.log("function : is_in_value end");
    return res;
}

function offset_of_value_seen(v) {
    // console.log("extern_fc start");
    for (val in extern_value_area) {
        if (extern_value_area[val][1] === v) return extern_value_area[val][0];
    }

    return null;
}

function extern_record_location(v) {
    // console.log("extern_rl start");
    var i = obj_counter;
    extern_value_area.push([i, v]);
    obj_counter++;
    // console.log("extern_rl end");
    return 0;
}

//Provides: caml_output_val
//Requires: caml_marshal_constants, caml_int64_to_bytes, caml_failwith
var caml_output_val = function (){
    function Writer () { this.chunk = []; }
    Writer.prototype = {
	chunk_idx:20, block_len:0, size_32:0, size_64:0,
	write:function (size, value) {
	    //console.log("write (size = "+size+" ; value = "+value+") ; chuck = "+this.chunk_idx);
	    for (var i = size - 8;i >= 0;i -= 8)
		this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
	},
	write_code:function (size, code, value) {
	    console.log("write_code (size = "+size+") ; code = "+code+" ; value = "+value+" ! ; chuck = "+this.chunk_idx);
	    this.chunk[this.chunk_idx++] = code;
	    for (var i = size - 8;i >= 0;i -= 8)
		this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
	},
	finalize:function () {
	    this.block_len = this.chunk_idx - 20;
	    this.chunk_idx = 0;
	    this.write (32, 0x8495A6BE);
	    this.write (32, this.block_len);
	    this.write (32, obj_counter);
	    this.write (32, this.size_32);
	    this.write (32, this.size_64);
            console.debug("OUTPUT_VAL FINALIZE");
            console.debug(this.block_len);
            console.debug(obj_counter);
            console.debug(this.size_32);
            console.debug(this.size_64);
	    return this.chunk;
	}
    }
    return function (v) {
        console.log("\n##### caml_output_val #####");
	var writer = new Writer ();
        obj_counter = 0;
        extern_value_area = new Array();
	var stack = [];
	function extern_rec (v) {
            //console.debug("extern rec start : value = ");
	    var cst = caml_marshal_constants;
            //console.debug(v);

            if ( is_long(v) ) {
                //console.log("LONG");
                // Numeric value
		if (v >= 0 && v < 0x40)
		    writer.write (8, cst.PREFIX_SMALL_INT + v);
		else if (v >= -(1 << 7) && v < (1 << 7))
		    writer.write_code(8, cst.CODE_INT8, v);
		else if (v >= -(1 << 15) && v < (1 << 15))
		    writer.write_code(16, cst.CODE_INT16, v);
		else
		    writer.write_code(32, cst.CODE_INT32, v);
            } else if ( v instanceof Array && v[0] === (v[0]|0) && v[0] == 255) {
                // Int 64
                //console.log("INT64");
		writer.write (8, cst.CODE_CUSTOM);
		for (var i = 0; i < 3; i++) writer.write (8, "_j\0".charCodeAt(i));
		var b = caml_int64_to_bytes (v);
		for (var i = 0; i < 8; i++) writer.write (8, b[i]);
		writer.size_32 += 4;
		writer.size_64 += 3;
		extern_record_location(v);
                return;                
            } else if ( is_value_unseen(v) ) {
                //console.log("IS_IN_VALUE_AREA");
	        if (v instanceof Array && v[0] === (v[0]|0)) {
		    if (v[0] < 16 && v.length - 1 < 8) {
                        //    console.debug("case 2");
		        writer.write (8, cst.PREFIX_SMALL_BLOCK + v[0] + ((v.length - 1)<<4));
                    }
		    else {
                        //   console.debug("case 3");
		        writer.write_code(32, cst.CODE_BLOCK32, ((v.length - 1) << 10) | v[0]);
                    }
		    writer.size_32 += v.length;
		    writer.size_64 += v.length;
		    if (v.length > 1) stack.push (v, 1);
                    extern_record_location(v);
	        } else if (v instanceof MlString) {
	            //	console.debug("instanceof Mlstring");
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
                    extern_record_location(v);
	        }

            } else if ( (cf = offset_of_value_seen(v)) != null ) {
                //console.log("EXTERN_FIND_CODE");
                var d = obj_counter - cf;
                if ( d < 0x100 )
                    writer.write_code (8, cst.CODE_SHARED8, d);
                else if ( d < 0x10000 ) 
                    writer.write_code (16, cst.CODE_SHARED16, d);
                else writer.write_code (32, cst.CODE_SHARED32, d);
            } else
                caml_failwith("output_value: abstract value (outside heap)");
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


//Provides: caml_raise_sys_error
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
    console.log("\n##### caml_raise_sys_error #####");
    caml_raise_with_string(caml_global_data[2], msg);
}



function my_caml_blit_string(x, i, s, j, l) {
    var x_str = x.getBytes().slice(i, l);
    var s_str = s.getBytes();
    var sl = s_str.length;

    var str = s_str.substring(0, j) + x_str + s_str.substring(j+l, sl);
    var str = new MlString(str);
    s.bytes = str.bytes;
    s.fullBytes = str.bytes;
    s.last = str.last;
    s.offset = j+l;
    return 0;
}


//Provides: caml_ml_input
function caml_ml_input (f, s, i, l) {
    console.log("\n##### caml_ml_input #####");
    console.log("Lecture dans "+f.title+" (i="+i+" ; l="+l+")");
    var l2 = f.getLen() - f.offset;
    if (l2 < l) l = l2;

    var str = read_in_input(f, l);
    my_caml_blit_string(str, 0, s, i, l);
    console.log("Ce qui a ete lu :");
    console.debug(str);
    console.log("On retourne l="+l);

    return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string
function caml_input_value (s) {
    console.log("\n##### caml_input_value #####");
    console.log("Lecture dans : "+s.title);
    caml_marshal_data_size (s, s.offset);
    var res = caml_input_value_from_string(s, s.offset);
    console.log("Ce qui a ete lu : ");
    console.debug(res);
    return res;
}


//Provides: caml_get_global_data
//Requires: caml_global_data
function caml_get_global_data () { 
    console.log("\n##### caml_get_global_data #####");
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

function MlstringToBlob(str){
    var str = str.toArray();
    var a = new Uint8Array(str.length);

    for (k in str) 
        a[k] = str[k];

    return new Blob([a.buffer], {type: "application/octet-stream;charset=utf-8"})
}



//Provides: std_exit_cmo
var std_exit_cmo = "\\067\\097\\109\\108\\049\\057\\057\\057\\079\\048\\048\\055\\000\\000\\000\\048\\099\\000\\000\\000\\056\\000\\000\\000\\000\\000\\000\\000\\080\\000\\000\\000\\033\\000\\000\\000\\058\\000\\000\\000\\057\\000\\000\\000\\000\\000\\000\\000\\132\\149\\166\\190\\000\\000\\000\\100\\000\\000\\000\\018\\000\\000\\000\\070\\000\\000\\000\\063\\008\\000\\000\\036\\000\\040\\083\\116\\100\\095\\101\\120\\105\\116\\080\\096\\160\\160\\145\\176\\064\\042\\080\\101\\114\\118\\097\\115\\105\\118\\101\\115\\065\\072\\160\\160\\146\\176\\064\\004\\010\\065\\092\\064\\160\\160\\004\\012\\048\\014\\085\\111\\108\\229\\022\\016\\039\\239\\072\\087\\181\\144\\226\\173\\195\\160\\160\\042\\080\\101\\114\\118\\097\\115\\105\\118\\101\\115\\048\\072\\054\\194\\084\\240\\234\\202\\217\\047\\191\\103\\171\\197\\037\\253\\218\\064\\064\\064\\064\\064";

function read_cmo(str) {
    var res = "";
    for ( var i=0 ; i < str.length ; i+=4 )
	res += String.fromCharCode(str.substring(i+1, i+4));
    return new MlString(res);
}

// Provides: caml_global_filesystem
// Requires: stdlib_cma, std_exit_cmo
var caml_global_filesystem = [0];
caml_global_filesystem["std_exit.cmo"] = read_cmo(std_exit_cmo);
caml_global_filesystem["stdlib.cma"] = read_cmo(stdlib_cma);

function createBinaryString (nMask) {
    // nMask must be between -2147483648 and 2147483647
    for (var nFlag = 0, nShifted = nMask, sMask = ""; nFlag < 32; nFlag++, sMask += String(nShifted >>> 31), nShifted <<= 1);
    return sMask;
}

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

    if ( ! (x instanceof MlString) ) { 
	if ( x == 1 || x == 2 ) { // stdout
	    var o = document.getElementById("output");
	    o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
        }
        return 0;
    } 
    console.log("Ecriture dans : "+x.title);
    // mlstrdebug(x);
    //console.log("x = ");
    //mlstrdebug(x);
    //console.log("s = ");
    //mlstrdebug(s);
    //console.debug("p = "+p);
    //console.debug("l = "+l);

    var xoffset = x.offset;
    var x_str = x.getBytes();
    var s = s.getBytes().slice(p, l);
    // console.log("x_str="+x_str);
    // console.log("s="+s);

    var xl = x_str.length;
    var sl = s.length;
    var off = xoffset + sl;
    var len = (off>xl)?off:xl;;
    var str = x_str.substring(0, xoffset) + s + x_str.substring(off, len);
    // console.log("str = ");
    // console.debug(str);
    var str = new MlString(str);

    x.bytes = str.bytes;
    x.fullBytes = str.bytes;
    x.len = str.len;
    x.last = str.last;
    x.offset = off;
    // mlstrdebug(x);

    // console.log("\nResultat :");
    // mlstrdebug(x);

    return 0;
}

function read_in_input(x, len) {
    var ofs = x.offset;
    var str = new MlString(x.getBytes().slice(ofs, ofs+len));
    x.offset += len;
    return str;
}


//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data, caml_global_filesystem
function caml_sys_open (x, y) {
    console.log("\n##### caml_sys_open #####");
    var id = x.toJsString();
    console.log("Open de : "+id);
    
    if ( list_mem(y, 0) ) console.log("RDONLY");
    if ( list_mem(y, 1) ) console.log("WRONLY");

    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[id];

    if ( list_mem(y, 1) ) {
	if (f != undefined) {
	    if ( list_mem(y, 4) ) { // Open_trunc
		var s = new MlString("");
		s.offset = 0;
		s.title = id;
		caml_global_filesystem[id] = s;
	    }
	    // mlstrdebug(caml_global_filesystem[id]);
	    return caml_global_filesystem[id];
	} else if (v) {
	    var s = new MlString (v);
	    s.offset = 0;
	    s.title = id;
	    return s;
	} else {
	    if ( list_mem(y, 3) ) {
		// console.log("Create !")
		var s = new MlString("");
		caml_global_filesystem[id] = s;
		s.offset = 0;
		s.title = id
		// mlstrdebug(s);
		return s;
	    } else
		caml_raise_sys_error (x + ": no such file or directory");
	}
    } else if ( list_mem(y, 0) ) {
	if (f != undefined) {
	    // mlstrdebug(caml_global_filesystem[id]);
	    var copy = new MlString(caml_global_filesystem[id].getBytes());
	    copy.offset = 0;
            copy.title = id;
	    return copy;
	} else if (v) {
	    var s = new MlString (v);
	    s.offset = 0;
	    s.title = id;
	    return s;
	} else {
	    return new MlString("");
	}
    }
}

//Provides: caml_sys_file_exists
//Requires: caml_global_data, caml_global_filesystem
function caml_sys_file_exists (x) {
    console.log("\n##### caml_sys_file_exists #####");
    console.log("Existe-t-il "+x+" ?");
    var b = (caml_global_data.interfaces[x])?1:0;
    if ( !b ) {
	var s = x.toJsString();
	return (caml_global_filesystem[s])?1:0;
    } else return b;
}

//Provides: caml_ml_open_descriptor_in
function caml_ml_open_descriptor_in (x) {
    console.log("\n##### caml_ml_open_descriptor_in #####");
    return x;
}

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) {
    console.log("\n##### caml_ml_open_descriptor_out #####");
    return x;
}

//Provides: caml_ml_output
function caml_ml_output (x, s, p, l) {
    console.log("\n##### caml_ml_output #####");
    console.log("Ecriture de (p="+p+") (l="+l+"): ");
    console.debug(s);

    // if ( ! (x instanceof MlString) ) { 
    //     if ( x == 1 || x == 2 ) { // stdout
    //         var o = document.getElementById("output");
    //         o.appendChild (document.createTextNode(s.toString().slice(p,p+l)));
    //     }
    // } else {
	add_to_output(x, s, p, l);
    // }

    return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output
function caml_ml_output_char (x, c) {
    console.log("\n##### caml_ml_output_char #####");
    console.log("Ecriture de : "+String.fromCharCode(c)+" (c="+c+")");
    return add_to_output(x, new MlString(String.fromCharCode(c)), 0, 1);
}

function binaryToInt8(s) {
    var i = 0;
    for ( n=0 ; n<8 ; n++ )
	i += s[7-n] * (1 << n);
    return i;
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output
function caml_ml_output_int (x, i) {
    console.log("\n##### caml_ml_output_int #####");
    // console.log("Ecritude de : "+i);

    var res = "";
    var s = createBinaryString(i);
    for ( var n=0 ; n<32 ; n+=8 ) {
	var str = s.slice(n, n+8);
	res += String.fromCharCode(binaryToInt8(str));
    }

    var res = new MlString(res);
    return add_to_output(x, res, 0, res.getLen());
}

//Provides: caml_output_value
//Requires: caml_ml_output, caml_output_value_to_string
function caml_output_value (x, v, fl) {
    console.log("\n##### caml_output_value #####");
    // console.log("Ecriture de :");
    // console.debug(v);
    var str = caml_output_value_to_string(v);
    return add_to_output(x, str, 0, str.getLen());
}

//Provides: caml_sys_get_argv const
//Requires: MlString
function caml_sys_get_argv () {
    console.log("\n##### caml_sys_get_argv #####");
    // caml_failwith("caml_sys_get_argv : not implemented");
    return [0, 0, 0];
}

//Provides: caml_sys_remove
function caml_sys_remove() {
    console.log("\n##### caml_sys_remove #####");
    // caml_failwith("caml_sys_remove : not implemented");
    return 0;
}

//Provides: caml_sys_exit
function caml_sys_exit(x) {
    console.log("\n##### caml_sys_exit #####");
    return x;
}

//Provides: caml_ml_seek_in
function caml_ml_seek_in(x, i) {
    console.log("\n##### caml_ml_seek_in #####");
    // console.log("Seek dans "+x.title+" a "+i);
    x.offset = i;
    return 0;
}

//Provides: caml_ml_seek_out
function caml_ml_seek_out(x, i) {
    console.log("\n##### caml_ml_seek_out #####");
    // console.log("Seek dans "+x.title+" a "+i);
    x.offset = i;
    return 0;
}

//Provides: caml_md5_chan
function caml_md5_chan(x, l) {
    console.log("\n##### caml_md5_chan #####");
    // console.log("Md5 lancé sur :"+x.title+" (l="+l+") ; Résultat = ");
    var str = x.getBytes();
    var len = (l==-1)?str.length:l;
    var s = caml_md5_string(x, 0, len);
    console.debug(s);
    return s;
}


//Provides: caml_ml_pos_out
function caml_ml_pos_out(x) {
    console.log("\n##### caml_ml_pos_out #####");
    // console.log("Offset de "+x.title+" = "+x.offset);
    return x.offset;
}

//Provides: caml_ml_close_channel
function caml_ml_close_channel(x) {
    console.log("\n##### caml_ml_close_channel #####");
    console.log("Close : "+x.title);
    return 0;
}

//Provides: caml_sys_close
function caml_sys_close(x) {
    console.log("\n##### caml_sys_close #####");
    return 0;
}


//Provides: caml_ml_input_int
function caml_ml_input_int(x) {
    console.log("\n##### caml_ml_input_int #####");
    var a = read_in_input(x, 4).toArray();
    var i = (a[0] << 24) | (a[1] << 16) | (a[2] << 8) | a[3];
    // console.log("Lecture dans : "+x.title+" de :");
    // console.debug(i);
    return i;
}



//Provides: caml_sys_system_command
function caml_sys_system_command(str) {
    console.log("\n##### caml_sys_system_command #####");
    console.log("Command = "+str);
    caml_failwith("caml_sys_system_command : not implemented");
}


