
//Provides: stdout_buf
stdout = "";

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) { return x; }

//Provides: caml_ml_output
//Requires: stdout_buf
function caml_ml_output (x, s, p, l) {
    // Redirection des canaux stdout et stderr
    if ( (x == 1 || x == 2) ) {
        if ( s instanceof MlString ) stdout += s.toString().slice(p, p+l);
        else  stdout += s.slice(p, p+l);
    }
    return 0;
}

