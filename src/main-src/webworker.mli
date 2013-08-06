
class type event = object
  method data : Js.js_string Js.t Js.readonly_prop
end

class type webWorker = object
  method onmessage : (event Js.t -> unit) Js.prop
  method postMessage : Js.js_string Js.t -> unit Js.meth
  method terminate : unit Js.meth
end

val webWorker : (Js.js_string Js.t -> webWorker Js.t) Js.constr

val onmessage : (event Js.t -> unit) -> unit
val postMessage : string -> unit
