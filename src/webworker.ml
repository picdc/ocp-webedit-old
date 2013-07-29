
class type event = object
  method data : Js.js_string Js.t Js.readonly_prop
end

class type webWorker = object
  method onmessage : (event Js.t -> unit) Js.prop
  method postMessage : Js.js_string Js.t -> unit Js.meth
  method terminate : unit Js.meth
end

let webWorker = Js.Unsafe.variable "Worker"

let onmessage f =
  (Js.Unsafe.coerce Dom_html.window)##onmessage <- Js.wrap_callback f

let postMessage msg =
  (Js.Unsafe.coerce Dom_html.window)##postMessage(Js.string msg)
