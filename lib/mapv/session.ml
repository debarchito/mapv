type mode =
  | Post_dump of { path : string }
  | Live of { window : int; bucket_size : int }

module type DRIVER = sig
  type t

  val step : t -> unit
  val is_done : t -> bool
  val get_status : t -> string
end

type t = { mode : mode; bus : Trace.Bus.t; recorder : Trace.Recorder.t option }

let create mode =
  let bus = Trace.Bus.create () in
  match mode with
  | Post_dump _ -> { mode; bus; recorder = Some (Trace.Recorder.attach bus) }
  | Live _ -> { mode; bus; recorder = None }

let bus t = t.bus

let run t (module VM : DRIVER) vm =
  match t.mode with
  | Post_dump { path } ->
      while not (VM.is_done vm) do
        VM.step vm
      done;
      (match t.recorder with
      | Some r -> Trace.Serializer.serialize_to_file path r
      | None -> ());
      Viz.Renderer.run_file path
  | Live { window; bucket_size } ->
      let model = Viz.Trace_model.create ~window ~bucket_size () in
      ignore (Trace.Bus.subscribe t.bus (Viz.Trace_model.ingest model));
      Viz.Renderer.run_live model
        ~step:(fun () -> VM.step vm)
        ~is_done:(fun () -> VM.is_done vm)
        ~status:(fun () -> VM.get_status vm)
