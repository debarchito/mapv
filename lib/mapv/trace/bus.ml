type t = {
  mutable next_seq : int;
  mutable next_id : int;
  mutable subs : (int * (Event.t -> unit)) list;
}

type subscription = { bus : t; id : int }

let create () = { next_seq = 0; next_id = 0; subs = [] }

let subscribe bus f =
  let id = bus.next_id in
  bus.next_id <- id + 1;
  bus.subs <- (id, f) :: bus.subs;
  { bus; id }

let unsubscribe { bus; id } =
  bus.subs <- List.filter (fun (i, _) -> i <> id) bus.subs

let has_subscribers bus = bus.subs <> []

let publish bus kind =
  if bus.subs <> [] then begin
    let seq = bus.next_seq in
    bus.next_seq <- seq + 1;
    let ev = { Event.seq; kind } in
    List.iter (fun (_, f) -> f ev) bus.subs
  end
