type t = { mutable rev : Event.t list; mutable n : int }

let create () = { rev = []; n = 0 }

let record t (ev : Event.t) =
  t.rev <- ev :: t.rev;
  t.n <- t.n + 1

let attach bus =
  let t = create () in
  ignore (Bus.subscribe bus (record t));
  t

let count t = t.n
let stream t = List.rev t.rev

let clear t =
  t.rev <- [];
  t.n <- 0
