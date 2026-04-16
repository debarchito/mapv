open Core

type trigger = Minor | Major

module Make (H : Heap.S) = struct
  let promote h ~promoted_count ~promoted_addrs addr =
    let tag = H.get_tag h addr in
    if tag = Tag.forward then H.get_fwd h addr
    else begin
      let size = H.get_size h addr in
      let dst = H.alloc_old h ~size ~tag in
      for i = 0 to size - 1 do
        H.write h dst i (H.read h addr i)
      done;
      H.set_fwd h addr dst;
      incr promoted_count;
      Queue.push dst promoted_addrs;
      dst
    end

  let scan_object h ~promoted_count ~promoted_addrs worklist addr =
    let size = H.get_size h addr in
    for i = 0 to size - 1 do
      match H.read h addr i with
      | Value.Ptr child when H.is_young h child ->
          let already_forwarded = H.get_tag h child = Tag.forward in
          let new_child = promote h ~promoted_count ~promoted_addrs child in
          H.write h addr i (Value.Ptr new_child);
          if not already_forwarded then Queue.push new_child worklist
      | _ -> ()
    done

  let fix_roots h ~promoted_count ~promoted_addrs worklist
      (roots : Value.t array array) =
    Array.iter
      (fun root_set ->
        for i = 0 to Array.length root_set - 1 do
          match root_set.(i) with
          | Value.Ptr addr when H.is_young h addr ->
              let already_forwarded = H.get_tag h addr = Tag.forward in
              let new_addr = promote h ~promoted_count ~promoted_addrs addr in
              root_set.(i) <- Value.Ptr new_addr;
              if not already_forwarded then Queue.push new_addr worklist
          | _ -> ()
        done)
      roots

  let drain h ~promoted_count ~promoted_addrs worklist =
    while not (Queue.is_empty worklist) do
      scan_object h ~promoted_count ~promoted_addrs worklist
        (Queue.pop worklist)
    done

  let minor h (cfg : Config.Gc.t) ~roots =
    H.run_finalizers_young h;
    H.on_gc h Heap.Minor_start;
    let promoted_count = ref 0 in
    let promoted_addrs = Queue.create () in
    let worklist = Queue.create () in
    fix_roots h ~promoted_count ~promoted_addrs worklist roots;
    drain h ~promoted_count ~promoted_addrs worklist;
    let dirty = ref [] in
    H.iter_dirty_old_chunks h (fun ci _chunk -> dirty := ci :: !dirty);
    List.iter
      (fun ci ->
        H.iter_chunk_objects h ci (fun addr ->
            scan_object h ~promoted_count ~promoted_addrs worklist addr);
        H.clear_card h ci)
      !dirty;
    drain h ~promoted_count ~promoted_addrs worklist;
    let promoted = !promoted_count in
    Queue.iter (fun dst -> H.on_promote h dst) promoted_addrs;
    H.reset_young h;
    H.on_gc h (Heap.Minor_end { promoted });
    let s = H.stats h in
    let threshold = int_of_float (float_of_int s.old_total *. 0.5) in
    if s.old_used >= threshold then Some Major else None

  let mark h ~roots =
    let steps = ref 0 in
    let stack = Stack.create () in
    let push = function
      | Value.Ptr addr when H.is_old h addr && not (H.get_mark h addr) ->
          H.set_mark h addr true;
          Stack.push addr stack
      | _ -> ()
    in
    Array.iter (fun root_set -> Array.iter push root_set) roots;
    while not (Stack.is_empty stack) do
      let addr = Stack.pop stack in
      incr steps;
      let size = H.get_size h addr in
      for i = 0 to size - 1 do
        push (H.read h addr i)
      done
    done;
    !steps

  let sweep h =
    let steps = ref 0 in
    let freed = ref 0 in
    H.iter_old_chunks h (fun ci _chunk ->
        H.iter_chunk_objects h ci (fun addr ->
            incr steps;
            if H.get_mark h addr then H.set_mark h addr false
            else begin
              H.free_old h addr;
              incr freed
            end));
    (!steps, !freed)

  let major h (cfg : Config.Gc.t) ~roots =
    let mark_steps = mark h ~roots in
    H.on_gc h (Heap.Major_mark { steps = mark_steps });
    let sweep_steps, freed = sweep h in
    H.on_gc h (Heap.Major_sweep { steps = sweep_steps; freed });
    H.on_gc h Heap.Major_end

  let run h (cfg : Config.Gc.t) ~roots = function
    | Minor -> minor h cfg ~roots
    | Major ->
        major h cfg ~roots;
        None
end
