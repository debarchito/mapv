> [!NOTE]
>
> While, Mapv served its purpose to get me the grades required to pass the minor
> internal, I plan to repurpose Mapv into a full reference implementation for
> writing register machines in OCaml. I've discovered a lot of bugs and
> soundness issues since then and I'll be fixing them in the upcoming days. The
> documentation will improve, and Fold will also be repurposed into a reference
> Lisp implementation running on top of Mapv. The single major change is moving
> from `Bytes` to `Bigarray` to back the memory primitives. This mean re-writing
> the entire memory management sections. It's important since `Bytes` is managed
> by the OCaml heap whereas `Bigarray` lives on the C heap; this reduce the
> friction between the Mapv GC and OCaml GC. The canonical source is ref `v2` at
> [https://git.sr.ht/~debarchito/mapv/tree/v2](https://git.sr.ht/~debarchito/mapv/tree/v2).

## 1. Development

The preferred way to develop `mapv` and `fold` is using [Nix](https://nixos.org)
and [direnv](https://direnv.net).

```fish
direnv allow
```

Once the development shell is active, the example binaries can either be built
using `dune` or `Nix`. They wire the Raylib visualizer which gives us a low-level
view into the working of the Mapv VM. There are two flavours: a post-dump
analyzer that serializes a whole run to disk, and real-time analyzers that drive
the VM step-by-step inside the render loop.

```fish
# build everything
dune build --profile release

# post-dump analyzing: builds garbage pressure, writes _gc_crush.mapvt, then
# opens the visualizer on the dump
./_build/install/default/bin/gc_pressure
dune exec --profile release gc_pressure

# real-time analyzing
./_build/install/default/bin/live_no_gc          # tiny, non-allocating loop
./_build/install/default/bin/live_gc             # allocating, GC-driven workload
./_build/install/default/bin/live_gc_pressure    # forced GC-pressure workload
dune exec --profile release live_no_gc
dune exec --profile release live_gc
dune exec --profile release live_gc_pressure
```

The `mapv` library is available as a package that can be used as follow:

```nix
# Add mapv to inputs
inputs.mapv.url = "git+https://codeberg.org/debarchito/mapv";

# Using the overlay
overlays = [
  inputs.mapv.overlays.default
];
# then
buildInputs = [
  pkgs.ocamlPackages.mapv
]

# or, inline it directly
buildInputs = [
  inputs.mapv.packages.${system}.mapv
                       # ^ don't forget to make sure "system" is defined! 
];
```

> [!NOTE]
>
> We do not use [opam](https://opam.ocaml.org). Instead, add the required
> dependencies to [dune-project](/dune-project) and invoke `dune build`. This
> will fail the first time but prepare [mapv.opam](/mapv.opam) and/or
> [fold.opam](/fold.opam) accordingly. Now, to install the dependencies, invoke
> `direnv reload` and they'll be made available for all subsequent `dune build`
> and `nix build`/`nix run` invocations.

## 2. Tracing & visualization

The tracer emits a single ordered event stream from both the VM and the heap
(`Mapv.Trace.Event`) over a synchronous pubsub bus (`Mapv.Trace.Bus`). A
`Mapv.Session` wires a program and a visualizer together; two modes are
supported:

- **Post-dump** (`bin/gc_pressure`): run the VM to completion while a `Recorder`
  captures every event, serialize to `MAPVT` v3, then open the visualizer on the
  file.
- **Real-time** (`bin/live_no_gc`, `bin/live_gc`, `bin/live_gc_pressure`): drive
  the VM step-by-step inside the render loop, so programs that never terminate
  can still be inspected as they run. The visualizer keeps a bounded ring window
  (default `200_000` events) and folds evicted events into coarsely aggregated
  archive buckets. `live_no_gc` is a minimal non-allocating loop, `live_gc`
  builds and drops short-lived lists so the minor/major collectors run
  continuously, and `live_gc_pressure` is the same forced GC-pressure workload
  as `gc_pressure` but streamed live instead of dumped.

```fish
dune exec --profile release gc_pressure        # post-dump analyzing
dune exec --profile release live_no_gc         # real-time, no allocation
dune exec --profile release live_gc            # real-time, GC pressure
dune exec --profile release live_gc_pressure   # real-time, forced GC pressure
```

Wiring a session manually:

```ocaml
let session = Session.create (Session.Live { window = 200_000; bucket_size = 1000 }) in
Heap.Tracing.set_bus heap_ctx (Session.bus session);
Vm.Tracing.set_bus   vm_ctx   (Session.bus session);
let vm = VM.create config entry heap_ctx vm_ctx in
Session.run session (module VM) vm
```

Tracing can be turned off entirely at zero cost by using the `No_tracing`
functors (`Heap.No_tracing`, `Vm.No_tracing`) and plain `VM.run`. Old `MAPVT`
v2 dumps are still readable.

## 3. Tracing overhead metrics

`bin/metrics` is a headless benchmark for the post-dump path. It never starts the
visualizer. It runs two bytecode workloads with tracing off, with tracing on
(`Tracing` + `Recorder` + `MAPVT` serialize), and with a counting tracer that
publishes nothing, then writes raw CSV dumps for external analysis:

- `no_gc`: a pure register loop (`AddI`, `SubI`, `Jnz`) that never touches the
  Mapv heap.
- `gc_pressure`: the same forced allocation pattern as `bin/gc_pressure`
  (25 × 1000 inner iterations, three allocations each, plus one written object
  per outer iteration), so it exercises minor GC, promotion, major GC, and
  write barriers.

```fish
dune build --profile release bin/metrics/main.exe
./_build/default/bin/metrics/main.exe
# or with knobs:
./_build/default/bin/metrics/main.exe \
  --out-dir metrics --reps 5 --warmup 1 \
  --no-gc-iters 250000 --outer 25 --inner 1000 --alloc-size 10 \
  --sample-rates 1,2,4,8 --keep-traces
```

Every measured run executes in a forked child so the parent's heap, the OCaml
GC, and the process RSS baseline are not polluted by earlier runs. The four
dumps are:

- `metrics_runs.csv`: one wide row per run. Timing (`setup_ns`, `exec_wall_ns`,
  serialization, total), memory (`rss_*`, OCaml `gc_*`, Mapv `mapv_*`), trace
  integrity (`seq_gaps`, `seq_duplicates`, `roundtrip_ok`), traced event counts
  (`ev_*`), and the matching ground-truth counts (`actual_*`) all live here.
- `metrics_fidelity.csv`: one row per workload × sample rate × event kind. For
  ordinary kinds `expected_ops == actual_ops`; for `read`/`write` the expected
  count is the exact number that the shared heap tick publishes at that sample
  rate. `capture_ratio = traced/actual`, `expected_ratio = expected/actual`,
  and `fidelity_ratio = traced/expected`.
- `metrics_actual.csv`: ground-truth operation counts from the counting tracer,
  plus the count run's execution time and peak memory.
- `metrics_summary.csv`: mean/median/std/min/max/n for every numeric run column,
  grouped by phase, workload, tracing mode, and sample rate.

`heap_*`, `gc_*`, and `rss_*` describe the harness process. `mapv_*` describes
the Mapv heap, while `ev_gc_*` and `actual_gc_*` describe the Mapv collector;
`mapv_gc_minor_ns` and `mapv_gc_major_ns` are the wall-clock time spent in
minor and major collections, and `mapv_gc_{minor,major}_count` are the number
of completed collections. GC timing uses the collector's event boundaries:
minor runs from `Minor_start` to `Minor_end`, and major runs from the preceding
`Minor_end` through `Major_end`, so major mark and sweep are included. The
`ev_*` counts come from the serialized trace,
and `actual_*` from a separate `Heap.No_tracing`/`Vm.No_tracing` run that still
invokes the tracer hooks. The `read`/`write` sampling is exact: a single tick is
shared by both, incrementing on every access, and an event is published when
`tick mod sample_rate == 0`. Thus `sample_rate` only attenuates reads and
writes, not instructions, allocations, or GC events.

## 4. Formatting

```fish
nix fmt
# or
nix run .#fmt
# or
fd -e ml -e mli --exclude _build | xargs ocamlformat --inplace
```

## 5. Licensing

`mapv` is licensed under [GNU LGPLv3](/LICENSE-LGPLv3) only while `fold` is
licensed under [GNU GPLv3](/LICENSE-GPLv3) only.
