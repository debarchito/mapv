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

Once the development shell is active, the `fold` binary can either be built
using `dune` or `Nix`. `fold` is the playground that wires the Raylib visualizer
which gives us a low-level view into the working of the Mapv VM. You can read
the source [here](./bin/fold/main.ml);

```fish
# build and run
dune build --profile release
./_build/install/default/bin/fold
# or
nix build .#fold
./result/bin/fold

# build and run in one go
dune exec --profile release fold
# or
nix run .#fold
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

- **Post-dump** (`bin/fold`): run the VM to completion while a `Recorder`
  captures every event, serialize to `MAPVT` v3, then open the visualizer on the
  file.
- **Real-time** (`bin/live`): drive the VM step-by-step inside the render loop,
  so programs that never terminate can still be inspected as they run. The
  visualizer keeps a bounded ring window (default `200_000` events) and folds
  evicted events into coarsely aggregated archive buckets.

```fish
dune exec --profile release fold   # post-dump analyzing
dune exec --profile release live   # real-time analyzing
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

## 3. Formatting

```fish
nix fmt
# or
nix run .#fmt
# or
fd -e ml -e mli --exclude _build | xargs ocamlformat --inplace
```

## 4. Licensing

`mapv` is licensed under [GNU LGPLv3](/LICENSE-LGPLv3) only while `fold` is
licensed under [GNU GPLv3](/LICENSE-GPLv3) only.
