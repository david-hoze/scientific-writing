# qec-notebook: Implementation Progress

## Status: Core Pipeline Working

The interactive notebook is functional with type-driven rendering.
Researchers can write Haskell expressions, evaluate them in a GHCi session,
and see structured visualizations in the browser.

## Architecture

```
Browser (JS)  <--WebSocket-->  Main.hs  <--stdin/stdout pipes-->  GHCi subprocess
   |                              |                                    |
   |  renders JSON as             |  classifies cell,                  |  evaluates expressions,
   |  cards/plots/tables          |  calls renderViaGHCi               |  calls renderToString
   |                              |                                    |  (from QEC.Notebook.Renderable)
```

### Key design decisions

- **Subprocess GHCi** (not GHC API): simpler, avoids linking GHC into the server.
  Communication via stdin/stdout pipes with unique sentinel markers to delimit responses.

- **`renderToString`** called inside GHCi: since we can't extract `Dynamic` values
  from a subprocess, we ask GHCi itself to call `renderToString ___qecIt`, which
  dispatches via the `Renderable` typeclass and returns JSON as a string.

- **`-fobject-code -O1`** flags on the GHCi subprocess: without these, GHCi
  interprets code via bytecode, which is 10-50x slower for computation-heavy
  functions like Monte Carlo simulation.

- **Sequential `sweep`**: replaced `parMap rdeepseq` with `map (force . runPoint)`
  because GHCi's bytecode interpreter doesn't benefit from sparks, and `parMap`
  caused massive memory accumulation.

- **Progress via stdout markers** (not TVar): since `sweep` runs inside a GHCi
  subprocess, we can't share TVars across process boundaries. Instead, the server
  detects `sweep` expressions, rewrites them to `sweepIO` (an IO variant that
  prints `___QEC_PROGRESS___ <completed> <total>` markers to stdout after each
  simulation point), and intercepts the markers to send WebSocket `progress`
  messages in real time.

## Confirmed Working Renderers

| Renderer | Expression | Time |
|---|---|---|
| `css_code` | `ldpcCatCode 0` | ~3s |
| `css_code` (small, with distance) | `repetitionCode 5` | ~30ms |
| `cat_qubit_params` | `defaultCatParams` | ~16ms |
| `pauli_channel` | `catNoise defaultCatParams` | ~29ms |
| `sim_result` | `runSimulation defaultSimConfig (repetitionCode 3) 0.01 42` | ~3s |
| `bin_matrix` | `cssHZ (repetitionCode 3)` | ~28ms |
| `threshold_plot` | `sweep (sweepCodes "rep" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) quickConfig` | ~787ms |
| `resource_estimate` | `estimateResources shorRSA2048 defaultCatParams RepetitionCat defaultFactory` | ~20ms |
| `resource_comparison` | `compareArchitectures shorRSA2048 defaultCatParams [RepetitionCat, SurfaceCode, LDPCCat]` | ~20ms |
| `code_construction` (valid) | `mkCSSCode (cssHX (repetitionCode 3)) (cssHZ (repetitionCode 3))` | ~40ms |
| `code_construction` (invalid) | `mkCSSCode (cssHX (repetitionCode 3)) (cssHZ (repetitionCode 5))` | ~20ms |
| fallback (plain text) | `42 :: Int` | ~39ms |
| error detection | `nonExistentFunction` | ~16ms |

## Not Yet Implemented

- Save/Load notebook (server stubs exist, no file I/O)
- Cancel running computation (stub exists)

## Recently Implemented

- **Progress streaming for sweep computations**: The server detects `sweep`
  expressions (prefix match), rewrites them to `sweepIO`, and uses
  `evalIOExprInSession` (monadic `<-` bind in GHCi, which executes the IO
  action). During execution, `sweepIO` prints `___QEC_PROGRESS___` markers
  to stdout. The server intercepts these via `collectUntilWithProgress` and
  sends WebSocket `progress` messages with `render_as: "sweep_progress"`,
  `data.completed`, and `data.total`. The frontend's existing `handleProgress`
  handler and `sweep_progress` renderer display the progress bar.

  Key files:
  - `qec-cat/src/QEC/Notebook.hs` — `sweepIO` function
  - `server/Session.hs` — `evalIOExprInSession`, `evalInSessionWithProgress`
  - `server/Main.hs` — sweep detection + progress callback wiring
  - `server/Protocol.hs` — `SM_progress` constructor

- **Reset session**: `CM_reset` now calls `resetSession` which runs `:load` in
  GHCi (clears all user bindings and loaded modules), then re-imports the
  notebook prelude. The `reset_clears_bindings` test now passes.

## Troubleshooting Log

### 1. GHCi `it` binding clobbered by sentinel

**Symptom:** Every expression returned type `()` and show text `()`.

**Cause:** `evalInSession` used `putStrLn sentinel` to mark the end of output,
which overwrites GHCi's `it` binding with `()`. Subsequent `:type it` returned `()`.

**Fix:** Changed to `evalExprInSession` which binds the expression result to
`___qecIt` via `let` before printing the sentinel, then queries `:type ___qecIt`.

### 2. `cssDistance` hanging on large codes

**Symptom:** `ldpcCatCode 0` (n=136) hung for minutes during `renderToString`.

**Cause:** `cssDistance` uses brute-force search that is exponential in code size.
The `Renderable CSSCode` instance called `cssDistance` unconditionally.

**Fix:** Made `cssDistance` conditional on `n <= 50`. Larger codes show `d = null`.
Also made the Tanner graph conditional on `n <= 200`.

### 3. `show` flooding the pipe for large values

**Symptom:** `ldpcCatCode 0` hung during rendering (after fixing `cssDistance`).

**Cause:** `renderViaGHCi` called `show ___qecIt` before `renderToString`.
For large `CSSCode` values, `show` dumps enormous binary matrix representations
through the pipe, taking minutes.

**Fix:** Moved `show` to fallback-only path (when `renderToString` fails), and
truncated to `take 2000 (show ___qecIt)`.

### 4. `parMap rdeepseq` causing memory exhaustion

**Symptom:** `sweep` with `defaultSimConfig` (10000 trials × 15 points) caused
the machine to run out of disk space via pagefile expansion (11.6 GB pagefile
on a drive with 323 MB free).

**Cause:** `parMap rdeepseq` in `sweep` created sparks for all simulation points
simultaneously. In GHCi's interpreted mode, sparks don't run truly parallel but
the thunks and intermediate data structures accumulate in memory.

**Fix:** Replaced `parMap rdeepseq` with `map (force . runPoint)` — sequential
evaluation with strict forcing after each point.

### 5. Windows UTF-8 encoding crash

**Symptom:** `sweep` completed but no result was returned. Server log showed:
```
*** Exception: <stdout>: hPutChar: invalid argument (cannot encode character '\226')
```

**Cause:** The GHCi subprocess stdout handle used Windows' default code page,
which cannot encode Unicode characters. The `sweepCodes` function generates
labels containing `ℓ` (Unicode ℓ = U+2113), which appears in the JSON output
from `renderToString`.

**Fix:** Set UTF-8 encoding on both sides:
- Server side: `hSetEncoding inh/outh/errh utf8` on the pipe handles
- GHCi side: `hSetEncoding stdout utf8` and `hSetEncoding stderr utf8` during
  session initialization

### 6. `defaultBPConfig` not in scope

**Symptom:** `let quickConfig = SimConfig 500 defaultBPConfig 1` silently failed.
The subsequent `sweep` call then reported `quickConfig` not in scope.

**Cause:** `defaultBPConfig` is exported by `QEC.Decoder.BP` but was not
re-exported through `QEC.Notebook`. Silent failure occurred because GHCi errors
go to stderr (which was being drained silently at the time).

**Fix:** Added `QEC.Decoder.BP` to the re-export list in `QEC.Notebook`.
Also added stderr logging (`[ghci:stderr]` prefix) to make future debugging easier.

### 7. Stale `___qecIt` binding on error

**Symptom:** When an expression failed (e.g., `simulate` instead of `runSimulation`),
the cell showed the previous cell's result instead of an error.

**Cause:** The `let ___qecIt = (expr)` binding failed silently (error on stderr),
leaving the previous value of `___qecIt` intact. The server then rendered the
stale value.

**Fix:** Reset `___qecIt` to `()` before each evaluation. If the type comes back
as `()`, report an error instead of rendering.

### 8. WebSocket compression mismatch (Node.js test only)

**Symptom:** The Node.js `ws` test client received the `ready` message but crashed
on subsequent messages with `RSV1 must be clear`.

**Cause:** The Haskell `websockets` library's default options enable per-message
deflate compression. The Node.js `ws` library didn't negotiate compression,
so compressed frames were rejected.

**Fix:** Set `WS.connectionCompressionOptions = WS.NoCompression` in the server.
Not needed for browser clients (which negotiate compression), but makes the
Node.js test client work reliably.

### 9. Cabal package index missing

**Symptom:** `cabal run` failed with `unknown package: vector`.

**Cause:** Fresh environment without a downloaded Hackage package index.

**Fix:** `cabal update` to download the index.

## Testing

### Haskell test suite (`cabal test`)

The project has a proper test suite in `test/` using the tasty framework.
See [testing.md](testing.md) for the full guide.

```
cabal test notebook-tests             # all tests (~4s after first build)
cabal test notebook-tests --test-option='--long'   # include sweep tests (~30s)
cabal test notebook-tests --test-option='-p /Unit/' # unit tests only
```

**66 tests total** (37 unit + 29 integration):

- **Unit tests** — pure functions: `classifyCell` (16 cases), message
  builders (6 cases), `cleanTypeStr` (5 cases), Protocol JSON codecs
  (10 cases). No GHCi, no network.
- **Integration tests** — full WebSocket server on an ephemeral port
  backed by a real GHCi session. Tests connection handling, every
  renderer type, declarations, error detection, state persistence,
  and session reset.

All 66 tests pass (37 unit + 29 integration).

### Legacy ad-hoc test (Node.js)

The old `test-ws.js` smoke test is superseded by the Haskell test suite
but still works for quick manual checks:

```
npm install ws
cabal run qec-notebook    # in one terminal
node test-ws.js           # in another
```

### Manual browser test

1. `cabal run qec-notebook` (wait for "GHCi ready" message)
2. Open http://localhost:8170
3. Try expressions one per cell:
   - `ldpcCatCode 0` — CSS code card with Tanner graph
   - `defaultCatParams` — cat qubit physics card
   - `catNoise defaultCatParams` — Pauli channel inline display
   - `cssHZ (repetitionCode 3)` — binary matrix grid
   - `repetitionCode 5` — small CSS code with distance
   - `runSimulation defaultSimConfig (repetitionCode 3) 0.01 42` — simulation result
   - `let quickConfig = SimConfig 500 defaultBPConfig 1` then
     `sweep (sweepCodes "rep" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) quickConfig`
     — threshold plot
   - `42 :: Int` — plain text fallback
