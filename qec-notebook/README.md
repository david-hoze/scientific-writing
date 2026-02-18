# qec-notebook

Interactive research notebook for the [qec-cat](../qec-cat/) quantum
error correction library. Write Haskell expressions in browser cells,
evaluate them against a persistent GHCi session, and see type-driven
visualizations: code parameters as summary cards, simulation results
as threshold plots, resource estimates as comparison tables.

## Prerequisites

- GHC 9.6+ and Cabal 3.0+
- The `qec-cat` library (sibling directory, referenced via `cabal.project`)

```
cabal update
cabal build qec-cat
```

## Quick start

```
cabal run qec-notebook
```

Wait for `GHCi ready. Open http://localhost:8170 in your browser.`
(first launch compiles `qec-cat` with `-fobject-code -O1`, which
takes 1--2 minutes).

Open http://localhost:8170 and try these cells:

```haskell
repetitionCode 5                -- CSS code card with distance
defaultCatParams                -- cat qubit physics card
catNoise defaultCatParams       -- Pauli channel display
cssHZ (repetitionCode 3)        -- parity-check matrix grid
runSimulation defaultSimConfig (repetitionCode 3) 0.01 42  -- MC result

let qc = SimConfig 500 defaultBPConfig 1
sweep (sweepCodes "rep" repetitionCode [3,5]) (noiseRange 0.02 0.1 3) qc
                                -- threshold plot
```

### Keyboard shortcuts

| Key | Action |
|-----|--------|
| Shift+Enter | Run cell, move to next |
| Ctrl+Enter | Run cell, stay |
| Ctrl+Shift+N | New cell |
| Ctrl+Shift+D | Delete cell |
| Escape | Cancel evaluation |

## Testing

The project has a Haskell test suite (unit + integration) using
[tasty](https://hackage.haskell.org/package/tasty). No Node.js or
browser required.

```
cabal test notebook-tests
```

This starts a real GHCi session and WebSocket server on an ephemeral
port, so the first run takes ~5--10 seconds for GHCi startup. See
[docs/testing.md](docs/testing.md) for details.

To include the long-running sweep tests (~30 s):

```
cabal test notebook-tests --test-option='--long'
```

To run a single test:

```
cabal test notebook-tests --test-option='-p /eval_css_code/'
```

## Project structure

```
qec-notebook/
  qec-notebook.cabal      -- executable + test-suite stanzas
  server/
    Main.hs                -- HTTP/WebSocket entry point
    Protocol.hs            -- JSON message types (ClientMsg, ServerMsg)
    Eval.hs                -- cell classification, result message builders
    Render.hs              -- type-driven rendering via GHCi
    Session.hs             -- GHCi subprocess management
    Stream.hs              -- TVar accumulator for sweep progress
  static/
    index.html             -- notebook UI
    notebook.js            -- cell management, WebSocket client
    renderers.js           -- type-specific visualization functions
    style.css              -- dark theme
  test/
    Main.hs                -- test runner
    Test/Unit/Eval.hs      -- classifyCell, message builders, cleanTypeStr
    Test/Unit/Protocol.hs  -- JSON codec round-trips
    Test/Integration/WebSocket.hs  -- full server end-to-end tests
  docs/
    qec-notebook-spec.md   -- design specification
    testing.md             -- test architecture and guide
    PROGRESS.md            -- implementation log
```

## Documentation

- [Design specification](docs/qec-notebook-spec.md) -- architecture,
  protocol, rendering pipeline
- [Testing guide](docs/testing.md) -- test suite structure, how to
  run, how to add tests
- [Progress log](docs/PROGRESS.md) -- implementation history,
  troubleshooting notes

## Configuration

| Flag | Default | Description |
|------|---------|-------------|
| `--port PORT` | 8170 | HTTP/WebSocket server port |
