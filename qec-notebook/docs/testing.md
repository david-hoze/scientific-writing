# qec-notebook Testing Guide

## Overview

The test suite lives in the `notebook-tests` test-suite stanza of
`qec-notebook.cabal`. It is split into two layers:

- **Unit tests** (37 tests) -- pure functions, no GHCi, no network.
  Run in milliseconds.
- **Integration tests** (25 tests) -- spin up a real Warp+WebSocket
  server backed by a GHCi subprocess, connect via WebSocket, exchange
  JSON messages, and assert on the responses.

All tests use the [tasty](https://hackage.haskell.org/package/tasty)
framework with `tasty-hunit` for assertions.


## Running the tests

### All tests

```
cabal test notebook-tests
```

First run builds `qec-cat` and starts GHCi (~5--10 s). Subsequent
runs reuse the build cache and take ~4 s.

### Pattern filter (single test or group)

```
# Single test
cabal test notebook-tests --test-option='-p /eval_css_code/'

# Entire group
cabal test notebook-tests --test-option='-p /classifyCell/'
cabal test notebook-tests --test-option='-p /Connection/'
```

### Long-running sweep tests

Sweep tests are behind a `--long` flag because they run Monte Carlo
simulations (~30 s):

```
cabal test notebook-tests --test-option='--long'
```

### Verbose output

```
cabal test notebook-tests --test-show-details=always
```


## Test structure

### `test/Main.hs`

Entry point. Assembles the unit and integration trees:

```haskell
main = do
  integ <- Test.Integration.WebSocket.tests
  defaultMain $ testGroup "qec-notebook"
    [ testGroup "Unit" [Test.Unit.Eval.tests, Test.Unit.Protocol.tests]
    , integ
    ]
```

Integration tests are built in `IO` because the server must start
before the test tree is constructed.

### `test/Test/Unit/Eval.hs`

Tests the pure functions from `Eval` and `Render`:

| Group | What it tests | Count |
|-------|---------------|-------|
| `classifyCell` | Cell kind detection (Declaration / Expression / GHCiCommand) for 16 inputs including edge cases like leading whitespace and pragmas | 16 |
| `message builders` | `buildResultMsg`, `buildDeclOkMsg`, `buildErrorMsg` -- field values and JSON round-trip through `encode`/`eitherDecode` | 6 |
| `cleanTypeStr` | Strips `___qecIt ::` prefix from GHCi `:type` output, handles spacing | 5 |

### `test/Test/Unit/Protocol.hs`

Tests JSON serialization for the WebSocket protocol types:

| Group | What it tests | Count |
|-------|---------------|-------|
| `ClientMsg FromJSON` | Parsing `eval`, `cancel`, `reset` messages from JSON | 3 |
| `ServerMsg ToJSON` | Encoding `ready`, `result` (all three statuses) to expected JSON structure | 5 |
| `malformed JSON` | Missing `type` field and unknown type both fail to parse | 2 |

### `test/Test/Integration/WebSocket.hs`

Full end-to-end tests. A single GHCi-backed server runs on an
ephemeral port for the entire integration group.

| Group | Tests | What it covers |
|-------|-------|----------------|
| Connection (4) | `ready_on_connect`, `malformed_json`, `unknown_message_type`, `concurrent_connects` | Server startup, resilience to bad input, multiple clients |
| Expression evaluation (9) | `eval_int_literal`, `eval_css_code`, `eval_cat_params`, `eval_pauli_channel`, `eval_bin_matrix`, `eval_sim_result`, `eval_string`, `elapsed_ms_present`, `haskell_type_present` | Every renderer type, timing, type annotation |
| Declarations (4) | `let_binding`, `let_then_use`, `import_module`, `ghci_command_set` | Bindings, imports, GHCi commands |
| Errors (4) | `undefined_variable`, `type_error`, `failed_eval_unit`, `empty_source` | Error detection and server resilience |
| Sequence & state (2) | `binding_persists`, `binding_chain` | GHCi state across evaluations |
| Reset (2) | `reset_clears_bindings` (XFAIL), `reset_reimports` | Session reset (not yet implemented) |
| Sweep (2, `--long`) | `sweep_produces_result`, `sweep_sends_progress` | Full simulation sweep and progress messages |


## Architecture

### Server lifecycle

```
tests :: IO TestTree
  1. openFreePort          -- bind port 0, get assigned port
  2. initSession           -- start GHCi subprocess (cabal exec ghci)
  3. async $ runSettingsSocket ... (testApp session)
  4. waitForServer port    -- poll until WebSocket handshake succeeds
  5. return TestTree with withResource for cleanup
```

Cleanup cancels the server thread, closes the GHCi session, and
releases the socket.

### Why sequential?

All integration tests run sequentially (`sequentialTestGroup`).
The GHCi session uses sentinel markers on stdin/stdout to delimit
responses. Concurrent evaluations would interleave sentinels and
corrupt the output stream.

Each test opens its own WebSocket connection (cheap) but they all
share the same GHCi session. Tests that define bindings (e.g.,
`let y = repetitionCode 5`) affect subsequent tests in the same
group -- this is intentional for testing state persistence.

### Server wiring in tests

The `app` function from `server/Main.hs` cannot be imported
directly (module name conflict), so the integration test module
defines its own `testApp` that mirrors the production wiring using
the same `Session`, `Eval`, `Render`, and `Protocol` modules.
This means the test exercises the real evaluation and rendering
pipeline, just not the `Main.hs` glue.

### Timeouts

| Scope | Timeout |
|-------|---------|
| Normal `recvMsg` | 60 s |
| Long eval `recvMsgLong` | 120 s |
| Server startup `waitForServer` | 30 s |

If GHCi hangs or a test deadlocks, it fails with a timeout rather
than blocking CI.


## Adding a new test

### Unit test

Add a `testCase` to the appropriate group in `Test.Unit.Eval` or
`Test.Unit.Protocol`:

```haskell
testCase "my_new_case" $
  classifyCell "foreign import" @?= Declaration
```

If the module is already listed in the cabal `other-modules`, no
other changes are needed.

### Integration test

Add a `testCase` inside the relevant `sequentialTestGroup`:

```haskell
testCase "eval_my_type" $ withConn getPort $ \conn -> do
  (result, _) <- evalAndWait conn "mt-1" "myExpression"
  assertTextField result "status" "ok"
  assertTextField result "render_as" "my_renderer"
```

Key helpers:

| Helper | Purpose |
|--------|---------|
| `withConn getPort action` | Open a WS connection, verify `ready`, run action |
| `sendEval conn cellId source` | Send an eval message |
| `evalAndWait conn cellId source` | Send eval, collect result (skipping progress) |
| `evalAndWaitLong conn cellId source` | Same with 120 s timeout |
| `sendReset conn` | Send a reset message |
| `assertTextField val field expected` | Assert a JSON string field equals expected |
| `assertFieldNull val field` | Assert a JSON field is null or absent |
| `getField field val` | Extract a JSON field as `Maybe Value` |
| `getTextField field val` | Extract a JSON string field as `Maybe Text` |

### Adding a new test module

1. Create `test/Test/YourModule.hs`
2. Add `Test.YourModule` to `other-modules` in the `notebook-tests`
   stanza of `qec-notebook.cabal`
3. Import and include in `test/Main.hs`


## Known expected failures

| Test | Status | Reason |
|------|--------|--------|
| `reset_clears_bindings` | XFAIL | `CM_reset` is a no-op in the server. Will pass once reset is implemented. |


## CI notes

- The test suite requires `qec-cat` to be buildable (`cabal exec ghci`
  imports `QEC.Notebook`). Build `qec-cat` before running tests.
- No external dependencies beyond Haskell -- no Node.js, no npm, no
  browser.
- The ephemeral port avoids conflicts with other services.
- GHCi startup adds ~5--10 s to the first test run. Unit tests alone
  complete in <1 s but still start GHCi (because all tests share one
  executable). To skip integration tests during quick iteration, use:
  ```
  cabal test notebook-tests --test-option='-p /Unit/'
  ```
