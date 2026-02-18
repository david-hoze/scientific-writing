# qec-notebook: Specification for the qec-cat Interactive Research Notebook

**Version**: 0.1.0  
**Date**: February 2026  
**Status**: Draft

---

## 1. Overview

qec-notebook is a browser-based interactive notebook for QEC research
using the qec-cat Haskell library. Researchers write Haskell expressions
in cells. The notebook evaluates them against a persistent GHCi session
and renders results using type-driven visualization: the return type of
each expression determines how it is displayed — code parameters as
summary cards, simulation results as threshold plots, resource estimates
as comparison tables.

### 1.1 Design principles

1. **Cells are compositions.** Every cell either constructs an algebraic
   object or transforms one. "Configure, simulate, plot" is not three
   separate steps — it is a chain of typed compositions that the notebook
   renders at each stage.

2. **Types drive rendering.** The researcher never writes `plot(...)`.
   The notebook inspects the result type and selects the appropriate
   renderer. Unknown types fall back to `show`.

3. **State persists across cells.** A `let` binding in cell 1 is
   available in cell 5. The session is a single GHCi context.

4. **Long computations stream.** Monte Carlo sweeps push incremental
   results over the WebSocket. The threshold plot builds up in real
   time.

### 1.2 Architecture

```
┌─────────────────────────────────────┐
│         Browser Frontend            │
│  ┌───────────┐  ┌────────────────┐  │
│  │ Cell       │  │ Type-Driven    │  │
│  │ Editor     │  │ Renderers      │  │
│  │ (CodeMirror│  │ (D3/Vega-Lite) │  │
│  └─────┬─────┘  └───────┬────────┘  │
│        │                 ▲           │
│        ▼                 │           │
│  ┌─────────────────────────────┐    │
│  │     WebSocket Client        │    │
│  └──────────┬──────────────────┘    │
└─────────────┼───────────────────────┘
              │ ws://localhost:8170
              │
┌─────────────┼───────────────────────┐
│  Notebook   ▼  Server (Haskell)     │
│  ┌─────────────────────────────┐    │
│  │     WebSocket Handler       │    │
│  └──────────┬──────────────────┘    │
│             │                        │
│  ┌──────────▼──────────────────┐    │
│  │     GHCi Session            │    │
│  │     (qec-cat loaded)        │    │
│  │                             │    │
│  │  Renderable dispatch layer  │    │
│  └─────────────────────────────┘    │
└──────────────────────────────────────┘
```

---

## 2. WebSocket Protocol

All communication uses JSON over a single WebSocket connection at
`ws://localhost:8170/ws`. The frontend is the initiator; the server
responds and may push streaming updates.

### 2.1 Message envelope

Every message (both directions) is a JSON object with a `type` field:

```json
{ "type": "<message_type>", ...fields }
```

### 2.2 Client → Server messages

#### `eval` — Evaluate a cell

```json
{
  "type": "eval",
  "cell_id": "c-001",
  "source": "ldpcCatCode 0"
}
```

| Field     | Type   | Description                              |
|-----------|--------|------------------------------------------|
| `cell_id` | string | Unique cell identifier (client-assigned) |
| `source`  | string | Haskell expression or declaration        |

The server evaluates `source` in the persistent GHCi session.
Both expressions (`ldpcCatCode 0`) and declarations (`let x = 5`)
are supported. The server detects which it is:

- If `source` starts with `let `, `import `, `data `, `type `,
  `class `, `instance `, or `:` (GHCi command), treat as a
  **declaration/command** — evaluate for side effect, return ack.
- Otherwise, treat as an **expression** — evaluate, render, return
  result.

#### `cancel` — Cancel a running evaluation

```json
{
  "type": "cancel",
  "cell_id": "c-001"
}
```

Cancels an in-progress evaluation. The server sends `async`
`throwTo` to the evaluation thread. Best-effort — pure
computations cannot be interrupted between safe points.

#### `reset` — Reset the GHCi session

```json
{
  "type": "reset"
}
```

Restarts the GHCi session. All bindings are lost. The server
re-imports the qec-cat prelude automatically.

#### `save` / `load` — Notebook persistence

```json
{
  "type": "save",
  "notebook": {
    "cells": [
      { "cell_id": "c-001", "source": "ldpcCatCode 0" },
      { "cell_id": "c-002", "source": "repetitionCode 21" }
    ]
  },
  "path": "my-research.qec"
}
```

```json
{
  "type": "load",
  "path": "my-research.qec"
}
```

The server writes/reads a JSON file to the local filesystem.
On `load`, returns the full notebook structure. On `save`,
returns `{ "type": "saved", "path": "..." }`.

### 2.3 Server → Client messages

#### `result` — Evaluation completed

```json
{
  "type": "result",
  "cell_id": "c-001",
  "status": "ok",
  "haskell_type": "CSSCode",
  "render_as": "css_code",
  "data": {
    "n": 136,
    "k": 34,
    "d": 22,
    "hx_rows": 0,
    "hz_rows": 102,
    "hz_weights": [4, 4, 4],
    "label": "[136, 34, 22]"
  },
  "show_text": "CSSCode {cssHX = <0x136 matrix>, cssHZ = <102x136 matrix>}",
  "elapsed_ms": 42
}
```

| Field          | Type   | Description                                          |
|----------------|--------|------------------------------------------------------|
| `cell_id`      | string | Matches the request                                  |
| `status`       | string | `"ok"`, `"error"`, or `"decl_ok"`                    |
| `haskell_type` | string | Haskell type of the result (from GHCi `:type`)       |
| `render_as`    | string | Renderer key (see §4). `null` for fallback.          |
| `data`         | object | Structured JSON payload for the renderer             |
| `show_text`    | string | `show` output, used as fallback and for copy/paste   |
| `elapsed_ms`   | int    | Wall-clock evaluation time in milliseconds           |

For declarations (`let x = ...`):

```json
{
  "type": "result",
  "cell_id": "c-002",
  "status": "decl_ok",
  "haskell_type": null,
  "render_as": null,
  "data": null,
  "show_text": "",
  "elapsed_ms": 1
}
```

#### `error` — Evaluation failed

```json
{
  "type": "result",
  "cell_id": "c-001",
  "status": "error",
  "haskell_type": null,
  "render_as": "error",
  "data": {
    "error_type": "type_error",
    "message": "Couldn't match type 'Int' with 'CSSCode'\n  Expected: CSSCode\n  Actual: Int",
    "suggestions": []
  },
  "show_text": "",
  "elapsed_ms": 5
}
```

| `error_type` values     | Meaning                                      |
|--------------------------|----------------------------------------------|
| `"type_error"`           | GHC type error                               |
| `"parse_error"`          | Syntax error                                 |
| `"not_in_scope"`         | Undefined variable or module                 |
| `"runtime_exception"`    | Exception during evaluation                  |
| `"timeout"`              | Evaluation exceeded time limit               |
| `"cancelled"`            | User cancelled                               |

The `suggestions` field is reserved for future type-hole completions.

#### `progress` — Streaming update for long computations

```json
{
  "type": "progress",
  "cell_id": "c-005",
  "render_as": "sweep_progress",
  "data": {
    "completed": 15,
    "total": 25,
    "partial_results": [
      {
        "code_label": "[136,34,22] ell=0",
        "physical_pz": 0.01,
        "logical_err": 0.00021,
        "num_trials": 100000,
        "num_errors": 21
      }
    ]
  },
  "elapsed_ms": 34200
}
```

Progress messages are sent for any evaluation that takes
longer than 2 seconds. The frontend updates the renderer
incrementally — e.g. the threshold plot adds data points
as they arrive.

#### `stream_complete` — Streaming finished

```json
{
  "type": "result",
  "cell_id": "c-005",
  "status": "ok",
  "haskell_type": "[SweepResult]",
  "render_as": "threshold_plot",
  "data": { ... full results ... },
  "show_text": "...",
  "elapsed_ms": 182000
}
```

The final `result` message contains the complete data.
The frontend replaces the partial rendering with the
final version.

### 2.4 Connection lifecycle

1. Client connects to `ws://localhost:8170/ws`.
2. Server sends `{ "type": "ready", "version": "0.1.0", "prelude": [...] }`
   listing the auto-imported modules.
3. Client sends `eval` messages. Server responds with `result` (and
   optionally `progress`) messages.
4. On disconnect, the server keeps the GHCi session alive for 5 minutes.
   Reconnecting within that window resumes the session.

---

## 3. The Renderable Typeclass

### 3.1 Definition

```haskell
module QEC.Notebook.Renderable
  ( Renderable(..)
  , RenderOutput(..)
  , renderDynamic
  ) where

import Data.Aeson (Value, ToJSON, toJSON, object, (.=))
import Data.Text (Text)
import Data.Typeable (Typeable, TypeRep, typeOf, cast)
import Data.Dynamic (Dynamic, fromDynamic)

-- | A type that the notebook knows how to render.
class (Typeable a, Show a) => Renderable a where
  -- | Key used by the frontend to select a renderer.
  -- Must match an entry in the frontend renderer registry.
  renderKey :: proxy a -> Text

  -- | Structured JSON payload for the frontend renderer.
  -- This is the `data` field in the `result` message.
  renderJSON :: a -> Value

-- | The output produced by the rendering dispatch layer.
data RenderOutput = RenderOutput
  { roRenderAs  :: Maybe Text   -- ^ Nothing = use show fallback
  , roData      :: Maybe Value  -- ^ Nothing = no structured data
  , roShowText  :: String       -- ^ Always available
  , roType      :: String       -- ^ Haskell type string
  }
```

### 3.2 Dynamic dispatch

The GHCi evaluation API returns results as `Dynamic` values (or
as `String` via `:show`). We need to try each registered
`Renderable` instance until one matches. This is a closed
registry — we know all the types we care about.

```haskell
-- | Attempt to render a Dynamic value using registered instances.
-- Tries each known type in order; falls back to Show.
renderDynamic :: Dynamic -> String -> String -> RenderOutput
renderDynamic dyn showStr typeStr =
  case tryAll dyn of
    Just ro -> ro
    Nothing -> RenderOutput Nothing Nothing showStr typeStr
  where
    tryAll d = firstJust
      [ tryRender @CSSCode d
      , tryRender @(Either CSSCodeError CSSCode) d
      , tryRender @CatQubitParams d
      , tryRender @PauliChannel d
      , tryRender @SimResult d
      , tryRender @[SweepResult] d
      , tryRender @ResourceEstimate d
      , tryRender @[ResourceEstimate] d
      , tryRender @BinMatrix d
      , tryRender @(Either ChainComplexError ChainComplex) d
      ]

    tryRender :: forall a. Renderable a => Dynamic -> Maybe RenderOutput
    tryRender d = case fromDynamic @a d of
      Just val -> Just RenderOutput
        { roRenderAs = Just (renderKey (Proxy @a))
        , roData     = Just (renderJSON val)
        , roShowText = show val
        , roType     = show (typeOf val)
        }
      Nothing -> Nothing

    firstJust [] = Nothing
    firstJust (Just x : _) = Just x
    firstJust (Nothing : xs) = firstJust xs
```

**Why a closed registry instead of open dispatch?** GHCi returns
`Dynamic`, and there is no way to look up typeclass instances at
runtime from a `TypeRep` without Template Haskell or a plugin.
The closed list is explicit, simple, and covers all qec-cat types.
New types are added by appending one line to `tryAll`.

### 3.3 GHCi integration strategy

There are two approaches to extracting typed results from GHCi.
We use **Approach B** but fall back to **Approach A** for unknown
types.

**Approach A: String-based (fallback).** Evaluate the expression
with GHCi's `execStmt`, then get the `show` output via
`:print it`. The `haskell_type` field comes from `:type it`.
This always works but gives only text.

**Approach B: Dynamic-based (preferred).** After evaluating the
expression, use the GHC API's `dynCompileExpr` or `compileExpr`
to get a `Dynamic` value for `it`. Pass it to `renderDynamic`.
This requires that the result type has a `Typeable` instance
(true for all qec-cat types via `deriving stock`).

The sequence for each cell evaluation:

```
1. execStmt source             -- evaluate in session
2. typeStr <- execStmt ":t it" -- get type string
3. showStr <- execStmt "show it" -- get show output
4. dyn <- dynCompileExpr "it"  -- get Dynamic (may fail)
5. output <- case dyn of
     Just d  -> return (renderDynamic d showStr typeStr)
     Nothing -> return (RenderOutput Nothing Nothing showStr typeStr)
6. Send result message over WebSocket
```

Step 4 may fail for polymorphic types, IO actions, or types
without `Typeable`. The fallback (step 5, `Nothing` branch)
gives the researcher the `show` output, which is always
available.

### 3.4 Handling declarations vs expressions

The server inspects the source text to determine evaluation mode:

```haskell
data CellKind = Declaration | Expression | GHCiCommand

classifyCell :: String -> CellKind
classifyCell src
  | any (`isPrefixOf` stripped) declPrefixes = Declaration
  | ":" `isPrefixOf` stripped                = GHCiCommand
  | otherwise                                = Expression
  where
    stripped = dropWhile isSpace src
    declPrefixes =
      [ "let ", "import ", "data ", "type ", "newtype "
      , "class ", "instance ", "module ", "{-#"
      ]
```

For `Declaration` and `GHCiCommand`, the server evaluates for
side effect and returns `status: "decl_ok"`. For `Expression`,
it follows the full render pipeline.

---

## 4. Renderable Instances

Each instance defines `renderKey` (matched by the frontend) and
`renderJSON` (the structured payload). Below is the complete
registry.

### 4.1 `CSSCode` — Code parameter card

```haskell
instance Renderable CSSCode where
  renderKey _ = "css_code"
  renderJSON code = object
    [ "n"           .= cssNumQubits code
    , "k"           .= cssNumLogical code
    , "d"           .= cssDistance code
    , "hx_rows"     .= bmNumRows (cssHX code)
    , "hz_rows"     .= bmNumRows (cssHZ code)
    , "hz_weights"  .= uniqueRowWeights (cssHZ code)
    , "hx_weights"  .= uniqueRowWeights (cssHX code)
    , "label"       .= codeLabel code
    , "rate"        .= (fromIntegral (cssNumLogical code)
                        / fromIntegral (cssNumQubits code) :: Double)
    , "tanner"      .= tannerGraphJSON (cssHZ code)
    ]

-- | Compute unique row weights (e.g. [4] means all rows have weight 4).
uniqueRowWeights :: BinMatrix -> [Int]
uniqueRowWeights m = nub [ bvWeight (bmGetRow m i) | i <- [0 .. bmNumRows m - 1] ]

-- | Short label: "[n, k, d]"
codeLabel :: CSSCode -> String
codeLabel code = "[" ++ show n ++ ", " ++ show k ++ ", " ++ show d ++ "]"
  where
    n = cssNumQubits code
    k = cssNumLogical code
    d = cssDistance code

-- | Tanner graph as adjacency list for the frontend.
-- Nodes: checks (0..m-1), qubits (m..m+n-1).
-- Edges: (check, qubit) for each 1 in H_Z.
tannerGraphJSON :: BinMatrix -> Value
tannerGraphJSON m = object
    [ "num_checks" .= bmNumRows m
    , "num_qubits" .= bmNumCols m
    , "edges"      .= [ object [ "check" .= r, "qubit" .= c ]
                       | r <- [0 .. bmNumRows m - 1]
                       , c <- [0 .. bmNumCols m - 1]
                       , bmGetEntry m r c == GF2 True
                       ]
    ]
```

**Frontend renderer: `css_code`**

Renders as a card with:
- Headline: `[136, 34, 22]` in large monospace font
- Properties: rate (0.25), check weight (4), H_X rows (0), H_Z rows (102)
- If `hx_rows == 0`: note "Z-sector only (cat qubit regime)"
- Tanner graph: D3 force-directed bipartite layout. Check nodes (squares)
  on left, qubit nodes (circles) on right. Edges from the `tanner.edges`
  array. Only displayed for codes with n ≤ 200 (larger codes omit the
  graph — it would be unreadable).

### 4.2 `Either CSSCodeError CSSCode` — Construction result

```haskell
instance Renderable (Either CSSCodeError CSSCode) where
  renderKey _ = "code_construction"
  renderJSON (Right code) = object
    [ "status"  .= ("valid" :: Text)
    , "code"    .= renderJSON code
    ]
  renderJSON (Left err) = object
    [ "status"  .= ("invalid" :: Text)
    , "error"   .= describeError err
    ]

describeError :: CSSCodeError -> Value
describeError (DimensionMismatch nx nz) = object
  [ "kind"    .= ("dimension_mismatch" :: Text)
  , "message" .= ("H_X has " ++ show nx ++ " columns but H_Z has "
                   ++ show nz ++ " columns")
  ]
describeError OrthogonalityViolation = object
  [ "kind"    .= ("orthogonality_violation" :: Text)
  , "message" .= ("H_X · H_Z^T ≠ 0 — these matrices do not define"
                   ++ " a valid CSS code")
  ]
```

**Frontend renderer: `code_construction`**

If `status == "valid"`: delegates to `css_code` renderer.
If `status == "invalid"`: red-bordered card with the error message.
The `kind` field can be used for specific icons (dimension mismatch
gets a ruler icon, orthogonality violation gets a crossed-out
matrix icon).

### 4.3 `CatQubitParams` — Physical parameter summary

```haskell
instance Renderable CatQubitParams where
  renderKey _ = "cat_qubit_params"
  renderJSON params = object
    [ "alpha_sq"        .= cqAlphaSq params
    , "kappa1"          .= cqKappa1 params
    , "kappa2"          .= cqKappa2 params
    , "t_cycle_ns"      .= (cqTCycle params * 1e9)
    , "gamma"           .= cqGamma params
    , "kappa_ratio"     .= (cqKappa1 params / cqKappa2 params)
    -- Derived quantities (computed server-side)
    , "p_x"             .= px
    , "p_z"             .= pz
    , "p_y"             .= py
    , "bias"            .= (pz / max 1e-30 px)
    , "bias_log10"      .= logBase 10 (pz / max 1e-30 px)
    ]
    where
      PauliChannel px py pz = catQubitChannel params
```

**Frontend renderer: `cat_qubit_params`**

Two-column card:
- Left column: physical parameters (|α|², κ₁, κ₂, T_cycle, γ)
  with units and scientific notation
- Right column: derived channel (p_X, p_Z, p_Y, bias η)
  with a small horizontal bar showing the X/Z asymmetry on
  a log scale
- The bias is the hero number, displayed prominently:
  "η = 3.2 × 10¹⁶"

### 4.4 `PauliChannel` — Noise channel

```haskell
instance Renderable PauliChannel where
  renderKey _ = "pauli_channel"
  renderJSON (PauliChannel px py pz) = object
    [ "p_x"         .= px
    , "p_y"         .= py
    , "p_z"         .= pz
    , "p_total"     .= (px + py + pz)
    , "bias"        .= (pz / max 1e-30 px)
    , "bias_log10"  .= logBase 10 (pz / max 1e-30 px)
    ]
```

**Frontend renderer: `pauli_channel`**

Compact inline display:
`p_X = 2.9×10⁻¹⁹  p_Z = 9.5×10⁻³  p_Y = 2.8×10⁻²¹  η = 3.2×10¹⁶`
with the bias bar visualization as in `cat_qubit_params`.

### 4.5 `SimResult` — Single simulation result

```haskell
instance Renderable SimResult where
  renderKey _ = "sim_result"
  renderJSON result = object
    [ "total_trials"   .= simTotalTrials result
    , "logical_errors" .= simLogicalErrors result
    , "logical_rate"   .= rate
    , "std_error"      .= stdErr
    , "ci_lower"       .= max 0 (rate - 1.96 * stdErr)
    , "ci_upper"       .= min 1 (rate + 1.96 * stdErr)
    ]
    where
      rate   = logicalErrorRate result
      n      = fromIntegral (simTotalTrials result)
      stdErr = sqrt (rate * (1 - rate) / n)
```

**Frontend renderer: `sim_result`**

Single-line result card:
`p_L = 2.1 × 10⁻⁴  (21 / 100,000 trials)  95% CI: [1.3×10⁻⁴, 3.0×10⁻⁴]`

### 4.6 `[SweepResult]` — Threshold plot

This is the most important renderer. It displays the canonical QEC
threshold plot.

```haskell
data SweepResult = SweepResult
  { swCodeLabel  :: !String   -- e.g. "[136,34,22] ell=0"
  , swPhysicalPZ :: !Double   -- physical phase-flip rate
  , swLogicalErr :: !Double   -- logical error rate
  , swStdError   :: !Double   -- statistical uncertainty
  , swNumTrials  :: !Int
  , swNumErrors  :: !Int
  } deriving stock (Show)

instance NFData SweepResult where
  rnf (SweepResult a b c d e f) =
    a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` ()

instance Renderable [SweepResult] where
  renderKey _ = "threshold_plot"
  renderJSON results = object
    [ "series" .= groupBySeries results
    , "x_label" .= ("Physical phase-flip rate p_Z" :: Text)
    , "y_label" .= ("Logical error rate p_L" :: Text)
    , "x_scale" .= ("linear" :: Text)
    , "y_scale" .= ("log" :: Text)
    ]

-- | Group sweep results by code label for multi-series plot.
groupBySeries :: [SweepResult] -> Value
groupBySeries results = toJSON
  [ object
    [ "label"  .= label
    , "points" .= [ object
        [ "x"        .= swPhysicalPZ r
        , "y"        .= swLogicalErr r
        , "y_lower"  .= max 1e-15 (swLogicalErr r - 1.96 * swStdError r)
        , "y_upper"  .= (swLogicalErr r + 1.96 * swStdError r)
        , "trials"   .= swNumTrials r
        , "errors"   .= swNumErrors r
        ]
      | r <- pts
      ]
    ]
  | (label, pts) <- groupBy swCodeLabel results
  ]
```

**Frontend renderer: `threshold_plot`**

Full-width chart (Vega-Lite spec):
- X-axis: physical error rate (linear scale)
- Y-axis: logical error rate (logarithmic scale, range 10⁻⁸ to 1)
- One line per series (code variant), distinguished by color
- Error bars from `y_lower` / `y_upper` (95% CI)
- Tooltip on hover showing exact values and trial counts
- Curves that cross indicate threshold behavior
- Export button: SVG download for paper inclusion
- The p_L = p_Z diagonal as a dashed reference line

Color palette: qualitative scheme safe for color blindness
(Okabe-Ito or Tableau 10). Each code label gets a consistent
color.

### 4.7 `ResourceEstimate` — Single estimate

```haskell
instance Renderable ResourceEstimate where
  renderKey _ = "resource_estimate"
  renderJSON = toJSON  -- reuses existing ToJSON instance from QEC.Export
```

**Frontend renderer: `resource_estimate`**

Card with:
- Hero number: "47,456 total cat qubits" in large font
- Stacked bar breakdown: data / syndrome / routing / factory,
  each segment labeled with count and percentage
- Secondary info: code distance, runtime, logical error per cycle,
  number of factories, code family name

### 4.8 `[ResourceEstimate]` — Comparison table

```haskell
instance Renderable [ResourceEstimate] where
  renderKey _ = "resource_comparison"
  renderJSON estimates = object
    [ "columns" .= ([ "code_family", "total_qubits", "data_qubits"
                     , "syndrome_qubits", "routing_qubits"
                     , "factory_qubits", "code_distance"
                     , "runtime_seconds", "num_factories"
                     , "logical_error_per_cycle"
                     ] :: [Text])
    , "rows"    .= map toJSON estimates
    ]
```

**Frontend renderer: `resource_comparison`**

HTML table with sortable columns. The `total_qubits` column
is highlighted. Rows are grouped by algorithm if multiple
algorithms are present (detected by grouping on similar
`total_qubits` ranges or by a future `algorithm` field).

Below the table: grouped bar chart with one cluster per code
family, bars for total qubits, colored by component (data /
syndrome / routing / factory).

### 4.9 `BinMatrix` — Matrix display (diagnostic)

```haskell
instance Renderable BinMatrix where
  renderKey _ = "bin_matrix"
  renderJSON m = object
    [ "rows"         .= bmNumRows m
    , "cols"         .= bmNumCols m
    , "density"      .= density
    , "rank"         .= rank m
    , "display"      .= displayData
    ]
    where
      total    = bmNumRows m * bmNumCols m
      ones     = sum [ bvWeight (bmGetRow m r) | r <- [0 .. bmNumRows m - 1] ]
      density  = if total == 0 then 0.0
                 else fromIntegral ones / fromIntegral total :: Double
      -- Only include full matrix data for small matrices
      displayData
        | bmNumRows m <= 32 && bmNumCols m <= 64 =
            Just [ [ if bmGetEntry m r c == GF2 True then (1::Int) else 0
                   | c <- [0 .. bmNumCols m - 1] ]
                 | r <- [0 .. bmNumRows m - 1] ]
        | otherwise = Nothing
```

**Frontend renderer: `bin_matrix`**

If `display` is present: a dense grid of black (1) and white (0)
cells, like a QR code, sized to fit. Useful for seeing the
structure of parity check matrices (the fractal pattern in
LDPC-cat codes is visually distinctive).

If `display` is null (large matrix): summary card with dimensions,
rank, and density.

---

## 5. The `QEC.Notebook` Convenience Module

This module provides the high-level combinators that make the
7-cell research workflow possible. It is auto-imported in the
notebook session.

### 5.1 Module definition

```haskell
module QEC.Notebook
  ( -- * Sweep combinators
    sweep
  , sweepCodes
  , sweepNoise
  , SweepResult(..)

    -- * Cross-architecture comparison
  , compareArchitectures

    -- * Convenience constructors
  , catNoise
  , catNoiseAt
  , noiseRange
  , codeFamily

    -- * Re-exports (auto-available in notebook cells)
  , module QEC.Code.CSS
  , module QEC.Code.Repetition
  , module QEC.Code.Surface
  , module QEC.Code.LDPCCat
  , module QEC.Noise
  , module QEC.Noise.CatQubit
  , module QEC.Simulation
  , module QEC.Resource
  , module QEC.Resource.Algorithm
  , module QEC.Resource.MagicState
  ) where
```

### 5.2 `sweep` — The core research combinator

```haskell
-- | Sweep codes × noise points with simulation.
--
-- Runs Monte Carlo simulation for each (code, noise) pair.
-- Returns results suitable for threshold_plot rendering.
--
-- Example:
--
-- > sweep
-- >   [ ldpcCatCode ell | ell <- [0, 4, 8, 16, 33] ]
-- >   [ catNoise (defaultCatParams { cqAlphaSq = a }) | a <- [4,8,12,16,19] ]
-- >   bpOSD
-- >   defaultSimConfig { simNumTrials = 100000 }
sweep
  :: [(String, CSSCode)]   -- ^ (label, code) pairs
  -> [Double]              -- ^ physical p_Z values
  -> SimConfig             -- ^ simulation config
  -> [SweepResult]
sweep codes pzValues config =
  parMap rdeepseq runPoint
    [ (label, code, pz) | (label, code) <- codes, pz <- pzValues ]
  where
    runPoint (label, code, pz) =
      let seed   = hashSweepPoint label pz
          result = runSimulation config code pz seed
          rate   = logicalErrorRate result
          n      = fromIntegral (simTotalTrials result)
          stdErr = sqrt (rate * (1 - rate) / n)
      in SweepResult
        { swCodeLabel  = label
        , swPhysicalPZ = pz
        , swLogicalErr = rate
        , swStdError   = stdErr
        , swNumTrials  = simTotalTrials result
        , swNumErrors  = simLogicalErrors result
        }

    hashSweepPoint label pz =
      fromIntegral (hash (label, round (pz * 1e6) :: Int)) :: Word64
```

### 5.3 `sweepCodes` and `sweepNoise` — Labeled code constructors

```haskell
-- | Build labeled code list from a code family and parameter range.
--
-- Example:
--
-- > sweepCodes "LDPC-cat" ldpcCatCode [0, 4, 8, 16, 33]
sweepCodes :: String -> (Int -> CSSCode) -> [Int] -> [(String, CSSCode)]
sweepCodes family mkCode params =
  [ (family ++ " ℓ=" ++ show p, mkCode p) | p <- params ]

-- | Build noise points from a range of p_Z values.
-- Generates n logarithmically spaced points between lo and hi.
--
-- Example:
--
-- > noiseRange 0.001 0.15 20
noiseRange :: Double -> Double -> Int -> [Double]
noiseRange lo hi n =
  [ exp (logLo + fromIntegral i * step) | i <- [0 .. n - 1] ]
  where
    logLo = log lo
    logHi = log hi
    step  = (logHi - logLo) / fromIntegral (n - 1)
```

### 5.4 `compareArchitectures` — The paper's centerpiece

```haskell
-- | Compare resource estimates across code families for a given algorithm.
--
-- Example:
--
-- > compareArchitectures ecdlp256 defaultCatParams
-- >   [RepetitionCat, LDPCCat, SurfaceCode]
compareArchitectures
  :: Algorithm
  -> CatQubitParams
  -> [CodeFamily]
  -> [ResourceEstimate]
compareArchitectures algo params families =
  [ estimateResources algo params fam defaultFactory
  | fam <- families
  ]

-- | Compare across multiple algorithms AND code families.
--
-- > compareAll
-- >   [ecdlp256, shorRSA2048]
-- >   defaultCatParams
-- >   [RepetitionCat, LDPCCat, SurfaceCode]
compareAll
  :: [Algorithm]
  -> CatQubitParams
  -> [CodeFamily]
  -> [ResourceEstimate]
compareAll algos params families =
  [ estimateResources algo params fam defaultFactory
  | algo <- algos
  , fam  <- families
  ]
```

### 5.5 Convenience aliases

```haskell
-- | Compute the Pauli channel for cat qubit parameters.
catNoise :: CatQubitParams -> PauliChannel
catNoise = catQubitChannel

-- | Cat qubit noise at a specific |α|², using defaults for everything else.
catNoiseAt :: Double -> PauliChannel
catNoiseAt alphaSq = catQubitChannel (defaultCatParams { cqAlphaSq = alphaSq })

-- | Extract just p_Z from cat qubit parameters.
catPZ :: CatQubitParams -> Double
catPZ = pcPz . catQubitChannel

-- | Named algorithms (re-exported for convenience)
-- These are already defined in QEC.Resource.Algorithm:
--   ecdlp256    :: Algorithm
--   shorRSA2048 :: Algorithm
```

---

## 6. The GHCi Backend Server

### 6.1 Executable structure

```
executable qec-notebook
  main-is: Main.hs
  hs-source-dirs: notebook-server
  build-depends:
    base          >= 4.18 && < 5,
    qec-cat,
    ghc           >= 9.8,
    ghc-paths     >= 0.1,
    websockets    >= 0.13,
    aeson         >= 2.1,
    text          >= 2.0,
    bytestring    >= 0.11,
    async         >= 2.2,
    stm           >= 2.5,
    directory     >= 1.3,
    filepath      >= 1.4,
    containers    >= 0.6
  default-language: GHC2021
  ghc-options: -O2 -Wall -threaded -rtsopts "-with-rtsopts=-N"
```

### 6.2 Server startup

```haskell
main :: IO ()
main = do
  putStrLn "qec-notebook v0.1.0"
  putStrLn "Starting GHCi session..."
  session <- initSession
  putStrLn "GHCi ready. Starting WebSocket server on port 8170..."
  serveFrontend 8170  -- serves static files + WebSocket
```

### 6.3 Session initialization

The session pre-imports the notebook prelude so the researcher
starts with everything available:

```haskell
initSession :: IO Session
initSession = do
  session <- newGHCiSession
  -- Auto-import the notebook prelude
  evalInSession session "import QEC.Notebook"
  -- Convenience: make common types available unqualified
  evalInSession session "import Data.Word (Word64)"
  return session
```

The `import QEC.Notebook` brings in all re-exports (CSS codes,
noise models, simulation, resource estimation) plus the sweep
and comparison combinators. A researcher can immediately type
`ldpcCatCode 0` without any setup.

### 6.4 Evaluation pipeline

```haskell
handleEval :: Session -> CellId -> String -> WebSocket -> IO ()
handleEval session cellId source ws = do
  startTime <- getCurrentTime
  case classifyCell source of
    Declaration -> do
      result <- evalDecl session source
      case result of
        Right () -> sendJSON ws (declOkMsg cellId)
        Left err -> sendJSON ws (errorMsg cellId "parse_error" err)

    GHCiCommand -> do
      output <- evalCommand session source
      sendJSON ws (declOkMsg cellId) -- or include output

    Expression -> do
      -- Step 1: Evaluate expression, bind to `it`
      evalResult <- evalExpr session source
      case evalResult of
        Left err -> sendJSON ws (errorMsg cellId (classifyGHCError err) err)
        Right () -> do
          -- Step 2: Get type string
          typeStr <- evalCommand session ":t it"

          -- Step 3: Get show output
          showStr <- evalCommand session "show it"

          -- Step 4: Try to get Dynamic
          mDyn <- tryDynamic session "it"

          -- Step 5: Render
          let output = case mDyn of
                Just dyn -> renderDynamic dyn showStr typeStr
                Nothing  -> RenderOutput Nothing Nothing showStr typeStr

          -- Step 6: Send result
          elapsed <- diffTime startTime
          sendJSON ws (resultMsg cellId output elapsed)
```

### 6.5 Streaming for long computations

For `sweep` and other long-running functions, we need incremental
results. The approach: `sweep` writes to a shared `TVar` as each
data point completes. The server polls this `TVar` and sends
`progress` messages.

```haskell
-- In QEC.Notebook:

-- | Streaming sweep that writes results as they complete.
sweepStreaming
  :: TVar [SweepResult]   -- ^ shared accumulator
  -> [(String, CSSCode)]
  -> [Double]
  -> SimConfig
  -> IO [SweepResult]
sweepStreaming var codes pzValues config = do
  results <- forConcurrently allPoints $ \(label, code, pz) -> do
    let result = runPoint (label, code, pz)
    atomically $ modifyTVar' var (result :)
    return result
  return results
```

The server detects when an expression calls `sweep` (by checking
the type of `it` before full evaluation, or by wrapping known
long-running functions). It allocates a `TVar`, passes it to
the streaming variant, and spawns a poller:

```haskell
-- Poll every 2 seconds and send progress
poller :: TVar [SweepResult] -> CellId -> WebSocket -> IO ()
poller var cellId ws = forever $ do
  threadDelay 2_000_000  -- 2 seconds
  results <- readTVarIO var
  unless (null results) $
    sendJSON ws (progressMsg cellId results)
```

**Design note**: The streaming mechanism requires that `sweep`
exists in two forms — a pure version (for non-notebook use and
testing) and a streaming `IO` version (for the notebook). The
pure version is primary; the notebook server wraps it.

An alternative: the pure `sweep` uses `parMap`, which processes
all points and returns. For streaming, we replace `parMap` with
explicit `async` tasks that write to the `TVar`. The pure API
is unchanged — only the notebook server adds the streaming
layer.

### 6.6 Timeout and resource limits

| Limit               | Default | Configurable |
|----------------------|---------|--------------|
| Evaluation timeout   | 600 s   | Yes (per-cell or global) |
| Max memory (GHCi)    | 4 GB    | Yes (via +RTS -M) |
| Max result size      | 10 MB   | Yes |
| WebSocket msg limit  | 16 MB   | Yes |
| Progress poll interval | 2 s   | No |

Evaluation timeout is enforced via `System.Timeout.timeout`.
On timeout, the evaluation thread is killed and an error
message with `"error_type": "timeout"` is sent.

---

## 7. Frontend Specification

### 7.1 Technology stack

- **HTML/CSS/JS**: No framework for v1. Single `index.html`.
- **Cell editor**: CodeMirror 6 with Haskell syntax highlighting.
- **Charting**: Vega-Lite (via vega-embed) for threshold plots and
  bar charts. Declarative, handles log scales, error bars, tooltips,
  and SVG export natively.
- **Table**: Plain HTML `<table>` with sortable headers (20 lines
  of JS).
- **Tanner graph**: D3.js force-directed layout.
- **WebSocket**: Native browser `WebSocket` API.

All frontend assets are served by the Haskell server from a
`static/` directory. No build step, no npm. A researcher runs
`cabal run qec-notebook` and opens `http://localhost:8170`.

### 7.2 Renderer registry

The frontend maintains a map from `render_as` keys to render
functions:

```javascript
const RENDERERS = {
  css_code:           renderCSSCode,
  code_construction:  renderCodeConstruction,
  cat_qubit_params:   renderCatParams,
  pauli_channel:      renderPauliChannel,
  sim_result:         renderSimResult,
  threshold_plot:     renderThresholdPlot,
  sweep_progress:     renderThresholdPlot,   // same renderer, partial data
  resource_estimate:  renderResourceEstimate,
  resource_comparison: renderResourceComparison,
  bin_matrix:         renderBinMatrix,
  error:              renderError,
};

function renderCellOutput(msg) {
  const renderer = RENDERERS[msg.render_as];
  if (renderer && msg.data) {
    return renderer(msg.data, msg);
  } else {
    return renderFallback(msg.show_text, msg.haskell_type);
  }
}
```

### 7.3 Cell lifecycle

```
[idle] → user types code
      → user presses Shift+Enter (or clicks Run)
      → cell enters [running] state (spinner, input disabled)
      → frontend sends `eval` message
      → server sends `progress` messages → partial render updates
      → server sends `result` message
      → cell enters [complete] state
      → output area populated by renderer

[complete] → user edits source → cell returns to [idle]
           → previous output is dimmed (stale indicator)
           → user re-runs → back to [running]
```

### 7.4 Keyboard shortcuts

| Key           | Action                              |
|---------------|-------------------------------------|
| Shift+Enter   | Run cell, move to next cell         |
| Ctrl+Enter    | Run cell, stay in current cell      |
| Ctrl+Shift+N  | Insert new cell below               |
| Ctrl+Shift+D  | Delete current cell                 |
| Ctrl+Shift+↑  | Move cell up                        |
| Ctrl+Shift+↓  | Move cell down                      |
| Escape        | Cancel running cell                 |
| Ctrl+S        | Save notebook                       |

### 7.5 Notebook file format

```json
{
  "version": "0.1.0",
  "title": "LDPC-cat threshold analysis",
  "created": "2026-02-17T14:30:00Z",
  "cells": [
    {
      "cell_id": "c-001",
      "source": "ldpcCatCode 0",
      "output": null
    },
    {
      "cell_id": "c-002",
      "source": "sweep\n  (sweepCodes \"LDPC-cat\" ldpcCatCode [0,4,8,16,33])\n  (noiseRange 0.01 0.15 12)\n  defaultSimConfig { simNumTrials = 50000 }",
      "output": null
    }
  ]
}
```

Outputs are not stored (they may be large and are reproducible).
On load, the researcher re-runs cells.

---

## 8. Startup Prelude

When the notebook session initializes, the following is evaluated
automatically:

```haskell
-- Auto-imported by notebook server
import QEC.Notebook

-- Brings into scope:
--   Code constructors: repetitionCode, surfaceCode, ldpcCatCode
--   Noise: defaultCatParams, catQubitChannel, PauliChannel(..)
--   Simulation: runSimulation, runCSSSimulation, defaultSimConfig,
--               SimConfig(..), SimResult(..), logicalErrorRate
--   Resource: estimateResources, ResourceEstimate(..),
--             CodeFamily(..), ecdlp256, shorRSA2048
--   Notebook: sweep, sweepCodes, noiseRange, compareArchitectures,
--             compareAll, catNoise, catNoiseAt, catPZ, SweepResult(..)
--   GF2: BinMatrix, bmNumRows, bmNumCols (for advanced users)
--   CSS: CSSCode(..), mkCSSCode, cssNumQubits, cssNumLogical, cssDistance
```

A researcher can begin typing immediately. The first cell of a
new notebook shows a welcome message with examples:

```
Welcome to qec-notebook.

Try:
  ldpcCatCode 0                                    -- build a code
  defaultCatParams                                 -- see cat qubit physics
  sweep                                            -- run threshold analysis
    (sweepCodes "rep" repetitionCode [3,5,7,9])
    (noiseRange 0.01 0.2 15)
    defaultSimConfig
```

---

## 9. Example Session

The 7-cell workflow from the design discussion, showing the exact
Haskell expressions and expected renderer output:

**Cell 1**: `ldpcCatCode 0`
→ type: `CSSCode` → renderer: `css_code`
→ card: **[136, 34, 22]** | rate 0.25 | weight-4 checks | Z-sector only

**Cell 2**: `repetitionCode 21`
→ type: `CSSCode` → renderer: `css_code`
→ card: **[21, 1, 21]** | rate 0.048 | weight-2 checks | Z-sector only

**Cell 3**: `defaultCatParams { cqAlphaSq = 12 }`
→ type: `CatQubitParams` → renderer: `cat_qubit_params`
→ card: |α|²=12, p_Z=6.0×10⁻³, p_X≈10⁻¹⁰, **η = 1.7×10¹⁰**

**Cell 4**: `runSimulation defaultSimConfig (ldpcCatCode 0) 0.05 42`
→ type: `SimResult` → renderer: `sim_result`
→ line: **p_L = 2.1×10⁻⁴** (21/100,000) 95% CI [1.3×10⁻⁴, 3.0×10⁻⁴]

**Cell 5**:
```haskell
sweep
  (sweepCodes "LDPC-cat" ldpcCatCode [0, 4, 8, 16, 33])
  (noiseRange 0.01 0.15 12)
  defaultSimConfig { simNumTrials = 100000 }
```
→ type: `[SweepResult]` → renderer: `threshold_plot`
→ full threshold plot with 5 curves, crossover visible

**Cell 6**: `compareArchitectures ecdlp256 defaultCatParams [RepetitionCat, LDPCCat, SurfaceCode]`
→ type: `[ResourceEstimate]` → renderer: `resource_comparison`
→ comparison table: RepetitionCat 124,864 / LDPCCat 43,456 / Surface 3,145,728

**Cell 7**:
```haskell
compareAll [ecdlp256, shorRSA2048] defaultCatParams [RepetitionCat, LDPCCat, SurfaceCode]
```
→ type: `[ResourceEstimate]` → renderer: `resource_comparison`
→ 6-row comparison table, the paper's centerpiece figure

---

## 10. Build and Run

```bash
# Build the notebook server and library
cabal build qec-notebook

# Run
cabal run qec-notebook
# Output:
#   qec-notebook v0.1.0
#   Starting GHCi session...
#   GHCi ready. Starting WebSocket server on port 8170...
#   Open http://localhost:8170 in your browser.

# Or with custom port
cabal run qec-notebook -- --port 9000
```

### 10.1 Static frontend serving

The server serves `static/index.html` at `/` and all files under
`static/` at their relative paths. The directory structure:

```
notebook-server/
  Main.hs
  static/
    index.html        -- the notebook UI
    style.css         -- notebook styles
    notebook.js       -- cell management + WebSocket
    renderers.js      -- type-driven render functions
    codemirror/       -- CodeMirror 6 Haskell mode (vendored)
    vega/             -- vega-lite + vega-embed (vendored)
    d3.min.js         -- D3 for Tanner graphs (vendored)
```

All JS dependencies are vendored (no CDN, no npm). The notebook
works offline after first build.

---

## 11. Future Extensions (v2+)

These are explicitly out of scope for v1 but inform the design:

1. **Type-hole completions**: When the researcher types `sweep _ ...`,
   the notebook queries GHCi for the expected type and offers
   completions from the known constructors. Requires GHC API
   integration with the typed-hole mechanism.

2. **Inline documentation**: Hovering over a function name shows its
   Haddock documentation. Requires querying GHCi `:info`.

3. **Export to LaTeX**: Convert the comparison table and threshold
   plot to LaTeX-ready formats (pgfplots, booktabs).

4. **Collaborative sharing**: Hosted version where notebooks are
   shareable via URL. Requires a backend that manages multiple
   GHCi sessions and user authentication.

5. **Custom renderers**: Let researchers define new `Renderable`
   instances in notebook cells, with the frontend dynamically
   loading renderer functions from user-provided JS.

6. **Checkpoint/restore**: Save the full GHCi session state so
   notebooks can be reopened with all bindings intact without
   re-running cells.

7. **Code search**: Enumerate code constructions by typing
   constraints ("CSS codes with n < 200, k > 10, d > 8") and
   letting the notebook search over the algebraic parameter space.