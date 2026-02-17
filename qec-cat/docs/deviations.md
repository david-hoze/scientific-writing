# Implementation Deviations from Specification

This document bridges the planning spec (`qec-cat-implementation-specification.md`)
and the actual implementation. Read this if you're comparing code against the spec.

---

## Field Name Changes

The implementation uses shorter prefixes and, in some cases, different naming
conventions than the spec.

| Spec field | Implementation field | Module |
|---|---|---|
| `catAlphaSq` | `cqAlphaSq` | `QEC.Noise.CatQubit` |
| `catKappa1` | `cqKappa1` | `QEC.Noise.CatQubit` |
| `catKappa2` | `cqKappa2` | `QEC.Noise.CatQubit` |
| `catCycleTimeNs` | `cqTCycle` | `QEC.Noise.CatQubit` |
| `catSqueezing` | `cqGamma` | `QEC.Noise.CatQubit` |
| `pcPx` / `pcPy` / `pcPz` | `pX` / `pY` / `pZ` | `QEC.Noise` |
| `bpMinSumAlpha` | `bpMsAlpha` | `QEC.Decoder.BP` |
| `simShots` | `simNumTrials` | `QEC.Simulation` |
| `simNumCores` | `simNumChunks` | `QEC.Simulation` |
| `ecdlp_256` | `ecdlp256` | `QEC.Resource.Algorithm` |

## Units Change: Cycle Time

**Spec:** `catCycleTimeNs :: Double` stores cycle time in **nanoseconds** (default `500`).

**Implementation:** `cqTCycle :: Double` stores cycle time in **seconds** (default `500.0e-9`).

The rest of the physics formulas (bit-flip rate, phase-flip rate) consume `cqTCycle`
directly in seconds, so no internal conversion is needed. But if you're doing
back-of-envelope calculations assuming nanoseconds from the spec, divide by 1e9.

## Y Error Rate Formula

**Spec:** `pY ~ pX` (same exponentially suppressed rate as bit-flips, treating Y = iXZ
at the single-qubit level as comparable to X).

**Implementation:** `pY = pX * pZ` (product of independent X and Z error probabilities).

The implementation's formula is more physically correct for independent error channels:
the probability of *both* a bit-flip and a phase-flip occurring on the same qubit in
the same cycle is the product of the two rates. At the extreme bias of cat qubits
(pZ/pX > 10^6), the distinction is negligible for resource estimation since pY << pX
either way. The spec's approximation was a simplification; the implementation chose
the tighter bound.

## Simplified Simulation API

The spec's `SimConfig` included per-qubit error rates, number of QEC rounds, and
separate OSD configuration. The implementation simplifies this:

- Physical error probability `pZ` and RNG seed are passed as arguments to
  `runSimulation` rather than stored in the config.
- `simRounds` is not implemented (code-capacity noise model only, no circuit-level).
- `simOSDConfig` is removed; OSD-0 is always used as the BP fallback.
- `simStdError` is not computed in `SimResult`; the logical error rate is available
  via the `logicalErrorRate :: SimResult -> Double` function.
- `runSimulation` is pure (takes a `Word64` seed) rather than `IO`-based.

## Resource Estimation API

The spec has `estimateResources :: Algorithm -> CatQubitParams -> CSSCode -> ResourceEstimate`.

The implementation takes a `CodeFamily` enum (`RepetitionCat | SurfaceCode`) plus
explicit `FactoryParams` and `LayoutParams` instead of a concrete `CSSCode`. This
allows parametric sweeps over code families without constructing codes at every
distance. The implementation also adds `reNumFactories :: Int` to `ResourceEstimate`.

## Function Argument Order

Several functions reorder arguments compared to the spec, generally putting
configuration/config first (partial-application friendly):

- `bpDecode`: spec has `BinMatrix -> BinVec -> Vector Double -> BPConfig`,
  implementation has `BPConfig -> BinMatrix -> BinVec -> Vector Double`.
- `runSimulation`: spec has `CSSCode -> SimConfig -> IO SimResult`,
  implementation has `SimConfig -> CSSCode -> Double -> Word64 -> SimResult`.

## LDPC-Cat Stabilizer Pattern

The spec calls for reverse-engineering the 3x3 stabilizer pattern from Ruiz et al.
(arXiv:2401.09541). The implementation provides a general `torusCode` construction
that tiles any 3x3 pattern on an l x m torus, but the default pattern is a
**placeholder** that has not been validated against the paper's [165, 34, 22] code.
The `torusCode` framework is correct; only the specific pattern needs updating once
extracted from the paper's exhaustive search results.

## Modules Not Yet Implemented

These are documented as Phase 3/4 stretch goals in the spec (Section 7):

- `QEC.Code.XZZX` — XZZX surface code
- `QEC.Code.BivariateBicycle` — BB codes
- `QEC.Code.LiftedProduct` — Bias-tailored lifted product
- `QEC.Stabilizer.PauliFrame` — Pauli frame simulation
- `QEC.Decoder.MWPM` — Minimum-weight perfect matching

## Cabal Extensions

The spec lists 15 default extensions including `DataKinds`, `TypeFamilies`, `GADTs`,
and `RankNTypes`. The implementation enables only 5 (`StrictData`, `DerivingStrategies`,
`GeneralizedNewtypeDeriving`, `ScopedTypeVariables`, `BangPatterns`). The omitted
extensions are not needed by the current code and can be added when Phase 3/4 modules
require them.
