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

## Cat Qubit Bias: 10^16 vs Spec's 10^6

**Spec:** "bias eta ~ 10^6 at |alpha|^2 = 19."

**Implementation:** bias = p_Z / p_X = exp(gamma * |alpha|^2) = exp(2 * 19) = exp(38)
~ 3.2 x 10^16 at default parameters.

The ten-order-of-magnitude discrepancy arises because the implementation uses
gamma = 2 (squeezing-enhanced suppression) following Puri et al. 2020. The spec's
10^6 figure may have used a lower squeezing parameter, a different |alpha|^2, or
a different bias definition (e.g., the hardware ratio kappa_2/kappa_1 = 10^5 rather
than the error rate ratio). For reference:

| gamma | \|alpha\|^2 | bias = exp(gamma * \|alpha\|^2) |
|-------|-------------|-------------------------------|
| 2.0   | 19          | 3.2 x 10^16 (implementation)  |
| 1.0   | 19          | 2.4 x 10^8                    |
| 2.0   | 6.9         | 1.0 x 10^6 (matches spec)     |
| 0.73  | 19          | 1.0 x 10^6 (matches spec)     |

Both values (10^6 and 10^16) vastly exceed the minimum bias needed for Z-only
error correction to be effective (~100), so the discrepancy does not affect
the validity of the LDPC-cat approach.

## LDPC-Cat Code Construction

**Spec:** Claims @[165, 34, 22]@ base code from a 3x3 stabilizer tiled on a torus.

**Implementation:** Uses the cellular automaton (fractal) construction from
`FractalCode.py` in the paper's GitHub repository. The correct base code is
**[136, 34, 22]** with H=8 rows and L=17 columns (periodic boundary conditions),
giving n = 8×17 = 136 qubits and (H−2)×L = 102 checks. The 165 in the spec
likely referred to a non-periodic variant.

The parity check matrix H_Z has weight-4 rows, constructed via level-dependent
2×3 stabilizer patterns. The `torusCode` utility is retained but `ldpcCatCode`
now calls `fractalCode` which implements the paper's exact construction.

Extension: `ldpcCatCode ell` gives [136+8×ell, 34+2×ell, 22] with L = 17+ell.

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
