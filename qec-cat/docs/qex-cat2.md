# Implementing `qec-cat`: A Haskell Library for Biased-Noise Quantum Error Correction

**Cat qubits combined with LDPC codes represent the most promising path to low-overhead fault-tolerant quantum computing, and a well-typed Haskell library can enforce the intricate mathematical invariants these architectures demand.** This document provides a complete technical blueprint for `qec-cat` — a research library and accompanying paper targeting resource estimation for biased-noise architectures. The project sits at the intersection of three converging breakthroughs: Alice & Bob's demonstration of hour-long bit-flip times (September 2025), LDPC-cat codes encoding 100 logical qubits in 758 physical cat qubits (Ruiz et al., Nature Communications 2025), and the Pinnacle architecture achieving RSA-2048 factoring in under 100,000 qubits with qLDPC codes (Webster et al., February 2026). No Haskell QEC library currently exists — every major tool (Stim, PyMatching, qecsim) is written in Python/C++. This creates a genuine opportunity for a type-safe implementation that eliminates entire classes of dimension-mismatch and stabilizer-commutativity bugs at compile time.

---

## Part 1: The Haskell ecosystem provides adequate but unoptimized foundations

### Binary linear algebra over GF(2) requires custom implementation

No existing Haskell package provides high-performance binary linear algebra with bit-packing. The landscape includes `linear-code` (v0.2.0), which offers type-safe `LinearCode n k F2` parameterized by `KnownNat` dimensions with Gaussian elimination via `rref`, but uses small vectors internally and is unsuitable for performance-critical QEC simulation. The `galois-field` package (v1.0.2) provides mature GF(p^q) implementations including GF(2) with `Arbitrary`, `Random`, and `NFData` instances — excellent for the type-safe API layer. The `computational-algebra` package by @konn offers aggressive type-level features but is heavy-weight.

**The recommended approach is custom `Word64` bit-packing.** Each row of a binary matrix is stored as a `Data.Vector.Unboxed` of `Word64` values, where each word encodes 64 matrix entries. Row XOR reduces to a single `xor` instruction per word. GF(2) Gaussian elimination becomes O(n² × ⌈n/64⌉) — a factor-64 improvement over naive element-wise operations. This is approximately **100 lines of Haskell code** and dramatically outperforms any generic field approach. For symplectic matrices (central to stabilizer simulation), the same representation applies with the convention that a 2n-bit row stores (x₁...xₙ | z₁...zₙ). Sparse matrices can use `IntMap Word64` for row storage when stabilizer weights are low (≤6 for LDPC codes), but dense representation is preferred for SIMD-friendly inner loops.

### SIMD support in GHC is maturing but integer operations lag

GHC 9.12 (January 2025) landed basic SIMD support in the x86-64 native code generator — no longer requiring the LLVM backend. Types like `FloatX4#`, `DoubleX2#`, and `Int64X2#` map directly to XMM/YMM registers. However, **integer SIMD operations needed for XOR-heavy binary matrix workloads are still incomplete** — full integer vector arithmetic is targeted for GHC 9.14. The practical recommendation is to start with `Word64` bit-packing (which GHC compiles to efficient scalar XOR) and plan an FFI path to thin C kernels for AVX2 when profiling reveals bottlenecks.

FFI overhead is effectively zero for `unsafe` foreign calls: benchmarks from the `ffi-overhead` repository show Haskell `unsafe` FFI at **1197ms for 500M calls vs C at 1182ms** — a function-pointer jump with no marshalling. Using `Data.Vector.Storable` and `unsafeWith` to pass `Ptr Word64` directly to C functions avoids all marshalling costs. A 20-line C kernel for AVX2 XOR over packed binary rows, called via `foreign import ccall unsafe`, achieves near-native throughput. The realistic performance gap between pure Haskell inner loops and SIMD C for Pauli frame simulation is **10–50×** for large qubit counts, narrowing to **2–5×** for the bit-packed `Word64` approach without explicit SIMD.

### Parallelism for Monte Carlo is well-served by existing libraries

For embarrassingly parallel Monte Carlo sampling (millions of independent QEC shots), `Control.Parallel.Strategies` with `parMap rdeepseq` provides the simplest effective solution. The key pattern splits a splittable PRNG (`splitmix` or `pcg-random`, not `System.Random.StdGen`) across cores, then runs independent trial blocks:

```haskell
parMap rdeepseq (\gen -> replicateTrials trialsPerCore gen) splitGens
```

Compile with `ghc -O2 -threaded` and run with `+RTS -N -A64m` (larger nursery reduces GC contention). Chunk sizes of **1,000–10,000 trials per spark** balance overhead against load imbalance. For array-parallel computation, `massiv` (v1.0+) offers a work-stealing scheduler with demonstrated **2× speedup over Repa** (8ms vs 82ms on array sum benchmarks). `streamly` (v0.11.0) provides streaming concurrency with lock-free synchronization, useful for streaming Monte Carlo results with `parallelly`. For the initial implementation, `parMap` from `parallel` is recommended for simplicity; `massiv` arrays for the inner simulation data structures.

### Type-level programming, testing, and distribution

`GHC.TypeLits` provides `Nat`-kinded type-level naturals with `KnownNat` for bridging to runtime. The `ghc-typelits-natnormalise` plugin automatically solves many arithmetic equalities that GHC's constraint solver cannot handle (commutativity, associativity of `(+)`). The `ghc-typelits-knownnat` plugin derives `KnownNat` for arithmetic expressions. The `vector-sized` package wraps `Data.Vector` with fixed-length `Vector n a` types. For ergonomics, `TypeApplications` eliminates explicit proxy passing: `natVal @n` instead of `natVal (Proxy :: Proxy n)`.

**QuickCheck** is recommended over Hedgehog for testing algebraic invariants because the `Arbitrary` typeclass integrates cleanly with existing `galois-field` and `linear-code` instances. The `quickcheck-classes` package tests algebraic laws (Monoid, Group, Ring) automatically. The `tasty` framework with `tasty-quickcheck` provides the test runner. Properties like `prop_cssOrthogonality code = matMulGF2 (getHX code) (transposeGF2 (getHZ code)) == zeroMatrix` are natural to express.

For build and distribution, **Cabal** (v3.12+) is the recommended primary build tool, with **Nix flakes** via `haskell-flake` for reproducibility. Hackage publication uses `cabal sdist` and `cabal upload`. Static binaries via `pkgsStatic` overlay or Alpine/musl builds, and Docker multi-stage builds (`FROM haskell:9.10 AS build` → `FROM debian:bookworm-slim`), address non-Haskell user accessibility. **IHaskell** remains actively maintained (supports GHC 8.4–9.10) for notebook-style exploration, but the pragmatic pattern for publication-quality plots is CSV/JSON export via `aeson`/`cassava` with Python matplotlib rendering.

---

## Part 2: A layered type system enforcing QEC invariants at compile time

### 2.1 Finite field and algebraic foundations

```haskell
-- GF(2) element: newtype over Bool for zero-cost abstraction
newtype GF2 = GF2 { unGF2 :: Bool }
  deriving (Eq, Ord, Show, NFData, Arbitrary)

instance Num GF2 where
  GF2 a + GF2 b = GF2 (a `xor` b)
  GF2 a * GF2 b = GF2 (a && b)
  fromInteger n = GF2 (odd n)

-- Bit-packed binary vector: dense representation
newtype BinVec (n :: Nat) = BinVec { getBinVec :: Vector Word64 }

-- Dense binary matrix: row-major bit-packing
data BinMatrix (m :: Nat) (n :: Nat) = BinMatrix
  { bmRows    :: {-# UNPACK #-} !Int
  , bmCols    :: {-# UNPACK #-} !Int
  , bmWordsPerRow :: {-# UNPACK #-} !Int
  , bmData    :: !(Vector Word64)  -- row-major, ceil(n/64) words per row
  }

-- Sparse binary matrix: CSR format for LDPC parity checks
data SparseBinMatrix (m :: Nat) (n :: Nat) = SparseBinMatrix
  { sbmRowPtrs :: !(Vector Int)     -- length m+1
  , sbmColIdxs :: !(Vector Int)     -- nnz entries
  }

-- Group algebra F₂[G] for lifted product codes
data GroupAlgebra g = GroupAlgebra
  { gaGroup    :: !g
  , gaCoeffs   :: !(Map g GF2)  -- sparse representation
  }

-- Polynomial quotient ring F₂[x]/(x^n - 1)
newtype CyclicPoly (n :: Nat) = CyclicPoly { getCoeffs :: BinVec n }
```

### 2.2 Chain complexes and homology

```haskell
-- Chain complex: C₂ →∂₂ C₁ →∂₁ C₀ with ∂₁∘∂₂ = 0
data ChainComplex (r :: *) = ChainComplex
  { cc_d2 :: !(Matrix r)   -- ∂₂: C₂ → C₁
  , cc_d1 :: !(Matrix r)   -- ∂₁: C₁ → C₀
  -- INVARIANT: cc_d1 · cc_d2 = 0 (enforced by smart constructor)
  }

mkChainComplex :: Matrix r -> Matrix r -> Either ChainComplexError (ChainComplex r)
mkChainComplex d1 d2
  | cols d1 /= rows d2 = Left DimensionMismatch
  | d1 `matMul` d2 /= zeroMatrix = Left BoundarySquaredNonzero
  | otherwise = Right (ChainComplex d2 d1)

-- Homology: H₁ = ker(∂₁) / im(∂₂)
homologyDim :: ChainComplex GF2 -> Int
homologyDim cc = rankKernel (cc_d1 cc) - rankImage (cc_d2 cc)

-- Lifted product as typed functor over ring R
liftedProduct :: (Ring r) => ProtographMatrix r -> ProtographMatrix r -> ChainComplex r
```

### 2.3 Quantum codes with CSS orthogonality enforced

```haskell
-- Core typeclass hierarchy
class QuantumCode code where
  type NumQubits    code :: Nat
  type NumLogical   code :: Nat
  type Distance     code :: Nat
  parityCheckX :: code -> BinMatrix (ChecksX code) (NumQubits code)
  parityCheckZ :: code -> BinMatrix (ChecksZ code) (NumQubits code)

-- CSS code with H_X · H_Z^T = 0 verified at construction
data CSSCode (n :: Nat) (kx :: Nat) (kz :: Nat) where
  MkCSSCode :: (KnownNat n, KnownNat kx, KnownNat kz)
            => { cssHX :: BinMatrix kx n
               , cssHZ :: BinMatrix kz n }
            -> CSSCode n kx kz

mkCSSCode :: BinMatrix kx n -> BinMatrix kz n -> Either CodeError (CSSCode n kx kz)
mkCSSCode hx hz
  | matMulGF2 hx (transpose hz) /= zeroMatrix = Left OrthogonalityViolation
  | otherwise = Right (MkCSSCode hx hz)

-- Code family instances
data RepetitionCode (d :: Nat)    -- [d, 1, d]
data SurfaceCode (d :: Nat)       -- [[d², 1, d]]
data XZZXCode (dx :: Nat) (dz :: Nat)  -- [[dx·dz, 1, min(dx,dz)]]
data BivariateBicycle (l :: Nat) (m :: Nat)  -- [[2lm, k, d]]
data LDPCCat (ell :: Nat)         -- [165+8ℓ, 34+2ℓ, 22]
```

### 2.4–2.5 Syndrome circuits and stabilizer tableau

```haskell
-- Stabilizer tableau: 2n rows × (2n+1) columns
data Tableau (n :: Nat) where
  Tableau :: KnownNat n =>
    { tabXBits :: !(MVector s Word64)  -- 2n rows × ⌈n/64⌉ words
    , tabZBits :: !(MVector s Word64)
    , tabSigns :: !(MVector s Word8)
    } -> Tableau n

-- Pauli frame: lightweight error tracking
data PauliFrame (n :: Nat) (nFrames :: Nat) where
  PauliFrame :: (KnownNat n, KnownNat nFrames) =>
    { pfXBits :: !(Vector Word64)  -- n qubits × ⌈nFrames/64⌉ words
    , pfZBits :: !(Vector Word64)
    } -> PauliFrame n nFrames

-- Circuit as GADT with noise model threading
data Circuit (n :: Nat) (noise :: *) where
  Identity  :: Circuit n noise
  HGate     :: Finite n -> Circuit n noise -> Circuit n noise
  SGate     :: Finite n -> Circuit n noise -> Circuit n noise
  CNOTGate  :: Finite n -> Finite n -> Circuit n noise -> Circuit n noise
  Measure   :: Finite n -> Circuit n noise -> Circuit n noise
  NoiseOp   :: noise -> Finite n -> Circuit n noise -> Circuit n noise
```

### 2.6–2.8 Noise models, decoders, and resource estimation

```haskell
-- Cat qubit noise model parameterized by physical constants
data CatQubitNoise = CatQubitNoise
  { alphaSq    :: !Double    -- |α|², mean photon number
  , kappa1     :: !Double    -- κ₁, single-photon loss rate (Hz)
  , kappa2     :: !Double    -- κ₂, two-photon dissipation rate (Hz)
  , gateTimeNs :: !Double    -- T_gate in nanoseconds
  }

-- Compute error rates from physical parameters
bitFlipRate :: CatQubitNoise -> Double
bitFlipRate n = exp (-2 * alphaSq n)

phaseFlipRate :: CatQubitNoise -> Double
phaseFlipRate n = (kappa1 n / kappa2 n) * alphaSq n

noiseBias :: CatQubitNoise -> Double  -- η = p_Z / p_X
noiseBias n = phaseFlipRate n / bitFlipRate n

-- Distinguish bias-preserving vs bias-breaking gates at type level
data GateClass = BiasPreserving | BiasBreaking
data TypedGate (g :: GateClass) where
  BPCnot   :: TypedGate 'BiasPreserving
  BPToffoli :: TypedGate 'BiasPreserving
  Hadamard :: TypedGate 'BiasBreaking
  TGate    :: TypedGate 'BiasBreaking

-- Decoder typeclass with dimensional consistency
class Decoder dec where
  type DecoderSyndrome dec :: Nat
  type DecoderCorrection dec :: Nat
  decode :: dec -> BinVec (DecoderSyndrome dec) -> BinVec (DecoderCorrection dec)

-- Resource estimation: top-level function
estimateResources
  :: Algorithm           -- Shor, Grover, etc.
  -> CatQubitNoise       -- physical noise parameters
  -> QuantumCode code    -- code family choice
  -> ResourceEstimate    -- total qubit count breakdown

data ResourceEstimate = ResourceEstimate
  { dataQubits     :: !Int
  , syndromeQubits :: !Int
  , routingQubits  :: !Int
  , factoryQubits  :: !Int
  , totalPhysical  :: !Int
  , runtime        :: !Double  -- seconds
  , logicalErrorRate :: !Double
  }
```

---

## Part 3: An 18-week implementation plan anchored by early validation

### Weeks 1–4: Algebraic foundations and first validation checkpoint

**Week 1 — GF(2) linear algebra core.** Implement `GF2`, `BinVec`, `BinMatrix` with `Word64` bit-packing. Functions: `matMulGF2`, `transpose`, `gaussianElimGF2`, `rref`, `rank`, `kernel`. Tests: QuickCheck properties for associativity, idempotence of `rref`, rank-nullity theorem. Reading: Aaronson & Gottesman (2004).

**Week 2 — Sparse matrices and code types.** Implement `SparseBinMatrix` (CSR format). Define `QuantumCode` typeclass, `CSSCode` with smart constructor enforcing H_X · H_Z^T = 0. Implement repetition code and surface code generators. Tests: CSS orthogonality for all generated codes. Reading: Tillich & Zémor (2013) on hypergraph product.

**Week 3 — Stabilizer tableau.** Implement `Tableau n` with Aaronson-Gottesman 2n×(2n+1) representation. Clifford gates: H, S, CNOT, CZ, SWAP as row operations. Measurement (random and deterministic cases). Tests: verify against known stabilizer states (Bell states, GHZ states). Reading: Gidney (2021) on Stim.

**Week 4 — Validation milestone 1.** Verify repetition code and surface code against published thresholds. Compare syndrome extraction with Stim output for small circuits (d=3,5 surface code). Implement basic Monte Carlo error sampling. **Deliverable**: reproduce surface code threshold curve at code capacity (~10.3% depolarizing).

### Weeks 5–8: Pauli frame simulation and decoders

**Week 5 — Pauli frame simulation.** Implement `PauliFrame` with 64 frames packed per `Word64`. Reference frame sampling trick: run tableau once for reference, then propagate frames via XOR. Noise injection. Reading: Gidney (2021) Stim paper Section IV.

**Week 6 — Detector error model extraction.** Annotate circuits with detectors and observables. Error propagation through inverse tableau. Hypergraph output format compatible with decoder input. Reading: Stim documentation.

**Week 7 — Belief propagation decoder.** Implement min-sum BP on Tanner graph. Handle biased noise via asymmetric channel LLRs. Convergence detection. Short-cycle awareness. Tests: decode surface code syndromes, compare with known results. Reading: Roffe et al. (2020).

**Week 8 — OSD post-processing and validation milestone 2.** Implement OSD-0 and OSD-w (order ≤ 10). BP+OSD pipeline. **Deliverable**: reproduce Roffe et al. (2020) Fig. 3 — logical error rates for hypergraph product codes under BP+OSD. Compare decoder performance with `ldpc` Python package.

### Weeks 9–12: Cat qubit noise and advanced code families

**Week 9 — Cat qubit noise model.** Implement `CatQubitNoise` with physical parameters. Error rate formulas: p_X ∝ exp(−2|α|²), p_Z ∝ κ₁|α|²T_gate. Bias-preserving CNOT channel model from Guillaud & Mirrahimi (2019). Map to PAULI_CHANNEL parameters. Reading: Puri et al. (2020).

**Week 10 — XZZX and bivariate bicycle codes.** Implement XZZX surface code generator with rectangular geometry. Implement bivariate bicycle code generator from polynomial pairs over F₂[x,y]/(x^l−1, y^m−1). Validate [[144,12,12]] code parameters. Reading: Bonilla Ataides et al. (2021), Bravyi et al. (2024).

**Week 11 — LDPC-cat and lifted product codes.** Implement LDPC-cat [165+8ℓ, 34+2ℓ, 22] family generator. Implement lifted product construction. Bias-tailored Hadamard rotation on second block. Tests: verify code distances via minimum-weight codeword search. Reading: Ruiz et al. (2025), Roffe et al. (2023).

**Week 12 — Validation milestone 3.** Reproduce Ruiz et al. (2025) key result: logical error rate ≤ 10⁻⁸ with 758 cat qubits for 100 logical qubits at ε = 0.1% phase-flip rate. Compare LDPC-cat performance with bias-tailored lifted product codes under identical noise. **Deliverable**: match published figures within statistical error bars.

### Weeks 13–16: Resource estimation and cross-architecture comparison

**Week 13 — Algorithm compilation.** Implement Toffoli-count estimation for Shor's algorithm (RSA-2048 and ECDLP-256). Translation to cat qubit gate set (Clifford + Toffoli instead of Clifford + T). Reading: Gidney & Ekerå (2025), Gouzien et al. (2023).

**Week 14 — Magic state distillation and resource estimation.** Implement unfolded distillation model (Ruiz et al., arXiv:2507.12511): 53 qubits, 5.5 rounds, output error rate 3×10⁻⁷. Implement factory footprint calculation. Total qubit assembly: data + syndrome + routing + factory. **Deliverable**: reproduce Gouzien et al. 126,133 cat qubits for ECDLP-256.

**Week 15 — Cross-architecture comparison.** Generate comparison table: surface code (Gidney 2025) vs LDPC-cat (Ruiz 2025) vs repetition-cat (Gouzien 2023) vs qLDPC/Pinnacle (Webster 2026) for RSA-2048 and ECDLP-256. Parametric plots of total qubits vs bias ratio η. Sensitivity analysis over |α|², κ₁/κ₂, gate time.

**Week 16 — Elevator and Romanesco codes.** Implement elevator code construction (Shanahan & Ruiz 2026): concatenated inner repetition + outer classical code with logical ancilla elevation. Implement Romanesco code construction (Leroux & Iverson 2025): Clifford-deformed bivariate bicycle on hexagonal lattice. Compare all code families at η = 10⁴, 10⁵, 10⁶.

### Weeks 17–18: Paper writing and packaging

**Week 17 — Performance optimization and packaging.** Profile hot loops. FFI decision: if Pauli frame simulation is >100× slower than Stim, implement C kernel for inner XOR loop. Package with Nix flake, Docker image, Hackage candidate upload. Python wrapper via `subprocess` or `cffi` for accessibility.

**Week 18 — Paper completion.** Cross-architecture comparison table as centerpiece figure. Framework contribution section with concrete bug-prevention examples. Sensitivity analysis figures. Submit to Quantum journal (preferred) or Physical Review Research.

---

## Part 4: Stabilizer simulation targeting correctness first, performance second

### 4.1 The tableau stores 2n generators in bit-packed word arrays

The Aaronson-Gottesman tableau stores **2n rows × (2n+1) columns** of binary variables. Rows 1–n are destabilizer generators; rows n+1–2n are stabilizer generators. For generator Rᵢ, bits (xᵢⱼ, zᵢⱼ) encode the jth Pauli: 00=I, 10=X, 11=Y, 01=Z. The sign bit rᵢ encodes ±1 phase. The memory layout uses three arrays: `xBits` (2n × ⌈n/64⌉ `Word64` values), `zBits` (same shape), and `signs` (2n bytes). Total memory for n qubits is approximately **n²/8 bytes** — a 1000-qubit system requires ~125 KB, fitting comfortably in L2 cache.

The initial state |0⟩^⊗n has destabilizers Rᵢ = Xᵢ (xᵢⱼ = δᵢⱼ, zᵢⱼ = 0) and stabilizers Rₙ₊ᵢ = Zᵢ (x₍ₙ₊ᵢ₎ⱼ = 0, z₍ₙ₊ᵢ₎ⱼ = δᵢⱼ), all signs zero. Including destabilizers doubles storage but eliminates O(n³) Gaussian elimination during measurement, reducing it to **O(n²) worst-case** per measurement.

### 4.2 Clifford gates operate as word-parallel row transformations

Each gate updates all 2n rows independently. Per row i for qubit a:

- **Hadamard(a)**: `rᵢ ⊕= xᵢₐ · zᵢₐ`, then swap `xᵢₐ ↔ zᵢₐ`. In bit-packed form, swap the appropriate bit in the x-word and z-word for column a.
- **S(a)**: `rᵢ ⊕= xᵢₐ · zᵢₐ`, then `zᵢₐ ⊕= xᵢₐ`.
- **CNOT(a→b)**: `rᵢ ⊕= xᵢₐ · zᵢᵦ · (xᵢᵦ ⊕ zᵢₐ ⊕ 1)`, then `xᵢᵦ ⊕= xᵢₐ` and `zᵢₐ ⊕= zᵢᵦ`.
- **CZ(a,b)**: Equivalent to H(b)·CNOT(a,b)·H(b).
- **SWAP(a,b)**: Swap columns a and b in both x-bit and z-bit sections.

Each gate is **O(n)** time. The critical inner operation `rowsum(h,i)` sets generator h := h·i (Pauli product), computing the phase via the g-function. With bit-packing, CNOT becomes XOR of entire `Word64` rows — processing 64 qubits per instruction. Stim achieves **11 bitwise SIMD operations per 256-qubit group** using AVX; a Haskell implementation with `Word64` achieves 11 operations per 64-qubit group, roughly 4× slower per gate application but sufficient for correctness-first development.

### 4.3 Pauli frame simulation amortizes tableau cost across millions of shots

Stim's reference frame sampling trick works as follows. Run the expensive O(n²)-per-gate tableau simulation **once** to produce a single noiseless reference sample of all measurement outcomes. For all subsequent samples, simulate only the difference from the reference using a Pauli frame — a single Pauli product per qubit (2 bits: x_bit, z_bit) tracking accumulated bit/phase flips. Gate propagation through frames is purely XOR: S on qubit q sets `z_q ⊕= x_q`; CNOT(a→b) sets `x_b ⊕= x_a, z_a ⊕= z_b`. No phase tracking is needed.

The data structure packs **64 independent frames into each `Word64`**, with shape [n_qubits × ⌈n_frames/64⌉]. Each gate application operates on all 64 frames simultaneously via a single XOR instruction. Noise injection samples a random Pauli from the channel distribution and XORs it into the frame. Measurement result for frame f = `reference_result ⊕ x_bit[qubit][f]`. Detector events = XOR of relevant measurement results.

### 4.4 Detector error models connect circuits to decoders

A detector error model (DEM) is extracted by annotating circuits with detectors (parity constraints on measurement outcomes that should be deterministic under noiseless conditions) and observables. Each noise process produces a set of detector flips and observable flips. The DEM is structured as `error(p) D0 D1 L0` lines — each error mechanism triggers specific detectors with probability p. This defines a **Tanner graph** (hypergraph connecting error mechanisms to detectors) that decoders consume directly. With `decompose_errors=True`, hyperedges decompose into graphlike edges (≤2 detectors) for matching-based decoders.

### 4.5 Performance targets and FFI decision criteria

**Initial targets**: handle 1000+ qubits for tableau simulation; ~10–100 shots/second for d=20 surface code Pauli frame simulation; <1ms per decode for d<20. These are **10–50× slower than Stim** but sufficient for research-quality resource estimation. The FFI decision should be made after profiling: if the inner XOR loop (Pauli frame propagation) consumes >80% of runtime and pure Haskell achieves <1% of Stim throughput, implement a thin C kernel with AVX2 intrinsics called via `unsafe` FFI. The threshold for FFI intervention is approximately **10 shots/second for d=15 surface code circuits** — below this, the simulation becomes impractical for statistical convergence at low error rates.

---

## Part 5: Three decoder tiers from simple matching to BP+OSD

### 5.1 Belief propagation handles general LDPC codes with biased noise

The min-sum variant operates on the Tanner graph of the parity check matrix H. Variable-to-check messages carry log-likelihood ratios (LLRs) initialized from the channel error probabilities. Check-to-variable messages use the min-sum approximation: the output LLR magnitude equals the minimum incoming LLR magnitude, with sign equal to the product of all incoming signs. Normalized min-sum scales by α ≈ 0.9 to compensate for over-estimation. Iteration continues until either the syndrome is satisfied or a maximum iteration count (typically 100–1000) is reached.

For biased noise, channel LLRs reflect the asymmetry directly: `LLR_init = log((1-p_Z)/p_Z)` for Z-dominated noise, where p_Z ≫ p_X. Separate binary X/Z BP (rather than quaternary GF(4) BP) is appropriate when the noise bias is large. **The short-cycle problem is fundamental for quantum LDPC codes**: CSS codes have unavoidable length-4 cycles in their Tanner graphs from the commutativity constraint H_X · H_Z^T = 0, and code degeneracy (physically different errors equivalent up to stabilizers) prevents BP from converging. Solutions include serial scheduling (sequential rather than flooding message updates), memory-enhanced/relay BP (adding memory terms from previous iterations to break symmetry traps), stabilizer inactivation, and — most practically — OSD post-processing.

### 5.2 OSD transforms BP soft information into near-optimal hard decisions

OSD-0 takes BP's soft reliability output and the syndrome as inputs. It sorts columns of H by decreasing reliability, applies Gaussian elimination to find a full-rank submatrix H_S (the "information set" of linearly independent columns), then solves ê_I = H_S⁻¹ · s for the information positions while setting redundant positions to zero. Complexity is dominated by GF(2) Gaussian elimination: **O(n³)** worst-case, but the fast-syndrome variant (Panteleev & Kalachev 2021) terminates early when the syndrome becomes linearly dependent on reduced columns.

OSD-w extends this by enumerating test error patterns (TEPs) of Hamming weight ≤ w on the redundant positions, recomputing corrections for each. OSD-CS (combination sweep) with w ≤ 10 is the standard strong baseline. BP+OSD achieves **near-maximum-likelihood performance across the entire QLDPC code landscape** — hypergraph product codes, bicycle codes, toric codes (Roffe et al., Phys. Rev. Research 2020). An alternative is Localized Statistics Decoding (LSD) by Hillmann et al. (Nature Communications, 2025), which divides decoding into local sub-problems with better parallelism while matching BP+OSD performance.

### 5.3 MWPM handles surface codes with near-linear time

PyMatching v2 implements the sparse blossom algorithm (Higgott & Gidney, Quantum 9:1600, 2025), achieving **>1 million errors corrected per core second** with nearly linear scaling T ∝ N^1.03. At 0.1% circuit noise, it decodes distance-17 surface codes in **<1 μs per syndrome round**. MWPM applies when each error flips at most 2 detectors (graphlike errors) — covering surface codes, repetition codes, and toric codes. For general QLDPC codes where errors produce hyperedges, BP+OSD is required.

The recommended implementation strategy for `qec-cat`: **FFI to PyMatching/sparse_blossom C++** for MWPM (the blossom algorithm is extremely optimized and hard to match in any language), **pure Haskell implementation of BP+OSD** (the algorithm is more naturally expressed in a functional style), and **pure Haskell Union-Find decoder** as the first decoder implemented (simpler than MWPM, effectively linear time via O(n·α(n)), and the disjoint-set data structure maps well to Haskell's `STUArray`).

---

## Part 6: Cat qubit noise exhibits exponential bit-flip suppression with linear phase-flip cost

### 6.1 Three physical parameters govern everything

A cat qubit encodes information in a superposition of two coherent states |α⟩ and |−α⟩ of a superconducting microwave resonator. Two-photon dissipation at rate κ₂ confines the oscillator to the {|α⟩, |−α⟩} manifold via the Lindblad master equation **dρ/dt = κ₂ D[â² − α²]ρ + κ₁ D[â]ρ**, where κ₁ is the single-photon loss rate. The three governing parameters are:

- **|α|²** (mean photon number): controls noise bias. Typical range 2–19.
- **κ₁** (single-photon loss rate ≈ 1/T₁): sets phase-flip error rate. Target: low as possible.
- **κ₂** (two-photon dissipation rate): sets stabilization strength and gate speed. Target: κ₂/2π ≈ 2 MHz (achieved by Marquet et al.'s AutoCat circuit, PRX 14, 2024).
- **The key figure of merit is κ₁/κ₂ ≤ 10⁻⁵** for fault-tolerant computation.

### 6.2 Exponential vs linear: the fundamental asymmetry

**Bit-flip rate**: Γ_X ∝ exp(−2|α|²). The overlap between |α⟩ and |−α⟩ decreases exponentially with photon number, providing autonomous protection. For squeezed cat qubits (Rousseau et al., February 2025), the exponent improves to exp(−2γ|α|²) with **γ = 4.3** — a 74× improvement per added photon.

**Phase-flip rate**: Γ_Z = κ₁ · |α|². Each single-photon loss event dephases the cat qubit, and the rate grows linearly with photon number because more photons mean more opportunities for loss.

**Per-gate error probability**: p_Z ≈ (κ₁/κ₂) · |α|² for gates of duration ~1/κ₂. The CNOT gate time is T_CNOT ~ π/(2κ₂|α|²). The bias-preserving CNOT error model (Puri et al., Science Advances 2020) adds independent Z errors on control and target at rate κ₁|α|²T_gate, plus small correlated ZZ terms, while X errors remain exponentially suppressed. This is the key innovation: **the CNOT gate does not introduce additional bit-flips**.

### 6.3 Bias ratios span orders of magnitude

The noise bias η = p_Z/p_X = Γ_Z/Γ_X ∝ |α|² · exp(2|α|²) grows super-exponentially. At |α|² = 4, η ≈ 10³–10⁴; at |α|² = 8, η exceeds 10⁶. For resource estimation, typical values are η = 100, 1,000, 10,000. The mapping to PAULI_CHANNEL parameters is: p_x ≈ p_y ≈ exp(−2|α|²) and p_z ≈ κ₁|α|²T, so for η = 1000 with p_total = 10⁻³, one has p_z ≈ 10⁻³ and p_x ≈ 10⁻⁶.

### 6.4 Experimental results have exceeded theoretical roadmaps

**Réglade et al. (Nature 629, 2024; arXiv:2307.06617)**: Demonstrated quantum control of a cat qubit with **bit-flip times exceeding 10 seconds** — a 4-order-of-magnitude improvement — and phase-flip times >490 ns, using a new measurement scheme requiring no ancilla transmon. Affiliation: Alice & Bob / ENS-PSL.

**Rousseau et al. (February 2025; arXiv:2502.07892)**: Squeezed cat qubit with scaling exponent **γ = 4.3** (vs γ = 2 for standard cats). Achieved **22-second bit-flip time** at n̄ = 4.1 photons (160× improvement over unsqueezed cats on the same chip) with phase-flip time 1.3 μs. Alice & Bob, 40+ authors.

**AWS Ocelot / Putterman et al. (Nature, February 2025; arXiv:2409.17556)**: First scalable cat qubit chip with 5 data cat qubits + 4 transmon ancillas. Bit-flip times approaching **1 second** at |α|² = 4. Demonstrated repetition code distances d=3 and d=5 with logical error rates **1.72%/cycle (d=3) and 1.65%/cycle (d=5)** — the improvement from d=3 to d=5 confirms sub-threshold operation for phase-flip correction. Data at Zenodo DOI:10.5281/zenodo.14257632.

**Alice & Bob (September 2025)**: Bit-flip times **exceeding 1 hour** (33–60 minutes at 95% CI) at n̄ = 11 photons on the Helium 2 chip. Z gate fidelity 94.2% in 26.5 ns. Surpasses their 2030 roadmap target of 13 minutes by 4×. Three qubits demonstrated with bit-flip times of 189, 252, and 189 minutes at 11–16 photons.

---

## Part 7: Eight code families span the bias-performance Pareto frontier

### Repetition, surface, and XZZX codes form the baseline tier

The **repetition code** [n,1,n] is the infinite-bias limit of all topological codes. With weight-2 stabilizers and nearest-neighbor connectivity, it corrects only phase-flip errors — ideal when bit-flips are exponentially suppressed. The **surface code** [[d²,1,d]] (rotated: [[(d²+1)/2,1,d]]) with weight-4 stabilizers achieves circuit-level threshold ~0.5–1.1% under depolarizing noise. Google's Willow processor (Nature 638, 2025) demonstrated below-threshold operation with d=7, achieving error suppression factor Λ = 2.14±0.02.

The **XZZX surface code** (Bonilla Ataides et al., Nature Communications 2021; arXiv:2009.07851) replaces separate all-X and all-Z stabilizers with uniform XZZX operators. Under Z-biased noise, Z errors confine to one-dimensional strings, enabling 1D repetition-code decoding along each line independently. In the infinite-bias limit, threshold reaches **50%**. Finite-bias logical failure rate scales as O((p/√η)^{d/2}), improving by η^{−d/4} over unbiased codes. With rectangular geometry (d_X ≪ d_Z), overhead drops dramatically.

### Bivariate bicycle codes achieve 8% encoding rate

Bravyi et al. (Nature 627, 2024; arXiv:2308.07915) construct CSS codes from bivariate polynomial quotient rings R = F₂[x,y]/(x^l−1, y^m−1). Two polynomials a(x,y), b(x,y) ∈ R define H_X = [a | b] and H_Z = [b^T | a^T]. The flagship **[[144,12,12]] code** encodes 12 logical qubits in 144 physical qubits (rate **8.3%**, vs ~0.08% for surface code at same parameters). All check operators are weight-6; syndrome extraction requires only 7 CNOT layers regardless of code size. Circuit-level threshold is **~0.8%** under BP+OSD — on par with the surface code despite encoding 12× more logical qubits. Open source: github.com/sbravyi/BivariateBicycleCodes.

### LDPC-cat codes combine classical LDPC with noise bias for radical overhead reduction

Ruiz et al. (Nature Communications 16, 2025; arXiv:2401.09541) exploit the key insight that when cat qubits suppress bit-flips exponentially, the quantum code reduces to a **classical LDPC code correcting only phase-flips**. The [165+8ℓ, 34+2ℓ, 22] family achieves rate approaching 1/4 for large ℓ. At ℓ=33, the [429, 100, 22] code encodes 100 logical qubits in 429 data qubits. With 758 total cat qubits (including ancillas), this achieves **logical error rate ≤ 10⁻⁸ per cycle** at physical phase-flip probability ε = 0.1%. The codes have cellular automaton structure (each parity check row is a shifted version of the previous), enabling systematic parameterized families. 2D layout with short-range interactions is hardware-compatible. Open source: github.com/DiegoRuiz-Git/LDPCat.

### Elevator codes outperform at high bias, Romanesco codes exploit fractal structure

**Elevator codes** (Shanahan & Ruiz, arXiv:2601.10786, January 2026) concatenate inner repetition codes with an outer classical code. A logical ancilla qubit "elevates" through a vertical stack of repetition code blocks via transversal CNOTs, extracting bit-flip syndrome information. They outperform rectangular surface and XZZX codes for bias **η ≥ 7×10⁴**, reducing qubit overhead by **>50%** at p_Z = 10⁻³ and η = 10⁵. Not yet peer-reviewed.

**Romanesco codes** (Leroux & Iverson, arXiv:2506.00130, May 2025) are Clifford-deformed bivariate bicycle codes on hexagonal lattices. Each quantum stabilizer multiplies an all-X stabilizer from one cellular automaton code with an all-Z stabilizer from another, producing non-CSS, self-dual codes. In the infinite-bias limit, they decouple into two independent classical CA codes, yielding **distance scaling beyond what 2D topological quantum codes can achieve**. Named after the fractal vegetable due to the underlying CA structure.

**Bias-tailored lifted product codes** (Roffe et al., Quantum 7, 2023; arXiv:2202.01702) apply Hadamard rotation on the second block of N/2 qubits in a lifted product code, converting CSS stabilizers to mixed XZZX type. A [[416,18,≤20]] example achieves effective X-distance 26 at infinite bias. Under biased noise, error suppression improves by **orders of magnitude** compared to the un-tailored version. Standard lifted product codes actually degrade with increasing bias — bias-tailoring reverses this. Open source: github.com/quantumgizmos/bias_tailored_qldpc.

---

## Part 8: Resource estimation reveals order-of-magnitude advantages for biased-noise architectures

### 8.1 Algorithm compilation targets Toffoli count as the binding constraint

For **RSA-2048**, Gidney (arXiv:2505.15917, May 2025) achieves **<1M physical qubits** using surface codes, down from 20M in the 2019 estimate. Key innovations: approximate residue arithmetic (replacing modular arithmetic with residue number system using small primes), yoked surface codes (3× idle qubit density), and magic state cultivation (growing T/CCZ states almost as cheaply as CNOTs). The Toffoli count is ~6.5×10⁹. Physical assumptions: p = 10⁻³, 1 μs cycle time, nearest-neighbor connectivity.

For **ECDLP-256**, Gouzien et al. (PRL 131, 2023; arXiv:2302.06639) estimate **126,133 cat qubits** in 9 hours. The Toffoli count formula is 448n³·log₂(n) + 4090n³ for n-bit ECDLP. Physical assumptions: κ₁/κ₂ = 10⁻⁵, 500 ns cycle time, n̄ = 19 photons. From the same architecture, RSA-2048 requires **~350,000 cat qubits in 4 days**. Open source: github.com/ElieGouzien/elliptic_log_cat.

For cat qubit architectures, the gate set is **Clifford + Toffoli** rather than Clifford + T. The bias-preserving Toffoli is native (no distillation needed for the non-Clifford gate itself), but Hadamard gates require magic state consumption via gate teleportation. Translation from standard circuits maps T gates to Toffoli-based implementations.

### 8.2 Unfolded distillation slashes magic state factory cost

Ruiz et al. (arXiv:2507.12511, July 2025) introduce unfolded distillation for biased-noise qubits, operating at the **physical level** rather than the logical level. The protocol uses only **53 physical qubits** and **5.5 error correction rounds** to produce magic states with output error rate **3×10⁻⁷**, assuming bias η ≥ 5×10⁶ and phase-flip rate 0.1%. This achieves **>10× volume reduction** versus magic state cultivation (unbiased qubits) and **>100× reduction** versus standard distillation. The factory footprint is a small fraction of the total qubit budget — a decisive advantage for cat qubit architectures.

### 8.3 Routing and layout differ fundamentally between surface and LDPC architectures

Surface codes use **lattice surgery** for logical operations: merging and splitting code patches to implement logical CNOT, which requires ancilla patches roughly doubling the qubit count. Routing uses dedicated bus qubits between patches. LDPC codes change this picture: multiple logical qubits per code block (k > 1) means operations happen **within blocks** rather than between patches, dramatically reducing routing overhead. The Pinnacle architecture (Webster et al., arXiv:2602.11457, February 2026) achieves **<100,000 physical qubits for RSA-2048** using generalized bicycle qLDPC codes with distances 10 ≤ d ≤ 24. Key innovations include magic engines (single code blocks simultaneously performing distillation and injection, producing one magic state per logical cycle) and Clifford frame cleaning (enabling parallel access to shared quantum memory).

### 8.4 Total qubit count assembly

The resource estimation function computes:

```
total = data_qubits(code, n_logical)
      + syndrome_qubits(code, d)
      + routing_qubits(layout, connectivity)
      + factory_qubits(distillation_protocol, toffoli_rate)
```

|Architecture|RSA-2048|ECDLP-256|Code|Key Assumption|
|---|---|---|---|---|
|Surface code (Gidney 2025)|<1,000,000|—|Surface|p=10⁻³, 1μs cycle|
|Repetition-cat (Gouzien 2023)|~350,000|126,133|Repetition|κ₁/κ₂=10⁻⁵, 500ns|
|LDPC-cat (Ruiz 2025)|~100,000 est.|—|[165+8ℓ,34+2ℓ,22]|ε=0.1% phase-flip|
|qLDPC/Pinnacle (Webster 2026)|<100,000|—|GB codes|p=10⁻³, 1μs cycle|

---

## Part 9: Paper strategy targets the cross-architecture comparison as centerpiece

### Target results

The paper's primary contribution is a **cross-architecture comparison table** for ECDLP-256 and RSA-2048 across surface code, repetition-cat, LDPC-cat, elevator code, and qLDPC (Pinnacle-style) architectures, all computed within a single unified framework. Parametric plots of total physical qubits vs bias ratio η (from 1 to 10⁶) reveal the crossover points where biased-noise architectures overtake standard approaches. Sensitivity analysis varies |α|², κ₁/κ₂, and gate time to identify which physical parameters most constrain performance. The elevator code crossover at η ≥ 7×10⁴ (Shanahan & Ruiz 2026) should be reproducible.

### Framework contribution: type safety preventing specific bug classes

Concrete examples of bugs prevented by the type system strengthen the paper's software engineering contribution:

- **Dimension mismatch**: `matMulGF2 :: BinMatrix m k -> BinMatrix k n -> BinMatrix m n` makes multiplying incompatible matrices a compile-time error.
- **CSS orthogonality violation**: Smart constructor `mkCSSCode` rejects H_X, H_Z pairs where H_X · H_Z^T ≠ 0 — a bug that silently produces invalid codes in untyped frameworks.
- **X/Z confusion**: Phantom types `Stabilizer XType` vs `Stabilizer ZType` prevent applying X corrections from Z syndrome decoding — a common source of logical error rate anomalies.
- **Bias-breaking gate misuse**: `TypedGate 'BiasPreserving` vs `TypedGate 'BiasBreaking` forces explicit handling when a circuit includes Hadamard gates, preventing accidental bias assumption violations in noise modeling.

### Target venue

**Quantum** (quantum-journal.org) is the preferred venue — it publishes both theoretical QEC and software contributions, has published Roffe et al.'s bias-tailored lifted product codes and Stim. **Physical Review Research** is the backup venue for the resource estimation component if the software framework contribution is deprioritized.

---

## Part 10: Risk assessment identifies performance and scope as primary threats

### Performance risks are manageable with FFI escape hatches

Pure Haskell stabilizer simulation will be **10–50× slower than Stim** due to lack of AVX2 SIMD vectorization and GHC's garbage collector overhead. This is acceptable for research-quality resource estimation where statistical convergence requires ~10⁴–10⁶ shots (achievable in hours rather than minutes). The FFI decision criterion is clear: if profiling shows the Pauli frame XOR inner loop consuming >80% of runtime with <10 shots/second at d=15, implement a ~50-line C kernel with AVX2 intrinsics via `unsafe` FFI. The overhead of `unsafe` FFI is negligible (benchmarked at ~2.4ns per call). An alternative mitigation is to use Stim itself via FFI for circuit simulation while keeping the Haskell type system for code construction, noise modeling, and resource estimation — the "typed frontend, fast backend" pattern.

### Scope risks: the minimum viable library

The minimum viable library for the paper requires: (1) GF(2) linear algebra with bit-packing, (2) code generators for repetition, surface, XZZX, LDPC-cat, and at least one qLDPC family, (3) BP+OSD decoder, (4) cat qubit noise model, and (5) resource estimation pipeline producing the cross-architecture comparison table. Stabilizer tableau simulation and Pauli frame simulation can be deferred if code-capacity noise simulations (rather than full circuit-level noise) provide sufficient validation against published results. The 18-week timeline has **2 weeks of buffer** (weeks 16–17 can be shortened if elevator/Romanesco codes are deprioritized).

### Correctness risks require validation against Python reference implementations

Every major result must be validated against existing Python tools: Stim for circuit simulation outputs, the `ldpc` package (Roffe) for BP+OSD decoder accuracy, the `LDPCat` repository (Ruiz) for LDPC-cat code performance, and `elliptic_log_cat` (Gouzien) for resource estimation numbers. The validation milestones at weeks 4, 8, and 12 are designed to catch discrepancies early. The highest correctness risk is in the decoder implementation — off-by-one errors in GF(2) Gaussian elimination for OSD can silently degrade logical error rates without producing obviously wrong results. QuickCheck property tests (idempotence of `rref`, rank-nullity theorem, ∂² = 0) provide the primary defense.

### Adoption risks have established mitigation paths

Non-Haskell users face installation friction. Mitigation: Nix flake via `haskell-flake` (single `nix develop` command), Docker image with pre-built binary, and statically linked Linux binary via musl (eliminates all runtime dependencies). A thin Python wrapper using `subprocess` to call the Haskell binary with JSON input/output enables integration with existing Python QEC workflows. For maximum reach, publish both on Hackage (for Haskell users) and as a standalone CLI tool with Docker (for everyone else). **Speculative risk**: if the paper is rejected on the grounds that a Haskell framework is insufficiently accessible, the Python wrapper and comparison benchmarks against existing tools should address this concern.

### Open design decisions flagged for resolution

Several architectural choices remain open and should be resolved during weeks 1–3:

- **Type-level vs runtime code parameters**: Encoding (n,k,d) at the type level via `TypeLits` provides stronger guarantees but increases compilation time and complicates code families with runtime-determined parameters (e.g., LDPC-cat parameterized by ℓ). The recommended hybrid: use phantom types for X/Z distinction (zero cost), `KnownNat` for fixed dimensions known at compile time, and smart constructors with runtime validation for parameterized families.
- **Dense vs sparse matrix representation**: LDPC codes have sparse parity check matrices (weight 4–6), but Pauli frame simulation operates on dense bit vectors. The library should support both, with explicit conversion functions.
- **Stim FFI vs pure Haskell simulation**: Using Stim via FFI for circuit-level noise simulation would dramatically accelerate development but introduces a C++ dependency. Pure Haskell simulation maintains self-containment. The recommended approach: implement pure Haskell for correctness validation, with optional Stim FFI for performance-critical bulk sampling.
- **Decoder choice for LDPC-cat codes**: BP+OSD is the established decoder but may be overkill for the high-bias regime where the effective decoding problem is essentially classical LDPC. A simpler classical BP decoder without OSD may suffice when η > 10⁴, reducing implementation complexity.

---

## Conclusion

The `qec-cat` project is tractable within 18 weeks because the Haskell ecosystem provides adequate foundations (bit-packed `Word64` arithmetic, `parallel` strategies, `GHC.TypeLits`) and the performance-critical path has a clear FFI escape hatch. The type system architecture — combining phantom types for X/Z stabilizer distinction, `KnownNat` for dimensions, smart constructors for CSS orthogonality, and GADTs for bias-preserving gate classification — eliminates bug classes that silently corrupt results in untyped Python frameworks. The cross-architecture comparison table, spanning surface codes at ~1M qubits (Gidney 2025), repetition-cat at ~126K (Gouzien 2023), LDPC-cat at ~758 (Ruiz 2025), and qLDPC at <100K (Webster 2026), will be the first unified analysis across all major QEC paradigms for cryptographic workloads. The most significant open question is whether cat qubit hardware can sustain κ₁/κ₂ ≤ 10⁻⁵ at scale — Alice & Bob's hour-long bit-flip times at n̄ = 11 suggest this is achievable, but no multi-qubit chip has yet demonstrated the combined noise parameters assumed by resource estimates.