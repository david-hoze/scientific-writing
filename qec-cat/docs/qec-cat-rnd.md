# Research and Implementation Plan: `qec-cat` — Quantum Error Correction Resource Estimation for Biased-Noise Architectures

## 1. Executive summary

**Cat qubits combined with LDPC codes represent the most promising path to low-overhead fault-tolerant quantum computing, yet no rigorous cross-architecture resource estimation exists for this combination applied to real algorithms.** This project delivers two artifacts: a Haskell library (`qec-cat`) implementing the full pipeline from code construction to resource estimation with end-to-end type safety, and a research paper presenting novel cross-architecture resource estimates comparing cat-qubit + LDPC architectures against surface codes and qLDPC codes for cryptanalysis and quantum chemistry benchmarks.

The core scientific gap is clear. Gouzien et al. (PRL 2023) showed 256-bit ECDLP requires **126,133 cat qubits** using repetition codes. Ruiz et al. (Nature Communications 2025) demonstrated **758 cat qubits** suffice for 100 logical qubits of quantum memory using LDPC codes — a **5× improvement** in encoding rate over repetition codes. But nobody has combined LDPC-cat codes with the unfolded distillation protocol (Ruiz et al., arXiv:2507.12511, July 2025) to estimate full computation costs for cryptanalysis. Meanwhile, the Pinnacle Architecture (Webster et al., arXiv:2602.11457, February 2026) claims **<100,000 physical qubits** for RSA-2048 using qLDPC codes, and Gidney (arXiv:2505.15917, May 2025) achieves **<1 million qubits** with surface codes. A fair cross-architecture comparison — with identical algorithmic inputs and noise assumptions — does not exist.

The Haskell library fills a second gap: no QEC resource estimation tool exists in Haskell, despite the language's exceptional suitability for type-safe mathematical programming. The library will encode `[[n,k,d]]` code parameters at the type level, implement GF(2) linear algebra with bit-packed Word64 representations, and provide BP+OSD decoding with biased-noise priors. The research paper targets Quantum or Physical Review Research, presenting the first LDPC-cat resource estimates for RSA-2048 and 256-bit ECDLP alongside surface-code and qLDPC baselines.

**Expected outcomes:** (1) A reusable, extensible Haskell library for QEC resource estimation. (2) A paper demonstrating that LDPC-cat codes with unfolded distillation reduce physical qubit counts by **3–10×** over repetition-cat codes for cryptanalysis workloads, potentially reaching **<40,000 cat qubits** for 256-bit ECDLP.

---

## 2. Ecosystem assessment: Haskell libraries and tooling

### Binary linear algebra over GF(2) demands a custom bit-packed approach

No existing Haskell library provides production-quality GF(2) matrix operations suitable for QEC. The landscape includes `galois-field` (v1.0.2, actively maintained, wraps Integer arithmetic with modular reduction — correct but slow), `mod` by Bodigrim (`Data.Mod.Word` variant uses CPU-native Word arithmetic, **5×+ faster** than alternatives for element-level operations), and `linear-code` (v0.2.0, implements classical linear codes over F₂ with Gaussian elimination but uses naive non-packed representation). The `hmatrix` library wraps BLAS/LAPACK for floating-point only. Edward Kmett's `linear` works over polymorphic fields but supports only small fixed-size vectors (V2, V3, V4).

**The recommended approach is custom bit-packed GF(2) using `Data.Vector.Unboxed` with `Word64`.** Each bit stores one GF(2) element, yielding 64 elements per machine word. A row of a 2n-column symplectic matrix for n qubits requires ⌈2n/64⌉ words. Addition becomes XOR, inner products become AND+popCount, and Gaussian elimination operates on packed words with O(n/64) cost per row operation versus O(n) for unpacked representations. This is precisely how Google's Stim achieves its performance in C++.

**No Haskell library implements symplectic matrices or the symplectic inner product over GF(2).** This is entirely greenfield. The symplectic form Ω = [[0, Iₙ], [Iₙ, 0]] and operations like commutativity checking (∀g₁,g₂ ∈ S: g₁ Ω g₂ᵀ = 0 mod 2) must be built from scratch.

### SIMD and FFI provide a realistic path to C++-competitive performance

GHC 9.12 (December 2024) added **128-bit SIMD support** in the native code generator for x86, supporting `Int64X2#` packing/unpacking. GHC 9.14 extends this to all 128-bit integer vector types with arithmetic operations. However, **256-bit AVX2 and 512-bit AVX-512 still require the LLVM backend**, and GHC does not auto-vectorize.

For the critical inner loop — XOR of two bit-packed rows during Gaussian elimination or syndrome computation — the performance gap between scalar Haskell (`Data.Vector.Unboxed.zipWith xor`) and scalar C is **within 10–20%** at `-O2`. SIMD-vectorized C with AVX2 `vpxor` on 256-bit chunks delivers an additional **4× speedup** over scalar. The practical strategy: implement pure Haskell defaults with optional C kernels via `unsafe` FFI calls for hot paths. Benchmark data from the `dyu/ffi-overhead` project shows FFI overhead at **~2.4 ns per call** (1197 ms vs 1182 ms for 500M calls), making fine-grained FFI to C SIMD kernels viable.

The library should ship with a `cbits/` directory containing AVX2 kernels for row-XOR, syndrome extraction, and Hamming weight computation, enabled via a Cabal flag (`-f simd`).

### Parallelism and streaming Monte Carlo are well-supported

Three complementary approaches cover the parallelism needs:

- **`async`** (Simon Marlow, mature): `mapConcurrently` over independent Monte Carlo seeds provides embarrassingly parallel sampling with minimal boilerplate. GHC's runtime handles millions of lightweight green threads multiplexed onto OS threads via `-N`.
- **`streamly`** (v0.11.0, Composewell, actively maintained): High-performance streaming with declarative concurrency. `S.parMapM` distributes work across all cores with stream fusion yielding C-like serial performance. Ideal for streaming Monte Carlo: generate error samples concurrently, fold into logical error rate estimates.
- **`par`/`pseq`** (built into GHC): `parMap rdeepseq sampleFunction seeds` parallelizes pure computations via work-stealing. Best for pure functional Monte Carlo where results are collected lazily.

For parallel random number generation, `splitmix` (the default GHC random generator since `random-1.2`) provides statistically independent parallel streams via `splitSMGen`.

### Type-level programming encodes code parameters with full safety

GHC's `TypeLits` module provides type-level natural numbers (`Nat` kind) with `KnownNat` constraints and arithmetic type families (`+`, `*`, `^`, `<=`). The `ghc-typelits-natnormalise` plugin teaches GHC commutativity and associativity of type-level arithmetic, and `ghc-typelits-knownnat` derives `KnownNat` instances for compound expressions. The `singletons` library family (v3.5.1, maintained by Richard Eisenberg) bridges type-level and term-level code, enabling runtime construction of type-safe codes from dynamic input.

A stabilizer code can be encoded as:

```haskell
data StabilizerCode (n :: Nat) (k :: Nat) = StabilizerCode
  { stabGens   :: Matrix (n - k) (2 * n) GF2
  , logicalOps :: Matrix (2 * k) (2 * n) GF2 }
```

The `matrix-sized` package provides `SparseMatrix r c v a` with type-level dimensions and CSR representation, offering a starting point for sparse parity-check matrices. Full dependent types remain future work in GHC, but `KnownNat` + singletons suffice for this project.

### The QEC space in Haskell is completely open

**No Haskell library exists for stabilizer simulation, QEC analysis, or resource estimation.** Quipper (v0.8.2) is largely unmaintained (copyright 2011–2016, requires outdated GHC) and focuses on circuit generation, not QEC. The `HaskellForMaths` package provides `F2` elements used by `linear-code`. This emptiness is an opportunity: `qec-cat` would be the first Haskell QEC library.

### Testing, benchmarking, and project structure

**Property testing** with QuickCheck and Hedgehog under the `tasty` framework (v1.5.3) enables algebraic invariant testing: symplectic preservation (`M Ω Mᵀ = Ω mod 2`), stabilizer commutativity, code distance bounds. **Benchmarking** with `tasty-bench` (16× faster build than Criterion, CPU-time default) provides regression testing for GF(2) operations. **HPC** (built into GHC) tracks expression-level coverage. The project uses `cabal-version: 3.0` with separate library, test-suite, and benchmark stanzas, `c-sources: cbits/gf2_simd.c`, and `cc-options: -mavx2 -O3`.

---

## 3. Cat qubit physics and the noise model

### Exponential bit-flip suppression creates extreme noise bias

A cat qubit is a bosonic mode stabilized by an engineered two-photon driven-dissipative process with Lindblad jump operator L₂ₚₕ = √κ₂(â² − α²). The computational basis states |0⟩ and |1⟩ are coherent state superpositions |±α⟩, separated by distance 2|α| in phase space. The dominant error channel is single-photon loss at rate κ₁.

The fundamental noise asymmetry arises from the structure of the code space. **Bit-flip rate** Γ_X ∝ κ₁|α|² exp(−2|α|²) is exponentially suppressed with mean photon number |α|². **Phase-flip rate** Γ_Z ∝ κ₁|α|² grows linearly. The noise bias η = Γ_Z/Γ_X ∝ exp(2|α|²) can reach **10³ to 10⁶** for practical photon numbers of 5–20. This asymmetry is the key resource: it converts a quantum error correction problem into a largely classical one, since only phase flips need active correction.

Puri et al. (Science Advances, 2020) proved that a bias-preserving CX gate is possible with cat qubits but **not** with strictly two-level systems. The gate exploits a topological phase from rotation in phase space. Under bias η = 10⁴, the fault-tolerant threshold rises to **0.43%** (2× improvement), and Clifford overhead drops by **5×**. The complete bias-preserving gate set includes X, Z(θ), ZZ(θ), CX, and CCX (Toffoli). Critically, the T gate and Hadamard are **not** bias-preserving and require magic state distillation.

### Experimental milestones have accelerated dramatically

Experimental progress has been extraordinary. Réglade et al. (Nature, 2024) demonstrated **>10 seconds** bit-flip time with quantum control, a four-order-of-magnitude improvement. Rousseau et al. (February 2025) introduced squeezed cat qubits with scaling exponent γ = 4.3 (versus ~2 for standard cats), achieving **22-second bit-flip time** at only 4.1 photons with **no increase in phase-flip rate**. AWS's Ocelot chip (Putterman et al., Nature, February 2025) demonstrated a 5-cat-qubit distance-5 repetition code with **below-threshold** phase-flip correction and bit-flip times approaching 1 second. Most strikingly, Alice & Bob's Galvanic Cat design (September 2025) achieved **>1 hour bit-flip time** at 11 photons — exceeding their 2030 roadmap target by 4×. On the Helium 2 multi-qubit chip, three qubits showed bit-flip times of **189, 252, and 189 minutes** at 11–16 photons.

### Noise model parameterization for `qec-cat`

The library's `CatQubitNoise` type must capture:

|Parameter|Symbol|Typical range|Source|
|---|---|---|---|
|Single-photon loss rate|κ₁|Hardware-dependent|Measured|
|Two-photon dissipation rate|κ₂|~2π × 2 MHz|Réglade 2024|
|Loss ratio|κ₁/κ₂|10⁻⁵ to 10⁻³|Design target|
|Mean photon number|\|α\|²|2–20|Tunable|
|Phase-flip probability per cycle|p_Z|~κ₁\|α\|² × t_cycle|Derived|
|Bit-flip probability per cycle|p_X|~κ₁\|α\|² exp(−2\|α\|²) × t_cycle|Derived|
|Noise bias|η|10² to 10⁶+|Derived|
|Squeezing parameter|γ|2 (standard), 4.3 (squeezed)|Rousseau 2025|
|Cycle time|t_cycle|500 ns (target)|Gouzien 2023|

The key design insight: noise model parameters should be phantom-typed by architecture variant (`Standard`, `Squeezed`, `Galvanic`) to prevent mixing incompatible assumptions at the type level.

---

## 4. Code constructions: from repetition codes to Romanesco

### The repetition code baseline and its limitations

The simplest QEC approach for cat qubits is a 1D repetition code correcting only phase flips. Gouzien et al. (PRL 2023) used this architecture to estimate **126,133 cat qubits** for 256-bit ECDLP in 9 hours, assuming κ₁/κ₂ = 10⁻⁵, 500 ns cycle time, and 19 mean photons. The same paper estimates **~350,000 cat qubits** for RSA-2048 factoring. The associated Python code at `github.com/ElieGouzien/elliptic_log_cat` implements the full optimization (5 stars, 17 commits). The repetition code has rate k/n = 1/d, which becomes a bottleneck at high distances.

### LDPC-cat codes achieve 5× better encoding rates

Ruiz et al. (Nature Communications, January 2025) replaced the repetition code with classical LDPC codes, achieving **100 logical qubits on 758 cat qubits** (εL ≤ 10⁻⁸) using a [165+8ℓ, 34+2ℓ, 22] code family with cellular automaton structure. The key insight: since cat qubits suppress bit flips, only phase-flip correction is needed, and this is a purely **classical** coding problem. Classical LDPC codes — unlike quantum LDPC codes — can be implemented with 2D-local connectivity by folding a 1D chain into a snake pattern and adding connections between columns. The code repository at `github.com/DiegoRuiz-Git/LDPCat` includes Jupyter notebooks, Python scripts, and C++ components for Monte Carlo simulation (3 stars, 2 forks).

### Bias-tailored quantum LDPC codes extend to moderate bias

Roffe et al. (Quantum, 2023) introduced bias-tailored lifted product codes, applying a Hadamard rotation to one block of qubits to convert CSS stabilizers into mixed XZZX-type stabilizers. Under infinite Z-bias, the code decouples into two independent classical codes, and the effective distance can significantly exceed the depolarizing distance. A [[416, 18, d≤20]] code achieves orders-of-magnitude improvement in word error rate under increasing bias. The code is available at `github.com/quantumgizmos/bias_tailored_qldpc`.

### Bivariate bicycle codes and the Pinnacle Architecture

Bravyi et al. (Nature, 2024) introduced bivariate bicycle (BB) codes — a CSS construction over the group algebra of ℤ_r × ℤ_s. The flagship **[[144, 12, 12]] code** encodes 12 logical qubits in 288 total qubits (including ancillas), achieving **~10× better efficiency** than surface codes with weight-6 stabilizers and a biplanar layout. The Pinnacle Architecture (Webster et al., February 2026) builds on generalized bicycle codes with novel "magic engines" that simultaneously perform distillation and injection, achieving **<100,000 physical qubits** for RSA-2048 at p = 10⁻³ — approximately **10× fewer** than Gidney's surface-code estimate.

### Romanesco and Elevator codes target extreme bias

**Romanesco codes** (Leroux & Iverson, arXiv:2506.00130, May 2025) are Clifford-deformed bivariate bicycle codes on a hexagonal lattice that decouple into independent cellular automaton codes under strong bias. Their distance scaling exceeds any 2D topological quantum code in the biased limit due to fractal-like logical operators. **Elevator codes** (Shanahan & Ruiz, arXiv:2601.10786, January 2026) use two-level concatenation with a "logical elevator" ancilla moving through repetition code columns. At η ≥ 7×10⁴ and p_Z = 10⁻³, elevator codes reduce overhead by **>50%** versus XZZX surface codes for target logical error rate 10⁻¹².

### Code family hierarchy for the library

The library should implement a typeclass hierarchy:

```
QECCode
├── CSSCode
│   ├── RepetitionCatCode
│   ├── LDPCCatCode (Ruiz et al.)
│   └── BivariateBicycleCode (Bravyi et al.)
├── NonCSSCode
│   ├── BiasTailoredLiftedProduct (Roffe et al.)
│   ├── RomanescoCode (Leroux & Iverson)
│   └── ElevatorCode (Shanahan & Ruiz)
└── ConcatenatedCode (inner × outer)
```

Each code family implements: `codeParameters :: (Int, Int, Int)` for [[n,k,d]], `parityCheckX :: SparseMatrix GF2`, `parityCheckZ :: SparseMatrix GF2`, `logicalOperators :: LogicalOps`, and `syndromeExtractionCircuit :: NoiseModel -> StimCircuit`.

---

## 5. Decoders: BP+OSD with biased-noise priors

### Belief propagation adapted for asymmetric channels

The BP+OSD decoder (Roffe et al., Physical Review Research, 2020) is the workhorse for quantum LDPC codes. Belief propagation passes log-likelihood ratio (LLR) messages between variable and check nodes on the code's Tanner graph. For biased noise with error probabilities (p_X, p_Y, p_Z), the initial LLRs are:

LLR_Z = log(p_no_error / p_Z) — small for dominant Z errors, driving the decoder's attention. LLR_X = log(p_no_error / p_X) — large when X errors are rare (high bias).

Under extreme cat-qubit bias (η ≥ 10⁴), the decoder effectively ignores X errors and focuses entirely on phase-flip correction, reducing the decoding problem to classical LDPC decoding. BP runs for at most n iterations (block length) with product-sum or min-sum (with scaling factor ~0.625) update rules.

### Ordered Statistics Decoding rescues BP failures

When BP fails to converge (H·e_BP ≠ s, common due to short cycles in quantum Tanner graphs), OSD post-processing applies GF(2) Gaussian elimination on the most reliable bits (ranked by BP soft output), then optionally searches over unreliable bit combinations. **OSD-0** (Gaussian elimination only) costs O(n³); **OSD-w** (order w) costs O(n^(w+2)). The combination achieves **~9.9% threshold** for toric codes, approaching MWPM performance. Recent work on **BP+LSD** (Localized Statistics Decoding, arXiv:2406.18655) provides a parallelizable alternative to OSD with similar performance.

The reference implementation is Roffe's `ldpc` Python/C++ library (`github.com/quantumgizmos/ldpc`), implementing BP (product-sum, min-sum), OSD-0 with fast-syndrome optimization, and BP+LSD.

### Haskell implementation strategy for decoders

The decoder module should implement:

1. **BP engine**: Sparse matrix-vector operations on the Tanner graph. Use `IntMap`-based adjacency lists for the bipartite graph. LLR messages stored in unboxed `Double` vectors. The iterative nature maps naturally to a `State`-monad loop with early termination on convergence.
    
2. **OSD post-processor**: GF(2) Gaussian elimination on the bit-packed parity-check matrix (reusing the core GF(2) linear algebra module). Sorting by reliability via `Data.Vector.Algorithms.Intro`.
    
3. **Biased-noise channel model**: A `ChannelModel` type parameterized by per-qubit error probabilities. For cat qubits, this derives from the noise model in Section 3.
    
4. **FFI fallback**: For production Monte Carlo at scale (millions of decoding rounds), optionally call Roffe's `ldpc` C++ library via FFI, or Stim for syndrome generation. The Haskell decoder serves as a reference implementation with full type safety and property-testable algebraic correctness.
    

Property tests should verify: syndrome consistency (H·e = s for generated errors), decoder idempotency on valid codewords, and statistical convergence of logical error rates to known thresholds for well-studied codes (e.g., repetition code threshold = 11% for depolarizing noise).

---

## 6. Stabilizer simulation and syndrome sampling

### The Aaronson-Gottesman tableau is the core data structure

The CHP algorithm (Aaronson & Gottesman, Physical Review A, 2004) represents an n-qubit stabilizer state as a 2n × (2n+1) binary matrix: n stabilizer generators and n destabilizer generators, each an n-qubit Pauli string encoded as 2n bits (X-part|Z-part) plus a phase bit. Clifford gates (CNOT, H, S) update the tableau in **O(n)** per gate by modifying columns. Measurements take **O(n²)** per measurement.

Gidney's Stim (Quantum, 2021) improves this with three innovations: (1) tracking the **inverse tableau** makes deterministic measurements O(n) instead of O(n²); (2) cache-friendly data layout with AVX SIMD yields ~0.01 cycles per qubit per Clifford gate; (3) **Pauli frame sampling** generates a reference sample via full tableau simulation, then propagates batches of 1024 error frames through the circuit using only bitwise operations, reducing per-sample cost from O(n²) to **O(1)** per gate.

### Haskell tableau implementation

The `qec-cat` tableau should use the bit-packed GF(2) matrix representation from Section 2. A 100-qubit stabilizer state requires a 200 × 201 binary matrix = 200 rows × ⌈201/64⌉ = 200 × 4 = 800 Word64s = **6.4 KB** — easily fitting in L1 cache. Gate application modifies columns via bitwise operations on packed words. The inverse-tableau trick requires O(n) row operations per gate instead of per measurement.

For Pauli frame simulation, represent a batch of 64 error frames as a single matrix where each bit position in a Word64 corresponds to one frame. This gives **64× throughput** per instruction, approaching Stim's 1024× SIMD throughput via AVX at about 1/16 the hardware utilization — a reasonable Haskell-native performance point.

The output format should be compatible with Stim's **detector error model** (DEM) — a weighted hypergraph connecting detectors to logical observables — enabling interoperability with existing decoder ecosystems (PyMatching, ldpc).

---

## 7. Resource estimation pipeline

### The five-stage pipeline from algorithm to physical qubits

The resource estimation pipeline follows the structure validated by Azure QRE and Qualtran:

**Stage 1: Logical resource specification.** Input: logical qubit count, Toffoli/T-gate count, circuit depth, and target logical error rate. For RSA-2048, Gidney (2025) uses ~1,400 logical qubits and ~6.5 × 10⁹ Toffoli gates. For 256-bit ECDLP, Gouzien et al. use Shor's algorithm with Montgomery arithmetic.

**Stage 2: Code selection and distance optimization.** Given the physical noise model (cat qubit parameters) and target logical error rate, determine the optimal code family and distance. For LDPC-cat codes, the logical error rate per cycle per logical qubit is εL ≈ A × (p_Z / p_th)^((d+1)/2), where p_th is the code threshold. The distance d is the smallest integer satisfying εL × (total operations) < error budget.

**Stage 3: Magic state factory design.** The T gate (or Toffoli for cat qubits) requires magic state distillation. The unfolded distillation protocol (Ruiz et al., July 2025) uses **53 cat qubits** and **5.5 QEC rounds** to produce one magic state at logical error rate **3 × 10⁻⁷**, assuming η ≥ 5 × 10⁶ and p_Z = 0.1%. This is **>1 order of magnitude** more efficient than magic state cultivation for unbiased qubits and **>2 orders of magnitude** better than standard distillation. The factory count is determined by the required Toffoli throughput: factories = Toffoli_count / (algorithm_runtime / factory_cycle_time).

**Stage 4: Layout and routing.** LDPC-cat codes require a routing layer stacked on the data layer for lattice surgery. The routing overhead adds approximately 1 additional qubit per logical qubit for repetition-coded routing channels (Ruiz et al. 2025). For 2D-local architectures, routing contention can increase overhead by 20–50%.

**Stage 5: Physical resource totals.** Total cat qubits = (data qubits for code) + (ancilla qubits for syndrome measurement) + (routing qubits) + (magic state factory qubits × factory count). Runtime = (logical depth × logical cycle time) where logical cycle time = d × physical cycle time.

### The novel cross-architecture comparison

The paper's central contribution is a comparison table at constant algorithmic input:

|Architecture|Physical qubits (RSA-2048)|Physical qubits (256-bit ECDLP)|Source|
|---|---|---|---|
|Surface code (superconducting)|<1,000,000|~500,000 (extrapolated)|Gidney 2025|
|qLDPC (Pinnacle)|<100,000|TBD|Webster 2026|
|Cat + repetition code|~350,000|126,133|Gouzien 2023|
|**Cat + LDPC code (this work)**|**TBD (~50,000–150,000)**|**TBD (~25,000–60,000)**|**Novel**|

The "TBD" entries are the paper's primary computational results. The estimate range is based on the ~5× encoding rate improvement of LDPC-cat over repetition-cat (from 758 vs ~3,800 qubits for 100 logical qubits in quantum memory) combined with the ~8.7× magic state factory reduction from unfolded distillation.

---

## 8. Library architecture and type design

### Module structure

```
qec-cat/
├── src/QEC/
│   ├── GF2.hs                    -- Bit-packed Word64 GF(2) primitives
│   ├── GF2/Matrix.hs             -- Dense & sparse GF(2) matrices, Gaussian elimination
│   ├── GF2/Symplectic.hs         -- Symplectic form, commutativity checks
│   ├── Stabilizer.hs             -- Stabilizer tableaux, Pauli group
│   ├── Stabilizer/Tableau.hs     -- CHP algorithm, inverse tableau
│   ├── Stabilizer/PauliFrame.hs  -- Pauli frame simulation (batch sampling)
│   ├── Code.hs                   -- QECCode typeclass, CSSCode, NonCSSCode
│   ├── Code/Repetition.hs        -- Repetition cat codes
│   ├── Code/LDPCCat.hs           -- LDPC-cat codes (Ruiz et al.)
│   ├── Code/BivariateBicycle.hs  -- BB codes (Bravyi et al.)
│   ├── Code/BiasTailored.hs      -- Lifted product codes (Roffe et al.)
│   ├── Code/Romanesco.hs         -- Romanesco codes (Leroux & Iverson)
│   ├── Code/Elevator.hs          -- Elevator codes (Shanahan & Ruiz)
│   ├── Noise.hs                  -- CatQubitNoise, DepolarizingNoise, BiasedNoise
│   ├── Decoder.hs                -- Decoder typeclass
│   ├── Decoder/BP.hs             -- Belief propagation (product-sum, min-sum)
│   ├── Decoder/OSD.hs            -- Ordered statistics decoding
│   ├── Decoder/BPOSD.hs          -- Combined BP+OSD decoder
│   ├── MagicState.hs             -- Distillation protocols
│   ├── MagicState/Unfolded.hs    -- Unfolded distillation (Ruiz et al. 2025)
│   ├── ResourceEstimate.hs       -- Full pipeline: algorithm → physical resources
│   ├── ResourceEstimate/Layout.hs -- Routing, factory placement
│   └── Types.hs                  -- Type-level code parameters, phantom types
├── cbits/
│   ├── gf2_simd.c                -- AVX2 kernels for row XOR, weight, syndrome
│   └── gf2_simd.h
├── test/
│   ├── Main.hs                   -- tasty test runner
│   ├── QEC/GF2/Properties.hs     -- QuickCheck: field axioms, matrix identities
│   ├── QEC/Symplectic/Properties.hs -- Symplectic invariant preservation
│   ├── QEC/Code/Properties.hs    -- CSS orthogonality, distance bounds
│   └── QEC/Decoder/Properties.hs -- Syndrome consistency, known thresholds
├── bench/
│   ├── Main.hs                   -- tasty-bench runner
│   ├── GF2Bench.hs               -- Row XOR, Gaussian elimination scaling
│   └── DecoderBench.hs           -- BP+OSD throughput vs code size
├── examples/
│   ├── RSA2048.hs                -- Resource estimate for RSA-2048
│   └── ECDLP256.hs              -- Resource estimate for 256-bit ECDLP
└── qec-cat.cabal
```

### Core type design

The central design philosophy is: **code parameters at the type level, noise models as phantom-typed data, decoders as typeclass instances.**

```haskell
-- Type-level code parameters
data CodeParams = MkCodeParams Nat Nat Nat  -- n, k, d

-- Core code typeclass
class QECCode (code :: CodeParams -> Type) where
  type NoiseConstraint code :: Constraint
  parityCheckX :: code '(n,k,d) -> Matrix (n-k) n GF2
  parityCheckZ :: code '(n,k,d) -> Matrix (n-k) n GF2
  codeRate     :: code '(n,k,d) -> Rational
  logicalErrorRate :: NoiseModel -> code '(n,k,d) -> Double

-- Noise model with architecture phantom type
data Arch = Standard | Squeezed | Galvanic
data CatNoise (arch :: Arch) = CatNoise
  { kappa1Over2 :: Double, meanPhoton :: Double, cycleTime :: Double }

-- Resource estimate output
data ResourceEstimate = ResourceEstimate
  { physicalQubits   :: !Int
  , runtime          :: !Double        -- seconds
  , spacetimeVolume  :: !Double        -- qubit-seconds
  , magicFactories   :: !Int
  , qubitsPerFactory :: !Int
  , codeDistance      :: !Int
  , logicalErrorBudget :: !Double }
```

### Interoperability strategy

For production Monte Carlo at scale, the library should support FFI to established tools:

- **Stim** (C++): Generate syndrome samples via Stim's API, import detector error models. This avoids reimplementing the entire Pauli frame simulation engine while the Haskell-native version is developed.
- **ldpc** (Python/C++): Call Roffe's BP+OSD decoder for cross-validation against the Haskell-native implementation.
- **Export format**: Produce Stim circuit files (`.stim`) and detector error model files (`.dem`) for interoperability with PyMatching, Sinter, and the broader QEC tooling ecosystem.

---

## 9. Benchmarking plan and performance targets

### Micro-benchmarks for GF(2) linear algebra

|Operation|Target (n=1000)|Baseline comparison|
|---|---|---|
|Row XOR (bit-packed)|<50 ns|Stim: ~10 ns (AVX)|
|GF(2) Gaussian elimination|<10 ms|Custom C: ~2 ms|
|Symplectic inner product|<100 ns|Stim: ~20 ns|
|Hamming weight (popCount)|<30 ns|Hardware popcount|

These targets assume pure Haskell with `-O2`; the optional SIMD C kernels should close the gap to within 2× of Stim.

### Decoder benchmarks

|Code|Decoder|Target throughput|Reference|
|---|---|---|---|
|[[144,12,12]] BB|BP+OSD-0|>1000 decodings/sec|ldpc library: ~5000/sec (Python/C++)|
|d=15 repetition cat|BP|>50,000 decodings/sec|Trivial for repetition codes|
|[[758,100,22]] LDPC-cat|BP+OSD-0|>200 decodings/sec|Novel benchmark|

### Monte Carlo convergence targets

For threshold estimation to 3 significant figures, approximately 10⁶ samples are needed per data point (error rate). With 12 physical error rates and 5 code distances per code family, a full threshold scan requires ~60 million decoding rounds. At 1000 decodings/sec on a single core, this takes ~17 hours. With 8-core parallelism via `async`/`streamly`, this drops to ~2 hours — feasible for iterative development.

### End-to-end resource estimation validation

Cross-validate against published results:

- Gouzien et al.: reproduce 126,133 cat qubits for 256-bit ECDLP (within 5%)
- Ruiz et al.: reproduce 758 cat qubits for 100 logical qubits (exact match)
- Gidney 2025: reproduce <1M qubits for RSA-2048 using surface code model (within 10%)

These validation targets ensure the pipeline is correctly calibrated before producing novel LDPC-cat estimates.

---

## 10. Research paper strategy

### Target venues and novelty claim

**Primary target**: Quantum (quantum-journal.org), which published Gidney & Ekerå (2021), Stim (2021), and Roffe's bias-tailored codes (2023). **Secondary target**: Physical Review Research, which published Roffe's BP+OSD paper (2020).

**Central novelty claim**: The first end-to-end resource estimate for cryptanalysis (RSA-2048, 256-bit ECDLP) using LDPC-cat codes with unfolded distillation, providing a fair cross-architecture comparison against surface codes (Gidney 2025), qLDPC/Pinnacle (Webster 2026), and repetition-cat codes (Gouzien 2023).

### Paper structure

1. **Introduction**: The cryptanalytic relevance timeline — how close are we? Position LDPC-cat as the missing data point.
2. **Cat qubit noise model**: Formalize the parameterized noise model with experimental calibration from Réglade 2024, Putterman 2025, Rousseau 2025, and Alice & Bob 2025.
3. **LDPC-cat code construction**: Extend Ruiz et al.'s codes from memory to computation. Include routing overhead, syndrome extraction circuits.
4. **Unfolded distillation integration**: Compute magic state factory footprint and throughput for Toffoli production at scale.
5. **Resource estimation results**: Tables and plots comparing architectures at constant algorithmic input. Sensitivity analysis over κ₁/κ₂, |α|², cycle time, and target logical error rate.
6. **Discussion**: Identify crossover points (at what noise bias does LDPC-cat beat qLDPC?), engineering implications, and caveats.

### Key figures for the paper

- **Figure 1**: Architecture comparison bar chart — physical qubits for RSA-2048 across four architectures
- **Figure 2**: Pareto frontier — spacetime volume vs physical qubits for LDPC-cat codes at varying distances
- **Figure 3**: Sensitivity heatmap — physical qubit count as function of (κ₁/κ₂, |α|²)
- **Figure 4**: Threshold plots for LDPC-cat codes under circuit-level biased noise from Monte Carlo simulation
- **Figure 5**: Factory layout — visual representation of LDPC-cat + unfolded distillation chip architecture

### Authorship and collaboration strategy

The computational work is purely theoretical. Key collaboration opportunities:

- **Diego Ruiz** (Alice & Bob): co-author for LDPC-cat code expertise and unfolded distillation details
- **Joschka Roffe** (University of Edinburgh): BP+OSD decoder integration and bias-tailored code construction
- Reaching out with preliminary results and the open-source library would strengthen the paper substantially.

---

## 11. Implementation roadmap

### Phase 1: Core GF(2) engine (weeks 1–4)

Build the bit-packed GF(2) matrix library with Gaussian elimination, symplectic inner product, and row-reduction. Property-test all algebraic invariants. Benchmark against C baselines. Implement the stabilizer tableau (CHP algorithm) with inverse-tableau optimization. **Deliverable**: `QEC.GF2`, `QEC.GF2.Matrix`, `QEC.GF2.Symplectic`, `QEC.Stabilizer.Tableau` modules passing 100+ property tests.

**Key risk**: Performance of GF(2) Gaussian elimination. Mitigate by implementing the C/AVX2 kernel in parallel and benchmarking both paths from week 2.

### Phase 2: Code constructions (weeks 5–8)

Implement repetition cat codes (simplest, for validation against Gouzien et al.), LDPC-cat codes (port from Ruiz et al.'s Python/C++), and bivariate bicycle codes. For each code family: generate parity-check matrices, compute code parameters, verify CSS orthogonality (H_X · H_Z^T = 0 mod 2). **Deliverable**: `QEC.Code.Repetition`, `QEC.Code.LDPCCat`, `QEC.Code.BivariateBicycle` with validated parameters matching published results.

### Phase 3: Decoder and Monte Carlo (weeks 9–12)

Implement BP+OSD-0 decoder with biased-noise channel priors. Validate against Roffe's `ldpc` library for known codes. Build the Monte Carlo sampling infrastructure using `streamly` for parallel sampling with `splitmix` RNG. Estimate thresholds for LDPC-cat codes under circuit-level noise. **Deliverable**: `QEC.Decoder.BPOSD`, threshold plots for LDPC-cat codes, cross-validated against published results.

### Phase 4: Resource estimation pipeline (weeks 13–16)

Implement the five-stage pipeline. Port Gouzien et al.'s algorithm resource counts. Implement unfolded distillation factory model. Compute novel LDPC-cat resource estimates for RSA-2048 and 256-bit ECDLP. Generate all paper figures. **Deliverable**: `QEC.ResourceEstimate` module, draft paper figures, preliminary results.

### Phase 5: Paper writing and library polish (weeks 17–20)

Write the research paper. Polish the library API, documentation, and examples. Release on GitHub with Hackage publication. Submit paper to Quantum. **Deliverable**: Submitted paper, public library release.

**Total timeline**: 20 weeks (5 months) for a single developer working full-time, or 8–10 months at half-time.

---

## 12. Verified references and resources

### Core resource estimation papers

The following papers and repositories have been verified as of February 2026:

**Gouzien et al.** "Performance Analysis of a Repetition Cat Code Architecture: Computing 256-bit Elliptic Curve Logarithm in 9 Hours with 126,133 Cat Qubits." Physical Review Letters **131**, 040602 (2023). arXiv:2302.06639. Code: `github.com/ElieGouzien/elliptic_log_cat` (Python, 5 stars). **Verified**: 126,133 cat qubits, κ₁/κ₂ = 10⁻⁵, 19 photons, 500 ns cycle time.

**Ruiz et al.** "LDPC-cat codes for low-overhead quantum computing in 2D." Nature Communications **16**, 1040 (2025). Code: `github.com/DiegoRuiz-Git/LDPCat` (Python/Jupyter/C++, 3 stars). **Verified**: 758 cat qubits for 100 logical qubits, εL ≤ 10⁻⁸, phase-flip error 0.1%.

**Gidney.** "How to factor 2048 bit RSA integers with less than a million noisy qubits." arXiv:2505.15917 (May 2025). Data: Zenodo DOI 10.5281/zenodo.15347487. **Note**: Sole author (not Gidney & Ekerå as commonly cited). Update of Gidney & Ekerå, Quantum **5**, 433 (2021). **Verified**: <1M physical qubits, ~1,400 logical qubits, ~6.5 × 10⁹ Toffoli gates.

**Webster et al.** "The Pinnacle Architecture: Reducing the cost of breaking RSA-2048 to 100,000 physical qubits using quantum LDPC codes." arXiv:2602.11457 (February 2026). Iceberg Quantum. **Verified**: <100,000 physical qubits at p = 10⁻³. No public code repository. Preprint from startup; not yet peer-reviewed.

**Ruiz et al.** "Unfolded distillation: very low-cost magic state preparation for biased-noise qubits." arXiv:2507.12511 (July 2025). **Verified**: 53 cat qubits, 5.5 QEC rounds, logical error 3 × 10⁻⁷, >8.7× qubit reduction vs. unbiased approaches.

### Cat qubit physics

**Guillaud & Mirrahimi.** "Repetition Cat Qubits for Fault-Tolerant Quantum Computation." Physical Review X **9**, 041053 (2019). Also: Physical Review A **103**, 042413 (2021). **Verified**: Γ_X ∝ κ₁|α|² exp(−2|α|²), Γ_Z ∝ κ₁|α|², noise bias η ∝ exp(2|α|²).

**Puri et al.** "Bias-preserving gates with stabilized cat qubits." Science Advances **6**, eaay5901 (2020). **Verified**: Bias-preserving CX gate (impossible with two-level systems), threshold 0.43% at η = 10⁴, 5× Clifford overhead reduction.

**Réglade et al.** "Quantum control of a cat qubit with bit-flip times exceeding ten seconds." Nature **629**, 778–783 (2024). **Verified**: >10s bit-flip time, >490 ns phase-flip time.

**Rousseau et al.** "Enhancing dissipative cat qubit protection by squeezing." arXiv:2502.07892 (February 2025). **Verified**: γ = 4.3 scaling exponent, 22s bit-flip time at 4.1 photons, 160× improvement.

**Putterman et al.** "Hardware-efficient quantum error correction via concatenated bosonic qubits." Nature (February 2025). **Verified**: 5 cat qubits, distance-5 repetition code, below-threshold operation, bit-flip times approaching 1 second.

**Alice & Bob.** "A Cat Qubit That Jumps Every Hour." Blog/press release, September 2025. **Verified**: >1 hour bit-flip time at 11 photons on Galvanic Cat design (Helium 2 chip). Preliminary, not peer-reviewed.

### Code constructions and decoders

**Roffe et al.** "Decoding across the quantum low-density parity-check code landscape." Physical Review Research **2**, 043423 (2020). Code: `github.com/quantumgizmos/ldpc`. **Verified**: BP+OSD decoder, ~9.9% toric code threshold.

**Roffe et al.** "Bias-tailored quantum LDPC codes." Quantum **7**, 1005 (2023). Code: `github.com/quantumgizmos/bias_tailored_qldpc`. **Verified**: XZZX rotation on lifted product codes, orders-of-magnitude improvement under bias.

**Bravyi et al.** "High-threshold and low-overhead fault-tolerant quantum memory." Nature **627**, 778–782 (2024). Code: `github.com/sbravyi/BivariateBicycleCodes`. **Verified**: [[144,12,12]] code, ~10× efficiency vs surface code.

**Leroux & Iverson.** "Romanesco codes: Bias-tailored qLDPC codes from fractal codes." arXiv:2506.00130 (May 2025). **Verified**: Clifford-deformed BB codes on hexagonal lattice, fractal logical operators, distance scaling exceeding 2D topological codes under bias.

**Shanahan & Ruiz.** "Elevator Codes: Concatenation for resource-efficient quantum memory under biased noise." arXiv:2601.10786 (January 2026). **Verified**: >50% overhead reduction at η ≥ 7×10⁴, 2D-local construction with "elevator" ancilla.

**Aaronson & Gottesman.** "Improved Simulation of Stabilizer Circuits." Physical Review A **70**, 052328 (2004). **Verified**: CHP algorithm, O(n²) tableau, O(n) per gate.

**Gidney.** "Stim: a fast stabilizer circuit simulator." Quantum **5**, 497 (2021). Code: `github.com/quantumlib/Stim`. **Verified**: Inverse tableau, SIMD optimization, Pauli frame sampling.

### Haskell ecosystem (key verified packages)

|Package|Version|Status|Role in qec-cat|
|---|---|---|---|
|`vector`|Current|Actively maintained|Bit-packed GF(2) storage|
|`mod`|Current|Maintained (Bodigrim)|Element-level GF(2) reference|
|`galois-field`|1.0.2|Active|Finite field abstraction|
|`linear-code`|0.2.0|Experimental|Classical code reference|
|`async`|Current|Mature (Marlow)|Parallel Monte Carlo|
|`streamly`|0.11.0|Active (Composewell)|Streaming Monte Carlo|
|`singletons-th`|3.5.1|Active (Eisenberg)|Type-level/term-level bridge|
|`ghc-typelits-natnormalise`|Current|Active|Type arithmetic reasoning|
|`tasty`|1.5.3|Active|Test framework|
|`tasty-bench`|Current|Active|Benchmarking|
|`matrix-sized`|Current|Available|Type-safe sparse matrices|

### Existing resource estimation tools for reference

|Tool|Developer|Language|Open source|
|---|---|---|---|
|Azure QRE|Microsoft|Rust/Python|Yes (January 2024)|
|Qualtran|Google|Python|Yes|
|Stim + Sinter|Google|C++/Python|Yes|
|PyMatching v2|Higgott|C++/Python|Yes|
|ldpc (BP+OSD)|Roffe|Python/C++|Yes|

---

## Conclusion

This plan charts a path from an empty Haskell project to a published research paper in approximately 20 weeks. The scientific opportunity is sharply defined: LDPC-cat codes have been validated for quantum memory (758 qubits for 100 logical qubits) and unfolded distillation has been validated for low-overhead magic states (53 qubits per factory), but nobody has combined these components into a full computation resource estimate for cryptanalysis. The Haskell implementation fills a genuine ecosystem gap — no QEC library exists in the language — while providing type-safety guarantees impossible in the Python tools that dominate the field.

The highest-risk element is decoder performance: achieving sufficient throughput in Haskell-native BP+OSD for Monte Carlo convergence. The mitigation is clear — FFI to Roffe's C++ decoder for production runs, with the Haskell implementation serving as a property-tested reference. The highest-reward element is the cross-architecture comparison table: if LDPC-cat codes with unfolded distillation achieve **<50,000 cat qubits** for 256-bit ECDLP (a plausible 2.5–5× reduction from Gouzien's 126,133), this would establish cat-qubit architectures as competitive with or superior to the Pinnacle Architecture's <100,000 physical qubits — a result with immediate implications for cryptographic security timelines and quantum hardware roadmaps.