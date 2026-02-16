# Research Proposal Prompt: Typed Algebraic Framework for Biased-Noise Quantum Error Correction in Haskell

## Instructions to Claude

You are a research consultant helping me plan and implement a Haskell library and accompanying research paper for quantum error correction (QEC) resource estimation, targeting biased-noise architectures — specifically cat qubits combined with LDPC codes. I need you to produce a detailed, technically precise implementation plan. Search the web extensively to ground every recommendation in real code, real papers, and real Haskell ecosystem capabilities.

This is not a toy project. The goal is to produce:

1. A Haskell library (`qec-cat`) that implements the full pipeline from code construction to resource estimation, with end-to-end type safety.
2. A research paper with novel cross-architecture resource estimation results, targeting the _Quantum_ journal or _Physical Review Research_.

I have experience with Haskell's type-level programming. I am learning quantum error correction as I go. I have no experimental lab — this is purely theoretical/computational work.

## Context and Motivation

### The gap in the field

The fault-tolerant quantum computing community lacks end-to-end algorithm-level resource estimates for LDPC-cat code architectures. Key reference points:

- **Gouzien et al. (PRL 2023):** 126,133 cat qubits for 256-bit ECDLP using repetition codes. Code available at `github.com/ElieGouzien/elliptic_log_cat`.
- **Ruiz et al. (Nature Communications, Jan 2025):** LDPC-cat codes achieving 100 logical qubits on 758 cat qubits for quantum memory. Code at `github.com/DiegoRuiz-Git/LDPCat`. No algorithm-level resource estimate exists.
- **Gidney & Ekerå (arXiv:2505.15917, May 2025 update):** <1 million qubits for RSA-2048 using surface codes. The standard benchmark.
- **Webster et al. "Pinnacle Architecture" (arXiv:2602.11457, Feb 2026):** <100,000 physical qubits for RSA-2048 using qLDPC codes.
- **Shanahan & Ruiz "Elevator Codes" (arXiv:2601.10786, Jan 2026):** Concatenated codes for biased noise, 10,000× lower logical error rates with ~3× more qubits vs repetition codes at high bias.

Nobody has compared surface codes, qLDPC codes, repetition-cat codes, LDPC-cat codes, and elevator codes for the same target algorithm under consistent assumptions. This library enables that comparison.

### Why Haskell

The QEC pipeline has a natural decomposition:

1. **Code construction:** Define code families algebraically (lifted products, bicycle codes, repetition codes). These are categorical/algebraic structures — chain complexes over F₂, group algebras, polynomial quotient rings. Haskell's type system can enforce structural invariants (e.g., CSS orthogonality H_X · H_Z^T = 0) at compile time.
    
2. **Syndrome circuit generation:** Translate a code + connectivity constraint into a syndrome extraction circuit. The type system can guarantee every stabilizer is measured exactly once.
    
3. **Noise model composition:** Cat qubit physical parameters (|α|², κ₁/κ₂, gate times) map to effective Pauli error rates. The type system ensures noise channels are consistent with the circuit structure.
    
4. **Stabilizer simulation:** The Aaronson-Gottesman tableau algorithm — binary matrix operations over F₂ (symplectic matrices in Sp(2n, F₂)). Pauli frame sampling for fast Monte Carlo. These are pure functions on algebraic structures.
    
5. **Decoding:** Belief propagation + Ordered Statistics Decoding (BP+OSD) on factor graphs derived from the code's parity check matrix. Pure function from syndrome to correction.
    
6. **Resource estimation:** Given logical error rates from simulation, compute total physical qubit counts for target algorithms including magic state factory overhead, routing, and compilation.
    

Currently, the entire community does this in untyped Python with ad hoc scripts. Bugs silently produce wrong resource estimates. Our framework makes an entire class of bugs unrepresentable via types.

The categorical quantum mechanics program (Abramsky, Coecke et al.) demonstrates that category theory captures quantum computational structure naturally. Error correcting codes are algebraic objects (chain complexes, homology groups) with functorial relationships between constructions. Haskell is the language whose type system most naturally expresses these structures.

## What I Need You to Research and Produce

### Part 1: Haskell Ecosystem Assessment

Research and document the current state of every Haskell library relevant to this project. For each, evaluate maturity, performance, and fit:

- **Binary linear algebra over F₂:** What exists? (`linear`, `hmatrix`, custom `Data.Vector.Unboxed` with `Word64` bit-packing, etc.) What's the best approach for symplectic matrices, GF(2) Gaussian elimination, and sparse binary matrix operations? Benchmark data if available.
- **SIMD / vectorized operations:** GHC's SIMD primitives (`ghc-prim` SIMD), `vector` library with unboxed arrays, or FFI to a thin C kernel for AVX XOR loops. What's the realistic performance gap vs C++ for the inner loop of Pauli frame simulation?
- **Parallelism:** `async`, `par`/`pseq`, `streamly`, or similar for embarrassingly parallel Monte Carlo sampling. What's the current best practice for distributing millions of independent shots across cores?
- **Type-level programming:** `GHC.TypeLits` for qubit-count-indexed types, GADTs, type families, `singletons` library. What's the most ergonomic approach for types like `Tableau (n :: Nat)` where n is the qubit count?
- **Property-based testing:** `QuickCheck` and `hedgehog` for verifying algebraic invariants (symplectic constraints, CSS orthogonality, code distance lower bounds).
- **Build and distribution:** Cabal vs Stack. Hackage publication requirements. How to make the library practically installable for non-Haskell users (Nix? Static binaries? Docker?).
- **Visualization / output:** IHaskell for notebook-style development? Or generate data files (CSV/JSON) and use a thin Python layer for matplotlib plots?

### Part 2: Type System Architecture

Design the complete type hierarchy for the library. Be extremely specific — give actual Haskell type signatures, not pseudocode. Address:

**2.1 Finite field and algebraic foundations:**

- Types for F₂ (GF(2)), vectors over F₂, matrices over F₂ (both dense and sparse representations).
- Group algebras F₂[G] for finite groups G (needed for lifted product codes).
- Polynomial quotient rings F₂[x]/(x^n - 1) (needed for bicycle codes).

**2.2 Chain complexes and homology:**

- A `ChainComplex` type parameterized by the base ring.
- Boundary maps as typed morphisms. The type system should enforce ∂² = 0.
- The lifted product (tensor product of chain complexes) as a typed functor.

**2.3 Quantum codes:**

- A `QuantumCode` typeclass or type family hierarchy.
- CSS codes as a refinement where H_X · H_Z^T = 0 is enforced.
- Specific code families as instances: repetition code, surface code, XZZX surface code, bivariate bicycle codes, lifted product codes, the specific LDPC-cat [165+8ℓ, 34+2ℓ, 22] family from Ruiz et al., elevator codes.
- Code parameters (n, k, d) accessible at the type level or as verified runtime values.

**2.4 Syndrome circuits:**

- A `Circuit` type parameterized by qubit count, noise model, and code.
- Operations (gates, measurements, resets) as constructors.
- A circuit generator function that takes a `QuantumCode` and a connectivity constraint and produces a valid syndrome extraction circuit.
- Noise insertion as a typed transformation on circuits.

**2.5 Stabilizer tableau:**

- `Tableau n` as a symplectic binary matrix, parameterized by qubit count n.
- Clifford gates as endomorphisms on `Tableau n`, with the type guaranteeing symplecticity is preserved.
- Pauli frame type for fast bulk sampling.
- Measurement as a typed operation that updates the tableau and produces a classical bit.

**2.6 Noise models:**

- A `NoiseModel` typeclass with instances for depolarizing, biased Pauli, and cat qubit noise.
- `CatQubitNoise` parameterized by |α|², κ₁/κ₂, gate times. A typed function translating these to effective Pauli error rates using the Guillaud & Mirrahimi (2019) model.
- Bias-preserving gate noise: the CNOT/CX gate in the cat qubit architecture produces only Z-type errors (to leading order). The type system should distinguish bias-preserving from general Clifford operations.

**2.7 Decoders:**

- Belief propagation on a factor graph derived from the code's parity check matrix.
- Ordered Statistics Decoding (OSD) as post-processing.
- A `Decoder` typeclass with a function `decode :: Code c => c -> Syndrome c -> Correction c` ensuring dimensional consistency.
- Support for biased channel probabilities in the BP message update equations.

**2.8 Resource estimation:**

- Types for algorithm specifications (Toffoli count, logical qubit count, target error budget).
- Magic state distillation factory models (unfolded distillation from Ruiz et al. 2025).
- A top-level function: given cat qubit physical parameters + code family + target algorithm → total physical qubit count + computation time.

### Part 3: Implementation Plan

Provide a week-by-week implementation plan covering 18 weeks. For each week:

- What module(s) to build.
- What types and functions to implement.
- What tests to write (property-based tests for algebraic invariants).
- What validation milestone to hit (e.g., "reproduce Gouzien et al.'s 126,133 figure").
- What papers to read that week.

The plan should be ordered so that:

- Each week produces something testable.
- Earlier weeks build foundations that later weeks depend on.
- Validation against published results happens as early as possible.
- The cross-architecture comparison (the paper's headline result) is achievable by week 14.

### Part 4: Stabilizer Simulation — Detailed Algorithm Design

Research and specify in detail how to implement the stabilizer simulation in Haskell:

**4.1 Tableau representation:**

- How to represent the 2n × (2n+1) binary tableau (n stabilizer generators, each a 2n-bit Pauli string + 1 phase bit, plus n destabilizer generators) using unboxed `Word64` arrays.
- Memory layout for cache-friendly row operations.
- How Stim's "reference frame sampling" trick works and how to implement it in Haskell.

**4.2 Clifford gate operations:**

- Exact row operations for H, S, CNOT, CZ, SWAP on the binary tableau. Reference: Aaronson & Gottesman (2004), Gidney (2021).
- How to batch these operations for SIMD-friendly execution.

**4.3 Pauli frame simulation:**

- The key insight: after one reference sample, subsequent samples only track error propagation as XOR on bit vectors.
- Data structure for the Pauli frame (two bit vectors per qubit: X and Z components).
- How to inject Pauli noise channels (PAULI_CHANNEL_1 with asymmetric px, py, pz for biased noise).
- How measurement sampling works in the frame model.

**4.4 Detector error model extraction:**

- How to propagate each independent error through the circuit and record which detectors it flips.
- Output format: a hypergraph (list of detector-error associations with probabilities).
- How this hypergraph connects to the decoder (it defines the factor graph for BP).

**4.5 Performance optimization strategy:**

- Where the hot loops are (Pauli frame XOR operations during bulk sampling).
- Realistic performance targets: how many shots/second for a distance-5 surface code? Distance-22 LDPC code?
- When to consider FFI to a C inner loop vs pure Haskell with `vector` + `Word64`.

### Part 5: Decoder — Detailed Algorithm Design

**5.1 Belief Propagation:**

- The min-sum or sum-product algorithm on the code's Tanner graph.
- How to handle biased noise: the initial log-likelihood ratios reflect asymmetric px, py, pz.
- Convergence criteria and maximum iteration count.
- How to handle the "short cycles" problem that makes BP inexact on quantum LDPC codes (4-cycles in the Tanner graph).

**5.2 Ordered Statistics Decoding:**

- OSD-0 and OSD-w (order-w) post-processing.
- The algorithm: sort bits by BP soft output, take the least reliable subset, do Gaussian elimination over F₂, enumerate candidate corrections.
- Computational complexity and practical limits on the OSD order.
- Reference: Roffe et al. "Decoding Across the Quantum LDPC Code Landscape" (2020).

**5.3 Matching decoder (for comparison):**

- Minimum-weight perfect matching on the detector error model's matching graph.
- When this applies (surface codes, repetition codes) vs when BP+OSD is needed (LDPC codes).
- Whether to implement from scratch or FFI to an existing implementation.

### Part 6: Cat Qubit Noise Model — Detailed Specification

Research the exact noise model needed and specify every parameter:

**6.1 Physical parameters:**

- Mean photon number |α|² (typically 4–19).
- Single-photon loss rate κ₁ and two-photon dissipation rate κ₂. The ratio κ₁/κ₂ (currently ~10⁻³ demonstrated, ~10⁻⁵ targeted).
- Gate times for: identity (idle), CNOT (bias-preserving), Toffoli preparation, measurement, reset.

**6.2 Effective Pauli error rates:**

- Bit-flip rate: exponentially suppressed as exp(-2|α|²). Reference: Mirrahimi et al. (2014), Lescanne et al. (2020).
- Phase-flip rate: proportional to κ₁ · |α|² · T_gate. This is the dominant error.
- Gate error model for bias-preserving CNOT: produces Z⊗I, I⊗Z, and Z⊗Z errors at rates derived from the gate Hamiltonian. Reference: Guillaud & Mirrahimi (2019), Puri et al. (2020).
- Operational bias degradation during gates: at what bias ratio does the CNOT gate actually operate? Currently demonstrated: ~250 (Kerr-cat under dihedral RB). Theoretical models from Guillaud & Mirrahimi give higher values.

**6.3 The bias parameterization:**

- Define bias η = p_phase / p_bit.
- How η maps to the PAULI_CHANNEL parameters: px = py = p_bit, pz = p_phase, with pz >> px.
- Sensitivity analysis: how resource estimates change as η varies from 10² to 10⁸.

**6.4 Recent experimental values to calibrate against:**

- Réglade et al. (2024): >10 second bit-flip time, T_φ = 14–17 µs (multi-qubit), 70 µs (single-qubit).
- Rousseau et al. (Feb 2025): Squeezed cat qubits, 22 second bit-flip time at 4.1 photons, γ = 4.3 scaling exponent.
- Alice & Bob (Sep 2025, non-peer-reviewed): 33–60 minute bit-flip times at n̄=11 on Helium 2 chip.
- AWS Ocelot (Putterman et al., Nature, Feb 2025): First multi-cat-qubit chip, 1.65–1.75% logical error rate per cycle for repetition code.

### Part 7: Code Families — Detailed Mathematical Specifications

For each code family the framework must support, provide:

- The mathematical construction (parity check matrices, generator polynomials, or chain complex definition).
- Code parameters (n, k, d) as functions of the construction parameters.
- Connectivity requirements (2D local, non-local, degree constraints).
- How to generate the family programmatically in Haskell.
- Key references.

**Code families:**

1. Repetition code (baseline, simplest)
2. Surface code / rotated surface code (standard benchmark)
3. XZZX surface code (bias-aware variant)
4. Bivariate bicycle codes (IBM's approach, Bravyi et al. 2024)
5. The LDPC-cat [165+8ℓ, 34+2ℓ, 22] family (Ruiz et al. 2025)
6. Elevator codes (Shanahan & Ruiz, Jan 2026)
7. Romanesco codes (Leroux & Iverson, 2025) — bias-tailored qLDPC from fractal structures
8. Bias-tailored lifted product codes (Roffe et al., Quantum, 2023)

### Part 8: Resource Estimation Methodology

Specify how to go from "logical error rate per cycle" to "total physical qubits for RSA-2048":

**8.1 Algorithm compilation:**

- Shor's algorithm Toffoli count for RSA-2048: reference Gidney & Ekerå (2021, 2025).
- ECDLP circuit for 256-bit curves: reference Gouzien et al. (2023).
- How to translate Toffoli counts into the cat qubit gate set (magic state injection via gate teleportation).

**8.2 Magic state preparation:**

- Unfolded distillation protocol from Ruiz et al. (arXiv:2507.12511, Jul 2025): 53 qubits per magic state, 8.7× reduction vs Google's cultivation.
- Factory footprint: how many physical qubits, what cycle time, what output rate.
- The matching constraint: factory output rate must meet or exceed algorithm consumption rate.

**8.3 Routing and layout:**

- Logical qubit connectivity via lattice surgery (for repetition-cat and surface codes).
- Routing qubits overhead for 2D local architectures.
- How LDPC-cat codes handle logical operations (code deformation, lattice surgery, or other method from Ruiz et al.).

**8.4 Total qubit count assembly:**

- Data qubits + syndrome qubits + routing qubits + magic state factory qubits.
- How to express this as a function of: code parameters, physical error rate, target logical error rate, algorithm specification.

### Part 9: Paper Strategy

**9.1 Target results:**

- Table comparing: surface code, XZZX surface code, repetition-cat, LDPC-cat, elevator codes, bias-tailored qLDPC — all for 256-bit ECDLP and RSA-2048.
- Each row: architecture, physical qubit count, computation time, demonstrated vs projected hardware parameters, bias requirement.
- Parametric plots: total physical qubits vs bias ratio η for each architecture.
- Sensitivity analysis: which physical parameter improvements have the largest marginal impact on total qubit count.

**9.2 The framework contribution:**

- Describe the type system architecture and how it prevents the specific classes of bugs that plague Python-based resource estimation.
- Give concrete examples of bugs that are type errors in our framework but silent runtime errors in existing tools.
- Open-source the library on GitHub (and Hackage if ready).

**9.3 Target venue:**

- _Quantum_ journal (open access, fast review, where Stim, PyMatching, and Litinski's papers appeared).
- Alternative: _Physical Review Research_ (APS, open access, broader audience).

### Part 10: Risk Assessment

Identify and address:

- Performance risks: what if pure Haskell is too slow for the Monte Carlo sampling needed? At what point do we add FFI, and for what specifically?
- Scope risks: what is the minimum viable library that produces publishable results? What can be cut?
- Correctness risks: how do we validate the framework produces correct results when there's no existing Haskell QEC library to compare against? (Answer: reproduce published results from Python-based tools.)
- Adoption risks: how do we make a Haskell library usable by a community that uses Python? (Consider: Nix flakes, static binaries, Docker images, JSON/CSV interchange formats, maybe a thin Python wrapper via FFI in reverse.)

## Output Format

Produce a single, comprehensive document structured as follows:

1. **Executive Summary** (1 page)
2. **Ecosystem Assessment** (Part 1 above, with specific library recommendations and version numbers)
3. **Type System Architecture** (Part 2, with actual Haskell type signatures)
4. **Implementation Plan** (Part 3, week-by-week with milestones)
5. **Stabilizer Simulation Design** (Part 4, algorithm-level detail)
6. **Decoder Design** (Part 5, algorithm-level detail)
7. **Noise Model Specification** (Part 6, with equations)
8. **Code Family Specifications** (Part 7, with mathematical definitions)
9. **Resource Estimation Methodology** (Part 8, with equations)
10. **Paper Strategy** (Part 9)
11. **Risk Assessment and Mitigations** (Part 10)
12. **Reading List** (prioritized, with rationale for each paper)

Search the web extensively. Every technical claim should be grounded in a specific paper, library, or benchmark. Do not hand-wave performance claims — find actual numbers. Do not recommend libraries without checking they exist and are maintained.

Be honest about what you don't know or where the plan is speculative. Flag open design decisions that I'll need to make.

This document will be my primary working reference for the next 4–5 months. Make it detailed enough that I can start coding from it.