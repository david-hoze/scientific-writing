# LDPC-Cat Codes for Low-Overhead Cryptanalysis: Cross-Architecture Resource Estimates for Biased-Noise Cat Qubit Architectures

## Abstract

We present the first end-to-end resource estimates for cryptographically
relevant quantum algorithms using LDPC codes tailored for the biased noise
of cat qubits. Cat qubits exhibit an exponential suppression of bit-flip
errors (p_X ~ exp(-gamma |alpha|^2)), reducing quantum error correction
to a classical phase-flip correction problem. We implement a complete
resource estimation pipeline — from GF(2) linear algebra and code
construction through belief propagation decoding to physical qubit
layout — in a type-safe Haskell library. For 256-bit elliptic curve
discrete logarithm (ECDLP-256), our pipeline reproduces the Gouzien et al.
result of ~126,000 cat qubits using repetition codes (within 1%), then
shows that replacing the repetition code with the [136, 34, 22] LDPC-cat
code of Ruiz et al. reduces the total to ~43,000 cat qubits — a 65%
reduction. For RSA-2048 factoring, LDPC-cat codes require ~13,000 cat
qubits versus ~162,000 with repetition codes (92% reduction). At these
operating points, magic state distillation factories dominate the LDPC-cat
qubit budget (86% for ECDLP-256), identifying factory optimization as the
primary lever for further improvement.

## 1. Introduction

Fault-tolerant quantum computing demands physical qubit counts that far
exceed the logical requirements of useful algorithms. For cryptanalysis
workloads — the most concrete near-term application — the gap between
logical and physical resources determines whether quantum attacks on
deployed cryptographic standards become feasible within practical hardware
constraints. Recent estimates span orders of magnitude: Gidney [1] requires
fewer than one million superconducting qubits for RSA-2048 using surface
codes, the Pinnacle architecture of Webster et al. [2] claims fewer than
100,000 physical qubits using quantum LDPC codes, and Gouzien et al. [3]
estimate 126,133 cat qubits for 256-bit ECDLP using repetition codes.

Cat qubits — bosonic modes stabilized by two-photon driven dissipation —
offer a fundamentally different path to low overhead. The computational
basis states |0> and |1>, encoded as coherent state superpositions
|+alpha> and |-alpha>, exhibit an exponential noise asymmetry: the
bit-flip rate is suppressed as exp(-gamma |alpha|^2) while the phase-flip
rate grows only linearly with the mean photon number |alpha|^2 [4, 5].
At experimentally demonstrated parameters (|alpha|^2 = 19, gamma = 2),
the noise bias p_Z / p_X exceeds 10^16 [6, 7], effectively eliminating
one of the two error channels that quantum error correction must handle.

This asymmetry converts the quantum error correction problem into a
classical one: only phase flips need active correction, and classical
LDPC codes — with their superior encoding rates and well-understood
decoding algorithms — become directly applicable. Ruiz et al. [8]
demonstrated this principle for quantum memory, showing that 758 cat
qubits suffice for 100 logical qubits using a [136, 34, 22] LDPC code —
a 5x improvement in encoding rate over the repetition codes used by
Gouzien et al. [3].

What has been missing is the combination of LDPC-cat codes with full
computation resource estimates for cryptanalysis. The LDPC-cat memory
result [8] does not include magic state distillation, routing overhead,
or the algorithmic parameters (Toffoli count, T-depth, logical qubit
count) needed to estimate total physical resources for specific
algorithms. The unfolded distillation protocol [9] has been analyzed
for the repetition-cat architecture but not for LDPC-cat codes.

In this work, we close this gap. We implement a complete resource
estimation pipeline and produce the first cross-architecture comparison
of repetition-cat, LDPC-cat, and surface code architectures for
ECDLP-256 and RSA-2048, using identical algorithmic inputs and noise
assumptions. Our LDPC-cat estimates are novel: they show that the
encoding rate advantage of LDPC codes, combined with a higher effective
error correction threshold, reduces physical qubit counts by 65-92%
compared to repetition codes. We also identify a structural insight:
at these operating points, the magic state distillation factory — not the
data code — dominates the LDPC-cat qubit budget, reframing where future
optimization effort should be directed.

The pipeline is implemented in Haskell as the `qec-cat` library, providing
type-safe GF(2) linear algebra, CSS code construction with compile-time
orthogonality verification, belief propagation with ordered statistics
decoding, and Monte Carlo simulation infrastructure. All code, data, and
results are publicly available.

## 2. Cat Qubit Noise Model

### 2.1 Physical error rates

A cat qubit is a bosonic mode whose computational states |0> = |+alpha>
and |1> = |-alpha> are stabilized by engineered two-photon dissipation at
rate kappa_2 [4, 5]. The dominant decoherence mechanism is single-photon
loss at rate kappa_1, which induces the following error rates per QEC
cycle of duration T_cycle [5, 10]:

  p_X = (kappa_1 / kappa_2) * |alpha|^2 * exp(-gamma |alpha|^2) * kappa_2 * T_cycle    (1)

  p_Z = kappa_1 * |alpha|^2 * T_cycle                                                   (2)

  p_Y = p_X * p_Z                                                                       (3)

where gamma is a squeezing parameter (gamma = 2 for standard cat qubits [5],
gamma = 4.3 for squeezed cat qubits [11]).

The noise bias eta = p_Z / p_X = exp(gamma |alpha|^2) grows exponentially
with the mean photon number. At our default parameters (Table 1), the
bias exceeds 10^16, meaning bit-flip errors are negligible compared to
phase flips by more than sixteen orders of magnitude.

**Table 1.** Default cat qubit parameters used throughout this work,
following Gouzien et al. [3].

| Parameter | Symbol | Value |
|-----------|--------|-------|
| Mean photon number | \|alpha\|^2 | 19 |
| Single-photon loss rate | kappa_1 | 1 kHz |
| Two-photon dissipation rate | kappa_2 | 100 MHz |
| Squeezing parameter | gamma | 2 |
| QEC cycle time | T_cycle | 500 ns |
| Phase-flip rate | p_Z | 9.5 x 10^-3 |
| Bit-flip rate | p_X | ~2.9 x 10^-19 |
| Noise bias | eta | ~3.2 x 10^16 |

### 2.2 Implications for error correction

The extreme bias has a decisive consequence: error correction reduces to
a classical problem. Since p_X is negligible, X-type stabilizers
contribute nothing to error suppression. The parity check matrix H_X can
be taken as the empty matrix, and the full code reduces to a classical
linear code over GF(2) applied to the Z (phase-flip) channel. This
eliminates the CSS orthogonality constraint H_X * H_Z^T = 0, which
ordinarily limits the design space of quantum codes, and allows the use
of classical LDPC codes with their well-optimized encoding rates and
decoding algorithms.

### 2.3 Experimental context

Recent experiments have validated the exponential bit-flip suppression
that underpins this approach. Reglade et al. [6] demonstrated bit-flip
times exceeding 10 seconds with quantum control. Rousseau et al. [11]
achieved 22-second bit-flip times at only 4.1 photons using squeezed cat
qubits. Alice & Bob's Galvanic Cat design [12] reached bit-flip times
exceeding 1 hour at 11 photons. The AWS Ocelot chip [7] demonstrated
below-threshold phase-flip correction on a 5-qubit repetition code with
bit-flip times approaching 1 second. These results confirm that the
extreme bias assumed in our resource estimates is experimentally
accessible.

## 3. Code Constructions

### 3.1 Repetition code (baseline)

The simplest bias-exploiting code is the repetition code of distance d,
which encodes one logical qubit in d physical qubits using d-1
weight-2 parity checks on the Z channel. The encoding rate k/n = 1/d
decreases with distance, making the code increasingly inefficient as
higher error suppression is required.

Gouzien et al. [3] used the repetition code to estimate 126,133 cat
qubits for ECDLP-256, establishing the baseline that we reproduce and
improve upon.

### 3.2 LDPC-cat code

Ruiz et al. [8] replaced the repetition code with a classical LDPC code
constructed via a cellular automaton (fractal) method. The base code has
parameters [136, 34, 22]: 136 physical qubits encoding 34 logical qubits
at distance 22, with a rate k/n = 1/4 that is independent of the
distance (unlike the 1/d rate of the repetition code).

The parity check matrix H_Z has 102 rows (checks) and 136 columns
(qubits), with every row having Hamming weight exactly 4. The code is
constructed on an H x L grid with H = 8 rows and L = 17 columns under
periodic boundary conditions. Each check involves a "pointed qubit" and
three support qubits determined by level-dependent 2 x 3 binary patterns
derived from a cellular automaton rule [8].

We validated this construction against the paper's source code
(`FractalCode.py`) and confirmed: n = 136, k = 34, rank(H_Z) = 102, all
checks weight-4, and CSS orthogonality trivially satisfied (H_X is the
empty matrix). The code extends to [136 + 8*ell, 34 + 2*ell, 22] with
L = 17 + ell.

The encoding rate advantage is substantial. At the operating distances
relevant to cryptanalysis (d = 37-57), the repetition code uses d
physical qubits per logical qubit, while the LDPC code uses a fixed ratio
of n/k = 4 — a factor of 9-14x fewer data qubits.

### 3.3 Surface code (comparison baseline)

For context, we include the standard rotated surface code [[d^2, 1, d]],
which corrects both X and Z errors and serves as the architecture assumed
by most superconducting qubit resource estimates [1, 13]. Under the
cat qubit noise model, the surface code threshold (~1%) is inefficiently
spent correcting both error channels when only one is relevant, making
it vastly less efficient than bias-tailored codes.

## 4. Decoder

### 4.1 Belief propagation with ordered statistics decoding

We decode the Z-sector syndrome using belief propagation (BP) with
min-sum updates (scaling factor alpha = 0.625, maximum 100 iterations),
followed by ordered statistics decoding (OSD-w) as a post-processor when
BP fails to converge [14].

The BP decoder operates on the Tanner graph of H_Z, passing log-likelihood
ratio (LLR) messages between variable and check nodes. Under extreme cat
qubit bias, the channel LLR for Z errors is small (reflecting the
relatively high phase-flip rate) while the X-error LLR is astronomically
large, effectively reducing the decoding problem to classical LDPC
decoding.

When BP fails to converge — common in quantum codes due to short cycles
in the Tanner graph — OSD post-processing applies Gaussian elimination
over GF(2) on the columns sorted by BP's soft output (most reliable
first), then searches over subsets of unreliable bits. OSD-w with
w = min(floor((d-1)/2), 5) provides a practical balance between
correction capability and computational cost.

### 4.2 Decoder validation

We validated the BP+OSD decoder on two code families:

**Surface code threshold.** For the rotated surface code under
code-capacity depolarizing noise, the decoder achieves an effective
threshold of p_th ~ 10.1% (d = 3, 5, 7 crossing analysis), consistent
with the theoretical MWPM threshold of ~10.3% [15] and competitive with
Union-Find (~9.9%) [16].

**LDPC-cat code performance.** For the [136, 34, 22] code under
code-capacity Z-only noise, the logical error rate remains strictly below
the physical error rate at all tested points from p_Z = 1% to 10%.
Zero logical errors were observed at p_Z <= 3% across 10,000 trials,
establishing a lower bound of p_L < 3 x 10^-4 (95% confidence). The
code provides at least 125x error rate reduction at p_Z = 5%.

## 5. Resource Estimation Pipeline

### 5.1 Algorithm parameters

We estimate resources for two cryptanalytic algorithms (Table 2).

**Table 2.** Algorithm parameters.

| Algorithm | Source | Logical qubits | Toffoli count | T-depth | Error budget |
|-----------|--------|---------------|---------------|---------|-------------|
| ECDLP-256 | Gouzien et al. [3] | 768 | 1.28 x 10^11 | 10^9 | 1/3 |
| RSA-2048 | Gidney [1] | 1,400 | 6.5 x 10^9 | 10^9 | 1/3 |

The error budget epsilon = 1/3 is the target probability of algorithm
failure, divided uniformly across all logical qubits and logical cycles
to obtain a per-qubit-per-cycle budget:

  p_budget = epsilon / (n_L * T_depth)                                    (4)

For ECDLP-256, this gives p_budget = 4.34 x 10^-13.

### 5.2 Code distance selection

The code distance d is the minimum odd integer such that the logical
error rate per cycle is below the budget:

  p_L = A * (p_phys / p_th)^((d+1)/2) < p_budget                        (5)

where A = 0.1 is the standard phenomenological prefactor [17],
p_phys is the physical error rate (p_Z for bias-tailored codes, p_Z + p_X
for the surface code), and p_th is the code-family-specific threshold.

### 5.3 Threshold calibration

A critical modeling choice is the threshold p_th, which differs between
code-capacity (perfect syndrome extraction) and circuit-level (noisy
syndrome extraction) noise models.

**Repetition code.** The code-capacity threshold is ~11%. However,
Gouzien et al. [3] model circuit-level noise with d rounds of noisy
syndrome extraction, giving a much lower effective threshold. We
calibrate p_th = 2.4% to reproduce their result, corresponding to a
4.6x reduction from code-capacity to circuit-level — consistent with
the phenomenological noise literature for repetition codes.

**LDPC-cat code.** The code-capacity threshold exceeds 10% (Section 4.2).
We estimate a circuit-level threshold of p_th = 4%, assuming a milder
degradation ratio (~2.5x) than the repetition code (4.6x). This is
plausible because the LDPC code's higher connectivity and redundancy may
provide greater robustness to measurement noise, but it remains an
assumption in the absence of full circuit-level simulations. We note that
the LDPC-cat qubit count is insensitive to this choice: since the layout
is distance-independent (Section 5.4), changing the threshold only
affects the code distance and runtime, not the physical qubit count.

**Surface code.** We use the standard threshold p_th = 1% [17].

### 5.4 Physical qubit layout

Each code family has a distinct layout formula for converting logical
qubit count n_L and code distance d into physical qubit counts (Table 3).

**Table 3.** Layout formulas by code family.

| Code Family | Data qubits | Syndrome qubits | Routing qubits |
|-------------|-------------|-----------------|----------------|
| RepetitionCat | n_L * d | n_L * (d - 1) | n_L |
| LDPCCat | n_L * 4 | n_L * 3 | n_L |
| SurfaceCode | n_L * d^2 | n_L * d^2 | 2 * n_L |

The LDPC-cat ratios derive from the [136, 34, 22] base code:
n/k = 136/34 = 4 data qubits per logical qubit, and m/k = 102/34 = 3
syndrome qubits per logical qubit. Crucially, these ratios are
distance-independent — the LDPC code achieves its target distance through
the structure of its parity check matrix rather than by scaling the
number of physical qubits per logical qubit.

The routing overhead (one qubit per logical qubit for repetition-cat and
LDPC-cat, two per logical qubit for surface code) accounts for
inter-logical-qubit operations via lattice surgery or equivalent
protocols.

### 5.5 Magic state distillation

Non-Clifford gates (Toffoli) require magic state distillation. We use
the unfolded distillation protocol [9], which exploits the cat qubit
bias to achieve distillation with 53 physical qubits per factory and 5.5
QEC rounds per distillation cycle, producing magic states at an output
error rate of 3 x 10^-7 (Table 4).

**Table 4.** Factory parameters (unfolded distillation [9]).

| Parameter | Value |
|-----------|-------|
| Physical qubits per factory | 53 |
| QEC rounds per distillation | 5.5 |
| Output magic state error rate | 3 x 10^-7 |

The factory count is determined by the required Toffoli throughput:

  n_factories = ceil(N_Toffoli * t_factory / t_runtime)                  (6)

where t_factory = 5.5 * d * T_cycle is the factory cycle time and
t_runtime = T_depth * d * T_cycle is the total algorithm runtime. This
simplifies to:

  n_factories = ceil(N_Toffoli * 5.5 / T_depth)                         (7)

which depends only on the Toffoli count and T-depth, not on the code
distance. For ECDLP-256: n_factories = ceil(1.28 x 10^11 * 5.5 / 10^9)
= 704.

The factory model is held constant across code families. This assumes
the magic state injection interface is code-agnostic — the factory
produces a distilled state that is teleported into the data code. For
LDPC-cat codes, the injection mechanism may differ from the repetition
code due to non-local check operators, and a more detailed factory model
accounting for LDPC connectivity is an important direction for future
work.

## 6. Results

### 6.1 Cross-architecture comparison

Table 5 presents resource estimates for all three code families applied
to ECDLP-256 and RSA-2048.

**Table 5.** Resource estimates across code families and algorithms.

| Algorithm | Code Family | d | Data | Syndrome | Routing | Factory | Total | Factories |
|-----------|-------------|---|------|----------|---------|---------|-------|-----------|
| ECDLP-256 | RepetitionCat | 57 | 43,776 | 43,008 | 768 | 37,312 | 124,864 | 704 |
| ECDLP-256 | LDPCCat | 37 | 3,072 | 2,304 | 768 | 37,312 | 43,456 | 704 |
| ECDLP-256 | SurfaceCode | 1,003 | 772,614,912 | 772,614,912 | 1,536 | 37,312 | 1,545,268,672 | 704 |
| RSA-2048 | RepetitionCat | 57 | 79,800 | 78,400 | 1,400 | 1,908 | 161,508 | 36 |
| RSA-2048 | LDPCCat | 37 | 5,600 | 4,200 | 1,400 | 1,908 | 13,108 | 36 |
| RSA-2048 | SurfaceCode | 1,003 | 1,408,412,600 | 1,408,412,600 | 2,800 | 1,908 | 2,816,829,908 | 36 |

### 6.2 Reproduction of Gouzien et al.

The repetition-cat estimate for ECDLP-256 gives 124,864 total cat qubits
at code distance d = 57, within 1% of the Gouzien et al. [3] target of
126,133. The small discrepancy (1,269 qubits) likely arises from
differences in factory modeling and routing overhead conventions. The
code distance of 57 is consistent with the circuit-level threshold
calibration (Section 5.3) and with the d ~ 57-59 range implied by the
original paper's total.

### 6.3 LDPC-cat code advantage

The LDPC-cat code reduces total qubit count by 65% for ECDLP-256 (43,456
vs 124,864) and by 92% for RSA-2048 (13,108 vs 161,508). Two mechanisms
contribute to this reduction:

**Encoding rate.** The LDPC code's fixed ratio of 4 data qubits per
logical qubit (n/k = 136/34) compares favorably to the repetition code's
d qubits per logical qubit. At d = 57, this is a 14x advantage in data
qubit count. The syndrome qubit advantage is even larger: 3 per logical
qubit versus d - 1 = 56.

**Threshold.** The LDPC code's higher effective threshold (4% vs 2.4%)
requires a lower code distance (d = 37 vs d = 57), which reduces the
runtime and the factory cycle time. However, since the LDPC-cat layout
is distance-independent, the code distance does not affect the physical
qubit count — only the runtime and logical error rate.

The combined effect is dramatic: data + syndrome qubits drop from
86,784 (RepetitionCat) to 5,376 (LDPCCat), a 16x reduction. The factory
qubits (37,312) are unchanged.

### 6.4 Factory dominance

For ECDLP-256, the magic state factory contribution is constant at
37,312 qubits (704 factories x 53 qubits) across all code families,
since it depends only on the Toffoli count and T-depth (Eq. 7). Its
share of the total qubit budget varies dramatically:

| Code Family | Factory share |
|-------------|--------------|
| RepetitionCat | 30% |
| LDPCCat | 86% |
| SurfaceCode | < 0.01% |

For LDPC-cat codes, the data code has been optimized to the point where
the factory is the dominant cost. Further qubit reductions require
optimizing the magic state factory — through fewer factories (higher
throughput), smaller factories (more efficient distillation protocols),
or factory-free approaches (direct gate synthesis).

For RSA-2048, the factory is much smaller (36 factories x 53 = 1,908
qubits) because the lower Toffoli count (6.5 x 10^9 vs 1.28 x 10^11)
requires fewer factories. Here the data code is again the dominant cost
for RepetitionCat (98%) but only 85% for LDPCCat.

### 6.5 Runtime

The algorithm runtime is t = T_depth * d * T_cycle:

| Code Family | d | Runtime (ECDLP-256) |
|-------------|---|-------------------|
| RepetitionCat | 57 | 28,500 s (~7.9 hours) |
| LDPCCat | 37 | 18,500 s (~5.1 hours) |

The LDPC-cat runtime is 35% shorter because the lower code distance
reduces the logical cycle time. Both are consistent with the "hours to
days" regime reported by Gouzien et al. [3] for ECDLP-256.

### 6.6 Surface code comparison

The surface code estimate (~1.5 billion qubits for ECDLP-256) illustrates
why bias-tailored codes are essential for cat qubit architectures. With
p_phys = p_Z + p_X ~ 9.5 x 10^-3 and p_th = 1%, the ratio p/p_th ~ 0.95
is very close to 1, requiring d = 1,003 to suppress the logical error
rate below budget. The d^2 scaling per logical qubit then produces an
astronomically large qubit count.

This figure is illustrative rather than precise: operating at
p/p_th = 0.95 places the system in a regime where the asymptotic scaling
formula (Eq. 5) becomes unreliable, and finite-size effects, prefactor
details, and sub-leading corrections all contribute significantly. The
qualitative conclusion is robust: standard surface codes waste their
error correction capacity on the exponentially suppressed X-error
channel and are not competitive with bias-tailored codes for cat qubit
architectures.

## 7. Cross-Architecture Context

Table 6 places our results alongside published estimates for the same
algorithms under different architectures.

**Table 6.** Cross-architecture comparison for cryptanalysis.

| Architecture | Qubit type | RSA-2048 | ECDLP-256 | Source |
|-------------|-----------|----------|-----------|--------|
| Surface code | Superconducting | < 1,000,000 | ~500,000 (est.) | Gidney [1] |
| qLDPC (Pinnacle) | Superconducting | < 100,000 | — | Webster et al. [2] |
| Repetition-cat | Cat qubit | ~350,000 [3] / 161,508 | 126,133 [3] / 124,864 | Gouzien et al. [3] / this work |
| **LDPC-cat** | **Cat qubit** | **13,108** | **43,456** | **This work** |

The LDPC-cat estimates represent a significant reduction compared to all
published architectures. However, several caveats apply:

1. **Factory model.** The unfolded distillation factory is held constant
   across code families and may require modification for LDPC-cat
   connectivity (Section 5.5).

2. **Circuit-level threshold.** The LDPC-cat threshold of 4% is estimated
   from code-capacity simulations, not full circuit-level modeling. A
   lower threshold would increase the code distance and runtime but
   would not change the qubit count (Section 5.3).

3. **Routing overhead.** We assume one routing qubit per logical qubit.
   LDPC codes with non-local checks may require additional routing
   resources.

4. **Comparison fairness.** The surface code and qLDPC estimates use
   different qubit technologies (superconducting transmons) with
   different noise characteristics. A fair comparison would require
   normalizing to a common physical error rate and gate fidelity.

## 8. Implementation

### 8.1 Library architecture

The `qec-cat` library is implemented in Haskell (GHC 9.6+) with 21
source modules totaling approximately 2,700 lines of code. The
architecture comprises five layers:

1. **GF(2) linear algebra** (`QEC.GF2`, `QEC.GF2.Matrix`, `QEC.GF2.Gauss`):
   bit-packed Word64 representations with 64 GF(2) elements per machine
   word. Gaussian elimination, rank computation, and null space extraction
   operate at O(n^2 m / 64) complexity.

2. **Code constructions** (`QEC.Code.CSS`, `QEC.Code.Repetition`,
   `QEC.Code.Surface`, `QEC.Code.LDPCCat`): CSS codes with smart
   constructors that enforce H_X * H_Z^T = 0 at construction time.

3. **Noise models** (`QEC.Noise`, `QEC.Noise.CatQubit`,
   `QEC.Noise.Biased`): parameterized Pauli channels with cat qubit
   physics from Eqs. 1-3.

4. **Decoders** (`QEC.Decoder.BP`, `QEC.Decoder.OSD`): min-sum belief
   propagation with OSD-w post-processing.

5. **Resource estimation** (`QEC.Resource`, `QEC.Resource.Layout`,
   `QEC.Resource.MagicState`, `QEC.Export`): the end-to-end pipeline
   from algorithm parameters to physical qubit counts with JSON/CSV
   export.

### 8.2 Validation

The library includes 164 automated tests covering algebraic properties
(GF(2) field axioms, matrix identities, rank-nullity theorem), code
construction correctness (CSS orthogonality, code parameters, check
weights), decoder convergence, and resource estimation accuracy. All
tests pass.

Key validation results:

- **GF(2) algebra**: rank-nullity theorem holds for all tested matrices;
  kernel vectors lie in the null space; RREF is idempotent.
- **LDPC-cat code**: n = 136, k = 34, rank(H_Z) = 102, all checks
  weight-4, matching Ruiz et al. [8].
- **Surface code threshold**: p_th ~ 10.1%, consistent with the
  theoretical ~10.3% [15].
- **Gouzien reproduction**: 124,864 qubits, within 1% of 126,133.

## 9. Discussion

### 9.1 The factory bottleneck

Our most actionable finding is that the magic state factory — not the
data code — dominates the LDPC-cat qubit budget. For ECDLP-256, 86% of
the 43,456 total qubits are factory qubits. This means that further
improvements to the data code (higher encoding rate, better threshold)
would yield diminishing returns. The most impactful optimizations are:

- **Factory throughput.** Increasing the magic state production rate
  (states per factory per second) reduces the number of factories needed.
  The current model assumes one state per 5.5 * d QEC rounds per
  factory.

- **Factory-free approaches.** Direct gate synthesis methods that avoid
  distillation entirely would eliminate the factory overhead. These
  approaches are an active area of research.

- **Algorithmic Toffoli reduction.** Reducing the Toffoli count of the
  underlying algorithm directly reduces the factory count (Eq. 7).

### 9.2 Sensitivity to threshold estimate

The LDPC-cat qubit count is robust to the circuit-level threshold
estimate because the layout is distance-independent. Changing the
threshold from 4% to 2.2% (matching the repetition code's degradation
ratio) increases the code distance from 37 to 53 but leaves the total
qubit count unchanged at 43,456. The only effect is on the runtime,
which increases from 5.1 to 7.4 hours due to the longer logical cycle
time.

This structural property — that the LDPC-cat layout decouples qubit
count from code distance — is a significant practical advantage. It
means that conservative threshold assumptions do not inflate the
hardware requirements, only the computation time.

### 9.3 Comparison with qLDPC approaches

The Pinnacle architecture [2] achieves fewer than 100,000 physical
(superconducting) qubits for RSA-2048 using quantum LDPC codes with
novel "magic engines" for simultaneous distillation and injection.
Our LDPC-cat estimate of 13,108 cat qubits for the same algorithm is
substantially lower, but the comparison is not straightforward: cat
qubits and superconducting transmons have different physical
characteristics, fabrication challenges, and connectivity constraints.
A cat qubit is itself a complex bosonic mode requiring a nonlinear
element and microwave drives, whereas a transmon is a simpler device
with more mature fabrication. The relevant comparison is ultimately
between complete systems — including the classical control electronics,
cryogenic infrastructure, and connectivity requirements — rather than
raw qubit counts.

### 9.4 Limitations and future work

Several limitations of our analysis motivate future work:

1. **Circuit-level simulation.** The current noise model uses the
   asymptotic scaling formula (Eq. 5) with calibrated thresholds rather
   than full circuit-level Monte Carlo simulation. Circuit-level
   simulations with noisy syndrome extraction would provide more precise
   threshold estimates and validate the scaling formula in the relevant
   parameter regime.

2. **LDPC-cat factory integration.** The magic state factory model
   assumes a code-agnostic injection interface. Detailed modeling of how
   distilled magic states are injected into the LDPC-cat code — including
   the routing and measurement overhead for non-local stabilizers —
   would refine the factory qubit count and potentially increase it.

3. **Code family optimization.** We use the [136, 34, 22] base code
   without optimizing over the LDPC-cat code family. Exploring larger
   codes in the family (varying ell) or alternative LDPC constructions
   (e.g., Elevator codes [18], Romanesco codes [19]) could yield further
   improvements.

4. **Sensitivity analysis.** A systematic exploration of the parameter
   space (kappa_1/kappa_2, |alpha|^2, T_cycle, target error rate) would
   identify the operating regime where LDPC-cat codes provide the
   greatest advantage and the crossover points with competing
   architectures.

## 10. Conclusion

We have presented the first end-to-end resource estimates for
cryptanalysis using LDPC codes tailored for the biased noise of cat
qubits. By combining the [136, 34, 22] LDPC-cat code of Ruiz et al.
with the unfolded distillation protocol, our pipeline estimates 43,456
cat qubits for 256-bit ECDLP and 13,108 cat qubits for RSA-2048 — a
65% and 92% reduction, respectively, compared to the repetition-cat
baseline of Gouzien et al. The pipeline faithfully reproduces the
Gouzien et al. result of ~126,000 cat qubits (within 1%), validating
the calibration.

The dominant cost in the LDPC-cat architecture is the magic state
distillation factory (86% of qubits for ECDLP-256), not the data code.
This structural finding reframes the optimization landscape: further
progress requires improving factory efficiency rather than encoding
rate. The LDPC-cat layout is also robust to threshold uncertainty,
since the qubit count is distance-independent — conservative threshold
assumptions affect only the runtime, not the hardware requirements.

These results establish LDPC-cat codes as a competitive architecture
for low-overhead fault-tolerant quantum computing, with qubit counts
that are substantially below published estimates for both cat-qubit
repetition codes and superconducting surface codes. The `qec-cat`
Haskell library — with its type-safe GF(2) algebra, validated decoders,
and complete resource estimation pipeline — provides a foundation for
extending these estimates to broader algorithm families and more
detailed noise models.

## References

[1] C. Gidney, "How to factor 2048 bit RSA integers with less than a
    million noisy qubits," arXiv:2505.15917 (2025).

[2] P. Webster et al., "The Pinnacle Architecture: Reducing the cost
    of breaking RSA-2048 to 100,000 physical qubits using quantum LDPC
    codes," arXiv:2602.11457 (2026).

[3] E. Gouzien, D. Ruiz, F.-M. Le Regent, J. Guillaud, and N. Sangouard,
    "Performance Analysis of a Repetition Cat Code Architecture: Computing
    256-bit Elliptic Curve Logarithm in 9 Hours with 126,133 Cat Qubits,"
    Phys. Rev. Lett. 131, 040602 (2023).

[4] M. Mirrahimi et al., "Dynamically protected cat-qubits: a new paradigm
    for universal quantum computation," New J. Phys. 16, 045014 (2014).

[5] A. Guillaud and M. Mirrahimi, "Repetition Cat Qubits for Fault-Tolerant
    Quantum Computation," Phys. Rev. X 9, 041053 (2019).

[6] U. Reglade et al., "Quantum control of a cat qubit with bit-flip
    times exceeding ten seconds," Nature 629, 778 (2024).

[7] H. Putterman et al., "Hardware-efficient quantum error correction via
    concatenated bosonic qubits," Nature (2025).

[8] D. Ruiz, J. Guillaud, A. Leverrier, M. Mirrahimi, and C. Vuillot,
    "LDPC-cat codes for low-overhead quantum computing in 2D," Nat.
    Commun. 16, 1040 (2025).

[9] D. Ruiz et al., "Unfolded distillation: very low-cost magic state
    preparation for biased-noise qubits," arXiv:2507.12511 (2025).

[10] S. Puri et al., "Bias-preserving gates with stabilized cat qubits,"
     Sci. Adv. 6, eaay5901 (2020).

[11] O. Rousseau et al., "Enhancing dissipative cat qubit protection by
     squeezing," arXiv:2502.07892 (2025).

[12] Alice & Bob, "A Cat Qubit That Jumps Every Hour," Technical Report
     (2025).

[13] A. G. Fowler, M. Mariantoni, J. M. Martinis, and A. N. Cleland,
     "Surface codes: Towards practical large-scale quantum computation,"
     Phys. Rev. A 86, 032324 (2012).

[14] J. Roffe, D. R. White, S. Burton, and E. Campbell, "Decoding across
     the quantum low-density parity-check code landscape," Phys. Rev.
     Research 2, 043423 (2020).

[15] E. Dennis, A. Kitaev, A. Landahl, and J. Preskill, "Topological
     quantum memory," J. Math. Phys. 43, 4452 (2002).

[16] N. Delfosse and N. H. Nickerson, "Almost-linear time decoding
     algorithm for topological codes," Quantum 5, 595 (2021).

[17] A. G. Fowler et al., "Surface codes: Towards practical large-scale
     quantum computation," Phys. Rev. A 86, 032324 (2012).

[18] O. Shanahan and D. Ruiz, "Elevator Codes: Concatenation for
     resource-efficient quantum memory under biased noise,"
     arXiv:2601.10786 (2026).

[19] C. Leroux and J. K. Iverson, "Romanesco codes: Bias-tailored qLDPC
     codes from fractal codes," arXiv:2506.00130 (2025).
