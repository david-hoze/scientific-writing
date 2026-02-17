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
code of Ruiz et al. reduces the total to ~43,000 cat qubits under
optimistic assumptions about routing and factory injection. A
sensitivity analysis over uncertain model parameters (circuit-level
threshold, routing overhead, factory injection cost) shows total qubit
counts ranging from 43,000 (optimistic) to 122,000 (pessimistic) for
ECDLP-256, compared to 125,000 for the repetition-cat baseline. The
advantage is robust for RSA-2048 but becomes marginal for ECDLP-256
under the most pessimistic assumptions. At the
optimistic operating point, magic state distillation factories dominate
the LDPC-cat qubit budget (86%), identifying factory optimization as the
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
of repetition-cat and LDPC-cat architectures for ECDLP-256 and RSA-2048,
using identical algorithmic inputs and noise assumptions. Our LDPC-cat
estimates are novel: they show that the encoding rate advantage of LDPC
codes reduces data and syndrome qubit counts by 16x compared to
repetition codes. We accompany the baseline estimates with a sensitivity
analysis over three uncertain model parameters — circuit-level threshold,
routing overhead, and factory injection cost — showing the range of
outcomes under optimistic through pessimistic assumptions. We also
identify a structural insight: at the baseline operating point, the
magic state distillation factory — not the data code — dominates the
LDPC-cat qubit budget, reframing where future optimization effort should
be directed.

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

  p_X = kappa_1 * |alpha|^2 * exp(-gamma |alpha|^2) * T_cycle            (1)

  p_Z = kappa_1 * |alpha|^2 * T_cycle                                     (2)

  p_Y = p_X * p_Z                                                         (3)

Equation (1) captures the exponential bit-flip suppression: the factor
exp(-gamma |alpha|^2) arises from the tunnel splitting between the
coherent states |+alpha> and |-alpha>, which decreases exponentially
with the mean photon number. The remaining factors kappa_1 * |alpha|^2
give the single-photon loss rate (the attempt rate for tunneling), and
T_cycle converts from a rate to a per-cycle probability. The squeezing
parameter gamma equals 2 for standard cat qubits [5] and 4.3 for
squeezed cat qubits [11].

Equation (2) has no exponential suppression: each single-photon loss
event causes a phase-space displacement that can flip the relative phase
between |+alpha> and |-alpha>, and this occurs at the full single-photon
loss rate.

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
qubits. The AWS Ocelot chip [7] demonstrated below-threshold phase-flip
correction on a 5-qubit repetition code with bit-flip times approaching
1 second. Alice & Bob reported bit-flip times exceeding 1 hour at 11
photons on their Galvanic Cat design [12]; this result has not yet been
published in a peer-reviewed venue but, if confirmed, would further
strengthen the case for extreme-bias operation. These results confirm
that the noise bias assumed in our resource estimates is experimentally
accessible with current or near-term hardware.

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

### 3.3 Surface code (reference point)

The standard surface code [14] is included in the pipeline as a reference
architecture but omitted from the main results tables. At cat qubit
noise parameters (p_Z = 9.5 x 10^-3, p_th = 1%), the ratio p/p_th =
0.95 is very close to 1, requiring d > 1,000 and producing qubit counts
exceeding 10^9 — confirming that surface codes are not competitive with
bias-tailored codes when noise is dominated by a single Pauli channel.

## 4. Decoder

### 4.1 Belief propagation with ordered statistics decoding

We decode the Z-sector syndrome using belief propagation (BP) with
min-sum updates (scaling factor alpha = 0.625, maximum 100 iterations),
followed by ordered statistics decoding (OSD-w) as a post-processor when
BP fails to converge [13].

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
with the theoretical MWPM threshold of ~10.3% [14] and competitive with
Union-Find (~9.9%) [15].

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

For ECDLP-256, this gives p_budget = 4.34 x 10^-13. The entire budget is
allocated to data errors (QEC failure). Magic state distillation errors
are handled separately by the factory's output error rate (3 x 10^-7 per
state), which is far below the per-cycle budget and does not drive the
code distance. This allocation is valid when the distillation error per
state is much smaller than the data error budget per cycle — satisfied
here by a factor of ~10^6. In regimes where the factory output error
rate approaches the per-cycle budget (e.g., with higher physical error
rates or shorter algorithms requiring less suppression), the error
budget would need to be explicitly partitioned between data and
distillation contributions, and the code distance might increase.

### 5.2 Code distance selection

The code distance d is the minimum odd integer such that the logical
error rate per cycle is below the budget:

  p_L = A * (p_phys / p_th)^((d+1)/2) < p_budget                        (5)

where A = 0.1 is the standard phenomenological prefactor [16] (absorbing
sub-leading corrections from code geometry and boundary effects),
p_phys is the physical error rate (p_Z for bias-tailored codes), and
p_th is the code-family-specific threshold.

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
We use p_th = 4% as a baseline estimate for the circuit-level threshold,
assuming a milder degradation ratio (~2.5x) than the repetition code
(4.6x). This assumption — that the LDPC code's higher connectivity and
check redundancy provide greater robustness to measurement noise — is
plausible but unvalidated by circuit-level simulation. Applying the
repetition code's 4.6x ratio would give p_th ~ 2.2%.

The sensitivity to this assumption is analyzed quantitatively in
Section 6.6. The key finding is that the LDPC-cat data and syndrome
qubit counts are distance-independent, so the threshold affects the
code distance and runtime but not the qubit count from these
components. However, when routing overhead depends on distance (as in
the pessimistic routing model), the threshold does affect the total.

**Threshold validity at d = 37.** The baseline LDPC-cat estimate uses
d = 37 at p_th = 4%. Solving Eq. 5, the minimum threshold at which
d = 37 satisfies the ECDLP-256 error budget is p_th >= 3.8%. If the
true circuit-level threshold is below 3.8%, d = 37 does not achieve
the required logical error rate, and a higher distance is needed. At
p_th = 3.5%, the required distance increases to d = 41; at p_th = 2.4%,
to d = 57 (Table 7 in Section 6.6).

### 5.4 Physical qubit layout

Each code family has a distinct layout formula for converting logical
qubit count n_L and code distance d into physical qubit counts (Table 3).

**Table 3.** Layout formulas by code family.

| Code Family | Data qubits | Syndrome qubits | Routing qubits |
|-------------|-------------|-----------------|----------------|
| RepetitionCat | n_L * d | n_L * (d - 1) | n_L |
| LDPCCat (baseline) | n_L * 4 | n_L * 3 | n_L |
| LDPCCat (pessimistic routing) | n_L * 4 | n_L * 3 | n_L * d |

The LDPC-cat data and syndrome ratios derive from the [136, 34, 22] base
code: n/k = 136/34 = 4 data qubits per logical qubit, and
m/k = 102/34 = 3 syndrome qubits per logical qubit. These ratios are
distance-independent — the LDPC code achieves its target distance through
the structure of its parity check matrix rather than by scaling the
number of physical qubits per logical qubit.

**Routing overhead.** The baseline model assumes one routing qubit per
logical qubit, following the repetition-cat convention where qubits are
arranged in a 1D chain with nearest-neighbor connectivity and lattice
surgery requires a single ancilla channel. For LDPC codes, this
assumption is optimistic: the [136, 34, 22] code is constructed on a
grid with periodic boundary conditions and level-dependent check
patterns, meaning inter-logical-qubit operations may require routing
through the non-local check structure. In the pessimistic routing model,
we assume d routing qubits per logical qubit, reflecting the possibility
that lattice surgery operations in an LDPC code require ancilla chains
of length proportional to the code distance. We note that d-linear
routing is not necessarily an upper bound: if the Tanner graph cannot be
embedded in 2D without high crossing number, long-range connections may
require swap chains whose length grows super-linearly with the code
block size, potentially exceeding n_L * d. The true routing overhead
depends on the physical layout and connectivity of the target hardware;
determining it precisely requires detailed circuit-level analysis of
the [136, 34, 22] Tanner graph embedding, which we leave to future work.
The sensitivity analysis (Section 6.6) should therefore be read as
exploring a plausible range rather than bounding the worst case.

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

**Factory injection overhead.** The factory model is held constant across
code families. This assumes a code-agnostic injection interface: the
factory produces a distilled |T> state that is teleported into the data
code. For the repetition code, this interface is straightforward — the
1D chain structure allows direct injection via a transversal CNOT.
For LDPC-cat codes, the injection mechanism is less clear. The non-local
check structure means that a distilled magic state may need to be routed
through additional ancilla qubits to reach the target logical qubit,
or that intermediate teleportation steps are required. This could
increase the effective factory footprint beyond 53 qubits per factory.
In the sensitivity analysis (Section 6.6), we model this as additional
ancilla qubits per factory: +20 (moderate) and +50 (pessimistic).

## 6. Results

### 6.1 Baseline estimates

Table 5 presents resource estimates under the baseline assumptions
(p_th = 4% for LDPCCat, 1 routing qubit per logical, 53 qubits per
factory).

**Table 5.** Baseline resource estimates (optimistic assumptions).

| Algorithm | Code Family | d | Data | Syndrome | Routing | Factory | Total |
|-----------|-------------|---|------|----------|---------|---------|-------|
| ECDLP-256 | RepetitionCat | 57 | 43,776 | 43,008 | 768 | 37,312 | 124,864 |
| ECDLP-256 | LDPCCat | 37 | 3,072 | 2,304 | 768 | 37,312 | 43,456 |
| RSA-2048 | RepetitionCat | 57 | 79,800 | 78,400 | 1,400 | 1,908 | 161,508 |
| RSA-2048 | LDPCCat | 37 | 5,600 | 4,200 | 1,400 | 1,908 | 13,108 |

### 6.2 Reproduction of Gouzien et al.

The repetition-cat estimate for ECDLP-256 gives 124,864 total cat qubits
at code distance d = 57, within 1% of the Gouzien et al. [3] target of
126,133. The small discrepancy (1,269 qubits) likely arises from
differences in factory modeling and routing overhead conventions. The
code distance of 57 is consistent with the circuit-level threshold
calibration (Section 5.3) and with the d ~ 57-59 range implied by the
original paper's total.

### 6.3 LDPC-cat code advantage

Under baseline assumptions, the LDPC-cat code reduces total qubit count
by 65% for ECDLP-256 (43,456 vs 124,864) and by 92% for RSA-2048
(13,108 vs 161,508). Two mechanisms contribute to this reduction:

**Encoding rate.** The LDPC code's fixed ratio of 4 data qubits per
logical qubit (n/k = 136/34) compares favorably to the repetition code's
d qubits per logical qubit. At d = 57, this is a 14x advantage in data
qubit count. The syndrome qubit advantage is even larger: 3 per logical
qubit versus d - 1 = 56.

**Threshold.** The LDPC code's higher assumed threshold (4% vs 2.4%)
requires a lower code distance (d = 37 vs d = 57), which reduces the
runtime and the factory cycle time. However, since the LDPC-cat data
and syndrome counts are distance-independent, the code distance does not
affect these components of the qubit count — only the runtime, logical
error rate, and (in the pessimistic routing model) the routing overhead.

The combined effect on data and syndrome qubits is dramatic: they drop
from 86,784 (RepetitionCat) to 5,376 (LDPCCat), a 16x reduction. The
factory qubits (37,312) are unchanged.

### 6.4 Factory dominance

For ECDLP-256 under baseline assumptions, the magic state factory
contribution is constant at 37,312 qubits (704 factories x 53 qubits)
across all code families, since it depends only on the Toffoli count and
T-depth (Eq. 7). Its share of the total qubit budget varies dramatically:

| Code Family | Factory share |
|-------------|--------------|
| RepetitionCat | 30% |
| LDPCCat (baseline) | 86% |

For LDPC-cat codes, the data code has been optimized to the point where
the factory is the dominant cost. Further qubit reductions require
optimizing the magic state factory — through fewer factories (higher
throughput), smaller factories (more efficient distillation protocols),
or factory-free approaches (direct gate synthesis).

For RSA-2048, the factory is much smaller (36 factories x 53 = 1,908
qubits) because the lower Toffoli count (6.5 x 10^9 vs 1.28 x 10^11)
requires fewer factories.

### 6.5 Runtime

The algorithm runtime is t = T_depth * d * T_cycle:

| Code Family | d | Runtime (ECDLP-256) |
|-------------|---|-------------------|
| RepetitionCat | 57 | 28,500 s (~7.9 hours) |
| LDPCCat (p_th = 4%) | 37 | 18,500 s (~5.1 hours) |
| LDPCCat (p_th = 2.4%) | 57 | 28,500 s (~7.9 hours) |

The LDPC-cat runtime at the baseline threshold is 35% shorter because
the lower code distance reduces the logical cycle time. At the
pessimistic threshold (2.4%), the runtime equals the repetition code's.
Both are consistent with the "hours to days" regime reported by Gouzien
et al. [3] for ECDLP-256.

### 6.6 Sensitivity analysis

The baseline LDPC-cat estimates rest on three uncertain parameters:
the circuit-level threshold, the routing overhead, and the factory
injection cost. Table 7 shows the ECDLP-256 qubit count under different
assumption combinations. Table 8 shows the same for RSA-2048.

**Table 7.** Sensitivity analysis for ECDLP-256 LDPCCat (n_L = 768,
704 factories).

| Scenario | p_th | d | Routing model (total routing qubits) | Factory/unit | Total |
|----------|------|---|--------------------------------------|-------------|-------|
| Baseline | 4.0% | 37 | 1/logical (n_L = 768) | 53 | 43,456 |
| Lower threshold | 2.4% | 57 | 1/logical (n_L = 768) | 53 | 43,456 |
| Moderate routing | 4.0% | 37 | 3/logical (3 n_L = 2,304)* | 53 | 44,992 |
| Moderate factory | 4.0% | 37 | 1/logical (n_L = 768) | 73 (+20) | 57,536 |
| Combined moderate | 2.4% | 57 | 3/logical (3 n_L = 2,304)* | 73 (+20) | 59,072 |
| Distance-scaled routing | 4.0% | 37 | d/logical (n_L d = 28,416) | 53 | 71,104 |
| Distance-scaled routing | 2.4% | 57 | d/logical (n_L d = 43,776) | 53 | 86,464 |
| Pessimistic | 2.4% | 57 | d/logical (n_L d = 43,776) | 103 (+50) | 121,664 |
| RepetitionCat (ref.) | 2.4% | 57 | 1/logical (n_L = 768) | 53 | 124,864 |

*The moderate routing scenario (3 routing qubits per logical qubit)
is motivated by analogy with surface code lattice surgery, where
two ancilla channels are needed per logical qubit boundary for
X and Z basis measurements. For bias-tailored codes that only
correct Z errors, one of the two channels may be unnecessary,
but the non-planar connectivity of the [136, 34, 22] code likely
requires at least one additional routing qubit beyond the baseline
for inter-block communication. The factor of 3 should be understood
as a representative intermediate scenario rather than a derived
physical prediction.

Key observations:

1. **Threshold alone does not affect qubit count.** Changing p_th from
   4% to 2.4% at fixed routing (1/logical) leaves the total unchanged
   at 43,456 — only the runtime increases (5.1 to 7.9 hours). This is
   the structural advantage of the distance-independent layout.

2. **Routing is the dominant uncertainty.** The distance-scaled routing
   model (d qubits per logical) adds 28,000-44,000 qubits depending
   on p_th, more than doubling the non-factory contribution.

3. **Factory injection overhead matters.** Adding 50 ancilla qubits per
   factory (+94% per factory) increases the factory total from 37,312
   to 72,512.

4. **The pessimistic scenario is marginal for ECDLP-256.** At 121,664
   total qubits (p_th = 2.4%, d-scaled routing, +50 factory ancillas),
   the LDPC-cat count is only 3% below the repetition-cat baseline of
   124,864 — a margin well within the modeling uncertainty. The
   LDPC-cat advantage is established under baseline assumptions but
   becomes marginal under the most pessimistic combination. Moreover,
   the pessimistic routing model (d/logical) may not bound the worst
   case (Section 5.4), so the true pessimistic scenario could exceed
   the repetition-cat baseline. The advantage is more robust for
   RSA-2048, where even the pessimistic estimate (95,108) remains
   41% below the repetition-cat baseline (Table 8).

**Table 8.** Sensitivity analysis for RSA-2048 LDPCCat (n_L = 1,400,
36 factories).

| Scenario | p_th | d | Routing model (total routing qubits) | Factory/unit | Total |
|----------|------|---|--------------------------------------|-------------|-------|
| Baseline | 4.0% | 37 | 1/logical (n_L = 1,400) | 53 | 13,108 |
| Lower threshold | 2.4% | 57 | 1/logical (n_L = 1,400) | 53 | 13,108 |
| Combined moderate | 2.4% | 57 | 3/logical (3 n_L = 4,200)* | 73 (+20) | 16,628 |
| Distance-scaled routing | 2.4% | 57 | d/logical (n_L d = 79,800) | 53 | 93,308 |
| Pessimistic | 2.4% | 57 | d/logical (n_L d = 79,800) | 103 (+50) | 95,108 |
| RepetitionCat (ref.) | 2.4% | 57 | 1/logical (n_L = 1,400) | 53 | 161,508 |

For RSA-2048, the factory count is only 36, so factory injection
overhead has minimal impact. The dominant uncertainty is routing: at
d-scaled routing (d = 57), the routing qubits (79,800) exceed the
data + syndrome total (9,800) by 8x. Even in the pessimistic scenario,
the LDPC-cat estimate (95,108) remains 41% below the repetition-cat
baseline (161,508).

## 7. Cross-Architecture Context

Table 9 places our results alongside published estimates for the same
algorithms under different architectures.

**Table 9.** Cross-architecture comparison for cryptanalysis. LDPC-cat
ranges reflect the sensitivity analysis from Tables 7-8 (baseline to
pessimistic).

| Architecture | Qubit type | RSA-2048 | ECDLP-256 | Source |
|-------------|-----------|----------|-----------|--------|
| Surface code | Superconducting | < 1,000,000 | ~500,000 (est.) | Gidney [1] |
| qLDPC (Pinnacle) | Superconducting | < 100,000 | — | Webster et al. [2] |
| Repetition-cat | Cat qubit | 161,508 | 124,864 | This work (reproducing [3]) |
| **LDPC-cat** | **Cat qubit** | **13,000 - 95,000** | **43,000 - 122,000** | **This work** |

Several caveats apply to this comparison:

1. **Factory model.** The unfolded distillation factory is held constant
   across code families and may require modification for LDPC-cat
   connectivity (Section 5.5). The sensitivity analysis (Tables 7-8)
   partially addresses this by modeling +20 and +50 ancilla overhead
   per factory.

2. **Circuit-level threshold.** The LDPC-cat threshold of 4% is
   estimated from code-capacity simulations, not full circuit-level
   modeling. The minimum threshold at which the baseline d = 37 achieves
   the ECDLP-256 error budget is 3.8% (Section 5.3).

3. **Routing overhead.** The true routing overhead for LDPC-cat codes is
   unknown and depends on the physical embedding of the Tanner graph.
   The baseline (1/logical) and pessimistic (d/logical) bounds bracket
   a wide range.

4. **Comparison fairness.** The surface code and qLDPC estimates use
   superconducting transmons with different noise characteristics. Cat
   qubits are themselves complex bosonic modes requiring nonlinear
   elements and microwave drives. A fair comparison requires evaluating
   complete systems — including classical control, cryogenics, and
   connectivity — rather than raw qubit counts.

5. **RSA-2048 baseline of 13,108 qubits.** This figure, while
   striking, depends on the most optimistic assumptions (1 routing qubit
   per logical, code-agnostic factory injection, p_th = 4%). Under
   pessimistic assumptions, the RSA-2048 estimate increases to 95,108 —
   comparable to the Pinnacle architecture's < 100,000 superconducting
   qubits. The order-of-magnitude gap between baseline and pessimistic
   estimates underscores the importance of detailed routing and factory
   modeling for LDPC-cat architectures.

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
  theoretical ~10.3% [14].
- **Gouzien reproduction**: 124,864 qubits, within 1% of 126,133.

## 9. Discussion

### 9.1 The factory bottleneck

Our most actionable finding is that the magic state factory — not the
data code — dominates the LDPC-cat qubit budget under baseline
assumptions. For ECDLP-256, 86% of the 43,456 total qubits are factory
qubits. This means that further improvements to the data code (higher
encoding rate, better threshold) would yield diminishing returns. The
most impactful optimizations are:

- **Factory throughput.** Increasing the magic state production rate
  (states per factory per second) reduces the number of factories needed.
  The current model assumes one state per 5.5 * d QEC rounds per
  factory.

- **Factory-free approaches.** Direct gate synthesis methods that avoid
  distillation entirely would eliminate the factory overhead. These
  approaches are an active area of research.

- **Algorithmic Toffoli reduction.** Reducing the Toffoli count of the
  underlying algorithm directly reduces the factory count (Eq. 7).

However, the factory dominance is partially an artifact of the optimistic
baseline routing model. Under pessimistic routing (d qubits per logical),
the routing contribution (28,000-44,000 qubits) approaches the factory
contribution (37,000-73,000 qubits), and both must be optimized.

### 9.2 Robustness of the LDPC-cat advantage

The sensitivity analysis (Section 6.6) shows that the LDPC-cat advantage
over repetition codes is established under baseline assumptions and
persists across most tested scenarios. For RSA-2048, the advantage is
substantial even in the most pessimistic case (95,108 vs 161,508, a
41% reduction). For ECDLP-256, the advantage is clear under baseline
assumptions (43,456 vs 124,864, a 65% reduction) but becomes marginal
under the most pessimistic combination: 121,664 qubits, only 3% below
the repetition-cat baseline — a margin well within modeling uncertainty.

The LDPC-cat advantage is most fragile for ECDLP-256 under extreme
routing assumptions. If routing overhead scales with d AND the true
threshold is low (forcing high d), the routing qubits approach the
repetition code's data qubit count, eroding the encoding rate advantage.
Furthermore, since d-linear routing may not bound the worst case
(Section 5.4), the true pessimistic scenario could exceed the
repetition-cat baseline for ECDLP-256. This points to routing and
physical layout as the most critical open questions for LDPC-cat
architectures.

### 9.3 Comparison with qLDPC approaches

The Pinnacle architecture [2] achieves fewer than 100,000 physical
(superconducting) qubits for RSA-2048 using quantum LDPC codes with
novel "magic engines" for simultaneous distillation and injection.
Our LDPC-cat baseline of 13,108 cat qubits for the same algorithm is
substantially lower, but this comparison requires significant caveats.
Under pessimistic assumptions, our estimate rises to 95,108 — convergent
with Pinnacle's < 100,000 figure. This convergence is noteworthy: two
very different approaches (classical LDPC codes on biased-noise bosonic
qubits vs quantum LDPC codes on superconducting transmons) arrive at
similar physical qubit counts for the same algorithm under their
respective pessimistic/optimistic assumptions. Whether this reflects a
fundamental resource floor for RSA-2048 or a coincidence of modeling
choices is an open question. Moreover, cat qubits and superconducting transmons
have different physical characteristics: a cat qubit is a complex
bosonic mode requiring a nonlinear element and microwave drives, whereas
a transmon is a simpler device with more mature fabrication. The
relevant comparison is ultimately between complete systems — including
the classical control electronics, cryogenic infrastructure, and
connectivity requirements — rather than raw qubit counts.

### 9.4 Limitations and future work

Several limitations of our analysis motivate future work:

1. **Circuit-level simulation.** The current noise model uses the
   asymptotic scaling formula (Eq. 5) with calibrated thresholds rather
   than full circuit-level Monte Carlo simulation. The baseline LDPC-cat
   estimate at d = 37 requires p_th >= 3.8% to meet the error budget.
   Circuit-level simulations with noisy syndrome extraction are needed
   to determine whether this condition is satisfied. If the true
   threshold is below 3.8%, the required distance increases (Table 7),
   but the qubit count is unaffected under the baseline routing model.

2. **LDPC-cat routing and factory integration.** The routing model is
   the dominant source of uncertainty. Determining the true routing
   overhead requires embedding the [136, 34, 22] Tanner graph in a
   planar geometry and analyzing the ancilla requirements for lattice
   surgery between logical qubits. Similarly, the magic state injection
   interface for LDPC-cat codes has not been designed in detail; the
   number of additional ancilla qubits per injection is unknown.

3. **Code family optimization.** We use the [136, 34, 22] base code
   without optimizing over the LDPC-cat code family. Exploring larger
   codes in the family (varying ell) or alternative LDPC constructions
   (e.g., Elevator codes [17], Romanesco codes [18]) could yield further
   improvements in encoding rate or threshold.

4. **Decoding latency.** The resource estimates assume that decoding
   completes within one QEC cycle (T_cycle = 500 ns). For the repetition
   code, minimum-weight decoding of a 1D chain is O(d) and easily meets
   this budget. For the [136, 34, 22] LDPC-cat code, BP decoding with
   up to 100 iterations on a 136-variable Tanner graph is more
   expensive. If BP+OSD decoding cannot keep pace with the 500 ns cycle
   time, the architecture would need either (a) a faster decoder
   (e.g., hardware-accelerated BP), (b) a longer cycle time (increasing
   runtime proportionally), or (c) a sliding-window approach that
   tolerates multi-cycle decoding delay at the cost of additional
   syndrome buffer qubits. The decoding latency question is particularly
   acute for the OSD post-processor, which involves Gaussian elimination
   on a 102 x 136 matrix — feasible but non-trivial at 500 ns. We note
   that classical LDPC decoders in communications hardware achieve
   sub-microsecond latencies for comparable code sizes, suggesting this
   is an engineering rather than fundamental constraint.

5. **Physical parameter sensitivity.** A systematic exploration of the
   noise model parameter space (kappa_1/kappa_2, |alpha|^2, T_cycle,
   target error rate) would identify the operating regime where LDPC-cat
   codes provide the greatest advantage and the crossover points with
   competing architectures.

## 10. Conclusion

We have presented the first end-to-end resource estimates for
cryptanalysis using LDPC codes tailored for the biased noise of cat
qubits. By combining the [136, 34, 22] LDPC-cat code of Ruiz et al.
with the unfolded distillation protocol, our pipeline estimates 43,456
cat qubits for 256-bit ECDLP and 13,108 for RSA-2048 under baseline
assumptions, representing a 65% and 92% reduction compared to the
repetition-cat baseline. A sensitivity analysis over three uncertain
parameters (threshold, routing, factory injection) shows the advantage
is robust for RSA-2048 (pessimistic estimate 95,000, 41% below
baseline) but becomes marginal for ECDLP-256 under the most
pessimistic assumptions (122,000, only 3% below the repetition-cat
baseline).

The pipeline faithfully reproduces the Gouzien et al. result of ~126,000
cat qubits (within 1%), validating the calibration. The dominant cost
under baseline assumptions is the magic state distillation factory (86%
for ECDLP-256), though routing overhead becomes comparably important
under pessimistic assumptions. This finding reframes the optimization
landscape: progress requires improving factory efficiency, reducing
routing overhead through better physical layouts, or both.

The sensitivity analysis identifies routing and physical layout as the
most critical open questions. Detailed circuit-level simulation of the
LDPC-cat code under noisy syndrome extraction, combined with a concrete
Tanner graph embedding and factory injection protocol, would narrow the
uncertainty range substantially.

The `qec-cat` Haskell library — with its type-safe GF(2) algebra,
validated decoders, and complete resource estimation pipeline — provides
a foundation for extending these estimates to broader algorithm families,
more detailed noise models, and alternative LDPC code constructions.

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

[12] Alice & Bob, "A Cat Qubit That Jumps Every Hour," company technical
     report (2025). Not peer-reviewed; not available as a preprint.
     Cited for experimental context only; no quantitative parameters
     from this source enter our resource estimates.

[13] J. Roffe, D. R. White, S. Burton, and E. Campbell, "Decoding across
     the quantum low-density parity-check code landscape," Phys. Rev.
     Research 2, 043423 (2020).

[14] E. Dennis, A. Kitaev, A. Landahl, and J. Preskill, "Topological
     quantum memory," J. Math. Phys. 43, 4452 (2002).

[15] N. Delfosse and N. H. Nickerson, "Almost-linear time decoding
     algorithm for topological codes," Quantum 5, 595 (2021).

[16] A. G. Fowler, M. Mariantoni, J. M. Martinis, and A. N. Cleland,
     "Surface codes: Towards practical large-scale quantum computation,"
     Phys. Rev. A 86, 032324 (2012).

[17] O. Shanahan and D. Ruiz, "Elevator Codes: Concatenation for
     resource-efficient quantum memory under biased noise,"
     arXiv:2601.10786 (2026).

[18] C. Leroux and J. K. Iverson, "Romanesco codes: Bias-tailored qLDPC
     codes from fractal codes," arXiv:2506.00130 (2025).
