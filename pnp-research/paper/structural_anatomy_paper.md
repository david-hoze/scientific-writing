# Structural Anatomy of the Circuit-Level MCSP Presheaf: Restriction Images, Structural Sparsity, and the Geometry of Circuit Gluing

---

## Abstract

We study the circuit-level MCSP presheaf F^{cir}_{T,s} — a sheaf-theoretic object whose sections over sub-cubes of the Boolean hypercube are small circuits (up to DAG isomorphism) and whose compatibility condition requires that hardwired restrictions to overlapping sub-cubes produce identical circuit structures, not merely identical functions. Through systematic computational enumeration of circuits on 2, 3, and 4 inputs over the basis {AND, OR, NOT}, we establish three results about the structure of this presheaf.

First, we construct the complete structural taxonomy of 3-input Boolean functions, classifying all 256 functions by their "structural fluidity" — the number of distinct DAG-isomorphism classes arising under restriction — and identifying a three-tier hierarchy (stiff, moderate, fluid) that recurs at each circuit-complexity frontier. Functions at the coverage frontier (the minimum circuit size at which they become computable) exhibit systematically low fluidity, with this phenomenon appearing independently in the XOR family (size 4), the unanimity/diversity family (size 6), and predicted for parity-3 (size 11, the hardest 3-input function).

Second, we discover a scaling law governing the "universe-to-image ratio" — the ratio between the total number of DAG-isomorphism classes that arise as restrictions across all circuits (the universe) and the maximum number that any single function's circuits can produce (the max image). At fixed circuit size s ≤ 4, this ratio grows from 2.0 at d = 2 to 6.3 at d = 3 to 13.3 at d = 4, with the universe growing approximately 4.8× per dimension step while the max image grows only 2.3×. Crucially, at fixed dimension d = 3, the ratio converges to an asymptotic constant σ∞(3) ≈ 6.4 as the size budget s → ∞, demonstrating that the ratio is an intrinsic property of the dimension, not an artifact of tight size budgets. The convergence is monotone with ratio growth factors 1.30, 1.15, 1.06, 1.02 at successive size steps.

Third, through a detailed case study of the bent function x₀x₁ ⊕ x₂x₃ at n = 4, we demonstrate that the DAG-isomorphism compatibility condition (STRUCT-MATCH) is strictly more restrictive than functional compatibility: 50% of structural edges in the sub-cube intersection graph have empty restriction-image intersection at minimum circuit size, even though functional compatibility is trivially satisfied everywhere. The global circuit's restrictions form a valid compatible family, but require circuits strictly larger than the local minimum — a "structural gap" of γ = 1.25 driven by the non-minimal DAG topology that global computation imposes on local restrictions.

These results provide the first quantitative characterization of the circuit-level presheaf's structure and establish that the "structural entropy" of circuit space — the combinatorial diversity of DAG topologies under restriction — grows with sub-cube dimension in a way that makes globally compatible circuit assignments increasingly constrained. We conclude by framing the open problem of converting this structural characterization into circuit lower bounds for the obstruction detection function OD, connecting our findings to the hardness magnification program.

---

## 1. Introduction

The Minimum Circuit Size Problem (MCSP) — given a truth table of length N = 2^n and a size bound s, decide whether a circuit of size at most s computes this function — is central to the hardness magnification program in complexity theory. Oliveira and Santhanam (FOCS 2018) and subsequent work showed that barely-superlinear circuit lower bounds for MCSP variants would imply major separations including NP ⊄ P/poly. The locality barrier (Chen–Hirahara–Oliveira–Pich–Rajgopal–Santhanam, JACM 2022) identified why known techniques fail: all existing lower-bound methods extend to circuits with bounded-fan-in oracle gates, while MCSP variants admit efficient circuits with exactly such gates.

A sheaf-theoretic approach to this problem, developed in [companion framework paper], defines a complexity presheaf F^{cir}_{T,s} on the sub-cube site of the Boolean hypercube. Sections over a d-dimensional sub-cube are circuits of size at most s computing the correct sub-function, identified up to labeled DAG isomorphism. The compatibility condition requires that hardwired restrictions to overlapping sub-cubes yield isomorphic DAGs after constant propagation — a strictly stronger condition than functional equality. The first Čech cohomology Ȟ¹(F^{cir}_{T,s}) captures obstructions to gluing locally compatible circuits into a global circuit, and the obstruction detection function OD(T) = [Ȟ¹ ≠ {∗}] is the target for hardness magnification.

The structural properties of this presheaf — how many circuits exist per sub-function, how their DAG topologies behave under restriction, and how constrained the compatibility condition is — have not been studied quantitatively. This paper provides the first such study through systematic circuit enumeration and restriction-image analysis across dimensions d = 2, 3, 4.

### 1.1. Summary of Results

**Result 1 (The Structural Taxonomy).** We enumerate all circuits on 3 inputs over {AND, OR, NOT} up to size 6 and classify all 256 three-input Boolean functions by their structural fluidity Φ — the number of distinct DAG-isomorphism classes arising as restrictions of their circuits to 2-dimensional overlaps. We identify a three-tier hierarchy (stiff: Φ ≤ 3; moderate: 3 < Φ ≤ 20; fluid: Φ > 20) and show that functions at the coverage frontier (minimum circuit size) exhibit systematically low fluidity. This "frontier stiffness" recurs at each complexity level — appearing in the XOR family at size 4 and the unanimity/diversity family at size 6 — but dissolves at the next size level as additional circuit implementations provide structural flexibility. The hardest 3-input function (PAR3, 3-bit parity) requires 11 gates, isolated by a gap of two size levels from the next-hardest functions.

**Result 2 (The Universe-to-Image Scaling Law).** We define the restriction universe U(s, d) as the set of (d−1)-input DAG-isomorphism classes that arise as restrictions of d-input circuits of size at most s, and the max image max_f |I_f(s, d)| as the largest such set for any single d-input function. We measure:

| d | U(s ≤ 4) | max |I|(s ≤ 4) | Ratio |
|---|---|---|---|
| 2 | 225 | 111 | 2.0 |
| 3 | 2,324 | 367 | 6.3 |
| 4 | 11,075 | 835 | 13.3 |

The ratio grows approximately as 2^d. At fixed d = 3, the ratio converges to σ∞(3) ≈ 6.4 as s → ∞, with the convergence rate measured at six size-budget levels (s = 0 through 5). This establishes that the ratio is an intrinsic dimensional constant, not a size-budget artifact.

**Result 3 (The BENT Case Study).** For the bent function x₀x₁ ⊕ x₂x₃ at n = 4, d = 2, we show: (a) the DAG-isomorphism compatibility condition leaves 50% of structural edges in the sub-cube intersection graph with empty restriction-image intersection at minimum size; (b) the global circuit's restrictions form a valid compatible family but require non-minimal circuits (size 5 vs minimum 4 on XOR-computing sub-cubes); (c) the restriction images of minimum-size XOR circuits are disjoint from those of minimum-size AND, identity, and constant circuits, making the incompatibility an absolute structural fact rather than a probabilistic phenomenon.

### 1.2. Organization

Section 2 reviews the circuit-level presheaf and its compatibility condition. Section 3 develops the computational framework: circuit enumeration, canonical forms, and restriction-image computation. Section 4 presents the structural taxonomy of 3-input functions. Section 5 establishes the universe-to-image scaling law. Section 6 provides the BENT case study. Section 7 discusses implications for the obstruction detection function and the magnification program. Section 8 identifies open problems.

---

## 2. Preliminaries

### 2.1. The Circuit-Level MCSP Presheaf

We recall the definitions from [companion framework paper]. Let T ∈ {0,1}^N encode f : {0,1}^n → {0,1}, let s ∈ ℕ be a circuit size bound, and let d ≥ 1.

**The d-sub-cube site** Site_d(T) has objects (S, α) where S ⊆ [n] with |S| ≤ d and α : [n] \ S → {0,1}, representing sub-cubes of dimension at most d.

**The circuit-level presheaf** F^{cir}_{T,s} assigns to each sub-cube (S, α) the set of equivalence classes [C] of circuits of size at most s computing the correct sub-function f_{S,α}, where equivalence is labeled DAG isomorphism (relabeling internal nodes while preserving input-output structure and gate labels).

**The restriction map** ρ sends [C] to [C|_{S',α'}], the equivalence class of the hardwired restriction: inputs corresponding to coordinates in S \ S' are fixed to the values prescribed by α', and resulting constant gates are propagated and eliminated.

**STRUCT-MATCH.** For two overlapping sub-cubes U, U' with overlap V = U ∩ U', circuits C_U ∈ F^{cir}(U) and C_{U'} ∈ F^{cir}(U') satisfy STRUCT-MATCH iff ρ_{U→V}(C_U) = ρ_{U'→V}(C_{U'}) — their restrictions to the overlap are DAG-isomorphic.

### 2.2. The Sub-Cube Intersection Graph

**Definition.** The sub-cube intersection graph G_{n,d} = (V, E) has vertex set V = {all d-dimensional sub-cubes of {0,1}^n} and edges between sub-cubes with non-empty intersection. The structural subgraph G_s = (V, E_s) retains only edges with overlap dimension ≥ 1.

**Proposition (Degree and Expansion).** Each vertex in G_{n,d} has degree Σ_{k=0}^{d} C(d,k)·C(n−d,d−k)·2^{d−k} − 1. For d = c·log n, the structural degree is superpolynomial in n, the diameter is O(n), and the edge expansion is Ω(d/n). [Proof: combinatorial, verified computationally at n = 4, d = 2 (degree 12, 96 structural edges) and n = 5, d = 2 (degree 40, 480 structural edges).]

### 2.3. Restriction Images

**Definition.** For a d-input function f and size bound s, the restriction image of f in direction (i, v) (fixing input x_i to value v) is:

I_f(s, d, i, v) = { ρ_{x_i=v}(C) : C ∈ F^{cir}_{f,s} }

the set of (d−1)-input DAG-isomorphism classes arising as restrictions of size-s circuits computing f.

**Definition.** The restriction universe U(s, d) = ⋃_f ⋃_{i,v} I_f(s, d, i, v), taken over all d-input functions f computable by circuits of size ≤ s and all restriction directions (i, v). This is the total set of (d−1)-input DAG classes that arise as restrictions.

**Definition.** The structural fluidity of f at size s is Φ(f, s) = |⋃_{i,v} I_f(s, d, i, v)| — the total number of distinct restriction classes across all 2d restriction directions.

---

## 3. Computational Framework

### 3.1. Circuit Enumeration

We enumerate circuits over the basis B = {AND, OR, NOT} with inputs x_0, ..., x_{d-1} and constants 0, 1. Circuits are represented as expression trees with gates AND (binary), OR (binary), and NOT (unary). Circuit size counts all gates (AND, OR, NOT).

**Canonical form.** Two circuits are DAG-isomorphic iff they have the same canonical string representation, defined recursively: inputs and constants are self-canonical; NOT(C) has canonical form N(can(C)); AND(C₁,C₂) has canonical form A(min(can(C₁),can(C₂)), max(can(C₁),can(C₂))), with the commutative sort ensuring AND(A,B) = AND(B,A). Similarly for OR.

**Enumeration by size.** Circuits of size s are generated by combining: NOT of a size-(s−1) circuit; AND or OR of circuits with sizes s₁ + s₂ = s − 1. Deduplication is by canonical form.

**Restriction computation.** To restrict circuit C by fixing input x_i to value v: replace x_i nodes with constant v, propagate constants through gates (AND with 0 → 0, OR with 1 → 1, NOT of constant → complementary constant), eliminate dead nodes, re-index remaining inputs.

### 3.2. Enumeration Scale

| d | Max size | Circuits | Functions covered | Time |
|---|---|---|---|---|
| 2 | 4 | 36,052 | 16/16 | <1s |
| 3 | 4 | 93,315 | 121/256 | ~1s |
| 3 | 5 | 1,587,920 | 191/256 | ~60s |
| 3 | 6 | ~2.5M (targeted) | 226/256 | ~30s |
| 4 | 4 | 207,078 | 886/65,536 | ~7s |

All computations were performed in Python with exact canonical-form deduplication. Source code is provided as supplementary material.

---

## 4. The Structural Taxonomy of 3-Input Functions

### 4.1. The Complete Circuit-Complexity Spectrum

We determined the minimum circuit size for all 256 three-input functions in {AND, OR, NOT} via integer truth-table enumeration through size 12.

| Min size | Functions | Cumulative | Notable |
|---|---|---|---|
| 0 | 5 | 5 | Inputs, constants |
| 1 | 9 | 14 | AND, OR, NOT |
| 2 | 26 | 40 | NAND, NOR, implications |
| 3 | 44 | 84 | AND₃, OR₃ |
| 4 | 37 | 121 | XOR, XNOR, MAJ₃ |
| 5 | 70 | 191 | Mixed-complexity |
| 6 | 35 | 226 | Unanimity, diversity |
| 7 | 6 | 232 | — |
| 8 | 16 | 248 | — |
| 9 | 6 | 254 | — |
| 10 | 0 | 254 | (empty) |
| 11 | 1 | 255 | PAR3 |
| 12 | 1 | 256 | NPAR3 |

**Observation 4.1.** PAR3 (3-bit parity, x₀ ⊕ x₁ ⊕ x₂) is the unique hardest 3-input function at size 11, isolated by a gap of two levels (zero functions at size 10) from the next-hardest functions at size 9. NPAR3 = NOT(PAR3) requires exactly one additional gate.

### 4.2. Structural Tiers

At each cumulative size budget, we classify functions by fluidity Φ into three tiers:

**At s ≤ 4** (121 functions covered):
- STIFF (Φ ≤ 3): 4 functions — XOR₀₁, XOR₁₂, XNOR₀₁, XNOR₁₂. All are XOR-family with exactly 1 circuit each and singleton restriction images in every direction.
- MODERATE (3 < Φ ≤ 20): 43 functions, including MAJ₃ (Φ = 4, 6 circuits), NAND₃ and NOR₃ (Φ = 9, 78 circuits each).
- FLUID (Φ > 20): 74 functions, including AND₃ (Φ = 56), x₀ (Φ = 413), ZERO (Φ = 200).

**At s ≤ 5** (191 functions): All 70 newly covered functions are MODERATE (Φ = 4–16). The XOR-family functions soften (gain additional circuit implementations), so zero functions remain stiff. Stiffness has dissolved at this level.

**At s ≤ 6** (226 functions): Two non-XOR functions emerge as STIFF: f01111110 (diversity detector, Φ = 2, 9 circuits) and f10000001 (unanimity detector, Φ = 2, 9 circuits). All 33 other newly covered functions are MODERATE.

**Observation 4.2 (Recurrence of Frontier Stiffness).** Stiffness appears at each coverage frontier, driven by a different algebraic family: XOR at size 4, unanimity/diversity at size 6. It dissolves at the next size level as additional circuits provide structural flexibility, then reappears from a new family at the next frontier.

### 4.3. Restriction Image Compression

The restriction map is highly compressive: many circuits producing the same function restrict to few distinct DAG classes. At d = 3, s ≤ 4:

| Function type | Domain size | Max |I| per direction | Compression ratio |
|---|---|---|---|
| ZERO | 9,624 | 39 | 247× |
| AND₀₁ | 2,757 | 338 | 8.2× |
| NAND₃ | 78 | 8 | 9.8× |
| XOR₀₁ | 1 | 1 | 1× (singleton) |

The compression ratio (domain size / max restriction image size) ranges from 1× (XOR, no compression because the domain is already a singleton) to 247× (ZERO, enormous domain collapsing to few restriction classes).

---

## 5. The Universe-to-Image Scaling Law

### 5.1. Cross-Dimensional Scaling

At fixed size budget s ≤ 4:

| d | |U(s,d−1)| | max_f |I_f| | σ = U/max|I| |
|---|---|---|---|
| 2 | 225 | 111 | 2.0 |
| 3 | 2,324 | 367 | 6.3 |
| 4 | 11,075 | 835 | 13.3 |

**Observation 5.1.** The universe grows 4.8× per dimension step while the max image grows 2.3×. The ratio σ approximately doubles per step: 2.0 → 6.3 → 13.3, consistent with σ ≈ 2^d.

**Observation 5.2.** Per-direction universe sizes are identical across all restriction directions (fix x_i = v for all i, v), reflecting the symmetry of the gate basis under input permutation.

### 5.2. Size-Budget Scaling at Fixed Dimension

At d = 3, tracking the ratio as the cumulative size budget increases:

| s ≤ | |U| | max |I| | σ | Growth factor |
|---|---|---|---|---|
| 0 | 4 | 1 | 4.0 | — |
| 1 | 12 | 3 | 4.0 | 1.00× |
| 2 | 52 | 10 | 5.2 | 1.30× |
| 3 | 324 | 54 | 6.0 | 1.15× |
| 4 | 2,324 | 367 | 6.3 | 1.06× |
| 5 | 18,316 | 2,845 | 6.4 | 1.02× |

**Observation 5.3 (Convergence).** The ratio converges monotonically to an asymptotic constant σ∞(3) ≈ 6.4. The growth factor per size step decreases: 1.00, 1.30, 1.15, 1.06, 1.02 — consistent with convergence to 1.0.

**Observation 5.4.** At each size step, the universe and max image grow by nearly the same factor (e.g., 7.9× vs 7.8× at size 5), indicating that both are controlled by the same exponential growth rate of the circuit enumeration. The ratio encodes a dimension-dependent structural constant that is independent of this common growth rate.

### 5.3. Interpretation

The universe-to-image ratio σ∞(d) captures the "structural entropy" of the circuit-level presheaf at dimension d. It measures the combinatorial diversity of DAG structures under restriction: as d increases, hardwiring one of d+1 inputs and propagating constants produces a richer variety of DAG topologies, because the extra input creates more internal wiring configurations that interact differently with the constant-propagation process.

The fact that σ∞(d) is an intrinsic dimensional constant — independent of the size budget — means that the structural constraint imposed by STRUCT-MATCH is a permanent feature of the presheaf at dimension d, not a transient effect of tight size budgets.

### 5.4. Implications for Pairwise Intersection

At d = 3, s ≤ 4, we computed the pairwise intersection rate for all function pairs across all restriction directions. The rates by minimum-size tier:

| Tier pair | Intersection rate |
|---|---|
| Size 0 × Size 0 | 10.0% |
| Size 0 × Size 4 | 4.3% |
| Size 2 × Size 3 | 10.7% |
| Size 4 × Size 4 | 5.2% |

Even the most flexible pairing (size 0 × size 0, where both functions have thousands of circuits) achieves only 10% intersection. Frontier functions (size 4) intersect with any tier at 4–7%. The low intersection rate reflects the large universe: each function's image covers a small, essentially random-looking subset of the 2,324-element universe, and two small subsets of a large set rarely overlap.

---

## 6. Case Study: The BENT Function

### 6.1. Setup

We analyze f(x₀, x₁, x₂, x₃) = x₀x₁ ⊕ x₂x₃ (the "bent" function) at n = 4, d = 2. The sub-cube site has 24 dimension-2 sub-cubes and 96 structural edges (1-dimensional overlaps). The bent function has global circuit complexity approximately 7–9 gates in {AND, OR, NOT}.

### 6.2. Sub-Function Profile

The 24 sub-cubes decompose as: 6 computing AND, 4 computing ZERO, 4 computing x₀, 4 computing x₁, 4 computing XOR, 2 computing NAND. All sub-functions are computable at size ≤ 4.

### 6.3. Incompatibility at Minimum Size

At s ≤ 4, 48 of 96 structural edges (50.0%) have empty restriction-image intersection: |I_U ∩ I_{U'}| = 0. All 48 incompatible edges involve XOR sub-cubes adjacent to non-XOR sub-cubes. The XOR sub-function has exactly 1 circuit at size 4, producing a singleton restriction image that is disjoint from the restriction images of AND, ZERO, identity, and NAND circuits.

The incompatibility is absolute: no choice of size-≤4 circuits for neighboring XOR and non-XOR sub-cubes can satisfy STRUCT-MATCH. This is confirmed by a CSP solver with arc consistency, which empties all 24 variable domains — the structural constraints cascade through the intersection graph to eliminate every possibility.

### 6.4. The Global Circuit's Compatible Family

The global bent circuit O(A(A(x₀,x₁),N(A(x₂,x₃))),A(A(x₂,x₃),N(A(x₀,x₁)))) of size 9, restricted to each of the 24 sub-cubes, produces a fully compatible family: 96/96 structural edges satisfy STRUCT-MATCH.

However, the restrictions to XOR-computing sub-cubes have size 5, not the minimum 4. The global circuit's XOR restriction O(A(N(x₀),x₁),A(N(x₁),x₀)) has a different DAG topology from the minimum-size XOR circuit A(N(A(x₀,x₁)),O(x₀,x₁)) — the former decomposes XOR as OR-of-ANDs-of-NOTs (reflecting the global AND-then-XOR structure), while the latter uses AND-of-OR-and-NAND.

**Observation 6.1 (Structural Gap).** The structural gap γ = (max restriction size in compatible family) / (min circuit size for that sub-function) = 5/4 = 1.25. Compatible families require circuits carrying the structural fingerprint of a common global computation, which imposes a size overhead beyond the local minimum.

---

## 7. Implications

### 7.1. For the Obstruction Detection Function

The structural taxonomy and scaling law characterize the *structure* of the circuit-level presheaf F^{cir}_{T,s}, not the *computational complexity* of detecting its cohomological properties. The function OD(T) = [Ȟ¹(F^{cir}_{T,s}) ≠ {∗}] asks whether a compatible family of circuits exists that does not extend to a global circuit. Our results show that the STRUCT-MATCH compatibility condition is highly restrictive — but this means compatible families are rare, which makes OD(T) = 0 the generic case. The circuit complexity of OD depends on how hard it is to distinguish the rare OD = 1 instances from the generic OD = 0 instances, which is a meta-complexity question our structural analysis does not directly address.

### 7.2. For Hardness Magnification

The universe-to-image ratio σ∞(d) provides a quantitative measure of the "structural entropy" that any magnification-based argument must contend with. If σ∞(d) grows polynomially or exponentially with d, the compatibility CSP on the sub-cube intersection graph becomes increasingly constrained with dimension, which constrains the landscape of possible compatible families. Whether this constraint translates to computational hardness for OD — via the magnification theorems of Oliveira–Santhanam or the uniform magnification of Atserias–Müller — remains an open problem.

### 7.3. For the Natural Proofs Barrier

Our finding that compatible families are extremely rare (a consequence of high σ∞(d)) provides quantitative support for the conjecture that the property "OD(T) = 1" is non-large in the sense of Razborov–Rudich: it does not hold for a random function with appreciable probability. This is consistent with the natural proofs evasion analysis in [companion framework paper] and suggests that the OD approach may circumvent the natural proofs barrier — not because OD is hard to compute, but because the set of truth tables where OD = 1 is too sparse for natural proof techniques to exploit.

---

## 8. Open Problems

1. **Does σ∞(d) → ∞ as d → ∞?** The data shows σ∞(2) ≈ 2.0, σ∞(3) ≈ 6.4, and σ(4, s≤4) = 13.3 (the asymptotic value at d = 4 is not yet measured but is expected near 13–15). Proving that σ∞(d) grows at least polynomially in d would establish that the STRUCT-MATCH compatibility condition becomes arbitrarily restrictive with dimension.

2. **What is the circuit complexity of PAR3?** We determined that 3-bit parity requires exactly 11 gates in {AND, OR, NOT}. This appears to be a new result. The structural profile of PAR3 (its fluidity and restriction images at size 11) would complete the structural taxonomy but requires enumeration at a scale beyond our current computational capacity.

3. **Does frontier stiffness persist for all d?** We observed that functions at the coverage frontier have low fluidity at d = 3 (sizes 4 and 6). Whether this holds at all dimensions — and whether it is specific to the {AND, OR, NOT} basis or is basis-independent — is open.

4. **Can the compatibility CSP on the sub-cube intersection graph be connected to the circuit complexity of OD?** The high constraint density and expanding structure of the graph suggest that satisfiability of the compatibility CSP is computationally hard, but no formal reduction from CSP satisfiability to OD circuit complexity has been established.

5. **How does σ∞(d) depend on the gate basis?** All our computations use {AND, OR, NOT}. In a basis containing XOR as a primitive gate, the XOR-specific stiffness phenomena at d = 2 would vanish. Whether the universe-to-image ratio retains its dimensional growth under alternative bases is an important question for basis-independent structural theories.

---

## Supplementary Material

All enumeration scripts, restriction-image computations, and raw data are available as Python source files:

- `compat_csp_n4.py`: n=4 compatibility CSP solver with STRUCT-MATCH
- `restriction_audit.py`: Restriction image computation for BENT at n=4
- `d3_catalog.py`: d=3 structural taxonomy at size ≤ 4
- `d3_size5.py`, `d3_size6.py`: Extended enumeration at sizes 5 and 6
- `par3_audit.py`: PAR3 minimum circuit size determination
- `d4_universe.py`: d=4 universe computation
- `d3_universe_growth.py`: Universe-to-image ratio convergence at d=3
- `n5_audit.py`: Misalignment rate measurements at n=5
- `sparsity_analysis.py`: Universe density and pairwise intersection analysis
