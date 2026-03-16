# Restriction Image Audit: Findings for BENT at n = 4

## The Definitive Empirical Picture of the Circuit-Level Obstruction

---

## Finding 1: The Global Circuit Produces a Valid Compatible Family

The manually constructed BENT circuit O(A(A(x₀,x₁),N(A(x₂,x₃))),A(A(x₂,x₃),N(A(x₀,x₁)))) of size 9 was restricted to all 24 sub-cubes. The result: **96/96 structural edges pass STRUCT-MATCH.** The global circuit's restrictions form a fully compatible family.

This means **H¹(F^{cir}_{BENT,9}) = {∗} — trivial**. At size bound s = 9, there IS a global circuit, so the presheaf has a global section and the descent condition is satisfied.

This was expected (BENT has low circuit complexity ≈ 7–9 gates), but it provides the baseline: a valid compatible family exists, produced by the global circuit's restrictions.

---

## Finding 2: The Compatible Family Requires Non-Minimal Circuits

The global circuit's restrictions to the 24 sub-cubes have the following sizes:

- AND sub-cubes (6 of them): restriction = `A(x0,x1)`, size 1. *This is the minimum-size AND circuit.*
- ZERO sub-cubes (4): restriction = `c0`, size 0. *Minimum.*
- NAND sub-cubes (2): restriction = `N(A(x0,x1))`, size 2. *Minimum.*
- x₀ sub-cubes (4): restriction = `x1`, size 0. *Minimum.* (Input relabeling from the global circuit.)
- x₁ sub-cubes (4): restriction = `x0`, size 0. *Minimum.*
- **XOR sub-cubes (4): restriction = `O(A(N(x0),x1),A(N(x1),x0))`, size 5.** ***Not minimum — the unique minimum-size XOR circuit has size 4.***

The structural gap is concentrated entirely in the XOR sub-cubes. The global circuit's restriction to a XOR-computing sub-cube produces a size-5 formula, not the size-4 minimum. This size-5 formula carries the structural fingerprint of the global computation (it decomposes XOR as OR-of-ANDs-of-NOTs, reflecting the global circuit's AND-then-XOR structure). The minimum-size XOR circuit AND(OR(x₀,x₁),NAND(x₀,x₁)) has a completely different DAG topology.

**Structural gap: γ(BENT) = 5/4 = 1.25** (measured at the XOR sub-cubes where the gap occurs).

---

## Finding 3: Restriction Image Sizes Vary by Orders of Magnitude

The restriction image I(s, d, k) — the number of distinct DAG-isomorphism classes arising from restricting size-≤s circuits to a 1-dimensional overlap — varies dramatically across sub-function types:

| Sub-function | Domain size (s≤4) | |I| (fix one input to 0) | |I| (fix one input to 1) |
|---|---|---|---|
| XOR | 1 | 1 | 1 |
| XNOR | 1 | 1 | 1 |
| AND | 2,228 | 7 | 28 |
| NAND | 226 | 3 | 9 |
| ZERO | 9,624 | 39 | 39 |
| x₀ | 4,729 | 8 (one direction), 111 (other direction) | 8 or 111 |
| x₁ | 4,729 | 111 (one direction), 8 (other direction) | 111 or 8 |

The compression ratio — domain size divided by restriction image size — reveals the rigidity:

- **XOR: compression = 1/1.** The restriction map is injective on the singleton domain. There is exactly one circuit structure, and it restricts to exactly one overlap structure. Zero degrees of freedom.
- **AND: compression = 2228/7 ≈ 318.** Many circuits compute AND, but they collapse to only 7 distinct restriction classes when one input is fixed to 0. High compression = high STRUCT-MATCH selectivity.
- **ZERO: compression = 9624/39 ≈ 247.** Similar — enormous domain, few restriction classes.
- **x₀ (identity): asymmetric.** Fixing the "active" input gives 8 classes; fixing the "inactive" input gives 111 classes. The direction of restriction matters because the identity function has inherent asymmetry between its inputs.

---

## Finding 4: 50% of Structural Edges Have Empty Restriction Image Intersection

At size budget s ≤ 4, **48 of 96 structural edges (50.0%) have |I_U ∩ I_{U'}| = 0** — meaning there is no choice of size-≤4 circuits for U and U' that produces matching DAG structures on the overlap.

This is the quantitative backbone of the incompatibility observed in the previous CSP experiments. Half of all structural constraints are *fundamentally unsatisfiable* at the minimum-size budget, regardless of which circuits are chosen. The violation is not a consequence of poor circuit selection — it is a structural impossibility.

---

## Finding 5: The Incompatibility Is Concentrated at XOR–X Boundaries

The detailed per-pair breakdown reveals where the empty intersections occur:

**All 48 empty-intersection edges involve XOR sub-cubes overlapping with non-XOR sub-cubes** (AND, ZERO, x₀, x₁, or NAND).

The XOR sub-function has a singleton restriction image (|I| = 1 in every direction). When a XOR sub-cube overlaps with an AND sub-cube, the XOR side can only produce one specific DAG class on the overlap. The AND side produces 7 or 28 classes. If that one XOR class doesn't appear among the AND classes, the intersection is empty. And at size ≤ 4, it doesn't appear — because the unique size-4 XOR circuit's restriction is a different DAG class than any restriction of a size-≤4 AND circuit.

The XOR sub-cubes act as "structural bottlenecks" — rigid points in the constraint graph that force specific DAG classes on every adjacent overlap. Because XOR has a unique circuit implementation in {AND, OR, NOT}, there is zero flexibility. Any incompatibility at a XOR boundary is unresolvable.

---

## Finding 6: The CRITICAL COMPARISON — Why Minimum-Size Fails

The audit's most important output is the "CRITICAL COMPARISON" section: for several sub-cubes, the global circuit's restriction produces a DAG class that is **not achievable by any size-≤4 circuit**.

For x₀-computing sub-cubes (e.g., sub-cube [5]): the global circuit restricts to `c0` or `x1` on certain overlaps. The size-≤4 x₀ circuits restrict to a *different* set of DAG classes on the same overlaps. The global circuit's restriction class is absent from the size-≤4 achievable set.

This is the **restriction image misalignment** in concrete form. The global circuit carries structural information (from the AND-then-XOR decomposition of BENT) that propagates through restrictions to produce DAG classes unreachable by locally-optimal circuits. The misalignment is not just "the global circuit is bigger" — it is "the global circuit's structure is fundamentally different from any local optimum, and this difference is visible in the restriction images."

---

## Implications for the Combinatorial Propagation Conjecture

The n = 4 data provides the following empirical grounding:

**The restriction image I(s, d, k) is the correct measure of structural rigidity.** Functions with unique or near-unique circuit implementations (XOR, XNOR) have singleton restriction images, making them perfectly rigid structural bottlenecks. Functions with many implementations (ZERO, x₀) have larger restriction images but still orders of magnitude smaller than their domains, making STRUCT-MATCH highly selective.

**The conjecture should target restriction image misalignment, not image size alone.** The incompatibility arises not because restriction images are small, but because restriction images from adjacent sub-cubes *don't overlap*. The right quantity is |I_U ∩ I_{U'}| / (|I_U| · |I_{U'}|) — the probability that two random restriction classes from adjacent sub-cubes happen to match.

**The structural gap γ is driven by the hardest sub-functions.** For BENT, the gap comes entirely from the XOR sub-cubes, where the global circuit's restrictions are size 5 vs the minimum size 4. As n grows and sub-functions become more complex, the number of "bottleneck" sub-functions (those with few circuit implementations) should increase, widening the gap.

**For high-complexity truth tables (C(T) ≫ s), the picture inverts.** BENT has a small global circuit (size ≈ 9), so a compatible family exists — it's the global circuit's restrictions. For functions with circuit complexity ≫ s, no small global circuit exists, so compatible families (if they exist at all) must arise from circuits that are NOT restrictions of any global circuit. The restriction images of such "orphaned" compatible families are the key unexplored object. Whether they exist, and whether they can overlap across edges, is the question the Propagation Conjecture must answer.

---

## Formal Statement: Combinatorial Propagation Conjecture

**Conjecture (Restriction Image Misalignment).** Let T be a truth table with circuit complexity C(T) > s², and let F^{cir}_{T,s} be the circuit-level presheaf on the d-sub-cube site with d = c·log n and s = poly(n). For each structural edge (U, U') in G_{n,d}, define:

- I_U = im(ρ_{U→U∩U'}) ⊆ DAG-classes on the overlap, restricted from F^{cir}(U)
- I_{U'} = im(ρ_{U'→U∩U'}) ⊆ DAG-classes on the overlap, restricted from F^{cir}(U')

Then the **edge misalignment rate**

M(T, s) = (number of structural edges with I_U ∩ I_{U'} = ∅) / |E_s|

satisfies M(T, s) ≥ Ω(1/n^c) for a constant c depending on the dimension parameter.

**Why this is the right conjecture:** It targets the exact combinatorial object — restriction image intersection — that the n = 4 experiments show drives the incompatibility. It avoids Fourier analysis (which is blind to DAG structure) and instead works directly with the circuit DAG space under restriction. And it connects to the expansion analysis: if M(T, s) ≥ Ω(1/n^c), then combined with the edge expansion h_s ≥ Ω(d/n) of G_{n,d}, the misaligned edges are distributed across the entire graph, precluding any local repair.
