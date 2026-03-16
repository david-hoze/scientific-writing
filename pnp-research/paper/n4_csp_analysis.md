# The n = 4 Compatibility CSP: Empirical Anatomy of the Circuit-Level Obstruction

## Results from the d = 2 Sub-cube Site, Basis {AND, OR, NOT}

---

## Site Geometry

For n = 4, d = 2, the sub-cube site has 24 variables (dimension-2 sub-cubes) and 144 constraint edges (overlapping pairs). Of these, 96 are 1-dimensional overlaps (where STRUCT-MATCH imposes structural compatibility beyond functional equality) and 48 are 0-dimensional overlaps (single-point intersections, where compatibility reduces to output agreement). The constraint density is 144/276 ≈ 0.52 — over half of all sub-cube pairs interact.

Each sub-cube is a 4-point face of the 4-cube, with 2 free coordinates (the sub-function's inputs) and 2 fixed coordinates (determining which face).

---

## The Key Finding: BENT Function (x₀x₁ ⊕ x₂x₃)

**This is the headline result.** For the bent function T(x₀, x₁, x₂, x₃) = x₀x₁ ⊕ x₂x₃:

**F^{fn} compatible family: EXISTS.** Every sub-function on a 2-dimensional sub-cube is computable by a small circuit (AND, ZERO, x₀, x₁, XOR, or NAND, all with minimum sizes 0–4). At the functional level, these are trivially compatible — they all compute the correct sub-function.

**F^{cir} compatible family: DOES NOT EXIST** at minimum size, or minimum size + 1.

This is the circuit-level obstruction in action: locally, every face of the 4-cube has a small circuit computing the correct sub-function. But no choice of circuits — one per face — satisfies STRUCT-MATCH on all overlaps. The DAG-isomorphism condition after hardwiring and constant propagation rules out every possible assignment.

The solver doesn't just fail after exhaustive search — **arc consistency alone empties every domain**. The structural constraints propagate so aggressively that no single sub-cube retains any viable circuit option. This means the obstruction is not a subtle corner case; it is a pervasive, cascading incompatibility driven by the interaction pattern of the 144 constraints.

At minimum-size + 1 (allowing larger circuits), 68 of 96 structural constraints are tighter than their functional counterparts, and the CSP remains infeasible even with a search space of ~10^18.

---

## Why Minimum-Size Circuits Are Incompatible

The mechanism becomes clear when we trace a specific global circuit. The bent function x₀x₁ ⊕ x₂x₃ has a global circuit of approximately 7 gates in {AND, OR, NOT}: compute a = AND(x₀, x₁) and b = AND(x₂, x₃), then output XOR(a, b) = OR(AND(a, NOT(b)), AND(NOT(a), b)).

When this global circuit is restricted to a 2-dimensional sub-cube — say, free coordinates {0, 2} with x₁ = 1, x₃ = 1 — the result after constant propagation is: OR(AND(x₀, NOT(x₂)), AND(NOT(x₀), x₂)). This is a **size-5** formula for XOR. It is NOT the minimum-size XOR formula, which is AND(OR(x₀, x₂), NOT(AND(x₀, x₂))) at size 4.

The two formulas compute the same function (XOR) but have different DAG structures. They are **not** DAG-isomorphic. The global circuit's restriction carries the structural "fingerprint" of the global computation — it decomposes XOR into AND/NOT-of-products rather than AND-of-OR-and-NAND. This fingerprint is what STRUCT-MATCH detects.

The minimum-size circuit (AND(OR, NOT(AND))) does not arise from any global circuit of the bent function. It is a "locally optimal but globally orphaned" structure. Choosing it for one sub-cube forces incompatible restrictions on neighboring sub-cubes, and the incompatibility cascades through the constraint graph until every domain is empty.

**This is the Sparse Domain Trap made concrete.** Restricting domains to minimum-size circuits (or near-minimum) loses the compatible family entirely, because compatible families require circuits that share a common global ancestor — and those circuits are inherently larger than minimum-size, carrying structural overhead from the global computation.

---

## The Other Truth Tables

**PARITY (x₀ ⊕ x₁ ⊕ x₂ ⊕ x₃):** Every sub-function is either XOR or XNOR, each having exactly 1 circuit structure in {AND, OR, NOT} at size ≤ 4. Domains are singletons. The CSP is trivially satisfiable — search space is literally 1. STRUCT-MATCH adds zero effective constraint because there is no choice to make. The restrictions on overlaps are just x₀ or NOT(x₀), which are canonical.

This makes parity anomalous for the F^{cir} analysis: the circuit-level presheaf behaves identically to the functional-level presheaf because the sub-functions have essentially unique circuit representations.

**THRESHOLD-2 (Σ ≥ 2) and MAJORITY-4 (Σ ≥ 3):** Compatible families exist. Sub-functions are AND, OR, and constants — all with small, non-unique circuits. At minimum size, domains are singletons and the CSP is trivially satisfiable. At minimum + 1, STRUCT-MATCH activates (36/96 constraints are structurally tighter than functional) but does not block: compatible families exist within the larger domains. These functions are "structurally benign" — their sub-functions have circuits that happen to be compatible.

**BALANCED (manual parity variant):** Behaves identically to parity — sub-functions are all XOR or XNOR with unique circuits.

---

## What This Proves About the Extractor Bottleneck

The n = 4 experiments confirm the domain-size dilemma with concrete evidence.

**The core trap:** A P-time extractor must produce circuits for each sub-cube. The natural strategy is to use a canonical construction (Lupanov or minimum-size search). But canonical circuits are structurally rigid — they don't carry the fingerprint of any global circuit, so they fail STRUCT-MATCH. The only circuits that satisfy STRUCT-MATCH are restrictions of a global circuit, which requires knowing a global circuit to begin with.

This is circular: to produce a compatible family (the witness for OD(T) = 1), you need a global circuit; but OD(T) = 1 means no small global circuit exists. The compatible family, when it exists for hard truth tables, must arise from circuits that are NOT restrictions of any small global circuit — circuits that are locally compatible in a way that "fakes" global coherence without having a global source.

Whether such "globally coherent but locally sourced" compatible families exist for hard truth tables is precisely Result B of the Step 3 blueprint. And whether they can be found in polynomial time is precisely the extractor question that the revised L_OD formalization flagged as the bottleneck.

The n = 4 experiments show this bottleneck is real, not hypothetical: even at n = 4, minimum-size circuits are incompatible for non-trivial functions, and the compatible families (when they exist) require specific, non-canonical circuit choices.

---

## Implications for Path A

These results sharpen the Path A assessment in several ways.

**Negative signal for P-uniform extraction:** The extractor must find circuits that are structurally compatible, not just functionally correct. No known canonical construction (Lupanov, brute-force minimization) produces such circuits. The extractor would need to solve a constraint satisfaction problem over exponentially large domains (2^{Θ(s log s)} circuits per sub-cube), with structural compatibility constraints that cascade aggressively (arc consistency prunes most of the domain). This looks computationally hard.

**The distinguisher question sharpens:** If compatible families for hard truth tables require specific non-canonical circuits, then detecting OD(T) = 1 requires detecting the existence of these special circuits — a problem that appears at least as hard as the compatibility CSP itself.

**Path B receives a boost:** The cascading nature of STRUCT-MATCH incompatibility — where arc consistency alone empties all domains for the BENT function — suggests that the circuit-level obstruction has strong non-local character. Local choices at one sub-cube propagate constraints that affect distant sub-cubes through chains of overlaps. This is exactly the kind of non-local interaction that Path B's direct amplification approach could potentially exploit.

---

## Recommended Next Steps

1. **Increase max_size to 6–7 and re-run BENT.** The global circuit's restrictions have size ~5–7. Including these in the domains should allow the CSP to find the compatible family that comes from the global circuit. Verifying this confirms the diagnostic: compatible families exist for BENT, but require non-minimal circuits.

2. **Test truth tables with high circuit complexity.** The BENT function has low global circuit complexity (~7 gates). For truly hard truth tables (circuit complexity ≫ poly(n)), the compatible families (if they exist) come from circuits that are NOT restrictions of any small global circuit. Testing random truth tables at n = 5 or 6 would probe this regime, though enumeration becomes expensive.

3. **Measure the "structural gap."** For functions where compatible families exist, measure the ratio: (minimum size of circuits in any compatible family) / (minimum size of circuits computing the sub-function). If this ratio grows with n, it quantifies the structural overhead that STRUCT-MATCH imposes — the price of compatibility. A growing ratio directly impacts the extractor's feasibility.

4. **Characterize the constraint propagation graph.** The fact that arc consistency empties all domains for BENT suggests the constraint graph has strong connectivity properties — the structural constraints form a "rigid" system where local incompatibility propagates globally. Characterizing this rigidity (e.g., via constraint graph expansion or tree-width) could connect the CSP structure to the non-locality results in Part III.
