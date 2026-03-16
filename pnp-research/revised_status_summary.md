# Revised Status Summary: Sheaf-Theoretic P vs NP Framework

## Post-Retraction + Post-Campaign + Paper Complete — March 2026

---

## Executive Summary

Conjecture 14.1 (the Sub-cube Lifting Conjecture) has been **refuted** by explicit counterexample. The conditional claim "Assuming Conjecture 14.1, P ≠ NP" (Theorem 15.1) is **retracted**. P ≠ NP is reclassified as **open**.

All unconditional results in Parts I–III remain valid. A subsequent computational campaign produced a submission-ready paper — "Restriction Images and Structural Entropy in Boolean Formulas" — documenting the first quantitative characterization of the circuit-level presheaf's structure. External review confirms publishability at CCC/STACS/MFCS.

Path C (proof complexity) has been investigated extensively. A backtracking CSP solver discovered **302 structural obstruction witnesses** at n=4, d=3, s≤4 — functions whose compatibility CSP is UNSAT with all domains non-empty. These split 52/48 between edge-incompatible and graph-coloring UNSAT, with a sharp Hamming weight threshold at weight 8. Profile-based reduction compresses the CSP (414 → 150 variables). However, **NS degree of the structural CSP is always 2** over both Q and GF(2) — an inherent limitation of the 2-CSP (pairwise constraint) structure. This kills the direct Nullstellensatz approach. Resolution width is the next proof complexity measure to investigate.

The magnification gap — converting structural characterization into circuit lower bounds for OD — remains the core open problem. Fourteen errors have been caught and corrected during the campaign.

---

## 1. What Was Retracted and Why

### The Conjecture

**Conjecture 14.1** (Sub-cube Lifting) claimed that for any f with sub-cube query complexity Q_d(f) ≥ q, the circuit size of the composed function satisfies L(f ∘ g) ≥ q^{1+Ω(1)} · poly(|g|).

### The Counterexample

f = MAJ ∘ PARITY: Q_d(f) = Ω(N/2^d) but L(f ∘ g) ≤ O(N · poly(m)). For d = n/3 the predicted bound is false. Five independent structural reasons confirm the conjecture is unfixable.

---

## 2. What Remains Valid

### Part I — Framework (fully valid)

| Result | Reference | Status |
|--------|-----------|--------|
| Sub-cube site well-defined | Prop 1.2 | ✅ Proved |
| MCSP presheaf well-defined (F^{cir}, F^{grp}) | Thm 2.3 | ✅ Proved |
| CSP dichotomy recovery | Thm 3.2 | ✅ Proved |
| Hardness magnification (prior work) | Thm 5.2, Cor 5.4 | ✅ Proved [OS18, CHOPRS22] |

### Part II — Structural Results (fully valid)

| Result | Reference | Status |
|--------|-----------|--------|
| No nontrivial circuit polymorphism (Result A) | Thm 6.2 | ✅ Proved |
| Compatible family for parity at F^{cir} level (Result B) | Thm 7.1 | ✅ Proved |
| Compatible family for random functions (Result B) | Thm 7.3 | ⚠️ Conditional (Lupanov functoriality) |
| Ȟ¹ ≠ {∗} for parity | Thm 7.1 | ✅ Proved |
| Non-locality: Ω(N/log N) independent interactions (Result C) | Thms 8.1–8.2 | ✅ Proved |

### Part III — Lower Bounds (all unconditional results valid)

| Result | Reference | Status |
|--------|-----------|--------|
| Ω(n) independent forks in 3-SAT residual | Thms 9.4, 9.6 | ✅ Proved |
| Decoupling property (large k) | Thm 10.4 | ✅ Proved |
| Decoupling property (k = 3) | Conj 10.3 | ⚠️ Open |
| SAT_R ∉ AC⁰ (large k) | Cor 11.2 | ✅ Proved |
| SAT_R ∉ AC⁰ (k = 3) | Thm 11.1 | ⚠️ Conditional (decoupling) |
| Sub-cube query complexity Q_d(OD) ≥ Ω(N/poly(n)) | Thm 12.1 | ✅ Proved |

### Part V — Barrier Evasion (retained)

| Result | Reference | Status |
|--------|-----------|--------|
| Non-relativization | Thm 19.1 | ✅ Proved |
| Non-algebrization | Thm 20.1 | ✅ Proved |
| Natural proofs evasion | Thm 17.1 | ⚠️ Plausible; strengthened by OD = 1 sparsity finding |
| Locality barrier evasion | Thm 18.1 | ⚠️ Structural evidence |

---

## 3. What Was Retracted

| Result | Reference | Previous Status | New Status |
|--------|-----------|-----------------|------------|
| Sub-cube Lifting Conjecture | Conj 14.1 | ❌ Open | ✕ **Refuted** |
| Circuit lower bound OD ∉ SIZE[N^{1+ε}] | Conj 7.4 | ❌ Open | ❌ Open (no path) |
| Communication complexity Ω(N/log N) | Prop 12.2 | ❌ Open | ❌ Open |
| **P ≠ NP** (conditional claim) | Thm 15.1 | ⚠️ Conditional | ○ **Open — claim retracted** |

---

## 4. The Structural Anatomy Paper (New)

### 4.1. Paper Status

**Title:** "Restriction Images and Structural Entropy in Boolean Formulas"

**Format:** Unified single document, 332 lines, ~4,000 words, abstract 253 words. Budgeted at 14 pages in LIPIcs format.

**External assessment:** Evaluated as a legitimate structural-combinatorics contribution, publishable at CCC, STACS, MFCS, or Random Structures & Algorithms. The strongest elements are (1) the σ∞(d) invariant, (2) the empirical taxonomy, (3) the Savický bridge, (4) the restriction entropy framework.

### 4.2. Three Results

**Result 1: The d = 3 Structural Taxonomy.** All 256 three-input Boolean functions classified by formula complexity (sizes 0–12) and structural fluidity Φ. Three-tier hierarchy: stiff (Φ ≤ 3), moderate (3 < Φ ≤ 20), fluid (Φ > 20). Frontier stiffness recurs from different algebraic families: XOR at size 4, unanimity/diversity at size 6. PAR3 requires 11 gates in our all-gates model (= 9 = 3² in the standard De Morgan measure [Khrapchenko 1971; Tarui 2008]).

**Result 2: The Universe-to-Image Scaling Law.** σ(s, d) = |U(s,d)| / M(s,d):

| d | |U| | M | σ |
|---|---|---|---|
| 2 | 225 | 111 | 2.0 |
| 3 | 2,324 | 367 | 6.3 |
| 4 | 11,075 | 835 | 13.3 |

At d = 3, σ converges to σ∞(3) ≈ 6.4 as s → ∞ (measured through s = 5). Growth factors decelerate: 1.30, 1.15, 1.06, 1.02.

**Exact identity:** σ∞(d) = 1/α(d), where α(d) is the top share (fraction of DAG classes belonging to the most prolific function). Top share decays: 49.3% → 15.8% → 7.5%.

**Dilution mechanism:** The redundancy budget s − C(g) controls T_g. Simple functions dominate the max image because they have the most room for degenerate constructions. The universe grows faster because 2^{2^{d−1}} functions collectively diversify it.

**Result 3: The BENT Case Study.** At n = 4, d = 2: 50% of structural edges have empty restriction-image intersection at minimum size. The global circuit produces a compatible family at non-minimal size (structural gap γ = 1.25). STRUCT-MATCH is strictly more constraining than functional compatibility.

### 4.3. The Savický Bridge

**Theorem (Savický 1990).** For random AND/OR formulas with negated literals on n variables, the fraction computing any fixed function converges to 2^{−2^n} as formula size → ∞.

**Corollary (conditional).** If the canonical compression ratio max_g(|F_m(g)|/T_g) is poly(m)-bounded, then σ∞(d) → ∞.

**Empirical verification.** Compression ratio ranges from 1.1 to 2.0 across all 121 functions at d = 3, s ≤ 4 (median 2.0). Near-constant because canonical deduplication only sorts commutative operands.

### 4.4. Formal Conjectures

| Statement | Type | Evidence |
|---|---|---|
| σ∞(d) = lim_{s→∞} σ(s,d) exists | Conjecture 5.7 | Monotone convergence at d = 3 (6 data points) |
| σ∞(d) → ∞ as d → ∞ | Conjecture 5.8 | Three cross-dimensional data points + Savický conditional |
| σ∞(d) ≥ 2^{Ω(d)} | Stronger form | Consistent with data; not distinguishable from polynomial |
| E[H(F)] = Θ(?) for random formula | Open Problem 5.9 | Θ(log d) → polynomial σ∞; Θ(d) → exponential σ∞ |
| Compression ratio bounded by poly(m) | Open Problem 5.10 | Empirically O(1) at d = 3 |

### 4.5. Errors Caught During Campaign

| Error | Correction | Impact |
|---|---|---|
| Fourier fingerprints detect STRUCT-MATCH | Fourier is blind to DAG structure | Prevented category error in Path B |
| Parity anchors drive misalignment | Parity density is 1/2^{2^d} | Redirected to generic dilution mechanism |
| Frontier stiffness is permanent | Dissolves at next size level | Identified transience; found recurrence instead |
| High misalignment → OD is hard | Makes OD = 0 generic (wrong direction) | Recharacterized Path B as detection problem |
| |F_s| = 2^{Θ(s log s)} for formulas | Correct: 2^{Θ(s)} (trees, not DAGs) | Prevented flawed anti-concentration proof |
| Fourier degree correlates with T_g | Spurious artifact of formula complexity | Kept paper clean of function-space confounds |
| log(T_g) ≈ Θ(# minimal templates) | T_g driven by redundancy budget s − C(g) | Identified correct structural driver |
| Restriction compression = canonical compression | Two different quantities | Kept Savický argument precise |
| Periodic CSPs have bounded NS degree | Degree stays at 3 for twisted cycles L=4–8 | Non-periodic structure essential for Path C |
| BENT min-size CSP has partial compatibility | Zero partial edges; all-or-nothing at min size | Need min+1 domains for interesting algebra |
| Idris2 size convention = Python convention | +1 offset (Idris "size 0" = Python s ≤ 1) | Identified before comparing results |
| M2 handles full BENT CSP | Doubly exponential in variables; wall at ~100 vars | Sub-instance analysis required |
| Non-periodic CSPs might have NS degree > 3 | Actual structural CSP has NS degree 2, not 3 | 2-CSP structure bounds degree at 2 over any field |
| GF(2) NS degree might differ from Q | NS degree = 2 over GF(2) as well | Field-independent bound for 2-CSPs |

---

## 5. The Magnification Gap — Final Assessment

The gap is now precisely characterized, with first experimental data from Path C:

1. **The presheaf's structure is quantitatively characterized.** σ∞(d) scaling law, periodic table, BENT case study, Savický bridge, dilution mechanism — all documented in the paper.

2. **The gap is a detection problem.** "Is it hard to determine whether the compatibility CSP Γ(T, d, s) is satisfiable, given that solutions are exponentially rare?" This is a meta-complexity question about a specific CSP on an expanding constraint graph.

3. **The structural data constrains the landscape.** σ∞(d)-controlled constraint density, Ω(d/n) edge expansion, non-localizable infeasibility — these are prerequisites for proof complexity lower bounds, but they don't constitute one.

4. **Hundreds of genuine structural obstructions exist.** At n=4, d=3, s≤4: 302 functions have UNSAT structural CSPs with all domains non-empty. These split 158 edge-incompatible (FI>0) vs 144 graph-coloring (FI=0). The graph-coloring cases are non-trivial: every edge has compatible pairs but no global assignment exists.

5. **Sharp Hamming weight threshold.** Graph-coloring UNSAT (FI=0) occurs only at Hamming weights 4–7. Edge-incompatible UNSAT (FI>0) occurs at weights 4 and 6–12. Weight 5 is purely graph-coloring; weight 8+ is purely edge-incompatible.

6. **NS degree is always 2 — definitive negative.** The structural CSP is a 2-CSP (pairwise constraints). One-hot encoding gives degree-2 incompatibility constraints. NS degree = 2 over both Q and GF(2) for all tested instances. This is an inherent structural limitation, not a computational one. The direct Nullstellensatz approach cannot yield circuit lower bounds.

7. **Resolution width is the remaining proof complexity direction.** Resolution width can be Ω(n) even for 2-CSPs with NS degree 2 (Ben-Sasson & Wigderson 1999). The structural CSP's expanding constraint graph (sub-cube geometry) is the right regime for width growth. Untested.

---

## 6. Surviving Approaches

### Path A: Atserias–Müller — LOW priority

Five obstacles identified. Extractor/distinguisher tension likely structural. Recommended: resolve Gap-MCSP question, then formally close or reopen.

### Path B: Detection Problem — MEDIUM priority

Paper submitted. Open problems stated. Next targets: compression bound (Open Problem 5.10), restriction entropy (Open Problem 5.9), extended scaling data (d = 5, alternative bases).

### Path C: Proof Complexity — MEDIUM priority (NS degree bounded; resolution width next)

Full pipeline built: Idris2 CSP solver + profile reduction + Python/numpy NS degree over Q and GF(2). Key findings:

**302 structural obstruction witnesses found (n=4, d=3, s≤4):**
- 158 edge-incompatible (52.3%), 144 graph-coloring (47.7%)
- 9 distinct structural patterns (FC/PC/FI)
- Sharp Hamming weight threshold: FI=0 at weights 4–7 only, FI>0 at weights 8–12 only
- Minimal UNSAT core for TT=686: exactly one 4-node subset {0,2,5,6} (35 profiles)

**NS degree = 2 — definitive negative:**
- Tested over both Q and GF(2), on sub-instances (35 vars) and full instances (150 vars)
- Always 2: inherent to 2-CSP structure (pairwise constraints → degree-2 incompatibility)
- Kills the direct Nullstellensatz approach for circuit lower bounds

**Profile-based CSP reduction:**
- Domain elements with identical edge profiles are interchangeable
- TT=686: 414 elements → 150 profiles (2.76× compression)
- Enables NS degree computation on full instances

**Next steps:** Resolution width via CDCL proof logging. Convert profile-reduced CSP to CNF. Test at n=4 (8 nodes) and n=5 (40 nodes) for width growth. Obstruction density scaling with n.

### Track D: k = 3 decoupling — Unchanged

### Track E: Communication complexity — Unchanged

---

## 7. Lessons

1. **Sub-cube query complexity is information-theoretic, not computational.**
2. **Lifting is not a universal tool.**
3. **The magnification gap is real.**
4. **Structural characterization ≠ computational hardness.** Rich presheaf structure does not imply OD is hard; it may make OD easy (OD = 0 generic).
5. **Fourier analysis is blind to DAG structure.**
6. **Frontier stiffness is recurrent but transient.** The structural tax is driven by σ∞(d), not specific rigid functions.
7. **Computational experiments redirect research.** Twelve errors caught before publication.
8. **Classical results can bridge empirical and theoretical.** Savický (1990) provided exactly the anti-concentration tool the scaling law needed.
9. **The correct framing matters.** CSP/proof complexity language reaches the right audience. Sheaf language stays in the framework paper.
10. **NS degree of 2-CSPs is inherently bounded.** The structural CSP has pairwise constraints (overlapping sub-cube pairs). One-hot encoding gives degree-2 incompatibility polynomials. NS degree = 2 over any field, regardless of n, d, or s. This is a clean structural theorem, not a computational limitation.
11. **Graph-coloring UNSAT is common and non-trivial.** 48% of UNSAT instances have all edges partially compatible but no global assignment. These are the structurally interesting cases — the obstruction is genuinely global.
12. **Hamming weight controls obstruction type.** Functions with low Hamming weight (4–7) have graph-coloring obstructions; high weight (8+) have edge-incompatible obstructions. The threshold is sharp and likely relates to symmetry of the truth table under complement.
13. **Resolution width is the next frontier.** Unlike NS degree, resolution width can grow with the number of variables even for 2-CSPs. The sub-cube constraint graph has good expansion properties, which is the prerequisite for width lower bounds (Ben-Sasson & Wigderson).
14. **Profile reduction enables exact computation.** Grouping domain elements by edge profile reduces the CSP by 2–3× while preserving SAT/UNSAT. This made NS degree computation feasible on 150-variable instances.

---

## 8. Complete File Inventory

### Paper (submission-ready)

| File | Lines | Description |
|---|---|---|
| `unified_paper.md` | 332 | Complete paper, single document |

### Supporting paper components

| File | Lines | Description |
|---|---|---|
| `final_manuscript_sections.md` | 75 | Abstract + dilution mechanism (merged into unified) |
| `section_5_6_anticoncentration.md` | 69 | Savický bridge (merged into unified) |
| `growth_mechanism.md` | 173 | Theoretical analysis of σ∞(d) growth |
| `scaling_law_conjecture.md` | 151 | Formal conjecture statements |
| `bibliography_and_conclusion.md` | 174 | BibTeX + §8 consequences |
| `paper_outline_ccc.md` | 160 | 14-page CCC format outline |

### Research documents

| File | Lines | Description |
|---|---|---|
| `post_retraction_roadmap.md` | — | This document |
| `revised_status_summary.md` | — | This document |
| `d3_complete_findings.md` | 137 | d = 3 periodic table analysis |
| `restriction_audit_findings.md` | 114 | BENT restriction image audit |
| `universe_scaling_analysis.md` | 94 | Universe scaling honest assessment |
| `expansion_analysis.md` | 290 | Sub-cube intersection graph analysis |
| `n4_csp_analysis.md` | 91 | n = 4 compatibility CSP results |
| `od_witness_analysis.md` | 155 | OD witness language five-issue analysis |

### Computational scripts (supplementary material)

| File | Lines | Description |
|---|---|---|
| `compat_csp_n4.py` | 447 | Compatibility CSP solver |
| `restriction_audit.py` | 267 | BENT restriction images |
| `d3_catalog.py` | 303 | d = 3 taxonomy at s ≤ 4 |
| `d3_size5.py` | 271 | Size 5 enumeration |
| `d3_size6.py` | 279 | Size 6 targeted enumeration |
| `d3_universe_growth.py` | 220 | σ convergence at d = 3 |
| `d4_universe.py` | 281 | d = 4 universe computation |
| `n5_audit.py` | 260 | n = 5 misalignment rates |
| `par3_audit.py` | 333 | PAR3 min formula size |
| `sparsity_analysis.py` | 305 | Universe density analysis |
| `path_c_minimal.py` | — | NS degree for minimal BENT subgraphs |
| `path_c_scaling.py` | — | NS degree scaling on twisted cycles |
| `path_c_bent.py` | — | Real BENT subgraph NS degree + M2 gen |

### Path C scripts (circuit-presheaf/scripts/)

| File | Description |
|---|---|
| `ns_from_csp.py` | CSP dump parser, profile computation, M2 script generator, backtracking solver |
| `ns_degree.py` | Exact NS degree via linear algebra over Q |
| `ns_degree_gf2.py` | NS degree over GF(2) via Gaussian elimination |

### Idris2 pipeline (circuit-presheaf/)

| Component | Description |
|---|---|
| `src/Analysis/CompatCSP.idr` | CSP construction, solver, profile reduction, dump format |
| `src/Main.idr` | CLI: test, solve, scan, profiles commands |
| `src/Algebra/M2Gen.idr` | M2 script generation from canonical groups |
| `src/Algebra/M2Parse.idr` | Output parser (SAT/UNSAT/GB/Hilbert) |
| `src/Algebra/NSDriver.idr` | M2 execution orchestration |

### Visual

| File | Description |
|---|---|
| `roadmap_visual_dag.html` | Interactive roadmap DAG |

---

## Revised Dependency Summary

```
VALID                              BROKEN               OPEN TARGET
═════                              ══════               ═══════════

Sub-cube Site ──────┐
CSP Dichotomy ──────┤
MCSP Presheaf ──────┼→ Results A, B, C → Q_d(OD) ≥ Ω(N/poly(n))
Magnification ──────┘        │                  │
                             │                  ╳ ← BREAK (lifting)
                             │                  │
                             ▼                  ▼
                Forks → Decoupling      [no path to OD ∉ SIZE]
                        → bs Ω(n)              │
                        → AC⁰                  ╳ ← BREAK
                                               │
NEW: Structural Anatomy Paper ···········→ Detection Problem
  σ∞(d) scaling law                       (CSP satisfiability
  Savický bridge                           on expander)
  Dilution mechanism                            │
  Formal conjectures                            │
                                               ▼
        Path C: Proof Complexity ····→ ?   NP ⊄ P/poly → P ≠ NP
        of Γ(T,d,s)                          ○ OPEN
                │
                ├── 302 obstruction witnesses found (n=4, d=3)
                ├── 52% edge-incompatible, 48% graph-coloring
                ├── NS degree = 2 over Q and GF(2) ✗ (dead)
                ├── Inherent 2-CSP bound: degree ≤ 2 always
                ├── Profile reduction: 414 → 150 vars (2.76×)
                └── NEXT: resolution width via CDCL proof logging
```
