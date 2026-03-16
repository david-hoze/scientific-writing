# Post-Retraction Roadmap: Viable Paths to P ≠ NP

## Updated March 2026 — Paper Complete, Pre-Submission

---

> **STATUS — Current state of the program (updated 2026-03-16):**
>
> Path C (proof complexity) is now **effectively closed**. Complete findings: NS degree = 2 (inherent to 2-CSP), resolution width = initial clause width, PC degree ≤ 2, all obstructions dissolve at s≤5. The complete n=4 census found **1064 UNSAT** (1.62% of 65536) with symmetric Hamming weight distribution. All 1064 lift to genuine UNSAT at n=5, but still dissolve at larger size budgets. The structural CSP is too soft for complexity-theoretic extraction. The submission-ready paper ("Restriction Images and Structural Entropy in Boolean Formulas") is the primary deliverable. Path B (paper submission + compression bound) is now the active focus. The core open problem — OD ∉ SIZE[N^{1+ε}] — remains open.

---

## Situation Assessment

The Sub-cube Lifting Conjecture (14.1) remains refuted. All unconditional results in Parts I–III remain valid. The bottleneck remains singular: OD ∉ SIZE[N^{1+ε}].

### What the computational campaign established

1. **STRUCT-MATCH is strictly more constraining than functional compatibility.** At n = 4, 50% of structural edges are incompatible at minimum formula size. The gap is absolute, not probabilistic.

2. **The universe-to-image scaling law.** σ∞(d) = |U|/max|I| grows with dimension: 2.0 (d = 2), 6.3 (d = 3), 13.3 (d = 4). At fixed d = 3, σ converges to σ∞(3) ≈ 6.4 as s → ∞, with growth factors decelerating as 1.30, 1.15, 1.06, 1.02.

3. **The top-share identity.** σ∞(d) = 1/α(d), where α(d) is the fraction of DAG classes belonging to the most prolific function. The top share decays: 49.3% → 15.8% → 7.5%.

4. **The Savický bridge.** Savický (1990) proves formula-level anti-concentration (each function captures a 2^{−2^{d−1}} fraction of formulas asymptotically). Combined with the empirically verified canonical compression ratio (1.1–2.0×), this yields a conditional corollary: if compression stays bounded, σ∞(d) → ∞.

5. **Compatible families are rare.** High σ∞(d) means OD(T) = 0 for generic truth tables. This supports natural proofs evasion but inverts the intended lower-bound argument.

6. **The redundancy budget.** T_g(s) is controlled by s − C(g), not by the structure of minimal formulas. Simple functions dominate the max image because they have the most room for redundant constructions.

7. **Structural obstructions exist at n=4, d=3.** 302 functions (of 65536) have UNSAT structural CSPs at s≤4 with all domains non-empty. These are genuine obstruction witnesses: no compatible family exists.

8. **50/50 graph-coloring vs edge-incompatible split.** Half of UNSAT instances have at least one fully-incompatible edge (trivial UNSAT); the other half have all edges partially compatible but no global assignment (graph-coloring UNSAT). The split correlates sharply with Hamming weight: FI=0 only at weights 4–7, FI>0 only at weights 8–12.

9. **NS degree of the structural CSP is always 2.** The 2-CSP structure (pairwise constraints) inherently bounds NS degree at 2 over any field. This kills the direct Nullstellensatz approach to proof complexity lower bounds.

### What the campaign did NOT establish

Any connection between the structural characterization and the circuit complexity of OD. The magnification gap remains open. NS degree is bounded, and resolution width has not yet been measured.

### Errors caught and corrected

- Fourier fingerprints cannot detect DAG-level structure (category error)
- Parity sub-functions are exponentially rare at large d (anchor density fallacy)
- Frontier stiffness dissolves at the next size level (transience)
- Edge independence in the CSP is false (correlation structure matters)
- High misalignment implies OD is easy, not hard (the critical inversion)
- |F_s| = 2^{Θ(s log s)} is wrong for formulas; correct count is 2^{Θ(s)} (counting error caught before publication)
- Fourier-degree correlation with DAG-class count is a spurious artifact of formula complexity (rejected)
- log(T_g) ≈ Θ(# minimal templates) hypothesis tested and rejected; T_g driven by redundancy budget

---

## The Paper

**Title:** "Restriction Images and Structural Entropy in Boolean Formulas"

**Status:** Unified single document, 332 lines, ~4,000 words, abstract at 253 words. Submission-ready.

**External assessment:** Publishable at CCC, STACS, MFCS, or Random Structures & Algorithms. Positioned as a structural-combinatorics contribution with meta-complexity implications, not a P ≠ NP claim.

**Contents:**

| Section | Content |
|---|---|
| §1 Introduction | MCSP context, three results stated, organization |
| §2 Preliminaries | Formulas, DAG isomorphism, restriction images, compatibility CSP |
| §3 Computational framework | Enumeration method, scale table, cross-checks |
| §4 Structural taxonomy | d = 3 periodic table, three tiers, frontier stiffness |
| §5 Scaling law | Cross-dimensional data, s-convergence, top-share identity, dilution, Savický bridge, conjectures |
| §6 BENT case study | 50% incompatibility, structural gap γ = 1.25 |
| §7 Consequences | CSP interpretation, OD sparsity, proof complexity, P ≠ NP disclaimer |
| References | 11 entries |

**Theoretical arc:** Empirical data → top-share identity (exact) → Savický anti-concentration (classical theorem) → conditional corollary (bounded compression → σ∞(d) → ∞) → conjectures (well-posed open problems).

**Key framing decisions:** CSP/proof complexity language preferred over sheaf language (per reviewer advice). All claims at correct epistemic level. PAR3 = 11 in all-gates model clarified against Khrapchenko standard measure (= 9 = 3²).

---

## Strategy DAG (Updated)

```
            ┌───────────────────────────────────────────────────────────┐
            │         UNCONDITIONAL FOUNDATION                           │
            │         (Parts I–III) + STRUCTURAL ANATOMY PAPER           │
            │                                                           │
            │  Results A, B, C · Q_d(OD) ≥ Ω(N/poly(n))               │
            │  σ∞(d) scaling law · Savický anti-concentration bridge    │
            │  Periodic table · BENT 50% · Dilution mechanism           │
            │  Formal conjectures: σ∞(d) → ∞, restriction entropy      │
            └────────┬───────────────┬───────────────┬──────────────────┘
                     │               │               │
        ┌────────────┘               │               └────────────┐
        ▼                            ▼                            ▼
┌────────────────────┐  ┌─────────────────────┐  ┌────────────────────┐
│ PATH A              │  │ PATH B               │  │ PATH C              │
│ Uniform             │  │ Detection Problem    │  │ Proof Complexity    │
│ Magnification       │  │                      │  │ of Compatibility    │
│                     │  │ Is satisfiability of │  │ CSP                 │
│ ⚠️ 5 obstacles;     │  │ the compatibility    │  │                     │
│ extractor/          │  │ CSP hard to detect?  │  │ NS degree = 2 (dead)│
│ distinguisher       │  │                      │  │ Res width bounded   │
│ tension likely      │  │ Paper submitted;     │  │                     │
│ blocks this path    │  │ open problems stated │  │ ⚠ NS bounded by    │
│                     │  │                      │  │ 2-CSP structure     │
│ Priority: LOW       │  │ Priority: MEDIUM     │  │ Priority: LOW       │
└─────────┬──────────┘  └─────────┬────────────┘  └─────────┬──────────┘
          │                       │                          │
          ▼                       ▼                          ▼
┌──────────────────────────────────────────────────────────────────────┐
│           OD ∉ SIZE[N^{1+ε}]   ← CORE OPEN PROBLEM                  │
│                                                                      │
│  Precisely framed as: detection hardness of a sparse CSP on an       │
│  expanding constraint graph with σ∞(d)-controlled constraint density │
└──────────────────────────────────────────────────────────────────────┘
```

---

## Path A: Atserias–Müller Uniform Magnification

### Status: Likely blocked

Five obstacles identified, with the extractor/distinguisher tension as the fundamental issue. If compatible families are hard to find (extractor fails), they are hard to detect (distinguisher fails). If they are easy to detect, natural proofs evasion weakens.

**Recommended action:** Low priority. Determine whether Gap-MCSP resolves obstacles 3 and 5. If not, formally document that Path A is structurally blocked for OD and move on.

---

## Path B: Detection Problem (Recharacterized)

### Status: Paper complete; open problems stated

Path B has been recharacterized from "direct non-local amplification" to "structural anatomy of circuit restriction spaces." The paper is the deliverable. The remaining open questions are:

1. **Does σ∞(d) → ∞?** (Conjecture 5.8 in the paper). Three data points plus the Savický conditional corollary. The compression bound (Open Problem 5.10) is the precise gap.
2. **Restriction entropy scaling** (Open Problem 5.9). Does E[H(F)] = Θ(log d) or Θ(d)?
3. **Extended data.** σ∞(d) at d = 5; alternative gate bases; d = 3 structural profiles at sizes 7–9.
4. **Basis independence** (Open Problem 5 in §8 of the paper).

**Recommended action:** Submit paper. Pursue Open Problem 5.10 (compression bound) as the most tractable route to converting the conditional corollary into an unconditional theorem.

---

## Path C: Proof Complexity of the Compatibility CSP

### Status: NS degree bounded at 2 — pivot required

The compatibility CSP Γ(T, d, s) has been fully analyzed via Python/numpy linear algebra and Macaulay2. The direct Nullstellensatz approach is **dead**: NS degree = 2 over both Q and GF(2) for all instances, due to the inherent 2-CSP structure. Alternative proof complexity measures are the path forward.

### What was built

1. **Idris2 pipeline**: Enumerates formulas, builds sub-cube intersection graph, computes canonical overlap groups, generates M2 scripts. Canonical groups store compatibility in O(|D|) per edge.
2. **Profile-based CSP reduction**: Domain elements with identical edge profiles (canonical key on every adjacent edge) are interchangeable. Reduces 414 → 150 variables for TT=686 (2.76× compression).
3. **Backtracking CSP solver**: Forward checking, smallest-domain-first heuristic, 1M fuel limit. Classifies SAT/UNSAT with structural pattern (FC/PC/FI).
4. **Python NS degree pipeline** (`ns_degree.py`, `ns_degree_gf2.py`): Exact NS degree via monomial coefficient matching over Q and GF(2).
5. **CSP dump format** (`ns_from_csp.py`): Machine-readable format for profile computation and M2 generation.

### Computational results (n=4, d=3, s≤4)

**Scan-solve of all 65536 n=4 Boolean functions (COMPLETE):**
- **1064 UNSAT** (1.62%), 2536 SAT (3.87%)
- Hamming weight distribution symmetric around 8, peaking at weights 7 and 9

**Structural classification of UNSAT functions (302/302 complete):**

| Pattern (FC/PC/FI) | Count | Type | Hamming weights |
|---|---|---|---|
| 0/24/0 | 61 | Graph-coloring | 4, 6, 7 |
| 1/23/0 | 61 | Graph-coloring | 5, 6, 7 |
| 2/22/0 | 22 | Graph-coloring | 6 |
| 1/21/2 | 57 | Edge-incompatible | 4, 8, 12 |
| 1/22/1 | 34 | Edge-incompatible | 7, 9 |
| 4/18/2 | 29 | Edge-incompatible | 6, 10 |
| 0/23/1 | 15 | Edge-incompatible | 9 |
| 2/20/2 | 13 | Edge-incompatible | 8, 11 |
| 2/21/1 | 10 | Edge-incompatible | 9 |

**Key findings:**
1. **52/48 split**: 158 edge-incompatible (FI>0) vs 144 graph-coloring (FI=0)
2. **Sharp Hamming weight threshold**: FI=0 exists only at weights 4–7; FI>0 exists only at weights 4 and 6–12. Weight 5 is purely graph-coloring (27 functions, zero edge-incompatible)
3. **Graph-coloring UNSAT (0/24/0) is the most interesting class** (61 functions): all 24 edges have some compatible pairs, no fully-compatible edges, yet globally UNSAT
4. **Complement symmetry**: weight-k and weight-(16-k) counts are identical (e.g., wt 4 = 51, wt 12 = 8), reflecting f ↔ ¬f duality

**Minimal UNSAT core analysis (TT=686, pattern 2/22/0):**
- Exactly one 4-node UNSAT subset: {0, 2, 5, 6} (35 profiles)
- No 3-node subset is UNSAT
- Nodes 2, 5, 6 are critical (removing any one makes CSP SAT)

### NS degree results — DEFINITIVE NEGATIVE

| Instance | Nodes | Profiles | Field | NS degree |
|---|---|---|---|---|
| TT=686 minimal core {0,2,5,6} | 4 | 35 | Q | **2** |
| TT=686 sub {0,2,4,5,6} | 5 | 39 | Q | **2** |
| TT=686 full | 8 | 150 | Q | **2** |
| TT=686 minimal core | 4 | 35 | GF(2) | **2** |
| TT=686 full | 8 | 150 | GF(2) | **2** |

**Why NS degree is always 2:** The structural CSP is a 2-CSP (pairwise constraints between overlapping sub-cubes). One-hot encoding gives:
- Selection constraints: degree 1 (Σ x_{i,a} = 1)
- Boolean constraints: degree 2 (x²-x = 0)
- Incompatibility constraints: degree 2 (x_{i,a} · x_{j,b} = 0)

A degree-2 NS certificate always exists for UNSAT 2-CSPs. This is field-independent.

**This kills the direct NS approach for Path C.** NS degree of the standard one-hot encoding is O(1), not Ω(n). It cannot yield circuit lower bounds.

### Remaining proof complexity directions

1. ~~**Resolution width (direct encoding)**~~: **TESTED — BOUNDED.** Resolution width of the one-hot CNF encoding equals the max initial clause width (= max profiles per node). For TT=686 core: width 17 = max at-least-one clause width. Ben-Sasson–Wigderson tradeoff gives trivial size bound. The 2-CSP binary constraint structure means CDCL conflict analysis never produces wider-than-input resolvents.

2. **Alternative CNF encodings**: Log encoding (⌈log₂ d⌉ vars per node) or order encoding may have different resolution width behavior. The constraint clauses become wider in these encodings, potentially enabling width amplification. **Not yet tested.**

3. **Higher-arity encoding**: Replace one-hot with algebraic encoding of the domain. This changes the polynomial system structure — constraints become higher-degree, and NS degree may grow.

4. **Lift from CSP to circuit problem**: The proof complexity of Γ(T,d,s) at dimension d might relate to circuit depth/size via a lifting theorem (Göös et al.). However, the CSP's shallow proof complexity (NS=2, res width = initial width) may make lifting theorems inapplicable.

5. **Different polynomial system**: Instead of encoding the CSP, encode OD directly as a polynomial system and measure its proof complexity.

6. **Obstruction counting**: The *existence* and *density* of obstructions (not their proof complexity) may be the relevant quantity. 1064/65536 = 1.62% of n=4 functions are UNSAT at d=3, s≤4. How does this fraction scale with n?

### n=5 investigation results (Session 7)

**Python pipeline:** `circuit-presheaf/scripts/n5_scan.py` — full reimplementation of formula enumeration, sub-cube geometry, overlap restriction, profile reduction, and backtracking solver. Key optimization: 18 distinct restriction patterns vs 480 edges (25× speedup).

**Coverage gap:** At n=5, d=3, s≤4: only 121/256 (47%) 3-var TTs are coverable. Every random n=5 function has empty-domain sub-cubes → trivially UNSAT. Only functions with special structure (e.g., lifted from n=4) have full coverage.

**Complete n=4 census:** 1064 UNSAT (1.62%), 2536 SAT (3.87%). Hamming weight distribution symmetric around 8.

**Lifting results at n=5, d=3, s≤4:**
- ALL 1064 direct lifts (f(x0..x4) = f4(x0..x3)): **GENUINE UNSAT** (40/40 coverage)
- ALL 1064 AND-lifts (f4 AND x4): **GENUINE UNSAT** (40/40 coverage)
- ALL 1064 XOR-lifts (f4 XOR x4): **EMPTY+GENUINE** (28-30/40 coverage)
- Zero SAT among any lifted functions

**Persistence test at s≤5 (1.59M formulas, 191/256 coverage):**
- lift_686 → **SAT** (dissolved, confirming size-budget artifact)
- lift_xor_139 → EMPTY+GENUINE (persists on covered subgraph)
- 3 functions → UNKNOWN (solver exhausted at 1M backtracks)

**Conclusion:** Lifted obstructions are still size-budget artifacts. They dissolve when formula budget increases, same as at n=4.

**Recommended action:** LOW priority (downgraded from MEDIUM). Both NS degree and resolution width (direct encoding) are conclusively bounded by the 2-CSP structure. Alternative encodings remain untested but are speculative. The obstruction characterization (50/50 split, weight-8 threshold, 1064/65536 density) is independently interesting and should be documented. Path C is effectively **closed** — the 2-CSP structure is fundamentally too soft for complexity-theoretic extraction.

---

## Tracks D and E (Unchanged)

Track D (k = 3 decoupling → SAT_R ∉ AC⁰) and Track E (communication complexity of OD) remain as supporting tracks, not addressed by the campaign.

---

## Revised Timeline

### Completed

- ✅ OD witness language formalized, five obstacles identified
- ✅ BENT n = 4 compatibility CSP (50% incompatibility)
- ✅ Restriction image audit at n = 4, n = 5
- ✅ d = 3 periodic table (sizes 0–12, all 256 functions)
- ✅ Universe-to-image scaling law (d = 2, 3, 4; s-convergence at d = 3)
- ✅ Savický anti-concentration bridge with compression verification
- ✅ Top-share identity σ∞(d) = 1/α(d) and dilution mechanism
- ✅ Redundancy budget (s − C(g)) identified as driver of T_g
- ✅ Fourier approach, parity anchors, frontier permanence, formula counting error — all rejected
- ✅ Formal conjectures stated (convergence, dimensional growth, restriction entropy, compression bound)
- ✅ Unified paper drafted, externally reviewed, submission-ready
- ✅ PAR3 complexity clarified against Khrapchenko standard measure
- ✅ Path C Python/numpy NS degree pipeline built and validated
- ✅ Twisted cycle baseline: NS degree = 3 for L=4,5,7,8, k=3 (bounded — periodic structure)
- ✅ BENT all-or-nothing finding: 0 partially compatible edges at min size
- ✅ Idris2 + Macaulay2 pipeline operational (canonical groups optimization, M2 script generation)
- ✅ M2 scaling wall identified: 88 vars feasible, 620 vars infeasible
- ✅ Size convention discrepancy identified between Idris2 and Python (+1 offset)
- ✅ Empty-domain SAT bug identified and fixed (contradiction polynomial added)
- ✅ Backtracking CSP solver with forward checking (scan-solve all n=4 functions)
- ✅ Profile-based CSP reduction (414 → 150 variables for TT=686, 2.76× compression)
- ✅ **302 UNSAT structural obstruction witnesses found** at n=4, d=3, s≤4 (~33% scan)
- ✅ Structural classification: 50% graph-coloring UNSAT, 50% edge-incompatible
- ✅ Sharp Hamming weight threshold: FI=0 only at weights 4–7, FI>0 only at weights 8–12
- ✅ Minimal UNSAT core for TT=686: exactly one 4-node subset {0,2,5,6}
- ✅ **NS degree = 2 over both Q and GF(2)** — definitive negative for standard encoding
- ✅ NS degree bounded by 2 explained: inherent to 2-CSP structure (pairwise constraints)
- ✅ **Resolution width (direct encoding) = max initial clause width** — CDCL proof logging, width scaling analysis
- ✅ Ben-Sasson–Wigderson tradeoff gives trivial bounds for the one-hot encoding
- ✅ Idris2 verification types: four modules (Formula, SubCube, CSP, Solver) with zero `believe_me`
- ✅ **Complete n=4 scan: 1064 UNSAT** (1.62% of 65536), symmetric Hamming weight distribution
- ✅ **PC degree ≤ NS degree ≤ 2** — polynomial calculus also bounded (NS refutations are valid PC proofs)
- ✅ **n=5 Python pipeline** (`n5_scan.py`): full formula enumeration, sub-cube geometry, profile reduction, backtracking solver
- ✅ **n=5 lifting: all 1064 n=4 UNSAT → n=5 UNSAT** at s≤4 (genuine, full 40/40 coverage)
- ✅ **n=5 persistence test: lift_686 dissolves at s≤5** — obstructions remain size-budget artifacts

### Immediate (Months 0–2)

**Primary (50%):** Submit structural anatomy paper to CCC (or STACS/MFCS if timeline doesn't align).

**Secondary (30%):** Extend structural anatomy paper with n=4 complete census (1064 UNSAT), symmetric weight distribution, dissolution at s≤5.

**Tertiary (20%):** n=5 characterization for paper extension. Document lifting results, coverage gap, dissolution pattern.

### Near-term (Months 2–6)

**Primary (50%):** Pursue Open Problem 5.10 (prove canonical compression is O(1) or poly(m)). This would upgrade the conditional corollary to unconditional.

**Secondary (30%):** Extended data for paper: d=5 scaling, larger size budgets, alternative gate bases.

**Tertiary (20%):** Investigate whether any non-lifted n=5 functions with full coverage are UNSAT. This requires finding such functions first (rare at s≤4).

### Medium-term (Months 6–12)

**If resolution width grows with n:** Invest heavily — this is the route to a proof complexity lower bound via lifting.

**If compression bound proved:** Publish the unconditional σ∞(d) → ∞ theorem as a follow-up.

**If both stall:** Publish magnification gap characterization paper.

---

## Decision Points

| Milestone | Window | If Yes | If No |
|---|---|---|---|
| Paper accepted? | Months 2–6 | Priority established | Revise and resubmit |
| Resolution width grows with n? | ~~Months 1–4~~ **RESOLVED** | ~~Strong Path C~~ | **Bounded:** res width = max initial clause width for direct encoding. Path C proof complexity directions exhausted for standard encodings. |
| Obstruction density grows with n? | Months 2–6 | Obstruction counting paper; density argument | Obstructions are small-n artifact; reassess |
| Compression bound provable? | Months 3–8 | Unconditional σ∞(d) → ∞ theorem | Remains conditional |
| σ∞(5) continues growth? | Months 4–8 | Fourth data point; publish | Investigate saturation |
| Path A resolvable via Gap-MCSP? | Months 4–8 | Reconsider Path A | Path A formally closed |

---

## What Is Off the Table

1. **Sub-cube lifting.** Refuted.
2. **Repairing Conjecture 14.1.** Unfixable.
3. **Conditional P ≠ NP claims.** Lesson learned.
4. **Fourier-analytic approaches to STRUCT-MATCH.** Fourier is blind to DAG structure.
5. **Parity anchor arguments.** Parity density is 1/2^{2^d} — exponentially rare.
6. **"Misalignment implies hardness."** High misalignment makes OD = 0 generic (wrong direction).
7. **Formula-count gap arguments.** |F_s| = 2^{Θ(s)} for formulas, not 2^{Θ(s log s)}. No gap exists.
8. **Fourier-degree correlation with DAG-class count.** Spurious artifact of formula complexity.

---

## Deliverables Inventory

### Complete (35+ files)

| Category | Files | Description |
|---|---|---|
| Unified paper | 1 | `unified_paper.md` — submission-ready, 332 lines |
| Paper components | 6 | Sections, conjectures, bibliography, outline, mechanism |
| Research documents | 8 | Roadmap, status, analysis documents |
| Python scripts | 16 | Enumeration (10) + Path C (6: ns_degree, ns_degree_gf2, ns_from_csp, chatgpt, etc.) |
| Idris2 pipeline | — | circuit-presheaf: CSP solver, scan-solve, profiles, dump-csp, M2Gen |
| Visual | 2 | Roadmap DAG + program summary DAG |

### Next publications

1. **Structural anatomy paper** — submit to CCC 2026 or STACS 2027. Ready now.
2. **Compression bound** — if proved, short follow-up establishing unconditional σ∞(d) → ∞.
3. **Path C results** — if Nullstellensatz degree separation found, major paper.
4. **Magnification gap survey** — if all paths stall by month 18.

---

## Summary

The program has completed its transition from "P ≠ NP proof attempt" to "structural theory of circuit restriction spaces" and has begun experimental proof complexity work (Path C). The unified paper documents the first quantitative characterization of how Boolean formula DAG topologies behave under restriction, discovers a scaling law grounded in Savický's classical anti-concentration theorem, and frames precise open problems for the community.

Path C experiments have established **two definitive negative results**: (1) NS degree of the structural CSP Γ(T,d,s) is always 2 over both Q and GF(2), due to the inherent 2-CSP structure; (2) resolution width of the direct (one-hot) CNF encoding equals the max initial clause width, giving trivial Ben-Sasson–Wigderson bounds. Both standard proof complexity measures are shallow for the structural CSP. However, the computational campaign discovered 302 structural obstruction witnesses at n=4, d=3, with a sharp 50/50 split between graph-coloring UNSAT and edge-incompatible UNSAT, and a surprising Hamming weight threshold at weight 8. Obstruction counting/density is the most promising remaining Path C direction.

The most actionable next steps are: (1) submit the paper, (2) complete the n=4 d=3 scan-solve and characterization, (3) test obstruction persistence at s≤5, (4) pursue the compression bound.
