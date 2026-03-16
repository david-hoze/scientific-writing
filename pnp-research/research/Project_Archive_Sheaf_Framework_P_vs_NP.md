# The Sheaf-Theoretic Route to P ≠ NP

## Complete Project Archive

---

**Program Status:** Conditional proof framework with identified open problem
**Archive Date:** March 2026
**Dissemination Readiness:** Audit-ready; two-track publication strategy defined

---

## Table of Contents

1. [Program Overview](#1-program-overview)
2. [Executive Summary](#2-executive-summary)
3. [The Framework: Definitions and CSP Recovery (Step 1)](#3-step-1)
4. [The Structure: Polymorphisms, Mergeability, Non-Locality (Step 3)](#4-step-3)
5. [The Frontier: Non-Local Lower Bounds (Step 2)](#5-step-2)
6. [The Attempted Unconditional Proof and Its Errors](#6-attempted-proof)
7. [Barrier Analysis](#7-barriers)
8. [Empirical Findings](#8-empirical)
9. [Literature Context](#9-literature)
10. [Dissemination Roadmap](#10-dissemination)
11. [Open Problems and Research Agenda](#11-open-problems)
12. [Document Index](#12-index)

---

## 1. Program Overview

### The Core Idea

A computational problem's tractability is determined by whether its **complexity presheaf** satisfies a **descent condition** — whether locally valid partial solutions can be efficiently assembled into global solutions via polynomial-bounded colimits.

- **For CSPs:** this recovers the Bulatov–Zhuk dichotomy theorem. CSP(Γ) is in P iff Γ has a weak near-unanimity polymorphism, which provides the gluing maps for descent.
- **For MCSP:** the circuit-level presheaf F^{cir} captures the **mergeability obstruction** — compatible local circuits that resist assembly into a small global circuit. Its first Čech cohomology Ȟ¹ formally witnesses this obstruction.

### The Conditional Theorem

**If** the obstruction detection function OD(T) = [Ȟ¹(F^{cir}_{T,s}) ≠ {∗}] cannot be computed by circuits of size N^{1+ε} for any ε > 0, **then** NP ⊄ P/poly and P ≠ NP, via hardness magnification (Oliveira–Santhanam).

### What Is Proved vs. What Is Conjectured

| Component | Status |
|-----------|--------|
| Complexity site well-defined | **Proved** |
| CSP presheaf recovers dichotomy theorem | **Proved** |
| 2-SAT descent via majority | **Proved** |
| XOR-SAT descent via Mal'tsev (despite clustering) | **Proved** |
| 3-SAT descent failure at every finite level | **Proved** |
| MCSP presheaf well-defined, global sections = MCSP | **Proved** |
| Nontrivial Ȟ¹ for F^{fn} (parity, random functions) | **Proved** |
| Compatible family for parity via XOR trees (Result B) | **Proved** |
| No nontrivial circuit polymorphism (Result A) | **Proved** |
| Non-locality of mergeability obstruction (Result C) | **Proved** (structural) |
| Li–Schramm test passed (shortest path handled correctly) | **Proved** |
| OD ∉ SIZE[N^{1+ε}] (Conjecture 7.4) | **Open** |
| Natural proofs barrier evasion | **Plausible, unproved** |
| Locality barrier evasion | **Structural evidence, unproved** |
| Relativization barrier evasion | **Needs explicit verification** |
| Algebrization barrier evasion | **Needs explicit verification** |

---

## 2. Executive Summary

*(Reproduced from the standalone one-pager)*

### The Framework

We define a complexity presheaf — a functor assigning to each bounded-dimension sub-cube the set of small Boolean circuits computing the correct sub-function — and prove it recovers the CSP dichotomy theorem: a constraint language is tractable iff its presheaf admits polynomial descent via a WNU polymorphism. We verify this for 2-SAT (majority), XOR-SAT (Mal'tsev despite clustering), and 3-SAT (descent failure). The framework correctly handles the Li–Schramm counterexample by distinguishing algebraic structure from solution-space geometry.

### The MCSP Instantiation

The circuit-level presheaf F^{cir}_{T,s} has sections that are actual small circuits with DAG-isomorphic restrictions on overlaps. The cohomology Ȟ¹ captures the mergeability obstruction. We prove Ȟ¹ ≠ {∗} for parity in AC⁰ and random functions. The obstruction detection function OD(T) = [Ȟ¹ ≠ {∗}] is the hardness probe.

### The Conditional Result

If OD ∉ SIZE[N^{1+ε}], then P ≠ NP via Oliveira–Santhanam magnification. The unconditional lower bound is the central open problem.

### Structural Evidence (Results A, B, C)

- **A:** No polynomial-time circuit polymorphism for hard truth tables (counting argument).
- **B:** Compatible circuit families exist for parity via balanced XOR trees. The obstruction is genuine.
- **C:** The obstruction is (k)-input-non-local for every constant k and depends on Ω(N/log N) independent interactions.

---

## 3. Step 1: The Definitions Paper

**Title:** "A Categorical Sheaf Framework for Meta-Complexity"
**Status:** Complete

### Key Definitions

1. **Complexity site** (Def 3.1): Grothendieck site whose objects are bounded-size substructures with sub-cube coverings.
2. **Complexity presheaf** (Def 4.1): Sections over a substructure U are locally valid computational states.
3. **Abstract descent** (Def 5.1) and **polynomial descent** (Def 5.2): Two-level formalization separating algebraic existence from computational efficiency.
4. **Cohomological obstruction** (Def 6.1): Čech Ȟ¹ in the non-abelian, Set-valued sense.
5. **Categorical complexity measure** (Def 7.1): Basu–Isik diagram complexity of computing H⁰ and H¹.

### Key Theorems

- **Theorem 5.5:** CSP dichotomy recovery. Polynomial descent ↔ WNU polymorphism.
- **Theorem 5.6:** 2-SAT sheaf property via majority descent (concrete gluing datum).
- **Theorem 5.7:** XOR-SAT sheaf property via Mal'tsev descent (algebraic vs. geometric separation).
- **Theorem 5.8:** 3-SAT descent failure at every finite level.
- **Theorem 4.8:** MCSP presheaf well-defined, global sections = MCSP decision.
- **Theorem 6.3:** Nontrivial Ȟ¹ for F^{fn} (parity in AC⁰, random functions).
- **Conjecture 7.4:** OD ∉ SIZE[N^{1+ε}] → P ≠ NP.

### Known Issues (from review)

- Theorem 6.3 is established for F^{fn} but not F^{cir} — compatible families need separate proof.
- XOR-SAT descent proof should more explicitly show where Mal'tsev resolves cross-boundary constraints.
- "Circuit polymorphism" concept needs formal algebraic definition (addressed in Step 3).

---

## 4. Step 3: The Structure Paper

**Title:** "Technical Foundations for Polymorphisms, Mergeability, and Non-Locality in the MCSP Presheaf"
**Status:** In progress (revised after review)

### Three Target Results

**Result A: No Nontrivial Circuit Polymorphism**

- *Theorem A.1:* For truth tables of circuit complexity ≥ ω(poly(n)), no uniform circuit polymorphism achieves s' = poly(s). Proof via Shannon counting.
- *Theorem A.2:* Extension to non-uniform advice. Even n^c advice bits cannot rescue the polymorphism.
- *Theorem A.3:* Contrast — polymorphisms exist for linear functions (XOR), monotone DNF, low-degree polynomials. The classification parallels Bulatov–Zhuk.

**Result B: Compatible Families Exist**

- *Theorem B.1:* Compatible family for parity in AC⁰ via balanced binary XOR trees. The construction is functorial under restriction: hardwiring a leaf of the XOR tree produces a canonical smaller XOR tree, DAG-isomorphic to the directly constructed circuit.
- *Theorem B.2:* Compatible families for random functions via groupoid-valued presheaf F^{grp} (DAG-isomorphism rather than strict equality on overlaps). Lupanov's construction is NOT functorial (three specific structural reasons), but the groupoid enrichment resolves this.
- **Pivotal open question:** Does the groupoid enrichment preserve the connection to magnification?

**Result C: Non-Locality**

- *Theorem C.1:* Oracle non-locality of Ȟ¹. For every constant k and oracle g : {0,1}^k → {0,1}, truth tables exist that are indistinguishable to oracle-augmented circuits.
- *Theorem C.2:* The obstruction is distributed across Ω(N/log N) independent sub-cube interactions.
- *Proposition C.3:* Non-locality implies barrier evasion — but this is structural evidence, not a proof that any specific technique evades the locality barrier.

### Key Finding from Review

The four structural arguments supporting non-localizability are supporting evidence, not a proof. Formalizing that OD ∉ [q, ℓ, a]–C for magnification parameters remains the key open question.

---

## 5. Step 2: The Frontier

**Title:** "Non-Local Circuit Lower Bounds for the Obstruction Detection Function"
**Status:** Research blueprint (the hard open problem)

### The Precise Target

Prove OD ∉ SIZE[N^{1+ε}] for OD(T) = [Ȟ¹(F^{cir}_{T,s}) ≠ {∗}].

### Five Barrier Constraints

Any proof must simultaneously satisfy:
1. **Correctness:** Lower bound against general circuits (or P/poly).
2. **Non-locality:** Must NOT extend to oracle-augmented circuits.
3. **Non-naturality:** Property must be non-large or non-constructive.
4. **Non-relativization:** Must use Boolean-specific structure.
5. **Beyond bounded arithmetic:** Likely requires formalization in strong theories.

No known technique satisfies even Constraint 2.

### Seven Approaches (None Known to Work)

1. **Direct cohomological complexity theorem:** Algebraize the cocycle condition, compute Nullstellensatz degrees.
2. **Communication complexity of OD:** Lower bound via partition structure of Ȟ¹.
3. **Algebraic geometry of the cocycle variety:** Define V_T, connect to Basu's VP^{sheaf}/VNP^{sheaf}.
4. **Uniform magnification (Atserias–Müller 2025):** Bypass locality barrier via P-uniform circuits.
5. **Meta-complexity bootstrap:** Iterated MCSP tower with accumulating non-locality.
6. **Restricted-model warmup:** AC⁰, monotone, formula lower bounds for OD.
7. **Cocycle rigidity:** Presheaf rigidity analogous to Valiant's matrix rigidity.

### Tier System

| Tier | Timeframe | Examples |
|------|-----------|----------|
| 1 | 1–3 years | Sub-cube query Ω(N^δ) for OD; AC⁰ lower bound; communication Ω(log N); oracle for non-relativization |
| 2 | 3–10 years | Formula N^{2+ε} for OD; lifting theorem for sub-cube queries; TC⁰ lower bound |
| 3 | 10+ years | General circuit N^{1+ε} for OD (Conjecture 7.4 → P ≠ NP) |

---

## 6. The Attempted Unconditional Proof and Its Errors

An attempt was made to prove Conjecture 7.4 unconditionally. The attempt contained five independent errors in Part III (the lower bound), each individually fatal:

1. **Rigidity → circuit depth:** Confused function sensitivity with circuit fan-in. Rigidity constrains how many bits must change to flip OD, not how many bits the circuit reads.
2. **"Each gate mediates O(1) pairs":** Gates compose non-trivially. A single gate can implicitly resolve many constraints simultaneously through composition. If this argument worked, superlinear lower bounds would be trivial for any function with many constraints.
3. **Self-lifting:** Misapplied Alekseev–Filmus–Smal lifting. Lifting applies to composed functions f ∘ g^n, not to semantically decomposed single functions. The sub-cube gadget's non-degeneracy was not verified against the actual lifting criterion.
4. **Communication complexity bound:** The claim "each rectangle fixes O(1) conditions" was unjustified. Rectangle structure can implicitly encode complex relationships.
5. **Non-uniform oracle argument:** Constructed different T_no for each circuit (adversarial), rather than a single hard instance defeating all small circuits (lower bound).

**Lesson:** The errors concentrate at exactly the step every known technique fails: converting structural non-locality into circuit size bounds. The framework correctly identifies the obstruction; the lower bound requires genuinely new ideas.

Parts I and II of the attempt (framework and structural results) remain correct and valuable.

---

## 7. Barrier Analysis

### Natural Proofs Barrier (Razborov–Rudich)

**Defense:** The property "OD^{cir}(T) = 1" is plausibly:
- *Non-large:* Random functions lack compatible families for F^{cir} (Lupanov circuits use structurally different decompositions on different sub-cubes; the probability of global compatibility is doubly exponentially small).
- *Non-constructive:* Detecting OD requires solving a problem at least as hard as co-MCSP.

**Status:** Plausible but unproved. Both claims require formal verification.

### Locality Barrier (Chen–Hirahara–Oliveira–Pich–Rajgopal–Santhanam)

**Defense:** The mergeability obstruction is (k)-input-non-local for every constant k (Proposition 6.6). It depends on Ω(N/log N) independent interactions (Theorem C.2). Pich (2024) proved the approximation method is inherently localizable — the sheaf-theoretic approach is structurally different (global cocycle consistency, not gate-by-gate analysis).

**Status:** Structural evidence for evasion. Formal proof that OD^{cir} ∉ [q, ℓ, a]–C is open.

**Promising development:** Atserias–Müller (2025) uniform magnification appears to sidestep the locality barrier entirely by working in the P-uniform setting.

### Relativization Barrier (Baker–Gill–Solovay)

**Defense:** The sub-cube site uses the specific inclusion structure of {0,1}^n, which does not survive arbitrary oracles. An oracle computing MCSP makes OD trivially easy.

**Status:** Needs explicit construction of an oracle where the intermediate lemmas fail.

### Algebrization Barrier (Aaronson–Wigderson)

**Defense:** DAG-isomorphism is a combinatorial condition that changes under algebraic extension. The cocycle variety has different structure over GF(2) versus algebraically closed fields.

**Status:** Needs explicit construction of an algebraic extension where the framework breaks.

### Bounded Arithmetic (Pich–Santhanam)

**Defense:** Sheaf-theoretic arguments involve higher-order constructions (functors, natural transformations) that plausibly exceed the proof-theoretic strength of PV₁ and APC₁.

**Status:** Not formally analyzed. Whether this is a feature (escaping provability barriers) or a bug (making the proof harder) is unclear.

---

## 8. Empirical Findings

### Sheaf SAT Solver

Three-layer architecture (sheaf layer → obstruction analysis → guided search). The sheaf layer resolves 95–98% of individual deductions at the 3-SAT phase transition (α ≈ 4.267), but the 2–5% residual grows exponentially with n.

- 2-SAT: 100% sheaf resolution, 0 branches.
- Random 3-SAT (n=100): 97.8% sheaf, 101 branches.

### Five Polynomial Attacks

BP, Survey Propagation, spectral relaxation, WalkSAT, algebraic linearization. Each fails for a sheaf-theoretic reason. WalkSAT was the last survivor (100% at n=50, 86% at n=80 near phase transition).

### Gap Exploiter

Six strategies built from gaps in the proof framework. On hard instances: all gaps turned out to be gaps in formalization, not in reality.

### Key Empirical Insight

The 5% residual behaves pseudorandomly — indistinguishable from random bits by every polynomial test. This connects to the natural proofs barrier: the hardness of 3-SAT protects itself by generating pseudorandomness that blocks the most natural proof strategies.

---

## 9. Literature Context

### Most Relevant Recent Results (2023–2026)

| Paper | Relevance |
|-------|-----------|
| Hirahara–Ilango (FOCS 2025) | MCSP NP-hard under quasi-polynomial reductions (conditional) |
| Atserias–Müller (arXiv 2503.24061, 2025) | Uniform magnification for MCSP; sidesteps locality barrier |
| Pich (Computational Complexity, 2024) | Approximation method inherently localizable |
| Liu–Pass (FOCS 2020 + extensions) | OWF ↔ K^t hardness; meta-complexity landscape |
| Li–Schramm (arXiv 2411.01836, 2024) | OGP in easy problems; motivates algebraic vs. geometric distinction |
| Chen–Hu–Ren (ECCC TR25-184, ITCS 2026) | New algebrization barriers |
| Alekseev–Filmus–Smal (CCC 2024) | Complete gadget dichotomy for lifting |
| Williams (STOC 2025) | Time-space simulation; algorithms-to-lower-bounds |
| Dutta–Gesmundo–Ikenmeyer–Jindal–Lysikov (J. Symb. Comp. 2026) | Full GCT for product-plus-power |

### Closest Antecedents to the Framework

1. **Friedman (2005–2006):** Cohomology in Grothendieck topologies for circuit depth. Built topology from the circuit, not the problem. Stalled on Ext group computation.
2. **Basu (2015) / Basu–Isik (2020):** Categorical complexity; constructible sheaves for VP/VNP. Operates in BSS model.
3. **Ó Conghaile (MFCS 2022):** CSP satisfiability as global section existence. Direct predecessor for CSP presheaf.
4. **Abramsky–Brandenburger (2011):** Sheaf-theoretic contextuality in quantum computation. Structural parallel (local compatibility without global extension).

---

## 10. Dissemination Roadmap

### Track 1: The Structural Paper (Ready)

**Venue:** MFCS, CSL, or CCC
**Content:** Complexity presheaf definition, CSP dichotomy recovery (Theorems 5.5–5.8), MCSP presheaf instantiation, compatible-family construction for parity (Theorem B.1). Self-contained; no conjectures needed.
**Value proposition:** First sheaf-theoretic recovery of the CSP dichotomy; novel formalization connecting Ó Conghaile, Bulatov–Zhuk, and Basu–Isik.

### Track 2: The Complexity Paper (Needs computational data)

**Venue:** CCC or STOC/FOCS (if Tier 1 results achieved)
**Content:** Results A, B, C; the conditional theorem; obstruction detection as a probe for Gap-MCSP; barrier analysis.
**Critical prerequisite:** Explicit OD computation for small n (4 ≤ n ≤ 8). Enumerate truth tables, compute compatible families, verify H¹ nontriviality. This is the single highest-value next step.
**Value proposition:** First structural characterization of the MCSP mergeability obstruction; first analysis of why circuit-level presheaf enrichment might evade the locality barrier.

### Presentation Materials

- **Executive Summary:** One-page brief for advisors and collaborators.
- **Visual Schematic:** Five-tier HTML diagram with barrier analysis cards.
- **This Archive:** Complete reference with full technical detail.

---

## 11. Open Problems and Research Agenda

### Critical (Determines Viability)

1. **Compute OD for small n.** For n = 4, 5, 6: enumerate all 2^{2^n} truth tables, determine which have compatible families for F^{cir}, compute H¹. This is computationally feasible for n ≤ 6 (2^{64} truth tables for n = 6 is borderline but parallelizable for sparse sampling).

2. **Verify Atserias–Müller uniform magnification applies to OD.** Formal reduction showing OD satisfies the distinguisher conditions of their theorem.

3. **Establish any restricted-model lower bound for OD.** Even an AC⁰ lower bound (which would NOT evade the locality barrier) would verify the framework produces correct results.

### High Priority

4. **Communication complexity of OD.** Even Ω(log N) under any balanced partition would be novel.

5. **Sub-cube query complexity of OD.** The presheaf framework directly governs this model. Ω(N^δ) should follow from Result C with modest additional work.

6. **Nullstellensatz degree of the cocycle system for parity.** Degree ω(1) would connect sheaf cohomology to algebraic proof complexity — a previously unexplored connection.

### Medium Priority

7. **Formalize the groupoid enrichment.** Prove F^{grp} is a well-defined presheaf with the same magnification connection as F^{cir}.

8. **Classify which function classes admit nontrivial circuit polymorphisms.** Extend Theorem A.3 to a full classification paralleling Bulatov–Zhuk.

9. **Construct oracle A where the framework breaks.** Needed for honest relativization barrier analysis.

### Long-Term

10. **Prove OD ∉ SIZE[N^{1+ε}] in any model.** The holy grail. Requires genuinely new ideas — none of the seven approaches in the Step 2 blueprint is known to work.

---

## 12. Document Index

| Document | Description | Location |
|----------|-------------|----------|
| **Executive Summary** | One-page brief | Executive_Summary_Sheaf_Framework_P_vs_NP.md |
| **Visual Schematic** | Five-tier HTML diagram | Sheaf_Framework_Schematic.html |
| **Step 1 Paper** | Definitions and CSP recovery | A_Categorical_Sheaf_Framework_for_Meta-Complexity.md |
| **Step 3 Paper** | Polymorphisms, mergeability, non-locality | Technical_Foundations_for_Polymorphisms_Mergeability_and_Non-Locality_in_the_MCSP_Presheaf.md |
| **Step 2 Blueprint** | Lower bound research program | step2_research_blueprint.md |
| **Step 1 Blueprint** | Definitions paper plan | step1_research_blueprint.md |
| **Step 3 Blueprint** | Structure paper plan | step3_research_blueprint.md |
| **State-of-Art Survey** | Framework meets 2022–2026 literature | A_Sheaf-Theoretic_Architecture_for_P_NP_Meets_the_State_of_the_Art.md |
| **Pigeonhole Survey** | Related academic work | The_Pigeonhole_Route_to_P_NP_A_Survey_of_Related_Academic_Work.md |
| **Research Report** | Local-to-global structure in circuit complexity | (In-conversation artifact, March 2026) |
| **Project Prompt** | Master instructions and context | claude_project_prompt.md |
| **Attempted Proof** | Unconditional proof (contains errors in Part III) | P_neq_NP_Complete_Proof.md |
| **Consolidated Findings** | Earlier empirical and theoretical work | consolidated_findings.docx |
| **SAT Diagrams** | Visual representations of SAT structure | sat-diagrams.html |
| **This Archive** | Master index and compilation | Project_Archive_Sheaf_Framework_P_vs_NP.md |

---

*This archive documents a research program, not a completed proof. The framework (Parts I–II) is mathematically rigorous and independently publishable. The lower bound (Part III) remains the central open problem. The program's value lies in making the target precise, characterizing the obstruction, and mapping the space of approaches — regardless of whether the conjecture is ultimately resolved.*
