# Paper Outline: CCC Format (12–14 pages)

## Title

**Restriction Images and Structural Entropy in Boolean Formulas**

## Authors

[redacted for submission]

---

## Target: 12–14 pages, CCC style (LIPIcs format, ~40 lines/page)

---

## Page Budget

| Section | Pages | Content |
|---|---|---|
| Abstract | 0.5 | Three results + CSP framing |
| §1 Introduction | 2.0 | Motivation, summary of results, organization |
| §2 Preliminaries | 1.5 | Formulas, DAG isomorphism, restriction, sub-cube graph |
| §3 Computational framework | 1.0 | Enumeration method, canonical forms, scale |
| §4 Structural taxonomy | 2.0 | d=3 periodic table, three tiers, frontier stiffness |
| §5 The scaling law | 3.0 | Core section: σ∞(d), convergence, dilution, Savický, conjectures |
| §6 Case study: BENT | 1.5 | 50% incompatibility, structural gap, global circuit family |
| §7 Consequences | 1.5 | CSP interpretation, sparsity, proof complexity, what we don't do |
| References | 1.0 | ~15 entries |
| **Total** | **14.0** | |

---

## Section-by-Section Plan

### Abstract (0.5 pages)

Use the abstract from `final_manuscript_sections.md`. Three results in order: scaling law, taxonomy, BENT. Close with CSP framing and magnification connection. No overclaims.

### §1. Introduction (2.0 pages)

**§1.1 Context.** (0.5p) MCSP and hardness magnification. The locality barrier. The gap between structural non-locality and circuit lower bounds. One paragraph, no deep formalism.

**§1.2 This paper.** (1.0p) Three results stated informally:
- Result 1: σ∞(d) scaling law with the three data points
- Result 2: Complete d=3 taxonomy with tiers
- Result 3: BENT incompatibility at 50%

State the Savický connection and the conjectures.

**§1.3 Organization.** (0.5p) Brief section guide. Mention supplementary code.

### §2. Preliminaries (1.5 pages)

**Definitions only — no proofs.** Keep tight.

- Formula over {AND, OR, NOT}: syntax, size (all gates counted), fan-out 1
- DAG isomorphism: canonical form via commutative sorting
- Restriction: hardwire input, propagate constants, re-index
- Restriction image I_f(s, d, i, v) and restriction universe U(s, d)
- Structural fluidity Φ(f, s)
- Sub-cube intersection graph G_{n,d}: vertices, structural edges
- STRUCT-MATCH: DAG-isomorphic restrictions on overlap
- Compatibility CSP Γ(T, d, s): variables, domains, constraints

Note on complexity measure: all gates counted, NOT not free. Reference Khrapchenko for standard De Morgan measure.

### §3. Computational Framework (1.0 pages)

- Enumeration by size with size-sum optimization
- Scale table (d=2,3,4; sizes reached; circuit counts; time)
- Correctness: cross-checks against known values (XOR=4, AND₃=2)
- Code availability statement

**No code listings.** Just describe the method and point to supplementary material.

### §4. The Structural Taxonomy of 3-Input Functions (2.0 pages)

**§4.1 The complexity spectrum.** (0.5p) Table: sizes 0–12, function counts. PAR3 = 11 in our model, = 9 = 3² in standard De Morgan (Khrapchenko). Note the gap at size 10.

**§4.2 Structural tiers.** (0.75p) Define stiff/moderate/fluid. Table at s ≤ 4: 4 stiff, 43 moderate, 74 fluid. Show tier evolution at s ≤ 5 (XOR softens) and s ≤ 6 (unanimity appears). State frontier stiffness recurrence.

**§4.3 Restriction image compression.** (0.75p) Table: ZERO 247×, AND 8.2×, XOR 1×. The restriction map is highly compressive. This is why STRUCT-MATCH is selective.

### §5. The Universe-to-Image Scaling Law (3.0 pages)

This is the core section. Every claim at the right epistemic level.

**§5.1 Cross-dimensional scaling.** (0.5p) Table: d=2,3,4 at s ≤ 4. Universe grows 4.8×/step, max image 2.3×/step. Ratio: 2.0, 6.3, 13.3.

**§5.2 Size-budget convergence.** (0.5p) Table: d=3, s=0 through 5. Growth factors 1.00, 1.30, 1.15, 1.06, 1.02. Convergence to σ∞(3) ≈ 6.4.

**§5.3 The top-share identity.** (0.5p) σ∞(d) = 1/α(d). Table: α = 49.3%, 15.8%, 7.5% at d=2,3,4. The top function is always a simple projection.

**§5.4 The dilution mechanism.** (0.5p) Redundancy budget s − C(g) controls T_g. Simple functions dominate because they have the most room for redundant constructions. But the number of contributing functions grows as 2^{2^{d-1}}, diluting the top share.

**§5.5 Anti-concentration via Savický.** (0.5p) State Savický's theorem (formula distribution → uniform). Canonical compression 1.1–2.0× (verified). Conditional corollary: bounded compression → σ∞(d) → ∞.

**§5.6 Conjectures and open problems.** (0.5p)
- Conjecture 1: σ∞(d) = lim_{s→∞} σ(s,d) exists for all d.
- Conjecture 2: σ∞(d) → ∞ as d → ∞. Stronger form: σ∞(d) ≥ 2^{Ω(d)}.
- Open Problem: Restriction entropy E[H(F)] = Θ(log d) or Θ(d)?

### §6. Case Study: The BENT Function (1.5 pages)

**§6.1 Setup.** (0.25p) n=4, d=2, 24 sub-cubes, 96 structural edges.

**§6.2 Incompatibility.** (0.5p) 48/96 edges empty intersection at s ≤ 4. All involve XOR boundaries. Arc consistency empties all domains.

**§6.3 The global circuit family.** (0.5p) Size-9 global circuit → 96/96 compatible. Structural gap γ = 5/4. Non-minimal restrictions carry global fingerprint.

**§6.4 The F^{cir} vs F^{fn} distinction.** (0.25p) Functional compatibility trivial everywhere; structural compatibility fails at 50%. STRUCT-MATCH is strictly more constraining.

### §7. Consequences for Meta-Complexity (1.5 pages)

**§7.1 The compatibility CSP.** (0.4p) Γ(T,d,s) as a meta-complexity object. Constraint density controlled by σ∞(d).

**§7.2 Sparsity of OD=1.** (0.4p) Compatible families are rare. Natural proofs evasion. Detection problem formulation.

**§7.3 Proof complexity.** (0.3p) Nullstellensatz degree of compatibility CSP. Connection to IPS. Unexplored direction.

**§7.4 What this work does not do.** (0.4p) No progress toward P ≠ NP. Toolkit for future attempts. Final sentence on DAG topology as complementary perspective.

### References (1.0 pages)

11 entries: Savický 1990, Lefmann–Savický 1997, Khrapchenko 1971, Tarui 2008, Brodsky–Pippenger 2005, Oliveira–Santhanam 2018, CHOPRS 2022, Håstad 1998, Chauvin–Flajolet–Gardy–Gittenberger 2004, Grochow–Pitassi 2014, Razborov–Rudich 1997.

---

## What to Cut (from the current 30+ pages of working documents)

- All roadmap material (Path A/B/C analysis) → separate internal document
- The n=5 misalignment data → mention in one sentence, details in supplementary
- The full periodic table listing of all 256 functions → supplementary appendix
- The OD witness language analysis → separate paper
- All discussion of P vs NP except the disclaimer in §7.4
- The growth mechanism heuristic details → condense to §5.4
- The Fourier rejection analysis → omit entirely
- The parity anchor hypothesis and its rejection → omit entirely
- The frontier stiffness recurrence across sizes 4/5/6 → condense to one paragraph in §4.2

## What NOT to Cut

- The σ∞(d) data tables (§5.1, §5.2, §5.3) — these are the paper
- The Savický bridge (§5.5) — this is what makes it theoretical
- The compression ratio verification (§5.5) — this is what makes the bridge honest
- The conjectures (§5.6) — these are what make it a research program
- The BENT 50% result (§6) — this is the concrete illustration
- The §7.4 disclaimer — this is what makes it publishable

---

## Style Notes for CCC/LIPIcs

- Theorems, propositions, conjectures in standard LaTeX environments
- Proofs in proof environments (the Savický corollary is the only nontrivial proof)
- Tables for all data (not inline numbers in prose)
- No color in tables or figures (CCC prints in B&W)
- Supplementary code referenced by URL, not included in body
- Keep the abstract under 250 words (current version is ~280, trim the BENT paragraph)
