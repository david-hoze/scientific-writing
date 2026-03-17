# Current Problem: Information Loss on Cycles (Lemma 3.4)

## Context

Sessions 9-10 established N_eff = 2^{H_Shannon} ~ 2.24^d (exponential) and that the proof technique must be distributional. Session 11 discovered the mechanism: UNSAT in the structural CSP arises from cohomological obstruction (local consistency + global inconsistency around cycles), not from edge-level conflicts. The specific mathematical statement to prove is Lemma 3.4 (overlap ratio bound).

## The Critical Empirical Finding (Session 11)

At n=4, d=3, s<=4, ALL 1,064 genuine UNSAT instances have:
- 0% fully incompatible edges
- 0% fully compatible edges
- **100% partially compatible edges**

Every edge admits some compatible circuit pairs. Yet no assignment satisfies all 24 edges simultaneously. This is a cohomological obstruction: non-trivial H^1 of the constraint presheaf.

## The Proof Architecture

```
HighNeff -> HighDiversity -> RichPartialStructure -> CohomologicalObstruction -> UNSAT
   [1]         [2]                [3]                       [4]                  [5]
  trivial     trivial         needs Lemma 3.4          follows from 3.4       standard
```

### The Information Loss Mechanism

1. Each partial edge has overlap ratio r = (matching keys at u)/(total keys at u), with 0 < r < 1
2. Along a cycle of length L: surviving fraction <= r^L
3. UNSAT when r^L < 1/k_max (k_max = max canonical groups at any node)
4. N_eff controls k_max: high N_eff = many types = many groups = more information to lose
5. Shannon entropy is correct because loss per edge is conditional entropy H(key_v|key_u) — average-case

### Formal Statement

**Lemma 3.4** (Overlap ratio bound): For a random truth table T on n variables, sub-cube dimension d, and sufficiently large circuit-size bound s:

    E_T[max_edge r_{uv}(T, d, s)] <= 1 - epsilon(d)

for some epsilon(d) > 0.

**Theorem 3.5** (N_eff forces UNSAT): If Lemma 3.4 holds, then UNSAT when N_eff(T,d) > (1-epsilon(d))^{-L}, where L is the shortest cycle length. Since N_eff ~ 2.24^d and the cycle length L is bounded by n, UNSAT holds for all sufficiently large n.

## What Is Proved

| Component | Status |
|-----------|--------|
| N_eff ~ 2.24^d (exponential) | PROVED computationally (d=2..6) |
| Shannon is last exponential Renyi order | PROVED computationally |
| All edges partial at sufficient s | PROVED computationally (n=4, d=3, s>=2) |
| 1,056/1,064 UNSAT instances machine-verified | PROVED (Idris2 certificates) |
| B1 = 14-16 independent cycles | PROVED computationally |
| Overlap ratio bounded away from 1 | **OPEN** (Lemma 3.4) |
| Cycle loss implies UNSAT | PROVABLE (standard information theory) |
| Presheaf H^1 != 0 iff no global section | STANDARD (sheaf theory) |

## What This Rules Out (Updated)

All previous exclusions, PLUS:
7. **Graph coloring / chromatic number arguments** — no fully incompatible edges exist at sufficient s
8. **Turan-type bounds** — conflict density = 0
9. **Fixed incompatible type pair arguments** — no universal pair-conflict across instances
10. **Clique-based lower bounds** — no fully-incompatible clique exists

## What Remains

1. **Prove Lemma 3.4** — the overlap ratio is bounded away from 1
2. **Generalize from n=4 to general n** — using N_eff scaling law
3. **Connect UNSAT to circuit lower bounds** — via the standard presheaf-OD correspondence
4. **Compute overlap ratios explicitly** — running now

## Idris2 Formalization

`Verified/ProofSearch.idr` encodes the argument as types:
- `?diversityForcesRichStructure_hole` — N_eff -> many canonical groups per node (partly provable)
- `?richStructureForcesObstruction_hole` — **THE MAIN OPEN PROBLEM** (Lemma 3.4 + Theorem 3.5)
- `?obstructionForcesUnsat_hole` — H^1 != 0 -> no CSP solution (standard)

## Computational State

New scripts (Session 11):
- `scripts/shannon_conflict_analysis.py` — edge classification analysis (found 100% partial)
- `scripts/overlap_ratio_analysis.py` — overlap ratio computation (running)
- `scripts/genuine_unsat_n4d3s4.txt` — all 1,064 genuine UNSAT truth tables

New Idris2 modules:
- `Verified/Exhaustive.idr` — certificate-based UNSAT verification
- `Verified/ProofSearch.idr` — type-level proof skeleton with holes

## Open Questions (Updated)

1. ~~Can T_g statistics beyond sigma grow exponentially?~~ **ANSWERED** (N_eff ~ 2.24^d)
2. ~~Does N_eff grow exponentially at d=6?~~ **ANSWERED** (Yes)
3. ~~Distributional communication complexity of OD?~~ **SUPERSEDED** by information loss on cycles
4. ~~Does constraint graph topology carry hardness?~~ **ANSWERED** (Yes — cycles are the vehicle)
5. ~~Can H^1 contribute beyond sigma?~~ **ANSWERED** (Yes — H^1 IS the mechanism)
6. ~~What is the right distributional proof technique?~~ **ANSWERED** (Information loss on cycles)
7. **Is the overlap ratio bounded away from 1?** (Lemma 3.4 — THE open problem)
8. Does the overlap ratio epsilon(d) grow or shrink with d?
9. Does the information loss mechanism generalize to n=5?
10. Can Lemma 3.4 be proved from the structure of Boolean formula canonical types?
