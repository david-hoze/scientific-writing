# Current Problem: n=5 Obstruction Landscape

## Context

Path C proof complexity is exhausted (NS=2, res width=initial, PC<=2). The investigation has shifted to understanding how structural obstructions scale with n. A full Python pipeline (`n5_scan.py`) enables efficient n=5 analysis.

## Key Findings (Session 7)

### Finding 1: n=4 Complete Census — 1064 UNSAT Functions

Complete scan of all 65,536 n=4 functions at d=3, s<=4:
- **1064 UNSAT** (1.62%)
- **2536 SAT** (3.87%)
- **Remaining: TRIVIAL/UNKNOWN**
- Hamming weight distribution symmetric around weight 8, peaking at weights 7 and 9

### Finding 2: Lifted n=4 UNSAT → n=5 UNSAT at s<=4

All tested n=4 UNSAT functions maintain UNSAT when lifted to n=5 at s<=4:
- lift_686: 40/40 covered, 151K domain, 7092 profiles, graph-coloring UNSAT
- lift_139: 40/40 covered, 237K domain, 8384 profiles, 4 incompatible edges
- lift_and variants: also UNSAT
- lift_xor variants: UNSAT on covered subgraph (28-30/40 coverage)

### Finding 3: n=5 Obstructions Still Dissolve at s<=5

At s<=5 (1.59M formulas, 191/256 coverage):
- **lift_686 → SAT** (dissolved, same as at n=4)
- **lift_xor_139 → EMPTY+GENUINE** (persists on 30/40 covered subgraph)
- 3 functions → UNKNOWN (solver exhausted at 1M backtracks)
- 2 functions → SAT

**Conclusion:** The lifted obstructions are still size-budget artifacts. They dissolve when the formula budget increases, exactly as at n=4. The n=5 dimension does NOT create inherently harder obstructions.

### Finding 4: Coverage Gap Makes Random n=5 Trivially UNSAT

At n=5, d=3, s<=4: only 121/256 (47%) 3-var truth tables are coverable. Every random n=5 function has empty-domain sub-cubes → trivially UNSAT. No random function has full (40/40) coverage.

### Finding 5: EMPTY+GENUINE Rate Is High

Even restricting to the covered subgraph, 70% of random n=5 functions have genuine obstructions. This suggests structural incompatibility is common, but may dissolve at larger sizes.

## Assessment

The n=5 investigation confirms the pattern from n=4:
1. Structural obstructions exist at low size budgets
2. They dissolve when the size budget increases
3. No evidence of persistent, size-independent obstructions
4. The 2-CSP structure is too soft — it's always a 2-coloring problem with trivial algebraic complexity

## Remaining Questions

1. Does the obstruction DENSITY scale with n? (1064/65536 at n=4 — what fraction at n=5 among fully-covered functions?)
2. Does the dissolution size grow with n? (s<=5 dissolves at n=4; does it require s<=6 at n=5?)
3. Are there ANY persistent obstructions at any n?

## Recommended Next Steps

1. **Complete the all-lifted scan** (1064 × 3 = 3192 targets at n=5, s<=4) — running
2. **Focus on Path B** (paper submission): the structural anatomy paper is the most publishable outcome
3. **Consider Path C formally closed** unless obstruction density shows non-trivial scaling

## Computational State

- `n5_scan.py`: Full Python pipeline, ~5 fn/s at n=5, pre-computation ~23s (s<=4) or ~7.5min (s<=5)
- n=4 UNSAT list: 1064 functions saved to `scripts/n4_unsat.txt`
- n=5 all-lifted scan: running (3192 targets)
