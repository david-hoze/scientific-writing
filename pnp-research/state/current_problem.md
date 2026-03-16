# Current Problem: Path C — Standard Proof Complexity Exhausted

## Context

Path C has been fully investigated for the two standard proof complexity measures. Both yield negative results.

## Key Findings

### Negative Result 1: NS degree = 2 (Session 4)

**NS degree of the structural CSP is always 2**, regardless of the function, field (Q or GF(2)), or sub-instance. This is inherent to the 2-CSP structure: one-hot encoding gives degree-2 constraints, and the certificate always exists at degree 2.

### Negative Result 2: Resolution width = max initial clause width (Session 6)

**Resolution width of the one-hot CNF encoding equals the max at-least-one clause width.** For TT=686 core (35 vars): proof width 17 = max initial clause width 17. The Ben-Sasson–Wigderson tradeoff gives: size ≥ 2^((17-17)²/35) = 1. Trivially bounded.

**Why:** The 2-CSP structure means all constraint clauses are binary (width 2). CDCL conflict analysis resolves these binary clauses with at-least-one (cardinality) clauses. The intermediate resolvents never exceed the cardinality clause width. No "amplification" occurs.

**Additional finding:** Node 2 (domain size 1) is essential — ALL UNSAT subsets of TT=686 contain it. Without the forced node, everything is SAT. Width is bimodal: 1 (unit prop, with enough propagation paths) or 16 (search needed, in sparse subgraphs).

## What's Exhausted

- ❌ NS degree over Q: always 2
- ❌ NS degree over GF(2): always 2
- ❌ Resolution width (direct/one-hot encoding): equals max initial clause width

## Remaining Directions (Low Confidence)

1. **Alternative CNF encodings** (log, order): Different encodings change clause structure, potentially enabling width amplification. Not yet tested, but speculative.
2. **Higher-arity polynomial encoding**: Changes degree structure of polynomial system.
3. **Lifting theorems**: CSP → circuit complexity, but shallow proof complexity makes this unlikely to work.
4. **Obstruction counting/density**: How does the fraction of UNSAT functions scale with n? This is *not* proof complexity but could still connect to circuit complexity.
5. **Different polynomial system**: Encode OD directly rather than the CSP.

## Recommended Priority

Path C proof complexity: **LOW** (downgraded from MEDIUM). Standard measures are exhausted. The most productive remaining work is:
- Complete the scan-solve for full UNSAT census at n=4
- Test obstruction persistence at s≤5
- Characterize the obstruction landscape (density vs n)
- Focus on Path B (paper submission) and the compression bound

## Computational State

- Scan-solve: running (stuck at 121/256), 1 UNSAT found (this restart). Previous incomplete scan found 302/65536.
- Resolution width tools: `cdcl_width.py`, `resolution_width.py`, `width_scaling.py` operational
- Profile computation: works for domains ≤ ~5K elements
- NS degree: works via linear algebra for ≤ ~200 profile variables
- Idris2 verified solver: operational, zero `believe_me`
