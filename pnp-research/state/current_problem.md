# Current Problem: Path C Proof Complexity — Beyond NS Degree

## Context

We've completed the first phase of Path C: computed NS degree for structural CSP obstruction witnesses at n=4, d=3. The result is decisive but negative for the direct NS approach.

## Key Finding

**NS degree of the structural CSP is always 2**, regardless of the function or sub-instance. This is an inherent property of the 2-CSP structure:
- Constraints are pairwise (between overlapping sub-cubes)
- One-hot encoding gives degree-2 incompatibility constraints
- Combined with degree-1 selection constraints → NS degree = 2

This means NS degree of the standard encoding **cannot grow with n** and thus cannot yield circuit lower bounds.

## What We've Established

1. **Hundreds of UNSAT structural CSPs at d=3, s≤4** — genuine obstruction witnesses
2. **Two classes**: edge-incompatible (FI>0, trivial UNSAT) and graph-coloring (FI=0, non-trivial)
3. **Minimal UNSAT cores have 4 nodes** (for TT=686: {0,2,5,6})
4. **Profile reduction** cuts variables from ~8K to ~150 (2.8x typical compression)
5. **NS degree = 2 for all tested instances** — the proof is always "shallow"

## The Question

The structural CSP Γ(T, d, s) is genuinely UNSAT for many functions, confirming that OD(T) = 1 (these functions have non-trivial cohomology). But the *proof complexity* of this UNSAT is low (NS degree 2). For circuit lower bounds, we need a proof complexity measure that grows with n.

## Possible Directions

1. **Resolution width**: Can be Ω(n) even for 2-CSPs with NS degree 2. Does the structural CSP's resolution width grow?
2. **NS degree over GF(2)**: Grigoriev showed Ω(n) for pigeonhole over GF(2). The structural CSP might have high GF(2)-NS degree.
3. **Higher-arity encoding**: Replace one-hot with algebraic encoding of the domain. This changes the degree of constraints and could increase NS degree.
4. **Different polynomial system**: Instead of encoding the CSP directly, encode the OD function or a related function as a polynomial system, and measure its proof complexity.
5. **Shift focus**: The *existence* of obstructions (not their proof complexity) may be what matters. Focus on counting/characterizing UNSAT functions and relating this to circuit complexity.
6. **Lifting from sub-cube CSP to circuit problem**: The proof complexity of the CSP at dimension d might relate to circuit depth or size lower bounds through a lifting theorem.

## What We Need from ChatGPT

Strategic reassessment of Path C given the NS degree = 2 finding. Which alternative proof complexity measure is most promising? Should we pivot to resolution width, GF(2) NS degree, or a different encoding?

## Computational State

- scan-solve: ~33% complete (302 UNSAT found of ~900 eligible at that point). Needs restart.
- Profile computation: works for domains ≤ ~5K elements (30s per function)
- NS degree: works via linear algebra for ≤ ~200 profile variables
- M2: confirmed 1 ∈ I for 39-variable sub-instance in < 1 second
