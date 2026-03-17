# N_eff: Exponential Structural Diversity in Boolean Formula Restrictions

## The Parameter

For d-variable Boolean formulas at size budget s, define the T_g distribution:
- T_g(d, s) = number of DAG-isomorphism classes of formulas at size ≤ s computing function g on d variables
- T(d, s) = Σ_g T_g(d, s) = total number of DAG classes
- p_g = T_g / T = formula-weighted probability of function g

The **Rényi effective type counts** at order α:
- N_α = 2^{H_α} where H_α = (1/(1-α)) log₂(Σ p_g^α)

Special cases:
- N_0 = |{g : T_g > 0}| (number of realized functions)
- N_1 = 2^{-Σ p_g log₂ p_g} (Shannon effective count = **N_eff**)
- N_2 = 2^{-log₂(Σ p_g²)} (collision effective count)
- N_∞ = 1/max p_g = T/max T_g (min-entropy effective count = **σ**)

## Computational Results (Theorem)

**Theorem (computational).** For basis {AND, OR, NOT} at d = 2, 3, 4, 5, 6:

| d | s | T | N_funcs | N_eff (α=1) | N_2 (α=2) | σ (α=∞) |
|---|---|---|---------|-------------|-----------|---------|
| 2 | 4 | 36,052 | 16 | 6.96 | 5.39 | 3.75 |
| 3 | 4 | 93,315 | 121 | 15.42 | 8.52 | 4.66 |
| 4 | 4 | 207,078 | 886 | 34.99 | 12.65 | 5.60 |
| 5 | 4 | 411,397 | 3,834 | 78.38 | 17.77 | 6.54 |
| 6 | 4 | 751,848 | 11,916 | 170.03 | 23.88 | 7.50 |

**Growth rates** (ratio d → d+1, uniform s ≤ 4):

| d→d+1 | N_eff ratio | N_2 ratio | σ ratio |
|-------|-------------|-----------|---------|
| 2→3 | 2.22 | 1.58 | 1.24 |
| 3→4 | 2.27 | 1.48 | 1.20 |
| 4→5 | 2.24 | 1.40 | 1.17 |
| 5→6 | 2.17 | 1.34 | 1.15 |

**Growth rate diagnostic:**
- N_eff: ratios CONSTANT at ~2.24 (d=2-5), slight dip at d=6 due to convergence → **EXPONENTIAL**
- N_2: ratios DECELERATING (1.58 → 1.34) → **POLYNOMIAL** (~d^{1.3})
- σ: ratios DECELERATING (1.24 → 1.15) → **POLYNOMIAL** (~d^{0.62}, consistent with σ ~ 1.33d²)

**Convergence analysis:** The s ≤ 4 budget becomes inadequate at d ≥ 5. Convergence correction (from s ≤ 3 vs s ≤ 4 vs s ≤ 5 comparison at d = 2,3,4) suggests the converged base is ~2.3, slightly higher than the raw 2.24. The s ≤ 5 growth rates at d = 2,3 (2.27, 2.38) ACCELERATE, supporting this.

**Conservative bound:** N_eff ≥ 2.0^d for d ≤ 6 (holds even at s ≤ 3).

## The Rényi Transition Theorem

**Theorem (computational).** The growth type of N_α transitions from exponential to polynomial at α* ∈ (1, 1.5):

- For α ≤ 1: N_α(d) grows exponentially (constant growth ratios, exp R² ≈ 1)
- For α ≥ 2: N_α(d) grows polynomially (decelerating growth ratios, poly R² > exp R²)

**Interpretation:** Shannon entropy is the last Rényi order with exponential growth. This means:
- Worst-case arguments (using N_∞ = σ) access only polynomial structural information
- Collision-type arguments (using N_2) access only polynomial structural information
- Average-case/distributional arguments (using N_1 = N_eff) access exponential structural information

## Connection to Circuit Complexity

At d = c · log(n):
- σ ~ (c · log n)^{0.62} — **polylogarithmic** in n
- N_2 ~ (c · log n)^{1.3} — **polylogarithmic** in n
- N_eff ~ n^{1.0c to 1.2c} — **polynomial** in n

This is a qualitative upgrade: N_eff gives polynomial parameters where σ gives polylogarithmic.

### Known routes and their limitations

1. **Per-edge proof complexity** (NS degree, PC degree, Resolution width):
   These use the per-edge constraint structure, governed by σ (worst case).
   Result: NS degree = 2, Resolution width = clause width. **Blocked by 2-CSP structure.**
   N_eff cannot help here because proof complexity measures use worst-case per-constraint parameters.

2. **Communication complexity of OD(T)**:
   The function OD: {0,1}^N → {0,1} has CC(OD) ≤ N trivially.
   Even with N_eff, the CC is at most N (read the whole input).
   For circuits: SIZE(f) ≥ CC(f) for any partition, but CC ≤ N gives only a linear bound.
   We need SIZE ≥ N^{1+ε} (superlinear). **CC approach cannot give superlinear bounds directly.**

3. **Williams algorithmic method**:
   N_eff gives profile entropy ~ d per node, enabling faster structural CSP solving.
   But the structural CSP ≠ Circuit-SAT, so no bridging reduction exists.
   **No known way to convert structural CSP speedup to Circuit-SAT speedup.**

### The open direction: distributional characterization

**Conjecture (N_eff Diversity Principle).** For a random truth table T on n variables, the structural CSP Γ(T, d, s) at d = c · log(n) has the following property:

The set of sub-function types {g_c : c ∈ sub-cubes} spans Ω(N_eff(d)) effective function types, and the compatibility constraints between these types form an Ω(N_eff(d))-chromatic constraint graph.

**Why this matters:** If the constraint graph is N_eff-chromatic, any circuit computing OD must distinguish between N_eff^Ω(1) type configurations. The question is whether this chromatic diversity forces superlinear circuit size.

**The gap:** Converting chromatic diversity to circuit size requires a technique that doesn't use CC, proof complexity, or algorithmic arguments directly. The most promising candidate is a **distributional KW variant** or **entropy-based feasible interpolation**.

## Summary

N_eff is a genuine exponential structural parameter (base ≥ 2.0, central estimate ~2.24) that captures the distributional richness of the formula complexity landscape. The Rényi spectrum reveals that this richness exists ONLY at the Shannon entropy level (α ≤ 1), not at collision (α = 2) or min-entropy (α = ∞). This constrains the proof technique to be distributional/average-case in nature.

The specific technical gap: no existing theorem converts distributional structural diversity (Shannon entropy) into circuit size lower bounds. Closing this gap requires either:
1. A distributional Karchmer-Wigderson theorem
2. A Shannon-entropy-based feasible interpolation result
3. A connection between chromatic diversity and circuit depth/size
4. An entirely new technique leveraging the average-case structure of OD
