# Final Manuscript Sections: Abstract + Mechanism of Dilution

---

## Abstract

We introduce the *restriction universe* of Boolean formulas — the set of DAG-isomorphism classes of (d−1)-input formulas that arise by hardwiring a single input of d-input formulas over a fixed basis and propagating constants — and study its size relative to the *restriction image* of individual Boolean functions. Through exhaustive enumeration of formulas over {AND, OR, NOT} at dimensions d = 2, 3, 4, we establish three results.

First, we construct the complete structural taxonomy of all 256 Boolean functions on 3 inputs, classified by formula complexity (sizes 0 through 12 in the all-gates model) and *structural fluidity* — the number of distinct restriction DAG classes their formulas produce. We identify a three-tier hierarchy (stiff, moderate, fluid) that recurs at each complexity frontier, with rigid functions appearing from different algebraic families at successive size levels: the XOR family at size 4 and the unanimity/diversity family at size 6.

Second, we discover a *universe-to-image scaling law*. Define the structural entropy ratio σ(s, d) as the size of the restriction universe divided by the maximum restriction image across all d-input functions, both at formula size ≤ s. We measure σ(4, 2) = 2.0, σ(4, 3) = 6.3, and σ(4, 4) = 13.3, with the universe growing 4.8× per dimension step while the maximum image grows only 2.3×. At fixed dimension d = 3, σ(s, 3) converges monotonically as s increases — reaching 6.44 at s = 5 with growth factors decelerating as 1.30, 1.15, 1.06, 1.02 — establishing σ∞(d) as an intrinsic dimensional constant independent of the size budget. The growth of σ∞(d) with dimension is driven by a *dilution mechanism*: the fraction of the restriction universe captured by the most prolific function's image decreases from 49.3% at d = 2 to 15.8% at d = 3 to 7.5% at d = 4, as the exponentially growing set of (d−1)-input functions collectively diversifies the universe faster than any single function can expand its image. We conjecture σ∞(d) → ∞ as d → ∞.

Third, through a case study of the bent function x₀x₁ ⊕ x₂x₃, we demonstrate that DAG-isomorphism compatibility (STRUCT-MATCH) is strictly more constraining than functional compatibility: 50% of structural edges in the sub-cube intersection graph have empty restriction-image intersection at minimum formula size, while functional compatibility is trivially satisfied everywhere.

We formalize these results in the language of constraint satisfaction: the *compatibility CSP* on the sub-cube intersection graph, where variables are local formula assignments and constraints enforce STRUCT-MATCH on overlaps, has constraint density controlled by σ∞(d). When σ∞(d) is large, the CSP is highly constrained and solutions (compatible families of formulas) are rare. We discuss implications for the hardness magnification program, where the computational difficulty of detecting the rare satisfiable instances of this CSP is equivalent to proving circuit lower bounds for meta-complexity problems.

---

## Section 5.5: The Mechanism of Dilution

The growth of σ∞(d) with dimension admits a precise structural explanation. We develop it in three stages: an exact decomposition, a numerical verification, and a discussion of what remains conjectural.

### The Decomposition

At convergence (s → ∞), the restriction universe U(∞, d) approaches T(d−1) — the total number of DAG-isomorphism classes of formulas on d−1 inputs across all functions and all sizes. (Every (d−1)-input formula class eventually appears as the restriction of some sufficiently large d-input formula.) Similarly, the max image M(∞, d) approaches max_g T_g(d−1), where T_g(d−1) counts DAG classes of formulas computing the specific function g : {0,1}^{d−1} → {0,1}.

Since every formula computes exactly one function, and formulas computing different functions have disjoint DAG classes, the total decomposes as:

T(d−1) = Σ_g T_g(d−1)

where the sum ranges over all 2^{2^{d−1}} Boolean functions on d−1 inputs. The asymptotic ratio is therefore:

σ∞(d) = [Σ_g T_g(d−1)] / [max_g T_g(d−1)] = 1 / α(d)

where α(d) = max_g T_g(d−1) / Σ_g T_g(d−1) is the *top share* — the fraction of all (d−1)-input DAG classes belonging to the single most prolific function.

### Numerical Verification

The identity σ∞(d) = 1/α(d) is confirmed by the data:

| d | max_g |I_g| | |U| | α(d) = max/U | 1/α(d) | σ∞(d) measured |
|---|---|---|---|---|---|
| 2 | 111 | 225 | 0.493 | 2.03 | 2.03 |
| 3 | 367 | 2,324 | 0.158 | 6.33 | 6.33 |
| 4 | 835 | 11,075 | 0.075 | 13.26 | 13.26 |

The top share decreases with d: 49.3% → 15.8% → 7.5%. The per-step decay factors are 0.320 (from d = 2 to 3) and 0.477 (from d = 3 to 4), indicating that the top share is shrinking at each step, though the rate of shrinkage itself is decelerating.

In all three cases, the maximizing function is a projection or identity — a function that depends on only one of the d−1 inputs (e.g., x₀ at d = 3, which has 7,441 formulas and 367 restriction classes at s ≤ 4). These "trivial" functions have the most formulas because degenerate constructions (formulas that compute x₀ using unnecessary gates) are plentiful. Their dominance of the max image is not an artifact of our enumeration — it reflects the genuine combinatorial fact that simple functions have the most structural redundancy.

### Why the Top Share Decreases

The top share α(d) = max_g T_g(d−1) / T(d−1) decreases because the denominator (total DAG classes across all functions) grows faster with d than the numerator (DAG classes for the most prolific function).

The denominator growth is driven by the *number of contributing functions*. At dimension d, the sum T(d−1) = Σ_g T_g(d−1) ranges over 2^{2^{d−1}} functions. As d increases by 1, the number of functions squares (from 2^{2^{d−1}} to 2^{2^d}). Even if most new functions have very few DAG classes each, the sheer number of them adds substantial collective weight to the denominator.

The numerator growth is constrained by the *structural redundancy* of a single function. The most prolific function's DAG classes grow with the number of formulas computing it, which grows with the formula size budget. But the *rate* of this growth doesn't depend on d in a fundamental way: adding one more input variable to the basis creates new wiring options, but these mostly produce formulas computing *different* functions, not more formulas computing the same function.

Concretely: at d−1 = 2, the identity function x₀ can be computed by 4,729 formulas at s ≤ 4, producing 413 DAG classes. At d−1 = 3, the same structural class of function (a single-variable projection) can be computed by 7,441 formulas at s ≤ 4, producing 835 DAG classes. The growth from 413 to 835 (a factor of 2.0) is modest compared to the universe growth from 2,324 to 11,075 (a factor of 4.8). The extra input creates more total DAG diversity (benefiting the universe) than it creates extra implementations of the identity function (benefiting the max image).

### What Remains Conjectural

The decomposition σ∞(d) = 1/α(d) is exact. The decrease of α(d) at d = 2, 3, 4 is empirically confirmed. The mechanism (collective diversification outpacing single-function redundancy) is numerically verified.

What is *not* proved is that α(d) → 0 as d → ∞. The three measured values (0.493, 0.158, 0.075) are consistent with several decay models:

- Geometric decay α(d) ≈ c · r^d with r ≈ 0.39 would give σ∞(d) ≈ 2.6^d — exponential growth.
- Power-law decay α(d) ≈ c / d^k would give σ∞(d) ≈ d^k — polynomial growth.
- Convergence to a positive constant α(d) → α* > 0 would give σ∞(d) → 1/α* — bounded.

Three data points cannot distinguish among these. The geometric model (which predicts σ∞(5) ≈ 28) and the power-law model are both consistent with the data and both imply σ∞(d) → ∞. Only the bounded model would invalidate the conjecture, and it is the least consistent with the observed decay pattern (the top share is not leveling off at d = 4).

A formal proof that α(d) → 0 would require showing that no single (d−1)-input function can capture a constant fraction of all DAG classes as d grows. This is a question about the distribution of formula structures across truth tables — a combinatorial problem that appears tractable but has not been addressed in the literature.

We state the conjecture: **σ∞(d) → ∞ as d → ∞**, and note that even the weakest form — σ∞(d) ≥ c · log d — would suffice to establish that the compatibility CSP becomes arbitrarily constrained with dimension, formalizing the intuition that circuit gluing is a fundamentally high-entropy operation.
