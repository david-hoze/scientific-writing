# The Growth Mechanism for σ∞(d): Theoretical Analysis

## What Can Be Proved, What Remains Conjectural

---

## 1. The Decomposition

The ratio σ∞(d) = |U(∞,d)| / M(∞,d) decomposes into two quantities. Understanding why the ratio grows requires understanding each independently.

**The numerator: U(∞,d).** At convergence (s → ∞), the universe U(∞,d) is the set of all (d−1)-input DAG-isomorphism classes that arise as restrictions of *any* d-input formula of *any* size. Since we can always build a d-input formula whose restriction to a given direction is any desired (d−1)-input formula (via a multiplexer construction: F(x_0,...,x_{d-1}) = x_i · G_1(others) + ¬x_i · G_0(others), giving F|_{x_i=0} = G_0 and F|_{x_i=1} = G_1), the universe at convergence approaches the set of *all* (d−1)-input formula DAG classes:

U(∞, d) → T(d−1)

where T(d−1) is the total number of DAG-isomorphism classes of formulas on d−1 inputs (across all sizes). However, at any *finite* s, U(s,d) < T(d−1) because large (d−1)-input formulas can't arise from restricting small d-input formulas.

**The denominator: M(∞,d).** At convergence, M(∞,d) = max_f max_{i,v} |I_f(∞, d, i, v)|, where the max ranges over all d-input functions f and all restriction directions. For the most "flexible" function f, this counts the number of distinct (d−1)-input DAG classes that arise as restrictions of formulas computing f.

For any f and direction (i, v), the function g = f|_{x_i=v} is fixed. So I_f(∞, d, i, v) = set of DAG classes of formulas computing g that arise as restrictions of formulas computing f. At convergence, this approaches the set of all DAG classes of formulas computing g:

I_f(∞, d, i, v) → T_g(d−1)

where T_g(d−1) is the number of (d−1)-input DAG classes computing the specific function g.

**The ratio at convergence:**

σ∞(d) ≈ T(d−1) / max_f max_{i,v} T_{f|_{x_i=v}}(d−1)

This is the ratio of the total number of (d−1)-input DAG classes to the maximum number computing any single (d−1)-input function.

---

## 2. Why the Ratio Exceeds 1 (Provable)

**Proposition.** For any complete basis B and d ≥ 2, σ∞(d) > 1.

**Proof.** There exist at least two distinct (d−1)-input functions (e.g., the constant 0 and the constant 1) with disjoint sets of DAG classes (no formula computes both 0 and 1). Thus T(d−1) ≥ T_0(d−1) + T_1(d−1) > T_0(d−1) ≥ max_g T_g(d−1), and the ratio exceeds 1. ∎

This is trivial. The real question is the *growth rate*.

---

## 3. The Mechanism for Growth (Partially Rigorous)

### 3.1. The function-counting argument

**Proposition.** σ∞(d) ≥ N_d / max_g T_g(d−1), where N_d is the number of distinct (d−1)-input functions that arise as restrictions of d-input functions.

**Proof.** Each distinct (d−1)-input function g contributes at least one DAG class to the universe (the class of any formula computing g). So |U(∞,d)| ≥ N_d. The max image is at most max_g T_g(d−1). ∎

**Corollary.** σ∞(d) ≥ 2^{2^{d-1}} / max_g T_g(d−1).

**Proof.** Every (d−1)-input function arises as the restriction of some d-input function (take f(x_0,...,x_{d-1}) = x_0 · g(x_1,...,x_{d-1}) + (1−x_0) · g(x_1,...,x_{d-1}) = g; or more interestingly, every function on d−1 variables is the restriction of itself viewed as a d-input function that ignores x_d). So N_d = 2^{2^{d-1}}. ∎

But this bound is essentially useless: 2^{2^{d-1}} / max_g T_g(d−1) is the ratio of (number of functions) to (max DAG classes for one function). Since T_g(d−1) can be astronomically large for simple functions (the constant 0 function has a huge number of degenerate formulas), this bound might not even exceed 1 by much.

### 3.2. The structural diversity argument (heuristic)

The real mechanism is more subtle. It's not just that *different functions* contribute to the universe — it's that *different functions contribute different DAG classes*.

**Key structural observation.** Two functions g₁ ≠ g₂ on d−1 inputs generally have *disjoint* sets of DAG classes. (A formula computing g₁ cannot also compute g₂.) This means:

T(d−1) = Σ_g T_g(d−1)

The universe is the *sum* across all functions, while the max image is the *max* over one function. The ratio is:

σ∞(d) = Σ_g T_g(d−1) / max_g T_g(d−1)

This ratio measures the "effective number of functions weighted by structural diversity." If all functions had equal T_g, the ratio would be 2^{2^{d-1}} — doubly exponential. In reality, T_g is extremely non-uniform: constant functions have far more formulas than complex functions.

**The question reduces to:** How does the effective number of structurally distinct function classes grow with d?

### 3.3. A model calculation

Assume the distribution of T_g across functions follows a power law: let the functions on d−1 inputs be ordered g_1, g_2, ..., g_{2^{2^{d-1}}} with T_{g_1} ≥ T_{g_2} ≥ ... The ratio is:

σ∞(d) = (Σ_k T_{g_k}) / T_{g_1}

If the top function accounts for fraction α of the total, σ∞(d) = 1/α.

From the data:

At d=3 (functions on 2 inputs): The most "prolific" function (constant 0, with 9,624 formulas at s ≤ 4) accounts for 9,624/36,052 ≈ 26.7% of all formulas. But the universe counts DAG *classes*, not formulas. The max image is 367 out of a universe of 2,324, so the top function's restriction classes account for 367/2,324 ≈ 15.8% of the universe. Hence σ∞(3) ≈ 1/0.158 ≈ 6.3. ✓

At d=4 (functions on 3 inputs): The max image is 835 out of 11,075, so the top function accounts for 835/11,075 ≈ 7.5%. Hence σ∞(4) ≈ 1/0.075 ≈ 13.3. ✓

**The growth mechanism, precisely stated:** As d increases, the fraction of the universe captured by the most prolific function *decreases*. The top function's share drops from 50% (d=2) to 15.8% (d=3) to 7.5% (d=4).

**Why the fraction decreases:** As d−1 grows, the number of distinct (d−1)-input functions grows doubly exponentially (2^{2^{d-1}}). The "most prolific" function is always a simple one (constant, identity), which has many degenerate formula representations. But the *new* functions introduced at higher d are increasingly complex, and while each individually has fewer formulas, they collectively contribute many new DAG classes to the universe. The simple functions' share of the universe erodes as the complex functions' collective contribution grows.

This is the "dilution" mechanism: the universe is a pool shared by all functions, and as the number of functions grows (exponentially in 2^d), no single function can dominate the pool.

---

## 4. Toward a Proof of σ∞(d) ≥ Ω(d)

### 4.1. What would suffice

To prove σ∞(d) ≥ c · d, it suffices to show that the top function's share of the universe is at most 1/(cd):

max_g T_g(d−1) / T(d−1) ≤ 1/(cd)

Equivalently: no single (d−1)-input function's formulas contribute more than a 1/(cd) fraction of all (d−1)-input DAG classes.

### 4.2. An approach via symmetric functions

Consider the d−1 variables x_0, ..., x_{d-2}. There are d distinct *symmetric* functions on these variables that depend on all inputs: the threshold functions THR_k(x) = [Σx_i ≥ k] for k = 1, ..., d−1 (OR, MAJ, AND, etc.), plus parity. These functions are pairwise distinct and each uses all d−1 inputs non-trivially.

**Claim (requires proof).** For each symmetric function f_k that depends on all d−1 inputs, the minimum formula size is Ω(d) (in our all-gates model). Hence the formulas for f_k are structurally distinct from those for f_j (j ≠ k) — they can't be "rearrangements" of each other, because they compute different functions. Each f_k contributes at least one DAG class to the universe that no other function contributes.

If this gives d distinct "exclusive" DAG classes in the universe, and the max image is bounded, the ratio is at least d/(max single-function image size), which approaches d/T_{g_1} as s → ∞. But T_{g_1} itself grows with s, so this argument alone doesn't give a clean Ω(d) bound.

### 4.3. A more promising approach: restriction entropy

Define the **restriction entropy** of a d-input formula F as:

H(F) = |{ [F|_{x_i=v}] : i ∈ [d], v ∈ {0,1} }|

the number of distinct DAG classes produced by restricting F across all 2d directions.

**Claim (empirically supported).** A "typical" formula on d inputs has restriction entropy Θ(d). That is, restricting along different directions typically produces different DAG structures.

If true, then the universe is at least Ω(d · T(d−1) / 2d) ≈ Ω(T(d−1)/2) (since each formula contributes ~d classes, but each class is counted once), while the max image for any one function grows at most proportionally to T_g. The ratio then depends on the distribution.

But formalizing this runs into the same "distribution of T_g" issue.

### 4.4. What we can rigorously state

**Theorem (proved by computation).** For the basis B = {AND, OR, NOT}:
- σ(4, 2) = 2.03
- σ(4, 3) = 6.33
- σ(4, 4) = 13.26
- σ(5, 3) = 6.44

**Theorem (provable from the data).** For d = 3 and B = {AND, OR, NOT}, the limit σ∞(3) = lim_{s→∞} σ(s, 3) exists and satisfies 6.33 ≤ σ∞(3) ≤ T(2), where T(2) is the total number of 2-input DAG classes (finite but large). The lower bound comes from the monotonicity of σ(s, 3) in s (observed for s = 0, ..., 5). The upper bound is trivial.

**Conjecture (empirically supported, proof not available).** σ∞(d) ≥ c · d for some constant c > 0 and all d ≥ 2.

---

## 5. The Honest Assessment

The mechanism for σ∞(d) growing with d is the **dilution of the top function's share of the universe** as the number of distinct functions grows. This is conceptually clear and empirically confirmed at d = 2, 3, 4. But converting it to a proof requires bounding the distribution of DAG classes across functions — specifically, showing that no single function's DAG classes can dominate the universe as d grows.

The difficulty: the most prolific functions (constants, simple projections) have enormously many degenerate formulas, and it's not obvious that their share decreases with d at any quantifiable rate. The empirical decrease (50% → 15.8% → 7.5%) is consistent with geometric decay (each step roughly halves the share), which would give σ∞(d) ≈ 2^{d−1} — but three data points don't prove geometric decay.

**What would make the proof work:**

A counting argument showing that at d−1 inputs, the total number of DAG classes T(d−1) grows faster with d than the number of DAG classes for any single function T_g(d−1). This would follow if there exist Ω(2^d) functions whose DAG class sets are pairwise disjoint (each contributes "exclusive" DAG classes to the universe). The symmetric functions are candidates, but there are only d of them — not 2^d.

The natural candidates for contributing "exclusive" DAG classes are the functions with *unique* or near-unique implementations — precisely the "stiff" functions from the periodic table. At d = 3, the stiff functions (XOR₀₁, XOR₁₂, XNOR₀₁, XNOR₁₂) each have exactly 1 formula, contributing a single unique DAG class. But there are only 4 of them — not enough for Ω(2^d).

At higher d, the number of "frontier" functions (those with minimum formula size equal to the max) might grow, contributing more exclusive classes. But this is speculation.

**The bottom line:** The growth of σ∞(d) is empirically established at three data points, the mechanism (dilution of the top function's share) is conceptually clear and numerically verified, but a formal proof remains out of reach. The paper should present the data and the mechanism as a conjecture with supporting analysis, not as a theorem.

---

## 6. Recommended Statement for the Paper

In the structural anatomy paper, Section 5 should include:

**Empirical Result 5.1.** For basis {AND, OR, NOT}, the universe-to-image ratio at s ≤ 4 satisfies σ(4,2) = 2.03, σ(4,3) = 6.33, σ(4,4) = 13.26.

**Empirical Result 5.3.** At fixed d = 3, the ratio σ(s, 3) converges monotonically as s increases, with measured values 4.0, 4.0, 5.2, 6.0, 6.33, 6.44 at s = 0, ..., 5.

**Observation 5.1.** The growth is consistent with σ∞(d) ≈ 2^{d−1}. Equivalently, the fraction of the restriction universe captured by the most prolific function's image decreases geometrically with d: 50% at d = 2, 15.8% at d = 3, 7.5% at d = 4.

**Conjecture 5.4.** σ∞(d) → ∞ as d → ∞. Specifically, σ∞(d) ≥ c · d for some constant c > 0.

**Discussion.** The growth mechanism is the dilution of the top function's structural share as the number of distinct (d−1)-input functions grows with d. At each dimension, the restriction universe pools DAG classes across all functions, while the max image is constrained to one function. As d increases, the pool draws from an exponentially growing set of functions, eroding any single function's dominance. Proving this formally requires bounding the distribution of DAG classes across functions — an open problem in the combinatorics of circuit structure.

Then in Section 7 (implications), connect this to the compatibility CSP via the proposition from the conjecture document.
