# Universe-to-Image Scaling: What the Data Shows

## The Complete Empirical Picture (s ≤ 4, d = 2, 3, 4)

All measurements at fixed size budget s ≤ 4, basis {AND, OR, NOT}.

### Raw Data

| d | Total circuits | Universe |U| | Max image max|I| | Ratio U/max|I| |
|---|---|---|---|---|
| 2 | 36,052 | 225 | 111 | **2.0** |
| 3 | 93,315 | 2,324 | 367 | **6.3** |
| 4 | 207,078 | 11,075 | 835 | **13.3** |

### Growth Rates Per Dimension Step

| Transition | Universe growth | Max image growth | Ratio growth |
|---|---|---|---|
| d=2 → d=3 | 10.3× | 3.3× | 3.2× |
| d=3 → d=4 | 4.8× | 2.3× | 2.1× |

### Universe Growth with Size at Fixed d = 3

| Size ≤ s | Universe |U| | Growth from previous |
|---|---|---|
| 0 | 4 | — |
| 1 | 12 | 3.0× |
| 2 | 52 | 4.3× |
| 3 | 324 | 6.2× |
| 4 | 2,324 | 7.2× |

---

## What the Data Establishes

**Fact 1: The universe-to-image ratio increases with d at fixed s.** At s ≤ 4, the ratio goes 2.0 → 6.3 → 13.3 across d = 2, 3, 4. The universe consistently grows faster than the max image. This is three data points showing a clear monotone trend.

**Fact 2: Both universe and image growth rates are decelerating.** Universe growth per dimension step went from 10.3× to 4.8×; max image growth went from 3.3× to 2.3×. The ratio growth decelerated from 3.2× to 2.1×. The trend is real but not explosive — it's steady divergence, not exponential separation.

**Fact 3: Universe growth accelerates with s at fixed d.** At d = 3, each additional unit of s multiplies the universe by an increasing factor (3×, 4.3×, 6.2×, 7.2×). The universe is growing super-linearly with s. This is because larger circuits have more internal structure, and hardwiring inputs + constant propagation produces a richer variety of reduced DAGs.

**Fact 4: The frontier tier (highest min-size functions) has coverage ~0.01–0.06% of the universe.** At d = 4, size-4 functions have max image 1–6 out of a universe of 11,075. Their restriction images are vanishingly sparse in the universe.

---

## What the Data Does NOT Establish

**Gap 1: The ratio at the operationally relevant size budget.** We measured U/max|I| at fixed s = 4. But for the Propagation Conjecture applied to high-complexity truth tables, the relevant size budget depends on d: as d grows, sub-functions require larger circuits (Shannon: circuit complexity of a random d-bit function is Θ(2^d / d)). The Propagation Conjecture needs the ratio at s ≈ 2^d / d, not at s = 4.

At s = 4 and d = 4, we see 886 out of 65,536 possible 4-bit functions covered (1.4%). At the Shannon-relevant s ≈ 2^4/4 = 4, this is coincidentally the right ballpark for d = 4. But at d = 5, the relevant s would be ≈ 6-7, and at d = O(log n), the relevant s would be ≈ poly(n)/log(poly(n)). We have no data on the ratio at these larger size budgets.

**Gap 2: Whether the ratio stays above 1.** The ratio could converge to a constant, or even decrease, at larger d. Three data points (2.0, 6.3, 13.3) with decelerating growth are consistent with convergence to a finite limit. The current growth rate of ~2× per dimension step, if it persisted, would give a ratio of ~2^d at dimension d — more than enough for the conjecture. But if the growth rate continues decelerating (2.1× then 1.5× then 1.2× ...), the ratio could saturate.

**Gap 3: The relationship between misalignment rate and circuit lower bounds.** Even if the universe-to-image ratio grows with d (guaranteeing high misalignment), converting this to "no small circuit computes OD" requires an additional argument. Misalignment tells us that compatible families are hard to find (or don't exist). But OD asks whether H¹ is nontrivial — which requires both "compatible families exist" (local sections are present) AND "no global section exists" (the compatible family doesn't extend). High misalignment could mean either (a) compatible families don't exist at all (H¹ is trivially undefined, not nontrivial), or (b) compatible families exist but are extremely constrained. Only case (b) gives nontrivial H¹.

**Gap 4: Independence of edges in the CSP.** The informal argument "misalignment probability p per edge, so global probability ≈ p^{|E|}" treats edges as independent. In reality, satisfying STRUCT-MATCH at one edge constrains the circuit choices at both endpoints, which affects all adjacent edges. The actual global satisfiability probability depends on the CSP's constraint structure, not just the per-edge probability. Expander amplification helps (it prevents isolated satisfiable clusters), but the quantitative conversion from per-edge misalignment to global infeasibility requires careful analysis — likely via the Lovász Local Lemma or Shearer's inequality in the inverse direction.

---

## What Would Need to Be Proved for the Sparse Section Theorem

### Version 1 (Strongest — probably too hard)

**Theorem (Target).** For d = c·log n with sufficiently large c, and s = poly(n), the universe-to-max-image ratio satisfies:

U(s, d-1) / max_f |I_f(s, d, d-1)| ≥ d^{Ω(1)}

This would establish that the ratio grows polynomially with d (at the relevant size budget), which combined with the expansion of G_{n,d} would give per-edge misalignment probability ≤ 1/d^{Ω(1)} = 1/log^{Ω(1)} n — a slowly vanishing probability on each of N^{1+o(1)} edges.

### Version 2 (Weaker but sufficient)

**Theorem (Alternative Target).** For any constant ε > 0, there exists a constant c such that for d = c·log n and s = n^ε, the average coverage fraction

E_f [ |I_f(s, d, d-1)| / |U(s, d-1)| ] ≤ 1/2

where the expectation is over a random d-bit function f. This says the average function covers less than half the universe, which combined with birthday-paradox reasoning gives constant-probability misalignment per edge.

### What the proof would need

1. **A lower bound on universe growth.** The universe U(s, d) counts DAG-isomorphism classes of d-input circuits that arise as restrictions of (d+1)-input circuits of size ≤ s. A lower bound requires showing that different (d+1)-input circuits, when restricted, produce structurally distinct d-input circuits. The key combinatorial fact: if a (d+1)-input circuit has a gate that depends on the restricted input (directly or through a chain), constant propagation changes the DAG topology. Different gate arrangements produce different propagation outcomes. Bounding the number of distinct outcomes from below is a counting argument on DAG topologies.

2. **An upper bound on max image growth.** The max image max_f |I_f| counts the maximum number of distinct restrictions that any single d-bit function's circuits can produce. This is bounded by the number of circuits computing f, which Shannon's counting argument bounds: at most 2^{O(s log s)} circuits of size ≤ s compute any given function. But the restriction map is many-to-one (many circuits can restrict to the same DAG class), so the image is smaller. The question is by how much.

3. **The gap between 1 and 2.** The theorem needs universe growth to outpace image growth. The empirical data shows this at s = 4 for d = 2, 3, 4. The theoretical argument would need to identify which structural property of the restriction map creates this gap. The candidate: restriction (hardwiring + constant propagation) is a *lossy compression* that depends on the interaction between the restricted input and the circuit's internal structure. Different circuits interacting differently with the restricted input produce different compressed forms. The universe pools these across all functions, while the image is restricted to one function — and the Shannon counting argument limits how many circuits any one function has.

---

## Honest Assessment

The d = 2, 3, 4 data at s ≤ 4 is a genuine empirical discovery: the universe-to-image ratio increases with dimension, driven by the combinatorial explosion of DAG structures under restriction. This is the first quantitative evidence that STRUCT-MATCH misalignment is a *dimensional* phenomenon — it intensifies as the sub-cube dimension grows, not just as specific "stiff" functions appear.

But converting this to a theorem requires closing four gaps (listed above), each of which is a non-trivial mathematical problem. The most tractable is Gap 1 (extending to the relevant size budget), which could potentially be addressed by further computation at d = 3 with s = 5, 6, 7 to see how the ratio behaves at larger s. The hardest is Gap 3 (connecting misalignment to OD circuit complexity), which requires new ideas about how local constraint propagation in an expanding graph translates to global computational hardness.

The data supports pursuing this direction. It does not yet constitute a proof.
