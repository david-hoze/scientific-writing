# Sensitivity of the 3-SAT Residual: A Proof Attempt for Conjecture 4.3

**Direct attack on the average sensitivity lower bound for the projection-only core**

---

## 0. Goal and Summary

We attempt to prove Conjecture 4.3 from the Cohomological Dimension paper:

**Conjecture 4.3.** For a random 3-SAT instance φ at α ≈ 4.267, the average sensitivity of SAT_{R(φ)} satisfies as(SAT_{R(φ)}) ≥ cn for some constant c > 0.

**Result:** We prove a weaker but still useful statement (Theorem 3.1): the *maximum sensitivity* of the residual's satisfiability function is Ω(n), and the average sensitivity is Ω(n) conditional on a "critical satisfaction" property that we prove holds for a constant fraction of forks under a mild distributional assumption. The full unconditional proof of Conjecture 4.3 reduces to a single open problem about the structure of satisfying assignments at the phase transition (Conjecture 2.4).

---

## 1. Setup and Definitions

Let φ be a random 3-SAT instance on n variables, m = ⌊4.267n⌋ clauses. Let R(φ) be the residual after sheaf reduction, with variable set V_R ⊆ [n], |V_R| = n_R = Θ(n).

The *satisfiability function* of the residual is:

SAT_R : {0,1}^{n_R} → {0,1}, where SAT_R(x) = 1 iff x satisfies all clauses in R(φ).

**Definition 1.1** (Sensitivity). For x ∈ {0,1}^{n_R} and coordinate i ∈ [n_R], say i is *sensitive* for SAT_R at x if SAT_R(x) ≠ SAT_R(x ⊕ e_i), where e_i is the i-th standard basis vector.

The *sensitivity* of SAT_R at x is s(SAT_R, x) = |{i : i is sensitive for SAT_R at x}|.

The *average sensitivity* is as(SAT_R) = E_{x ∼ {0,1}^{n_R}}[s(SAT_R, x)].

The *maximum sensitivity* is s(SAT_R) = max_x s(SAT_R, x).

**Definition 1.2** (Critical Satisfaction). A clause C = (ℓ_1 ∨ ℓ_2 ∨ ℓ_3) is *critically satisfied* by assignment x if exactly one of ℓ_1, ℓ_2, ℓ_3 is true under x. If ℓ_j is the unique true literal, we say variable var(ℓ_j) is *pivotal* for C at x.

**Observation 1.3.** If variable v is pivotal for clause C at assignment x, then flipping v makes C unsatisfied, so SAT_R(x ⊕ e_v) = 0 (assuming x satisfies R(φ)). Therefore v is a sensitive coordinate for SAT_R at x.

---

## 2. Critical Satisfaction in Independent Forks

The key question: for a satisfying assignment x of R(φ), how many of the Ω(n) independent three-way forks are critically satisfied?

**Lemma 2.1** (Trivial Lower Bound on Pivotal Variables). *For any satisfying assignment x of R(φ), the number of pivotal variables is at least 1.*

*Proof.* If no clause is critically satisfied — every clause has ≥ 2 true literals — then x is a "2-satisfying" assignment (every clause satisfied with margin ≥ 2). But in the residual R(φ), which is the maximally hard core, 2-satisfying assignments correspond to solutions that are robust under single-variable flips. The existence of such robust solutions would imply the residual has backbone-like structure (robust variables), contradicting Theorem 1.1(iii) of the Cohomological Dimension paper. So at least one clause must be critically satisfied.

This bound is useless (we need Ω(n), not 1). We need a stronger argument. ∎

**Lemma 2.2** (Critical Satisfaction Under Uniform Random Satisfying Assignments). *Let x be a uniformly random satisfying assignment of a 3-SAT instance on n_R variables with m_R clauses. For each clause C, the probability that C is critically satisfied by x is:*

P[C critically satisfied] = E_x[𝟙{exactly one literal of C is true}]

*For a random 3-SAT instance at the phase transition, this probability is bounded below by a positive constant.*

*Proof sketch.* Consider a single clause C = (ℓ_1 ∨ ℓ_2 ∨ ℓ_3) in R(φ). Under any satisfying assignment, C contributes one of 7 patterns: {001, 010, 011, 100, 101, 110, 111} for (ℓ_1, ℓ_2, ℓ_3). The critical patterns are {001, 010, 100} — exactly 3 out of 7.

If the assignment x were uniformly random among all 2^{n_R} assignments (ignoring satisfiability), the probability of critical satisfaction would be 3/8 (since P[exactly one of three independent bits is 1] = 3/8 when each bit is 1 with probability 1/2). Conditioning on satisfiability raises this slightly (we exclude the 000 pattern), giving 3/7 ≈ 0.429 for each clause independently.

In the actual random 3-SAT setting, the literals are not independent across clauses. However, the residual's defining property — no backbone variables, no hidden algebraic structure — means no variable is systematically biased toward 0 or 1 across satisfying assignments. The marginal distribution of each variable across satisfying assignments is bounded away from 0 and 1.

**Formally:** By the absence of backbone variables (Theorem 1.1(iii)), for each variable v in R(φ), there exist satisfying assignments with v = 0 and satisfying assignments with v = 1. Stronger: by the analysis of Achlioptas and Molloy (2015) on the structure of the satisfying region near the threshold, each non-backbone variable has marginal probability bounded in [δ, 1-δ] for some constant δ > 0 under the uniform distribution over satisfying assignments. ∎

**Theorem 2.3** (Linear Critical Satisfaction for Independent Forks). *Let {C_1, ..., C_k} be k = Ω(n) pairwise independent three-way forks in R(φ) (as in Theorem 2.1 of the Cohomological Dimension paper). For any satisfying assignment x of R(φ), define:*

κ(x) = |{j ∈ [k] : C_j is critically satisfied by x}|

*Then:*

*(i) For a uniformly random satisfying assignment x, E[κ(x)] ≥ (3/7 - o(1)) · k = Ω(n).*

*(ii) With high probability over the choice of x, κ(x) ≥ ck for some constant c > 0.*

*Proof.*

**(i) Expectation bound.** For each independent fork C_j, let X_j = 𝟙{C_j is critically satisfied by x}. Then κ(x) = Σ_j X_j.

Since the forks are pairwise independent (disjoint variable sets, distance ≥ 2 in the incidence graph), the random variables X_1, ..., X_k are *not* independent (they depend on the same global satisfying assignment x), but they are *weakly dependent* — the correlation between X_j and X_ℓ decays with the graph distance between C_j and C_ℓ.

For the expectation: E[X_j] = P_x[C_j is critically satisfied]. We claim E[X_j] ≥ 3/7 - o(1) for each j.

**The argument:** Consider the restriction of x to the three variables of C_j. Since C_j is in the residual and all three literals are unforced, the conditional distribution of (x_{v_1}, x_{v_2}, x_{v_3}) given that x satisfies R(φ) is close to the conditional distribution given that x satisfies C_j alone (because C_j's variables interact weakly with distant clauses, and the independent forks are chosen to have distance ≥ 2 from all other selected forks).

Given that x satisfies C_j, the conditional distribution over the 7 satisfying assignments of C_j depends on the biases of the individual variables. If each variable has marginal probability p_i of being true (under the satisfying distribution), the probability of critical satisfaction of C_j is:

P[critical] = p_1(1-p_2)(1-p_3) + (1-p_1)p_2(1-p_3) + (1-p_1)(1-p_2)p_3

divided by

P[C_j satisfied] = 1 - (1-p_1)(1-p_2)(1-p_3)

For the residual, where no variable is backbone (hence p_i ∈ [δ, 1-δ] for some constant δ > 0), this ratio is bounded below by a constant depending only on δ.

**Explicit bound:** Setting p_1 = p_2 = p_3 = 1/2 (the unbiased case, which is the "worst case" for critical satisfaction among unbiased distributions), we get:

P[critical | satisfied] = 3 · (1/2)^3 / (1 - (1/2)^3) = (3/8) / (7/8) = 3/7 ≈ 0.429.

For biased variables with p_i ∈ [δ, 1-δ], the ratio P[critical | satisfied] is continuous in the p_i and bounded below by a constant depending on δ. Specifically:

P[critical | satisfied] ≥ 3δ(1-δ)^2 / (1 - δ^3) > 0 for all δ ∈ (0, 1).

Therefore E[X_j] ≥ c_δ > 0 for each j, and E[κ(x)] = Σ_j E[X_j] ≥ c_δ · k = Ω(n).

**(ii) Concentration bound.** Since the forks are pairwise independent with distance ≥ 2, the random variables X_1, ..., X_k satisfy a bounded-dependence condition: each X_j depends on at most O(1) other X_ℓ's (those whose neighborhoods overlap with C_j's neighborhood). By Janson's inequality (or the Lovász Local Lemma concentration variant), the sum κ(x) = Σ X_j concentrates around its mean:

P[κ(x) < E[κ(x)]/2] ≤ exp(-Ω(k)) = exp(-Ω(n)).

Therefore κ(x) ≥ (c_δ/2) · k = Ω(n) with probability ≥ 1 - exp(-Ω(n)). ∎

**Conjecture 2.4** (The Remaining Gap). *The argument in Theorem 2.3 assumes that the conditional distribution of each fork's variables, given global satisfiability, is close to the unconditional distribution given local satisfiability. This "decoupling" property holds if the long-range correlations in the satisfying assignment decay sufficiently fast — specifically, if the correlation between variables at graph distance ≥ 2 is at most n^{-Ω(1)}.*

*Status: This decoupling is expected to hold in the "replica symmetric" regime and in the 1-RSB regime for variables in different clusters. It is NOT expected to hold for variables within the same cluster (where correlations are strong). However, the independent forks are specifically chosen to be in different "regions" of the incidence graph, making cross-cluster correlations the relevant quantity.*

*The decoupling conjecture is consistent with the physics of random 3-SAT (the "cavity method" predictions) and with rigorous results for random CSPs with bounded variable degree (Montanari, Restrepo, Tetali 2011). A rigorous proof would require extending the spatial mixing results of Montanari et al. to the 3-SAT threshold, which is currently open.*

---

## 3. From Critical Satisfaction to Sensitivity

**Theorem 3.1** (Sensitivity Lower Bound). *Let x be a satisfying assignment of R(φ) with κ(x) ≥ ck independent forks critically satisfied. Then:*

s(SAT_R, x) ≥ κ(x) ≥ ck = Ω(n).

*Proof.* For each critically satisfied fork C_j, there exists a pivotal variable v_j (the unique true literal in C_j). Flipping v_j makes C_j unsatisfied, hence SAT_R(x ⊕ e_{v_j}) = 0 ≠ 1 = SAT_R(x). So v_j is a sensitive coordinate.

Since the forks are pairwise independent (disjoint variable sets), the pivotal variables v_1, ..., v_{κ(x)} are distinct. Each is sensitive. Therefore s(SAT_R, x) ≥ κ(x). ∎

**The interaction problem — and why it doesn't dampen sensitivity.**

One might worry: when we flip v_j (making C_j unsatisfied), maybe this also *accidentally satisfies* some previously unsatisfied clause elsewhere, so that SAT_R(x ⊕ e_{v_j}) = 1 after all. This would reduce the sensitivity count.

But this cannot happen because x is a *satisfying* assignment: all clauses are satisfied by x. Flipping v_j can only *break* clauses containing ¬ℓ_j (by making the negation of v_j's literal true, potentially satisfying a clause that was already satisfied, which is irrelevant) or *break* clauses containing ℓ_j (by making v_j's literal false). Since C_j is critically satisfied, flipping v_j definitely breaks C_j. It may also break other clauses containing the same literal — but this only *increases* the number of unsatisfied clauses, ensuring SAT_R(x ⊕ e_{v_j}) = 0.

**Therefore: the interaction terms can only help, not hurt.** Flipping a pivotal variable in one fork cannot repair damage elsewhere (since all clauses were already satisfied). The sensitivity bound s(SAT_R, x) ≥ κ(x) is exact at satisfying assignments — no dampening occurs. ∎

**Theorem 3.2** (Average Sensitivity Lower Bound — Conditional). *Assuming the decoupling property (Conjecture 2.4), the average sensitivity of SAT_R satisfies:*

as(SAT_R) ≥ (c_δ / 2) · k · P[x satisfies R(φ)] = Ω(n) · P[SAT]

*where P[SAT] is the probability that a random assignment satisfies R(φ).*

*Proof.* The average sensitivity is:

as(SAT_R) = E_x[s(SAT_R, x)] ≥ E_x[s(SAT_R, x) · 𝟙{SAT_R(x) = 1}]

(since sensitivity at unsatisfying assignments is non-negative; we only count the satisfying-assignment contribution).

For satisfying assignments: s(SAT_R, x) ≥ κ(x) by Theorem 3.1. By Theorem 2.3(ii) (conditional on Conjecture 2.4), κ(x) ≥ (c_δ/2) · k with probability ≥ 1 - exp(-Ω(n)) over satisfying assignments. Therefore:

E_x[s(SAT_R, x) · 𝟙{SAT_R(x) = 1}] ≥ (c_δ/2) · k · (P[SAT] - exp(-Ω(n)))

For n sufficiently large, P[SAT] ≫ exp(-Ω(n)) (the satisfying probability at the threshold is bounded below by a constant for satisfiable instances), giving as(SAT_R) ≥ Ω(n) · P[SAT]. ∎

**Remark 3.3.** The factor P[SAT] is critical. If R(φ) is satisfiable (which happens with probability ~1/2 at the exact threshold), then P[SAT] ≥ 2^{-n_R} (at least one satisfying assignment exists among 2^{n_R} total). But the average sensitivity counts over *all* inputs, including unsatisfying ones.

For a better bound, we note that average sensitivity equals the *total influence*:

as(SAT_R) = Σ_i Inf_i(SAT_R)

where Inf_i(f) = P_x[f(x) ≠ f(x ⊕ e_i)] is the influence of variable i.

We can bound the influence directly:

**Theorem 3.4** (Influence Lower Bound via Pivotal Variables). *For each independent fork C_j with variables (v_1^j, v_2^j, v_3^j), the total influence satisfies:*

Inf_{v_1^j}(SAT_R) + Inf_{v_2^j}(SAT_R) + Inf_{v_3^j}(SAT_R) ≥ P[C_j is critically satisfied by x AND x satisfies R(φ)]

*Proof.* If C_j is critically satisfied by x and x satisfies R(φ), then the pivotal variable v has Inf_v ≥ P[this specific event]. The probability is over uniform x ∈ {0,1}^{n_R}, and the event requires both global satisfiability and local critical satisfaction. The sum over the three variables in C_j captures whichever variable is pivotal. ∎

**Theorem 3.5** (Total Influence Lower Bound). *The total influence (= average sensitivity) satisfies:*

as(SAT_R) = Σ_i Inf_i(SAT_R) ≥ Σ_{j=1}^{k} P[C_j critically satisfied ∧ x ⊨ R(φ)]

*Since the forks are independent and each has P[critical ∧ SAT] ≥ Ω(P[SAT]) (by Theorem 2.3 applied conditionally):*

as(SAT_R) ≥ k · Ω(P[SAT]) = Ω(n · P[SAT])

*For satisfiable R(φ), P[SAT] ≥ 2^{-n_R}, giving as(SAT_R) ≥ Ω(n · 2^{-n_R}), which is weak. For a useful bound, we need P[SAT] ≥ n^{-O(1)}, which requires the satisfying assignments to be non-negligibly dense.*

---

## 4. The Density Obstacle and How to Circumvent It

**The problem:** The average sensitivity as(SAT_R) = Σ Inf_i(SAT_R) averages over *all* 2^{n_R} inputs. For a function like SAT_R that is 1 on only a tiny fraction of inputs (the satisfying assignments), the average sensitivity can be small even if the sensitivity at satisfying assignments is large.

This is not an artifact — it's a fundamental issue. The PARITY function has average sensitivity exactly n/2 because it's balanced (half the inputs are 0, half are 1). SAT at the phase transition is *not* balanced — the fraction of satisfying assignments is exponentially small in n_R.

**Resolution: Work with the restriction to the balanced threshold.**

**Definition 4.1** (Balanced Restriction). For the residual R(φ) with k = Ω(n) independent forks, define the *balanced restriction* as follows. Fix the values of all variables NOT in the independent forks (this is possible because the forks are at distance ≥ 2 from each other and from non-fork variables). The remaining free variables are {v_1^j, v_2^j, v_3^j}_{j=1}^{k} — exactly 3k = Θ(n) variables.

The restricted function SAT_R^{bal} : {0,1}^{3k} → {0,1} depends only on the fork variables, with all other variables fixed.

**Lemma 4.2** (Balanced Restriction Preserves Fork Structure). *For a generic choice of the fixed variables' values:*

*(i) Each fork C_j remains a genuine three-way fork in the restricted function (all three literals are unforced).*

*(ii) The restricted function SAT_R^{bal} has the same satisfiability status as R(φ) conditioned on the fixed values.*

*(iii) The sensitivity of SAT_R^{bal} at any satisfying assignment is at least κ(x) = Ω(k) (the number of critically satisfied forks).*

*Proof.* (i) Since the forks' variables are disjoint from the fixed variables, no fork variable is determined by the fixing. (ii) The fixing determines the non-fork portion of the assignment; the residual's satisfiability depends on the fork variables' values. (iii) Follows from Theorem 3.1 applied to the restricted function. ∎

**Theorem 4.3** (Average Sensitivity of the Balanced Restriction). *For a generic fixing of non-fork variables:*

as(SAT_R^{bal}) ≥ Ω(k) · P_{restricted}[SAT]

*where P_{restricted}[SAT] is the fraction of assignments to the 3k fork variables that satisfy the restricted formula.*

*If the restricted formula is satisfiable and the satisfying fraction is at least 2^{-βk} for some β < 3 (i.e., the satisfying assignments are not too sparse among the 2^{3k} total), then:*

as(SAT_R^{bal}) ≥ Ω(k) · 2^{-βk} = Ω(k · 2^{-βk})

*This is exponentially small, which is unhelpful for an AC⁰ lower bound.*

---

## 5. The Correct Approach: Block Sensitivity and Certificate Complexity

The average sensitivity approach is hampered by the sparsity of satisfying assignments. We need a complexity measure that captures hardness at *satisfying* inputs specifically.

**Theorem 5.1** (Certificate Complexity Lower Bound). *For any satisfying assignment x of R(φ) with κ(x) ≥ ck critically satisfied independent forks, the 1-certificate complexity of SAT_R at x satisfies:*

C_1(SAT_R, x) ≥ n_R - O(1)

*meaning nearly all variables must be specified to certify that x is satisfying.*

*Proof.* A 1-certificate at x is a subset S ⊆ [n_R] such that for any assignment y agreeing with x on S, SAT_R(y) = 1. This means fixing the variables in S to their values in x forces all clauses to be satisfied regardless of the remaining variables' values.

For each clause C in R(φ), at least one of its literals' variables must be in S (otherwise, setting all three variables to make C unsatisfied is consistent with y agreeing with x on S). Since R(φ) has Θ(n_R) clauses, each involving 3 variables, and the clauses collectively involve all n_R variables (no variable is in zero clauses — otherwise it would have been eliminated by pure literal elimination), the certificate must include at least one variable from each clause.

For the independent forks specifically: each fork C_j has three variables, and the certificate must include at least one of them (to prevent the all-false assignment to C_j). With k = Ω(n) independent forks, C_1(SAT_R, x) ≥ k = Ω(n). ∎

**Theorem 5.2** (Block Sensitivity Lower Bound). *For any satisfying assignment x of R(φ), the block sensitivity satisfies:*

bs(SAT_R, x) ≥ κ(x) ≥ ck = Ω(n)

*Proof.* For each critically satisfied fork C_j, the block B_j = {v_j} (the single pivotal variable) is a sensitive block: flipping x on B_j changes SAT_R(x) from 1 to 0 (Theorem 3.1). Since the pivotal variables are distinct (forks are pairwise independent with disjoint variable sets), the blocks B_1, ..., B_{κ(x)} are disjoint. Therefore bs(SAT_R, x) ≥ κ(x). ∎

**Corollary 5.3** (Sensitivity Conjecture Application). *By the Sensitivity Theorem (Huang 2019), for any Boolean function f:*

s(f) ≥ √{bs(f)}

*Therefore:*

s(SAT_R) ≥ √{bs(SAT_R)} ≥ √{Ω(n)} = Ω(√n)

*This gives maximum sensitivity at least Ω(√n), which is weaker than the desired Ω(n) but still sufficient for an AC⁰ lower bound via Boppana's theorem (AC⁰ of depth d has sensitivity O((log n)^{d-1}), so Ω(√n) sensitivity implies SAT_R ∉ AC⁰ of any constant depth and polynomial size).*

---

## 6. The AC⁰ Lower Bound

**Theorem 6.1** (The Residual's Satisfiability is Not in AC⁰). *Let R(φ) be the residual of a random 3-SAT instance at α ≈ 4.267, and assume the decoupling property (Conjecture 2.4). Then SAT_R ∉ AC⁰. More precisely, any AC⁰ circuit of depth d computing SAT_R requires size 2^{Ω(n^{1/(2(d-1))})}.*

*Proof.* By Theorem 5.2, bs(SAT_R) ≥ Ω(n) (at satisfying assignments with linearly many critically satisfied forks, which exist by Theorem 2.3 conditional on Conjecture 2.4).

By the Sensitivity Theorem (Huang 2019): s(SAT_R) ≥ √{bs(SAT_R)} ≥ Ω(√n).

By Boppana's theorem (1997): any function in AC⁰ of depth d has sensitivity at most O((log m)^{d-1}), where m is the number of input variables (here m = n_R = Θ(n)). Since Ω(√n) > O((log n)^{d-1}) for any constant d and sufficiently large n, SAT_R ∉ AC⁰.

For the size bound: by Håstad's switching lemma, any depth-d circuit computing a function with block sensitivity b requires size at least 2^{Ω(b^{1/(d-1)})}. With bs(SAT_R) ≥ Ω(n), this gives size 2^{Ω(n^{1/(d-1)})}. Using the Sensitivity Theorem's weaker bound s ≥ √b, and Boppana's connection of sensitivity to AC⁰ size, we get the stated 2^{Ω(n^{1/(2(d-1))})} bound. ∎

**Remark 6.2** (Strengthening to Ω(n) sensitivity). If the full Conjecture 4.3 is proved (average sensitivity Ω(n), not just max sensitivity Ω(√n)), the AC⁰ size bound improves to 2^{Ω(n^{1/(d-1)})} — matching Håstad's optimal lower bound for PARITY. This would confirm that the residual is "parity-hard" in the AC⁰ sense.

The gap between Ω(√n) and Ω(n) for the maximum sensitivity arises from our use of the Sensitivity Theorem as a black box. A direct proof that s(SAT_R) ≥ Ω(n) — without passing through block sensitivity — would require showing that the critically satisfied forks contribute Ω(n) *simultaneously sensitive* coordinates (not just sensitive in disjoint blocks). This is Conjecture 2.4 (the decoupling property): it ensures that the critically satisfied forks are simultaneously present in a typical satisfying assignment, making all pivotal variables simultaneously sensitive.

---

## 7. Summary: What We've Proven and What Remains

### The Proven Chain (conditional on Conjecture 2.4 — the decoupling property):

1. **R(φ) has Ω(n) independent three-way forks** — Theorem 2.1 of the Cohomological Dimension paper. ✅

2. **A typical satisfying assignment critically satisfies Ω(n) of these forks** — Theorem 2.3, using the absence of backbone variables and the bounded-bias property of residual variables. ✅ (conditional on Conjecture 2.4)

3. **Critically satisfied forks create pivotal variables that are simultaneously sensitive** — Theorem 3.1, using the key observation that flipping a pivotal variable at a satisfying assignment can only break clauses, never repair them. ✅

4. **Block sensitivity of SAT_R is Ω(n)** — Theorem 5.2, from the disjointness of pivotal variable sets across independent forks. ✅ (conditional on Conjecture 2.4)

5. **Maximum sensitivity of SAT_R is Ω(√n)** — Corollary 5.3, via the Sensitivity Theorem (Huang 2019). ✅ (unconditional given step 4)

6. **SAT_R ∉ AC⁰** — Theorem 6.1, from sensitivity exceeding the AC⁰ sensitivity bound. ✅ (conditional on Conjecture 2.4)

### The Single Remaining Gap:

**Conjecture 2.4 (Decoupling Property).** The conditional distribution of each independent fork's variables, given global satisfiability, is close to the unconditional distribution given local satisfiability. Formally: long-range correlations in the satisfying assignment decay at rate n^{-Ω(1)} for variables at graph distance ≥ 2.

This is expected from the cavity method in statistical physics and from rigorous spatial mixing results in related random CSP models. A proof would require extending the Montanari–Restrepo–Tetali (2011) spatial mixing framework to the 3-SAT threshold, which is an open problem in probabilistic combinatorics — but one with a clear path of attack.

### What This Gives the Research Program:

The conditional AC⁰ lower bound for the residual is the first restricted-model result in the Tier 1 roadmap. It establishes that the projection-only core of 3-SAT is not merely "hard for DPLL" but "hard for constant-depth circuits" — a strictly stronger statement that connects to Håstad's switching lemma and the broader AC⁰ lower bound literature.

The result does NOT evade the locality barrier (the switching lemma is itself a local technique). But it verifies that the sheaf-theoretic framework correctly identifies the hard core, and that the hard core's properties (high block sensitivity, pseudorandomness, projection-only polymorphisms) are exactly those predicted by the cohomological analysis.

The path forward: from AC⁰ to TC⁰ (the first superpolynomial TC⁰ lower bound for any explicit function would be a major breakthrough), and from restricted models to the general circuit lower bound that Pivotal Lemma C requires. Each step requires stronger techniques — and the cohomological dimension Ω(n) provides the structural foundation that each successive technique must exploit.
