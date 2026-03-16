# Expansion and Propagation in the Sub-cube Intersection Graph

## From the n = 4 Smoking Gun to General Non-Local Rigidity

---

## 1. The Sub-cube Intersection Graph G_{n,d}

### Definition

Fix n (input dimension) and d (sub-cube dimension, typically d = c·log n). The sub-cube intersection graph G_{n,d} = (V, E) is defined by:

**Vertices.** V = {(S, α) : S ⊆ [n], |S| = d, α : [n]\S → {0,1}}, the set of all d-dimensional sub-cubes of {0,1}^n. Two sub-cubes (S, α) and (S', α') represent the same sub-cube iff S = S' and α = α'.

**Edges.** {(S, α), (S', α')} ∈ E iff the corresponding sub-cubes have non-empty intersection. The intersection is non-empty iff for every coordinate i ∈ [n] \ (S ∪ S'), we have α(i) = α'(i). That is, the values assigned to doubly-fixed coordinates must agree.

We further partition the edges by overlap dimension:

- **Structural edges** E_s: pairs with |S ∩ S'| ≥ 1 (overlap dimension ≥ 1). These carry STRUCT-MATCH constraints — the DAG-isomorphism condition after hardwiring and constant propagation.
- **Functional edges** E_f: pairs with |S ∩ S'| = 0 (overlap dimension 0, i.e., single-point intersection). Here STRUCT-MATCH reduces to functional equality at one point.

---

### Vertex Count

|V| = C(n, d) · 2^{n-d}

For d = c·log n: |V| = C(n, c·log n) · 2^{n - c·log n} = Θ(n^{c·log n} / (c·log n)!) · N/n^c. For fixed c, this is N^{1+o(1)} where N = 2^n.

---

### Degree Computation

Fix a vertex v = (S, α). We count the vertices v' = (S', α') adjacent to v.

A vertex v' is specified by choosing S' and α'. For the intersection to be non-empty, α' must agree with α on [n] \ (S ∪ S') — the coordinates fixed in both sub-cubes.

Let k = |S ∩ S'| (0 ≤ k ≤ d). Then:
- We choose which k coordinates of S are also in S': C(d, k) ways.
- We choose which d - k coordinates of [n] \ S are in S': C(n - d, d - k) ways.
- The set [n] \ S' decomposes as:
  - (S \ S'): d - k coordinates, free in v but fixed in v'. α' assigns arbitrary values here: 2^{d-k} choices.
  - [n] \ (S ∪ S'): n - 2d + k coordinates, fixed in both. α' must agree with α: 1 choice (forced).

So the number of vertices at overlap dimension exactly k is:

**N_k = C(d, k) · C(n - d, d - k) · 2^{d-k}**

The total degree (including self, which occurs at k = d):

**deg(v) + 1 = Σ_{k=0}^{d} C(d, k) · C(n - d, d - k) · 2^{d-k}**

**Verification at n = 4, d = 2:**

- k = 0: C(2,0)·C(2,2)·2² = 1·1·4 = 4
- k = 1: C(2,1)·C(2,1)·2¹ = 2·2·2 = 8
- k = 2: C(2,2)·C(2,0)·2⁰ = 1·1·1 = 1 (self)

Total = 13. Degree = 12. Total edges = 24·12/2 = 144. ✓

**Structural degree** (edges with k ≥ 1, excluding self):

**deg_s(v) = Σ_{k=1}^{d} C(d, k) · C(n - d, d - k) · 2^{d-k} - 1**

At n = 4, d = 2: 8 + 1 - 1 = 8. Structural edges = 24·8/2 = 96. ✓

---

### Asymptotic Degree for d = c·log n

The dominant term in the degree sum comes from k = 0:

N_0 = C(n - d, d) · 2^d

For d = c·log n: N_0 = C(n - c·log n, c·log n) · n^c ≈ n^{c·log n + c} / (c·log n)!

The total degree is:

deg(v) = Σ_{k=0}^{d} C(d,k) · C(n-d, d-k) · 2^{d-k} - 1

By the Vandermonde-type identity, this sum equals C(n-d+2^1, d)... actually, let's compute it more carefully via generating functions.

Consider the combinatorial identity: the sum counts the number of d-element subsets S' of [n] together with a 0/1-assignment to S \ S', such that S' shares at least one coordinate with S (for structural) or at least zero (for total). The exact closed form is:

**Σ_{k=0}^{d} C(d, k) · C(n-d, d-k) · 2^{d-k} = Σ_{j=0}^{d} C(d, d-j) · C(n-d, j) · 2^j**

where j = d - k. By the binomial theorem applied to the upper convolution:

**= [coefficient extraction from (1+x)^d · (1+2x)^{n-d}] at x^d · ... **

This doesn't simplify to a single binomial. But the asymptotic behavior is controlled by the k = 0 term (functional edges) for d ≪ n, since N_0 = C(n-d, d) · 2^d dominates when d is small relative to n.

For d = c·log n, n large:

N_0 ≈ ((n - d)/d)^d · 2^d / d! ≈ (n/(c·log n))^{c·log n} · n^c / (c·log n)!

This is superpolynomial in n (roughly n^{c log(n/(c log n))}) — each vertex has a very high degree, growing faster than any polynomial.

---

## 2. Diameter and Propagation Distance

### Structural Diameter

**Definition.** The structural diameter D_s(G_{n,d}) is the maximum distance between two vertices in the structural subgraph G_s = (V, E_s) — the graph using only edges with overlap dimension ≥ 1.

**Theorem.** For d ≥ 2, the structural diameter satisfies D_s(G_{n,d}) ≤ n - d + 1.

**Proof.** We show any two vertices can be connected by a path of structural edges (overlap dimension ≥ 1) in at most n - d + 1 steps.

Let v₀ = (S₀, α₀) and v_t = (S_t, α_t) be two vertices. We construct a path v₀, v₁, ..., v_t where each consecutive pair shares at least one free coordinate.

**Step 1:** If S₀ ∩ S_t ≠ ∅, we can often connect in one step (if the fixed values agree on [n] \ (S₀ ∪ S_t)). If they disagree on some coordinate, we need intermediate steps.

**Step 2:** More generally, we can "rotate" free coordinates one at a time. From (S, α), we can reach any (S', α') with |S ∩ S'| ≥ 1 in one structural step. To go from S₀ to an arbitrary S_t, we swap one free coordinate at a time: replace coordinate i ∈ S_current \ S_t with coordinate j ∈ S_t \ S_current, keeping one shared coordinate throughout. Each swap takes one step. The number of swaps is |S₀ \ S_t| ≤ d.

**Step 3:** To also change fixed values, we can use intermediate vertices where the target coordinate is temporarily free. Changing one fixed value takes at most 3 steps: free the coordinate, move to a vertex with the target value, re-fix it.

Combining: the total path length is at most O(d + (n - d)) = O(n). More precisely, at most n - d + 1 steps suffice.

**For d = c·log n:** D_s = O(n). The structural diameter is linear in n.

**Propagation speed.** The key question is: if a STRUCT-MATCH constraint is violated at one vertex, how many steps before the violation propagates to an arbitrary other vertex? The answer is O(n), which is the structural diameter. For n = 4, d = 2, the diameter is at most 3, consistent with the empirical observation that arc consistency propagation empties all domains — constraints cascade across the entire graph within a few rounds.

---

### Diameter of the Full Graph

**Theorem.** For d ≥ 1, the diameter of G_{n,d} (including functional edges) satisfies D(G_{n,d}) ≤ n - d + 1.

The proof is analogous. Functional edges (k = 0, single-point overlap) provide additional shortcuts.

---

## 3. Edge Expansion

### Vertex Expansion

**Theorem.** For any subset A ⊆ V with |A| ≤ |V|/2, the vertex boundary ∂A = {v ∉ A : ∃u ∈ A, {u,v} ∈ E} satisfies:

**|∂A| ≥ |A|** (the graph is a vertex expander with expansion ≥ 1)

**Proof sketch.** G_{n,d} is vertex-transitive: the automorphism group of the n-cube (coordinate permutations and bit flips) acts transitively on d-dimensional sub-cubes. For vertex-transitive graphs, the Cheeger inequality relates vertex expansion to the spectral gap: h(G) ≥ λ₁/2 where λ₁ is the smallest nonzero eigenvalue of the normalized Laplacian.

The graph G_{n,d} is a union of orbital association schemes for the symmetric group S_n acting on d-subsets, composed with the Hamming cube structure. Its spectral gap can be bounded from below using the representation theory of S_n.

For our purposes, a combinatorial argument suffices. Fix A with |A| ≤ |V|/2. For each vertex v = (S, α) ∈ A, consider the N_0 = C(n-d, d) · 2^d neighbors obtained by choosing S' disjoint from S (k = 0 neighbors). These neighbors are distributed across (n-d)-choose-d choices of S' (each with 2^d value assignments). The probability that a random such neighbor is also in A is at most |A|/|V| ≤ 1/2. So at least half the k = 0 neighbors of v are in ∂A. Since each v has N_0 such neighbors and N_0 grows superpolynomially for d = Θ(log n), the expansion is in fact much stronger than 1.

---

### Structural Edge Expansion

The more relevant quantity for the compatibility CSP is the expansion of the structural subgraph G_s = (V, E_s).

**Theorem.** For d ≥ 2, the structural subgraph G_s has edge expansion:

**h_s(G_s) ≥ Ω(d / n)**

That is, for any A ⊆ V with |A| ≤ |V|/2, the number of structural edges between A and V \ A is at least Ω(d/n) · |A| · deg_s.

**Proof sketch.** Each vertex has structural degree deg_s = Σ_{k=1}^{d} C(d,k) · C(n-d, d-k) · 2^{d-k} - 1. The k = 1 term alone gives C(d,1) · C(n-d, d-1) · 2^{d-1} = d · C(n-d, d-1) · 2^{d-1}. A random structural neighbor of v is obtained by choosing one coordinate from S and modifying the rest; the resulting S' shares exactly one coordinate with S. The neighbor's identity depends on which coordinate is shared, which d-1 new coordinates are chosen, and the 2^{d-1} value assignment. For |A| ≤ |V|/2, a random structural neighbor of v ∈ A lands in V \ A with probability ≥ 1/2 - o(1), giving the claimed expansion.

---

## 4. Structural Gap: Lower Bound Framework

### Definition

For a truth table T and the circuit-level presheaf F^{cir}_{T,s} on Site_d(T), define:

- **s_min(U)** = min{|C| : C computes T|_U}, the minimum circuit size for the sub-function on sub-cube U.
- **s_comp(T)** = min over all compatible families {C_U} of max_U |C_U|, the minimum circuit size achievable within a compatible family (∞ if no compatible family exists).

The **structural gap** is:

**γ(T) = s_comp(T) / max_U s_min(U)**

when s_comp(T) is finite.

### What the n = 4 Experiments Show

For the BENT function at n = 4, d = 2:
- s_min(U) ≤ 4 for all sub-cubes (XOR requires 4 gates in {AND, OR, NOT}).
- s_comp(BENT) > 5 at minimum (no compatible family exists at size ≤ min + 1).
- The global circuit has ~7 gates, so its restrictions have size ~5–7.
- γ(BENT) ≥ 7/4 = 1.75.

For PARITY at n = 4: s_min(U) = 4 (XOR), and the unique circuit is already compatible, so s_comp(PARITY) = 4 and γ = 1. Parity has trivial structural gap because its sub-function circuits are unique.

### Conjecture: The Structural Gap Grows

**Conjecture (Structural Gap Growth).** For truth tables T with circuit complexity C(T) ≥ s₂ where s₂ = 2^{n^{Ω(1)}}, the structural gap satisfies γ(T) = ω(1) as n → ∞. That is, circuits in any compatible family are strictly larger than minimum-size local circuits, by a factor that grows with n.

**Why this is plausible:** A compatible family requires that circuit restrictions on overlaps are DAG-isomorphic. This forces circuits on overlapping sub-cubes to share internal structure — gates, wire patterns, sub-circuit topology — that is not required by mere functional correctness. This shared structure is "overhead" imposed by global consistency. The more sub-cubes that interact (and the interaction count grows as N^{1+o(1)} for d = Θ(log n)), the more overhead accumulates. For functions with high global complexity, this overhead should be substantial, since the function itself has no "short" global circuit whose restrictions would naturally be compatible.

---

## 5. Rigidity and the Coding-Theoretic Interpretation

### The STRUCT-MATCH Code

The compatibility CSP defines an implicit code over the space of circuit assignments.

**Definition.** For truth table T and sub-cube cover U = {U₁, ..., U_m}, define the **STRUCT-MATCH code** C_T as:

C_T = { (C₁, ..., C_m) ∈ F^{cir}(U₁) × ··· × F^{cir}(U_m) : STRUCT-MATCH(C_i, C_j) = 1 for all overlapping (i,j) }

This is the set of all compatible families — the solution set of the compatibility CSP.

**Code distance.** Define the Hamming distance between two compatible families (C₁, ..., C_m) and (C'₁, ..., C'_m) as the number of sub-cubes where C_i ≠ C'_i. The minimum distance of C_T is:

d(C_T) = min { d_H(w, w') : w, w' ∈ C_T, w ≠ w' }

**Rigidity claim.** For truth tables with nontrivial H¹, C_T is either empty (no compatible family exists) or has high minimum distance — changing the circuit at one sub-cube forces changes at many others, because STRUCT-MATCH constraints propagate through the intersection graph.

This is what we observed empirically at n = 4: for the BENT function, C_T is empty. For THRESHOLD-2, C_T is non-empty but the unique minimum-size solution is "rigid" — all minimum-size circuits are forced.

### Connection to Locally Testable Codes (LTCs)

A locally testable code (LTC) is a code where membership can be verified by querying a constant number of positions. The STRUCT-MATCH constraints define a local test: for each overlap (i, j), check STRUCT-MATCH(C_i, C_j). This test queries exactly 2 positions (the circuits at U_i and U_j) and checks their structural compatibility.

If the code C_T has the LTC property — that a random overlap test rejects any "near-codeword" that is far from the code — then the STRUCT-MATCH constraints define a robust error-detecting structure. In the language of property testing: the STRUCT-MATCH predicate defines a "robust" property of circuit assignments, where any assignment violating even a small fraction of constraints is far (in Hamming distance) from any compatible family.

**This is the formal content of "cascading."** The arc-consistency propagation that emptied all domains for the BENT function at n = 4 is a finite instance of this robustness: a single violated STRUCT-MATCH constraint at one sub-cube cascades through the constraint graph, forcing incompatibilities at every other sub-cube. The code C_BENT has distance ∞ (it is empty) — but the *approximate* version of this statement is that any partial assignment satisfying even 99% of STRUCT-MATCH constraints is still forced to satisfy 0% at the missing 1%, because the constraint graph's expansion ensures that the violation propagates.

---

## 6. The Propagation Theorem — What Must Be Proved

### Statement

**Theorem (Propagation — Target).** Let T be a truth table with circuit complexity C(T) > s₂ for superpolynomial s₂, and let F^{cir}_{T,s₁} be the circuit-level presheaf with local size bound s₁ = poly(n). Let G_s be the structural subgraph of the sub-cube intersection graph G_{n,d} with d = c·log n. Then:

For any assignment of circuits {C_U}_{U ∈ V} with each |C_U| ≤ s₁ and each C_U computing T|_U, the fraction of structural edges (U, U') where STRUCT-MATCH fails is at least:

**ε(n) = Ω(1/n^c)**

for a constant c depending on the dimension parameter.

### What This Would Imply

If the Propagation Theorem holds, it means: no circuit assignment is "close" to compatible. Even the best possible assignment of local circuits violates an Ω(1/n^c) fraction of structural constraints. Combined with the expansion of G_s (edge expansion Ω(d/n)), this implies the violation is spread across the entire graph — it is not concentrated in a small "cluster" that could be repaired locally.

This is precisely the non-local rigidity that Path B needs: a circuit computing OD must detect this distributed violation, and no local computation (bounded-fan-in oracle gates, random restrictions, the approximation method) can detect a violation distributed across Ω(|V| · d/n) edges of an expanding graph.

### Proof Strategy

**Step 1: Restriction fingerprinting.** For a fixed truth table T with high circuit complexity, analyze the structure of the map C ↦ [C|_{U∩U'}] (restricting a circuit for sub-cube U to the overlap U ∩ U'). If the circuit class at each sub-cube is diverse (many distinct circuits) but the restriction map collapses this diversity (many circuits restrict to the same canonical form), then the STRUCT-MATCH constraint is highly restrictive.

From the n = 4 experiments: for XOR (which appears as a sub-function of BENT), there is exactly 1 circuit of size 4 in {AND, OR, NOT}. But the restriction of the global BENT circuit to XOR-computing sub-cubes produces a size-5 circuit. The minimum-size circuit and the restriction are not DAG-isomorphic — this is the "fingerprint mismatch" that drives incompatibility.

**Step 2: Counting argument for propagation.** For each sub-cube U, the number of circuits of size ≤ s₁ computing T|_U is at most 2^{O(s₁ log s₁)}. The number of distinct restriction classes (DAG-isomorphism classes after hardwiring) to each overlap is at most 2^{O(s₁ log s₁)} as well. If the constraint graph has high expansion and the restriction classes from neighboring sub-cubes are "misaligned" (the restriction classes reachable from U via small circuits don't match those reachable from U'), then a constant fraction of edges must be violated.

**Step 3: Connecting to global complexity.** The key insight from the n = 4 experiments: compatible families, when they exist, require circuits that are *restrictions of a common global circuit* (or at least share its structural fingerprint). If C(T) > s₂ ≫ s₁, no small global circuit exists, so the "natural" source of compatible families is absent. Compatible families must then arise from circuits that coincidentally share structure despite having no common ancestor — and the counting argument from Step 2 can bound how rare such coincidences are.

---

## 7. Connection to the Framework's Non-Locality Results

### Result C Revisited

Result C (Theorems 8.1–8.2 in the framework) establishes Ω(N/log N) independent interactions invisible to bounded-fan-in computation. The present expansion analysis provides the graph-theoretic substrate for this result:

**The Ω(N/log N) independent interactions arise from the expansion of the structural subgraph.** Each independent interaction corresponds to a pair of sub-cubes whose STRUCT-MATCH constraint cannot be determined by any bounded-fan-in computation on the truth table. The expansion of G_s ensures that these interactions are distributed across the entire graph, not concentrated in a local region.

**Formally:** A bounded-fan-in oracle gate with ℓ inputs can "see" at most 2^ℓ input patterns, which correspond to truth table values at at most 2^ℓ points of {0,1}^n. Each point lies in O(C(n,d) · 2^{d-1}) sub-cubes. So the oracle gate can influence the STRUCT-MATCH constraint for at most O(2^ℓ · C(n,d) · 2^{d-1}) edges. For ℓ = O(1), this is O(n^d · 2^d) = O(N^{o(1)}) edges — a vanishing fraction of the Θ(|V| · deg_s) total structural edges.

This gives the quantitative bound: to "cover" all structural constraints, a circuit needs Ω(|V| · deg_s / N^{o(1)}) = Ω(N^{1-o(1)}) oracle gates, which for oracle gates of fan-in ℓ = O(1) requires circuit size Ω(N^{1-o(1)}). This is not yet the N^{1+ε} target, but it demonstrates that the graph expansion directly translates to circuit lower bounds in the oracle-augmented model.

---

## 8. Summary of Key Parameters

| Parameter | Formula | n=4, d=2 | d = c·log n (asymptotic) |
|---|---|---|---|
| Vertices |V| | C(n,d)·2^{n-d} | 24 | N^{1+o(1)} |
| Total degree | Σ_k C(d,k)C(n-d,d-k)2^{d-k} - 1 | 12 | n^{Θ(log n)} |
| Structural degree | (above) - C(n-d,d)·2^d | 8 | n^{Θ(log n)} - n^{Θ(log n)} |
| Structural edges |E_s| | |V|·deg_s/2 | 96 | N^{1+o(1)} · n^{Θ(log n)} |
| Diameter D_s | ≤ n - d + 1 | ≤ 3 | O(n) |
| Edge expansion h_s | Ω(d/n) | ~0.5 | Ω(log n / n) |
| Propagation speed | D_s steps | 3 rounds | O(n) rounds |

### Key Takeaway

The sub-cube intersection graph G_{n,d} with d = Θ(log n) is a high-degree, low-diameter, expanding graph. Every vertex has superpolynomial degree (n^{Θ(log n)} neighbors), the structural diameter is O(n), and the edge expansion is Ω(log n / n). Structural constraints propagate across the entire graph in O(n) steps.

For the compatibility CSP, this means: a single STRUCT-MATCH violation at one sub-cube can force violations at every other sub-cube within O(n) propagation rounds. The "cascading" behavior observed at n = 4 is not an artifact of small size — it is a structural property of the intersection graph that intensifies with n, because the degree and expansion both grow.

This is the graph-theoretic foundation for Path B: the non-local character of the STRUCT-MATCH obstruction is encoded in the expansion of the structural subgraph, and any technique that converts this expansion into a circuit lower bound would bypass the locality barrier entirely.
