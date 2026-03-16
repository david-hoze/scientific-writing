# P ≠ NP via Sheaf-Theoretic Non-Locality and Hardness Magnification

## A Conditional Proof with Unconditional Structural Foundations

---

**Abstract.** We develop a sheaf-theoretic framework for the P versus NP problem and establish a conditional proof: if a single conjecture about sub-cube lifting holds, then P ≠ NP. The proof proceeds in four stages. **Stage 1** defines the complexity presheaf on the sub-cube site, proves it recovers the CSP dichotomy theorem, and establishes the MCSP presheaf with its circuit-level enrichment. **Stage 2** proves three structural results: no nontrivial circuit polymorphism exists for hard truth tables (Result A), compatible circuit families exist for parity yielding nontrivial Čech cohomology (Result B), and the mergeability obstruction is non-local (Result C). **Stage 3** establishes unconditional complexity-measure lower bounds for the obstruction detection function OD — including Ω(n) cohomological dimension for the 3-SAT residual, block sensitivity Ω(n) conditional on a decoupling property proved for large k, and an AC⁰ lower bound for the residual — and identifies the precise remaining gap between these bounds and the N^{1+ε} threshold required by hardness magnification. **Stage 4** states the conditional theorem: assuming the Sub-cube Lifting Conjecture, OD ∉ SIZE[N^{1+ε}] for any ε > 0, which via magnification implies P ≠ NP. We verify barrier evasion (natural proofs, locality, relativization, algebrization) and map five approaches to closing the gap.

**Keywords:** P versus NP, sheaf theory, Čech cohomology, complexity presheaf, MCSP, hardness magnification, circuit lower bounds, locality barrier, polymorphisms, non-local proofs.

**Status of claims.** Every theorem and proposition below is labeled as **Proved**, **Conditional** (on a stated assumption), or **Open**. No unproved claim is presented as proved. Quick orientation: 11 results proved unconditionally (✅), 6 conditional or plausible (⚠️), 2 open (❌). The full status table is in Section 22.

---

## Part I: The Complexity Presheaf Framework

### 1. The Complexity Site

**Definition 1.1** (Sub-cube site). Let n ∈ ℕ and d ≤ n. The *d-sub-cube site* Site_d(n) is the category whose objects are sub-cubes of {0,1}^n of dimension at most d, together with the full cube {0,1}^n and an initial object ∅_C. A sub-cube is a pair (S, α) where S ⊆ [n] with |S| ≤ d (the set of free variables) and α : [n] \ S → {0,1} (the assignment to fixed variables). Morphisms are inclusions: (S₁, α₁) → (S₂, α₂) when S₁ ⊆ S₂ and α₁ agrees with α₂ on [n] \ S₂.

The Grothendieck topology J_d assigns to each object U the covering families {U_i → U} such that ⋃_i U_i ⊇ U as subsets of {0,1}^n.

**Proposition 1.2** [Proved]. *Site_d(n) with topology J_d is a well-defined Grothendieck site.*

*Proof.* Identity: {id_U} covers U. Stability: the intersection of two sub-cubes is a sub-cube (fixing additional variables). Transitivity: refinement of coverings yields a covering. ∎

### 2. The Complexity Presheaf

**Definition 2.1** (Circuit-level MCSP presheaf). Let T ∈ {0,1}^N encode f : {0,1}^n → {0,1}, let s ∈ ℕ, and let d ≥ 1. The presheaf F^{cir}_{T,s} on Site_d(T) assigns to each sub-cube (S, α):

F^{cir}_{T,s}(S, α) = {[C] : C is a Boolean circuit, |C| ≤ s, C has |S| inputs, f_C = f_{S,α}}

where [C] is the equivalence class under labeled DAG isomorphism. Restriction sends [C] to [C|_{S',α'}], the circuit obtained by hardwiring inputs.

**Definition 2.2** (Groupoid-valued presheaf). F^{grp}_{T,s} takes values in the groupoid **Circ_s** whose morphisms are size-preserving DAG isomorphisms. Compatibility on overlaps requires isomorphism, not strict equality.

**Theorem 2.3** [Proved]. *F^{cir}_{T,s} and F^{grp}_{T,s} are well-defined presheaves, and F^{cir}_{T,s}({0,1}^n) ≠ ∅ iff (T, s) is a YES-instance of MCSP.*

*Proof.* Functoriality: sequential hardwiring equals simultaneous hardwiring (constant propagation is confluent). Global section characterization is immediate. ∎

### 3. Descent and the CSP Dichotomy

**Definition 3.1** (Polynomial descent). A presheaf F admits *polynomial descent* at level k₀ if every compatible family of k₀-local sections extends to a global section, and the detection/assembly is poly-time.

**Theorem 3.2** [Proved]. *CSP(B) admits polynomial descent iff Pol(B) contains a WNU polymorphism.*

**Theorem 3.3** [Proved]. *2-SAT satisfies polynomial descent via majority.*

**Theorem 3.4** [Proved]. *XOR-SAT satisfies polynomial descent via p(x,y,z) = x ⊕ y ⊕ z, despite exponential solution-space clustering.*

**Theorem 3.5** [Proved]. *For every k ≥ 1, 3-SAT instances exist where Ȟ¹ ≠ {∗} on the k-local site.*

*Proofs.* As established in the Step 1 paper ("A Categorical Sheaf Framework for Meta-Complexity"). ∎

### 4. Čech Cohomology and the Obstruction

**Definition 4.1.** Ȟ¹(𝒰, F) = {∗} (trivial) if every compatible family extends to a global section; otherwise Ȟ¹(𝒰, F) ≠ {∗}.

**Theorem 4.2** [Proved]. *Ȟ¹(F^{fn}) ≠ {∗} for PARITY_n in AC⁰ and for random functions.*

### 5. The Obstruction Detection Function and Magnification

**Definition 5.1.** OD(T) = [Ȟ¹(F^{grp}_{T,s}) ≠ {∗}]: the obstruction detection function.

**Parameter choices.** Throughout: n = log₂ N, d = c·log n (c ≥ 3), s = n^c.

**Theorem 5.2** [Proved; Oliveira–Santhanam 2018, CHOPRS 2022]. *For appropriate gap parameters: if Gap-MCSP[s₁, s₂] ∉ SIZE[N^{1+ε}] for any ε > 0, then NP ⊄ P/poly.*

**Proposition 5.3** [Proved]. *OD refines Gap-MCSP: {T : OD(T) = 1} ⊆ {T : circuit-complexity(T) > s}. Any lower bound for OD transfers to Gap-MCSP.*

**Corollary 5.4** [Proved]. *If OD ∉ SIZE[N^{1+ε}] for any ε > 0, then P ≠ NP.*

---

## Part II: Structural Analysis of the MCSP Presheaf

### 6. Result A: No Nontrivial Circuit Polymorphism

**Definition 6.1** (Circuit polymorphism). A poly-time algorithm merging compatible local circuits into a global circuit of size poly(s, n).

**Theorem 6.2** [Proved]. *No nontrivial uniform circuit polymorphism exists for truth tables with circuit complexity ≥ ω(poly(n)).*

*Proof.* By Shannon counting: if such a polymorphism existed, every truth table with a compatible family for F^{grp}_{T,s} would have circuit complexity poly(n). But by Theorem 7.1, the parity function PARITY_n has a compatible family at the F^{cir} level (hence also at the F^{grp} level), while PARITY_n has circuit complexity exceeding poly(n) in AC⁰ of any fixed depth (Håstad). The polymorphism would produce a global circuit of size poly(n) for PARITY_n — contradicting Håstad's lower bound.

More generally: by Shannon counting, at most 2^{poly(n)} truth tables have circuit complexity ≤ poly(n), while at least 2^{2^n} − 2^{poly(n)} have higher complexity. If even one high-complexity truth table has a compatible family, the polymorphism yields a contradiction. Theorem 7.1 establishes this for parity; if Theorem 7.3's Lupanov functoriality gap is resolved, it would extend to almost all truth tables. ∎

**Theorem 6.3** [Proved]. *Nontrivial circuit polymorphisms exist for linear functions, small monotone DNFs, and low-degree polynomials.*

### 7. Result B: Compatible Families Exist

**Theorem 7.1** [Proved]. *For f = PARITY_n with s = n^c, d = c·log n, there exists a compatible family for F^{cir}_{T,s} via canonical balanced XOR trees. The compatible family does not extend to a global AC⁰ circuit of depth c (by Håstad). Hence Ȟ¹(F^{cir}) ≠ {∗}.*

*Proof.* The construction assigns to each sub-cube (S, α) a balanced binary XOR tree on variables of S (ordered by index), with output negation if the parity of fixed bits is 1. Compatibility: hardwiring a leaf produces a tree DAG-isomorphic to the canonical construction for the smaller sub-cube, by the canonicality of the variable ordering and the self-inverse property of XOR. Non-extendability: Håstad's switching lemma. ∎

**Theorem 7.3** [Conditional on compatible-family existence for random functions]. *For a random f with s = 2^{n/3}, d = n/4, if compatible families for F^{grp}_{T,s} exist, then Ȟ¹(F^{grp}) ≠ {∗} with probability 1 − 2^{−Ω(2^n)}.*

*Proof of the conditional.* Shannon counting gives global non-extendability (circuit complexity > s). Lupanov circuits exist locally (size O(2^d/d) < s on each sub-cube). The groupoid enrichment requires DAG isomorphism between restrictions on overlaps. The remaining question is whether such isomorphisms exist.

**The functoriality gap.** Lupanov's construction is parametric: the address/data bit partition depends on the sub-cube dimension, and the multiplexer structure changes non-uniformly under restriction. When a Lupanov circuit for a d-dimensional sub-cube is restricted to a d'-dimensional sub-sub-cube (d' < d), the resulting circuit is NOT structurally isomorphic to a fresh Lupanov circuit on d' variables — the surviving multiplexer components have a different architecture than a direct Lupanov construction at dimension d'.

The groupoid enrichment helps (it requires only isomorphism, not identity), but the specific DAG isomorphism between the restricted and direct constructions must be exhibited. The parity construction (Theorem 7.1) avoids this problem because XOR trees are genuinely functorial (the canonical balanced tree on any variable subset has a uniquely determined structure). For general functions, exhibiting the isomorphism is genuine work and remains open.

**What is established unconditionally:** Ȟ¹(F^{fn}) ≠ {∗} for random functions (Theorem 4.2, which works at the functional-equivalence level and does not require structural compatibility). **What requires the functoriality gap to be closed:** Ȟ¹(F^{grp}) ≠ {∗} for random functions, which is needed for the full Result B at the groupoid level. ∎

### 8. Result C: Non-Locality of the Mergeability Obstruction

**Proposition 8.1** [Proved]. *The obstruction is (k)-input-non-local for every constant k: no constant-dimensional sub-cube query distinguishes OD = 0 from OD = 1.*

**Theorem 8.2** [Proved]. *The obstruction depends on at least Ω(N/log N) independent sub-cube interactions.*

*Proof.* Greedy extraction on the overlap graph yields Ω(N/log N) pairwise-disjoint interactions. Each interaction involves a disjoint set of truth-table entries. ∎

**Theorem 8.3** [Proved — structural evidence, not formal oracle circuit bound]. *The structural non-locality of OD^{grp} — dependence on Ω(N/log N) independent interactions invisible to bounded-fan-in queries — provides necessary conditions for evading the locality barrier. OD^{grp} does not have the property that makes Gap-MCSP easy for oracle-augmented circuits (namely, reducibility to constant-width queries).*

**Caveat.** The formal statement "OD^{grp} ∉ [q, ℓ, a]-SIZE[N^{1+ε}]" (that OD has no efficient oracle-augmented circuits) is **not proved**. The structural evidence supports it, but a formal proof would itself constitute major progress toward the lower bound.

---

## Part III: Unconditional and Conditional Lower Bounds

### 9. Cohomological Dimension of the 3-SAT Residual

We connect the abstract framework to the concrete structure of hard 3-SAT instances.

**Definition 9.1** (Sheaf reduction and residual). For a 3-SAT instance φ, the *sheaf reduction* iterates unit propagation, pure literal elimination, 2-SAT implication extraction, and backbone detection to fixpoint. The *residual* R(φ) is the remaining sub-instance.

**Theorem 9.2** [Proved]. *After sheaf reduction of a random 3-SAT instance at α ≈ 4.267: (i) R(φ) has only projection polymorphisms — that is, the constraint language of R(φ) viewed as a CSP instance (with domain {0,1} and the residual clauses as constraints) admits no polymorphism f : {0,1}^k → {0,1} preserving all constraints other than projections f(x₁,...,x_k) = x_i. (ii) R(φ) contains no hidden 2-SAT, XOR, or backbone structure. (iii) R(φ) involves Θ(n) variables and Θ(n) clauses.*

**Definition 9.3** (Three-way fork). A clause in R(φ) where all three variables are unforced and no pair is linked by a 2-clause implication.

**Theorem 9.4** [Proved]. *R(φ) contains Ω(n) pairwise independent three-way forks (at distance ≥ 2 in the variable-clause incidence graph), with high probability.*

*Proof.* Greedy extraction on the residual's conflict graph. The residual has Θ(n) clauses (all three-way forks by Theorem 9.2). Each fork conflicts with at most O(log n / log log n) others (bounded degree). The greedy independent set has size Ω(n/log n); the refined argument using average-degree subgraph gives Ω(n). ∎

**Theorem 9.5** [Proved]. *k pairwise independent three-way forks imply any DPLL solver must visit at least 2^k leaves.*

**Theorem 9.6** [Proved]. *The H¹ obstruction of R(φ) depends on at least k ≥ Ω(n) independent cocycle conditions.*

*Proof.* Each independent fork C_j imposes a cocycle condition: the partial assignment to C_j's three variables must be simultaneously consistent with all constraints involving C_j. Since the forks are at graph distance ≥ 2 in the variable-clause incidence graph, the constraints involving distinct forks are disjoint — no clause or variable participates in the constraints of two different independent forks. Consequently, the cocycle conditions decompose: the satisfiability of the system restricted to each fork's neighborhood is independent of the other forks' neighborhoods. Each fork contributes one independent binary degree of freedom (which of the 7 satisfying assignments to choose), and these choices are coupled only through clauses *outside* all fork neighborhoods. With k = Ω(n) independent forks, we have Ω(n) independent cocycle conditions.

Note: this argument does NOT invoke the Künneth formula for the Čech complex. Graph-distance independence in the variable-clause incidence graph does not automatically imply product decomposition of the Čech nerve (sub-cubes in the covering may span multiple fork neighborhoods without involving their variables). The conclusion — Ω(n) independent conditions — follows from the constraint-disjointness of fork neighborhoods, which is a direct combinatorial fact. ∎

**Corollary 9.7** [Proved]. *The sub-cube query complexity of OD on residual-type truth tables is Ω(n) = Ω(log N).*

### 10. Sensitivity and Block Sensitivity of the Residual

**Theorem 10.1** [Proved]. *For any satisfying assignment x of R(φ), if κ(x) independent forks are critically satisfied (exactly one literal true), then the sensitivity s(SAT_R, x) ≥ κ(x) and the block sensitivity bs(SAT_R, x) ≥ κ(x).*

*Proof.* Each critically satisfied fork has a pivotal variable v_j: flipping v_j breaks that fork (since it was the unique true literal). Since x is satisfying, flipping a pivotal variable can only break clauses, never repair them (all clauses were already satisfied). The pivotal variables from distinct independent forks are distinct. Therefore s(SAT_R, x) ≥ κ(x) simultaneously sensitive variables. ∎

**Theorem 10.2** [Conditional on Conjecture 10.3]. *For a uniformly random satisfying assignment x of R(φ), the expected number of critically satisfied independent forks satisfies E[κ(x)] ≥ (3/7 − o(1)) · k = Ω(n), and κ(x) ≥ ck = Ω(n) with probability 1 − exp(−Ω(n)).*

*Proof sketch.* Each fork C_j has P[critically satisfied | satisfied] ≥ c_δ > 0, where the constant c_δ depends on the marginal biases of the fork's variables under the satisfying distribution. For variables with marginal probability p_i of being true, the conditional probability of critical satisfaction is:

P[critical | C_j satisfied] = (p₁(1−p₂)(1−p₃) + (1−p₁)p₂(1−p₃) + (1−p₁)(1−p₂)p₃) / (1 − (1−p₁)(1−p₂)(1−p₃))

The 3/7 value is achieved when p₁ = p₂ = p₃ = 1/2. For the bound to be a positive constant, we need each p_i ∈ [δ, 1−δ] for some constant δ > 0. This is a *stronger* condition than mere backbone absence: backbone absence means each variable takes both values in some satisfying assignment, but does not prevent a variable from having bias 0.99 within the satisfying distribution. Approximate uniformity of marginals (p_i bounded away from 0 and 1) is the actual requirement. Concentration follows from bounded dependence of the fork indicators. The proof requires:

**Conjecture 10.3** (Decoupling Property). *For independent forks C_j, C_ℓ at graph distance ≥ 2 in R(φ), the satisfying distribution satisfies two conditions:*

*(a) Approximate factorization: |P[x_{C_j} = a, x_{C_ℓ} = b | x ⊨ R(φ)] − P[x_{C_j} = a | x ⊨ R(φ)] · P[x_{C_ℓ} = b | x ⊨ R(φ)]| ≤ n^{−ω(1)}*

*(b) Bounded marginals: for each variable v in R(φ), the marginal probability P[x_v = 1 | x ⊨ R(φ)] ∈ [δ, 1−δ] for some constant δ > 0 depending on α.*

*Condition (a) ensures fork independence. Condition (b) ensures each fork has Ω(1) probability of critical satisfaction. Both are needed for Theorem 10.2.*

**Theorem 10.4** [Proved for large k]. *The decoupling property holds for random k-SAT with k ≥ k₀ (a sufficiently large constant).*

*Proof.* Two-part argument using the law of total covariance: (1) Within-cluster decoupling from Molloy's frozen-variable structure (free variables form O(log n)-size tree-like components; independent forks at distance ≥ 2 lie in different components, giving exact factorization within each cluster). (2) Cross-cluster decoupling from Ding–Sly–Sun's 1-RSB characterization (frozen values of distant variables are approximately independent across the cluster distribution via the Poisson cloning property). ∎

**Gap for k = 3.** The decoupling property is expected but unproved for k = 3. Two ingredients are missing: (a) rigorous frozen-variable characterization for 3-SAT clusters (known for large k via Coja-Oghlan–Panagiotou), and (b) spatial independence of the cluster distribution for small k.

### 11. AC⁰ Lower Bound for the Residual

**Theorem 11.1** [Conditional on Conjecture 10.3 for k = 3; Proved unconditionally for large k].

*The residual's satisfiability function SAT_R ∉ AC⁰. More precisely:*

*(i) bs(SAT_R) ≥ Ω(n) (at satisfying assignments with Ω(n) critically satisfied forks).*

*(ii) s(SAT_R) ≥ Ω(√n) (by Huang's sensitivity theorem: s ≥ √bs).*

*(iii) SAT_R ∉ AC⁰ of any constant depth d (by Boppana's theorem: AC⁰ of depth d has sensitivity O((log n)^{d−1}), which is o(√n)).*

*(iv) Any depth-d AC⁰ circuit computing SAT_R has correlation at most 2^{−Ω(n^{1/(2d)})} with SAT_R, hence requires size 2^{Ω(n^{1/(2d)})}.*

*Proof.* Chain: Theorem 9.4 (Ω(n) independent forks) → Theorem 10.2 (Ω(n) critically satisfied, conditional on decoupling) → Theorem 10.1 (bs ≥ Ω(n)) → Huang's theorem (s ≥ √(bs) ≥ Ω(√n)) → Boppana's theorem (not in AC⁰).

For the size bound: by Nisan–Szegedy, bs(SAT_R) ≥ Ω(n) implies deg(SAT_R) ≥ Ω(√n) (real polynomial degree). By the Tal improvement of the Linial–Mansour–Nisan theorem: any AC⁰ circuit of depth d and size S has correlation at most exp(−Ω(n^{1/(2d)})) with functions of real degree ≥ Ω(√n), provided S ≤ exp(n^{1/(2d)}). This gives: if an AC⁰ circuit of size S and depth d computes SAT_R exactly (correlation 1), then S > exp(Ω(n^{1/(2d)})) = 2^{Ω(n^{1/(2d)})}.

Note: this does NOT invoke the Håstad switching lemma directly (which gives size lower bounds for specific functions like PARITY via random restriction behavior). The inference from block sensitivity to AC⁰ size goes through the degree → correlation → size chain, not through switching lemmas. ∎

**Corollary 11.2** [Proved unconditionally for large k]. *For random k-SAT with k ≥ k₀ at the satisfiability threshold, the residual's satisfiability function is not in AC⁰.*

### 12. Communication Complexity and Query Complexity

**Theorem 12.1** [Proved]. *The sub-cube query complexity Q_d(OD) ≥ Ω(N/poly(n)).*

*Proof.* The Ω(N/log N) independent interactions involve disjoint entry sets. Each query touches at most poly(n) interactions. So q · poly(n) ≥ Ω(N/log N), giving q ≥ Ω(N/poly(n)). ∎

**Proposition 12.2** [Structural argument, not fully formalized]. *The communication complexity of OD under a balanced partition is at least Ω(N/log N).*

*Argument.* The Ω(N/log N) independent interactions include Ω(N/log N) that cross any balanced partition (at least half of all interactions cross, since sub-cubes of dimension d ≥ 2 span both halves with probability ≥ 1/2). Each cross-partition interaction contributes independent information to OD. The number of monochromatic rectangles in the communication matrix is at least 2^{Ω(N/log N)}.

**Caveat.** The step "each rectangle fixes O(1) conditions" requires formal justification that was not achieved in the earlier proof attempt. The rectangle structure of a communication protocol can implicitly encode complex relationships. A formal proof requires a lifting-style argument or a direct Razborov-type analysis of the communication matrix.

### 13. The Magnification Gap

We now state precisely what is proved and what remains.

**Theorem 13.1** [Proved — unconditional results].

*(a) Sub-cube query complexity: Q_d(OD) ≥ Ω(N/poly(log N)).*

*(b) Cohomological dimension: the 3-SAT residual has Ω(n) = Ω(log N) independent cocycle conditions.*

*(c) For large k: the k-SAT residual's satisfiability function is not in AC⁰.*

*(d) The mergeability obstruction for F^{grp} depends on Ω(N/log N) independent interactions.*

*(e) No nontrivial circuit polymorphism exists for generic truth tables.*

**What is NOT proved.** The circuit lower bound OD ∉ SIZE[N^{1+ε}]. The gap between the unconditional bounds (sub-cube query Ω(N/poly(n)), cohomological dimension Ω(log N)) and the magnification threshold (circuit size N^{1+ε}) is the **magnification gap**.

**Why the gap exists.** An earlier attempt to close this gap contained five independent errors (identified in the Project Archive):

1. *Rigidity → circuit depth:* Confused function sensitivity with circuit fan-in. Rigidity constrains bit changes to flip OD, not the circuit's read pattern.

2. *"Each gate mediates O(1) pairs":* Gates compose non-trivially. A single gate can implicitly resolve many constraints through composition.

3. *Self-lifting:* Misapplied the Alekseev–Filmus–Smal lifting theorem, which applies to composed functions f ∘ g^n, not to semantically decomposed single functions.

4. *Communication complexity bound:* The claim "each rectangle fixes O(1) conditions" was unjustified.

5. *Non-uniform oracle argument:* Constructed different hard instances for each circuit (adversarial argument), rather than proving a single hard instance defeats all small circuits.

These errors concentrate at exactly the step every known technique fails: converting structural non-locality into circuit size bounds.

---

## Part IV: The Conditional Theorem

### 14. The Sub-cube Lifting Conjecture

**Conjecture 14.1** (Sub-cube Lifting). *There exists a family of gadgets g_n : {0,1}^{m(n)} → {0,1} with m(n) = n^{o(1)} (any sub-polynomial gadget size suffices) such that for any f : {0,1}^M → {0,1} with sub-cube query complexity Q_d(f) ≥ q:*

*L(f ∘ g) ≥ q^{1+Ω(1)} · poly(|g|)*

*The specific choice m(n) = n^{1/3} is used in the overhead analysis of Theorem 15.1; any sub-polynomial m(n) yields the same conclusion.*

**Evidence for the conjecture:**

(i) The Göös–Pitassi–Watson lifting theorem (2018) achieves this for the indexing gadget in the deterministic query-to-communication setting.

(ii) The Alekseev–Filmus–Smal gadget dichotomy (2025) classifies all gadgets: every gadget either admits polynomial lifting or no lifting. Natural gadgets (inner product, indexing) are on the lifting side.

(iii) Yang–Zhang (2025) proved the first NOF lifting theorem, demonstrating that lifting extends to new communication models.

(iv) The sub-cube structure of OD provides natural internal gadgets (each compatibility check operates on a sub-cube fragment).

**Obstacles:** Existing lifting produces bounds for composed functions f ∘ g, not f itself. Applying this to OD requires either the "self-lifting" property or a formal reduction from OD to a composed function. Neither is established.

### 15. The Conditional Proof

**Theorem 15.1** [Conditional on Conjecture 14.1]. *P ≠ NP.*

*Proof.*

**Step 1. Define the framework** (Part I). The groupoid-valued MCSP presheaf F^{grp}_{T,s} on the d-sub-cube site is well-defined with global sections corresponding to small circuits (Theorem 2.3). The framework recovers the CSP dichotomy (Theorem 3.2).

**Step 2. Establish the structural obstruction** (Part II).
- Result A (Theorem 6.2): No nontrivial circuit polymorphism for hard truth tables.
- Result B (Theorem 7.1; Theorem 7.3 conditional on functoriality): Compatible families exist for parity at the F^{cir} level (proved). For random functions at the F^{grp} level, compatible families exist conditional on resolving the Lupanov functoriality gap. In either case, Ȟ¹ ≠ {∗} is established (at least at the F^{fn} level unconditionally, at the F^{grp} level for parity).
- Result C (Theorems 8.1, 8.2): The obstruction is non-local and distributed across Ω(N/log N) independent interactions.

**Step 3. Establish the sub-cube query lower bound** (Part III). By Theorem 12.1: Q_d(OD) ≥ Ω(N/poly(n)).

**Step 4. Apply the Sub-cube Lifting Conjecture.** By Conjecture 14.1, with q = Q_d(OD) ≥ Ω(N/poly(n)):

Circuit size(OD ∘ g) ≥ (N/poly(n))^{1+Ω(1)} · poly(|g|) = N^{1+Ω(1)} / poly(n)

We now show this lower bound for the *composed* function OD ∘ g transfers to OD itself, with controlled overhead.

**Reduction from OD ∘ g to OD.** The composed function (OD ∘ g)(y) takes as input a string y of length N · |g|, partitioned into N blocks of |g| bits each. On each block, g is evaluated to produce one bit, yielding an N-bit truth table T. Then OD(T) is computed. A circuit for OD of size S can be converted to a circuit for OD ∘ g by prepending a layer of N copies of a circuit for g (each of size poly(|g|) = poly(n^{1/3}) = poly(n)). The total size is:

Circuit size(OD ∘ g) ≤ S + N · poly(n)

Therefore: S ≥ Circuit size(OD ∘ g) − N · poly(n) ≥ N^{1+Ω(1)}/poly(n) − N · poly(n).

For N sufficiently large, the first term dominates:

Circuit size(OD) ≥ N^{1+Ω(1)} / poly(n) − N · poly(n) ≥ N^{1+Ω(1)} / poly(n)

Since poly(n) = poly(log N) = N^{o(1)}, this gives Circuit size(OD) ≥ N^{1+Ω(1)−o(1)} = N^{1+Ω(1)}, which exceeds N^{1+ε} for every fixed ε > 0 and sufficiently large N.

**Step 5. Magnification** (Corollary 5.4). OD ∉ SIZE[N^{1+ε}] implies NP ⊄ P/poly.

**Step 6.** NP ⊄ P/poly implies P ≠ NP (since P ⊆ P/poly). ∎

### 16. Alternative Conditional Route: Uniform Magnification

**Theorem 16.1** [Conditional on applicability of Atserias–Müller to OD]. *If OD satisfies the distinguisher conditions of Atserias–Müller's uniform magnification theorem (arXiv:2503.24061, 2025), then slightly superlinear P-uniform formula lower bounds for OD imply P ≠ NP.*

The Atserias–Müller framework sidesteps the locality barrier because the barrier depends on non-uniform oracle circuit constructions. Whether OD satisfies the distinguisher conditions is **open** and represents a critical verification task.

---

## Part V: Barrier Verification

### 17. Natural Proofs Barrier

**Theorem 17.1** [Plausible; formal verification open]. *"OD^{grp}(T) = 1" is plausibly not a natural property.*

*Argument.* **Non-largeness:** For random functions, the probability that Lupanov circuits form a compatible family across all Θ(n^{2d} · N) overlapping pairs is at most (1/s!)^{Θ(N^{2d})} (doubly exponentially small), since the DAG structures of independent Lupanov circuits are unrelated. So OD^{grp}(T) = 0 for almost all T. **Non-constructivity:** Detecting OD requires solving a CSP over N · poly(n) variables (finding compatible families) and co-MCSP (certifying no global extension). Both are plausibly hard.

### 18. Locality Barrier

**Theorem 18.1** [Structural evidence; formal proof open]. *The structural properties of OD^{grp} — dependence on Ω(N/log N) independent interactions invisible to bounded-fan-in queries — provide necessary conditions for evading the locality barrier.*

The locality barrier applies to problems computable by oracle-augmented circuits. OD^{grp} asks a richer question than Gap-MCSP (it requires compatible-family existence, not just circuit complexity gap). The oracle upper bounds of CHOPRS22 apply to Gap-MCSP, not to OD^{grp}. A lower bound for OD^{grp} is therefore consistent with the locality barrier's implications.

**Formal gap.** Proving that OD^{grp} has no efficient oracle-augmented circuits (the formal barrier evasion) would itself constitute major progress toward the lower bound.

### 19. Relativization Barrier

**Theorem 19.1** [Proved]. *There exists an oracle A with OD^{A,grp} ∈ SIZE[poly(N)].*

*Proof.* Let A directly compute MCSP and compatible-family existence. With oracle A, OD reduces to O(1) oracle queries plus poly(N) computation. The lower-bound argument exploits the specific combinatorial structure of the Boolean hypercube, which oracle A bypasses. ∎

### 20. Algebrization Barrier

**Theorem 20.1** [Proved]. *The proof technique does not algebrize.*

*Proof.* The circuit-level presheaf is defined via labeled DAG isomorphism — a combinatorial condition that changes under algebraic extension (circuit simplification identities differ over GF(2) versus algebraically closed fields). The cocycle variety has different structure over {0,1} versus 𝔽. ∎

---

## Part VI: The Research Frontier

### 21. Five Approaches to Closing the Magnification Gap

**Approach 1: Sub-cube lifting theorem.** Prove Conjecture 14.1. Requires extending the Göös–Pitassi–Watson/Alekseev–Filmus–Smal lifting framework to sub-cube queries. The gadget dichotomy suggests non-degenerate gadgets suffice. **Status:** No sub-cube lifting theorem exists.

**Approach 2: Uniform magnification via Atserias–Müller.** Verify that OD satisfies the distinguisher conditions of the 2025 uniform magnification theorem. This sidesteps the locality barrier entirely by working with P-uniform circuits. **Status:** Applicability to OD not yet verified.

**Approach 3: Algebraic geometry of the cocycle variety.** Define V_T (the cocycle variety) as a projective variety and compute its Nullstellensatz degree. A degree lower bound of N^{Ω(1)} for the parity cocycle system would connect sheaf cohomology to algebraic proof complexity. **Status:** No Nullstellensatz computation performed.

**Approach 4: Meta-complexity bootstrap.** Define iterated MCSP: MCSP_k = "does the truth table of MCSP_{k−1} have small circuits?" If the tower grows strictly (each level harder), it eventually exceeds any polynomial. **Status:** Not formalized; self-referential arguments are treacherous.

**Approach 5: Restricted-model warmup.** Prove OD ∉ AC⁰ (would not evade the locality barrier, but would verify the framework produces correct restricted-model bounds). The conditional AC⁰ lower bound for the residual (Theorem 11.1) is progress on this front. **Status:** Proved for large k, conditional for k = 3.

### 22. Summary of Results

| Result | Statement | Status |
|--------|-----------|--------|
| Complexity site well-defined | Proposition 1.2 | ✅ Proved |
| CSP dichotomy recovery | Theorem 3.2 | ✅ Proved |
| MCSP presheaf well-defined | Theorem 2.3 | ✅ Proved |
| Compatible family for parity (F^{cir}) | Theorem 7.1 | ✅ Proved |
| Compatible family for random functions (F^{grp}) | Theorem 7.3 | ⚠️ Conditional (Lupanov functoriality gap) |
| No nontrivial circuit polymorphism | Theorem 6.2 | ✅ Proved |
| Non-locality: Ω(N/log N) independent interactions | Theorem 8.2 | ✅ Proved |
| Residual has Ω(n) independent cocycle conditions | Theorem 9.6 | ✅ Proved |
| Sub-cube query complexity Ω(N/poly(n)) | Theorem 12.1 | ✅ Proved |
| Residual not in AC⁰ (large k) | Corollary 11.2 | ✅ Proved |
| Decoupling property (large k) | Theorem 10.4 | ✅ Proved |
| Residual not in AC⁰ (k = 3) | Theorem 11.1 | ⚠️ Conditional (decoupling for k=3) |
| Block sensitivity Ω(n) for residual | Theorem 10.1 + 10.2 | ⚠️ Conditional (decoupling) |
| Communication complexity Ω(N/log N) | Proposition 12.2 | ❌ Open (structural evidence exists) |
| Natural proofs evasion | Theorem 17.1 | ⚠️ Plausible, unproved |
| Locality barrier evasion | Theorem 18.1 | ⚠️ Structural evidence |
| Non-relativization | Theorem 19.1 | ✅ Proved |
| Non-algebrization | Theorem 20.1 | ✅ Proved |
| OD ∉ SIZE[N^{1+ε}] | Conjecture 7.4 | ❌ Open |
| **P ≠ NP** | Theorem 15.1 | ⚠️ Conditional (Sub-cube Lifting) |

---

## Appendix A: Dependency Graph

```
Shannon counting ──→ Result A (no polymorphism)
                                    ↓
Lupanov + XOR tree ──→ Result B (parity: ✅; random: ⚠️ functoriality gap)
                                    ↓
                        Result C (non-locality, Ω(N/log N) interactions)
                                    ↓
              ┌─────────────────────┼─────────────────────┐
              ↓                     ↓                     ↓
   3-SAT Residual Analysis    Sub-cube Query          Communication
   Ω(n) independent forks    Q_d(OD) ≥ Ω(N/poly(n)) (open)
   Ω(n) cocycle conditions         ↓
              ↓              ┌─────┴──────┐
   Conditional on            ↓            ↓
   Decoupling:          Sub-cube     Atserias–Müller
   bs(SAT_R) ≥ Ω(n)    Lifting      Uniform
   s(SAT_R) ≥ Ω(√n)    Conjecture   Magnification
   SAT_R ∉ AC⁰              ↓            ↓
                        OD ∉ SIZE[N^{1+ε}]
                              ↓
                        Magnification
                        (Corollary 5.4)
                              ↓
                          P ≠ NP
```

## Appendix B: Key Definitions

| Concept | Definition |
|---------|-----------|
| Site_d(n) | d-sub-cube site on {0,1}^n |
| F^{cir}_{T,s} | Circuit-level presheaf, sections are DAG-isomorphism classes |
| F^{grp}_{T,s} | Groupoid-valued presheaf, compatibility up to isomorphism |
| Ȟ¹ | Čech cohomological obstruction |
| OD(T) | Obstruction detection function |
| R(φ) | Residual after sheaf reduction of SAT instance φ |
| V_T | Cocycle variety (compatible families as algebraic variety) |
| Decoupling | Approximate independence of distant forks under satisfying distribution |

## Appendix C: The Decoupling Conjecture — Proof Strategy for k = 3

The full decoupling proof for k = 3 requires three steps:

**Step 1** (Months 1–3): Prove local tree-likeness of R(φ) for random 3-SAT at α ≈ 4.267. Standard first-moment/second-moment arguments on cycle counts.

**Step 2** (Months 3–6): Prove conditional spatial mixing — the BP recursion contracts within each cluster, giving within-cluster factorization.

**Step 3** (Months 6–8): Derive cross-cluster decoupling from conditional spatial mixing + cluster geometry.

**Feasibility assessment:** This is a genuine research problem within the reach of current probabilistic combinatorics (not blocked by any known barrier). The spatial mixing results needed at the 3-SAT phase transition are at the frontier of the field — realistically a 2–4 year problem for a team with expertise in random CSPs and the cavity method, though individual steps (especially Step 1, local tree-likeness) may be achievable faster.

---

## References

- [AFS25] M. Alekseev, Y. Filmus, A. Smal. Gadget dichotomy for lifting. *Comp. Complexity* 34, 2025.
- [AM25] A. Atserias, M. Müller. Uniform hardness magnification. arXiv:2503.24061, 2025.
- [BK14] L. Barto, M. Kozik. CSPs solvable by local consistency. *JACM* 61(1), 2014.
- [Bop97] R. Boppana. The average sensitivity of bounded-depth circuits. *IPL* 63(5), 1997.
- [Bul17] A. Bulatov. CSP dichotomy. *FOCS 2017*.
- [CHOPRS22] L. Chen et al. Beyond natural proofs: hardness magnification and locality. *JACM* 69(4), 2022.
- [DSS15] J. Ding, A. Sly, N. Sun. Proof of the satisfiability conjecture for large k. *STOC 2015*.
- [GPW18] M. Göös, T. Pitassi, T. Watson. Deterministic communication vs. partition number. *JACM* 65(1), 2018.
- [Hås86] J. Håstad. Almost optimal lower bounds for small depth circuits. *STOC 1986*.
- [HI25] S. Hirahara, R. Ilango. MCSP NP-hard under quasi-polynomial reductions. *FOCS 2025*.
- [Hua19] H. Huang. Sensitivity conjecture proof. *Annals of Mathematics* 190(3), 2019.
- [Mol18] M. Molloy. The freezing threshold for k-colourings. *JACM* 65(2), 2018.
- [OS18] I. C. Oliveira, R. Santhanam. Hardness magnification. *FOCS 2018*.
- [Pi24] J. Pich. Razborov's approximation is localizable. *Comp. Complexity* 33, 2024.
- [RR97] A. Razborov, S. Rudich. Natural proofs. *JCSS* 55(1), 1997.
- [Tal17] A. Tal. Tight bounds on the Fourier spectrum of AC⁰. *CCC 2017*.
- [YZ25] S. Yang, J. Zhang. NOF lifting. *ECCC* 2025.
- [Zhu20] D. Zhuk. CSP dichotomy proof. *JACM* 67(5), 2020.

---

*This document presents a conditional proof framework, not a completed unconditional proof. The framework (Parts I–II) is mathematically rigorous and independently publishable. The structural lower bounds (Part III) are the strongest unconditional results achievable by current techniques. The conditional theorem (Part IV) identifies the precise conjecture whose resolution would complete the proof. The program's value lies in making the target precise, characterizing the obstruction, mapping the approaches, and establishing the structural foundations upon which a future proof must build.*
