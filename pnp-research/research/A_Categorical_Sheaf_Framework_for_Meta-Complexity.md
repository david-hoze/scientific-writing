# A Categorical Sheaf Framework for Meta-Complexity

**A Definitions Paper**

---

**Abstract.** We define a single mathematical object — the *complexity presheaf* of a computational problem — that unifies three existing formalisms: Basu and Isik's categorical complexity, measuring diagram complexity via limits and colimits of basic morphisms; Ó Conghaile's presheaf formalization of constraint satisfaction, casting CSP satisfiability as global section existence; and the Bulatov–Zhuk polymorphism theory, characterizing tractability of finite-domain CSPs through algebraic descent conditions. For constraint satisfaction problems, we prove the framework recovers the CSP dichotomy theorem: a constraint language admits polynomial descent — the sheaf-theoretic analogue of polynomial-time solvability — if and only if it possesses a weak near-unanimity polymorphism. We verify this explicitly for 2-SAT (via majority descent, with the gluing datum written out concretely), XOR-SAT (via Mal'tsev descent despite exponential solution-space clustering), and prove that 3-SAT fails descent at every finite level. We then instantiate the framework for the Minimum Circuit Size Problem (MCSP) using a *circuit-level presheaf* whose sections are actual small circuits — not merely the functions they compute — giving the presheaf a rich multi-section structure analogous to the CSP case. The first Čech cohomology of this enriched presheaf is designed to capture obstructions to *circuit mergeability*: the impossibility of assembling locally small circuits into a globally small circuit while respecting structural compatibility on overlaps. We prove nontrivial H¹ for the functional-equivalence quotient of this presheaf and identify the existence of compatible circuit families for the enriched presheaf as the pivotal open question. We conclude with a precise conjecture connecting the cohomological obstruction to hardness magnification.

**Keywords:** Complexity theory, sheaf theory, constraint satisfaction, categorical complexity, MCSP, hardness magnification, Čech cohomology, polymorphisms, P versus NP.

---

## 1. Introduction

### 1.1. Three formalisms in search of a common framework

The question of whether P equals NP has resisted resolution for over fifty years, but the surrounding mathematical landscape has grown immensely richer. Three independent research programs have developed formal tools that each capture aspects of the intuition that polynomial-time computation has fundamentally limited "resolution" when confronted with NP-complete problems. These programs operate in distinct mathematical languages — category theory, presheaf theory on relational structures, and universal algebra — yet they formalize closely related phenomena.

**Categorical complexity.** Basu and Isik [BI20] introduced a framework in which the complexity of an object in a category C, equipped with a chosen set of basic morphisms, is the minimum number of basic morphisms required in a *diagram computation* — an iterative construction of the object through limits and colimits. When C is the category of Boolean functions with single gates as basic morphisms, this recovers circuit complexity. Their central question — whether the image functor on the morphism category C^{•→•} has polynomially bounded complexity — constitutes a categorical P versus NP problem. Earlier, Basu [Bas15] defined sheaf-theoretic analogues of VP and VNP using constructible sheaves with Euler integration as the push-forward, and stated a sheaf-theoretic P versus NP conjecture. Isik [Isi19] proved NP-completeness of the universal circuit resultant in this geometric setting.

**Presheaves for constraint satisfaction.** Ó Conghaile [ÓC22] formalized both constraint satisfaction and structure isomorphism as problems of deciding the existence of global sections of presheaves ℋ_k(A,B). For relational structures A and B, sections over a subset S ⊆ A of size at most k are the homomorphisms from the induced substructure on S to B. The CSP question "does A map homomorphically to B?" becomes "does ℋ_k(A,B) have a global section?" The success of k-consistency algorithms corresponds to the existence of efficiently computable subpresheaves, and when k-consistency fails, Čech cohomology detects the obstructions to extending local sections globally.

**Polymorphisms as algebraic descent data.** The CSP dichotomy theorem, proved independently by Bulatov [Bul17] and Zhuk [Zhu20], states that for every finite constraint language Γ, the problem CSP(Γ) is either in P or NP-complete. The boundary is purely algebraic: CSP(Γ) is tractable if and only if Γ admits a weak near-unanimity (WNU) polymorphism. Polymorphisms are operations f : D^k → D compatible with all relations in Γ, forming the clone of Γ. Majority polymorphisms enable local consistency propagation; Mal'tsev polymorphisms enable Gaussian elimination; semilattice polymorphisms enable greedy forward chaining. The absence of all nontrivial polymorphisms characterizes NP-completeness.

### 1.2. The unifying construction

This paper defines a single mathematical object — the **complexity presheaf** — that subsumes these three constructions. The key definitions are:

1. A **complexity site** (Definition 3.1): a Grothendieck site whose objects are bounded-size substructures of a problem instance and whose coverings capture the locality of constraint information.

2. A **complexity presheaf** (Definition 4.1): a presheaf on the complexity site whose sections over a substructure U are the locally valid computational states for U, and whose global sections are the solutions to the problem.

3. An **abstract descent condition** (Definition 5.1) and a **polynomial descent condition** (Definition 5.2): a two-level formalization separating the algebraic question of whether gluing maps exist from the computational question of whether they can be found and applied efficiently. For CSPs, these coincide via the Bulatov–Zhuk theorem; for other problems, they may diverge.

4. A **cohomological obstruction** (Definition 6.1): the Čech cohomology H¹ of the complexity presheaf (in the non-abelian, Set-valued sense), whose nontriviality witnesses the failure of descent.

5. A **categorical complexity measure** (Definition 7.1): the Basu–Isik diagram complexity of computing H⁰ (finding a global section) and H¹ (certifying infeasibility) of the complexity presheaf.

We prove that this framework recovers the CSP dichotomy theorem (Theorem 5.5), verify the sheaf condition for 2-SAT (Theorem 5.6) and XOR-SAT (Theorem 5.7), prove descent failure for 3-SAT (Theorem 5.8), and establish that the MCSP presheaf is well-defined with its global section question equivalent to the MCSP decision problem (Theorem 4.8). We construct explicit truth tables with nontrivial H¹ for the functional-equivalence presheaf (Theorem 6.3) and identify compatible-family existence for the circuit-level presheaf as the pivotal open question for the framework.

### 1.3. Why MCSP is the right target

The Minimum Circuit Size Problem — given a truth table of length N = 2^n and a size bound s, decide whether a Boolean circuit of size at most s computes this function — occupies a unique position in complexity theory for three reasons.

First, **hardness magnification** (Oliveira–Santhanam [OS18], McKay–Murray–Williams [MMW19], Chen–Hirahara–Oliveira–Pich–Rajgopal–Santhanam [CHOPRS22]): barely-superlinear circuit lower bounds for certain MCSP variants in weak models would imply P ≠ NP and simultaneously rule out natural proofs. Specifically, if Gap-MCSP[s₁, s₂] cannot be computed by circuits of size N^{1+ε}, then NP ⊄ P/poly.

Second, **meta-complexity and self-referential hardness**: Liu and Pass [LP20] proved that one-way functions exist if and only if time-bounded Kolmogorov complexity K^t is mildly hard on average. Hirahara [Hir23] extended this, showing one-way functions exist if and only if NP is worst-case hard and distributional Kolmogorov complexity is NP-hard. This means MCSP hardness is intimately connected to the natural proofs barrier itself — if MCSP is easy, the barrier dissolves; if MCSP is hard, the barrier persists but meta-complexity techniques may circumvent it through non-natural proof strategies.

Third, **non-naturality**: the cohomological obstruction H¹ for the MCSP presheaf is plausibly *non-large* (it does not hold for a random function's truth table, since random functions require large circuits even on small sub-cubes with high probability) and potentially *non-constructive* (detecting it may itself be computationally hard). This offers a route around the Razborov–Rudich natural proofs barrier [RR97].

### 1.4. What this paper does not do

This paper does not prove P ≠ NP. It does not claim the concluding conjecture is likely true or nearly proved. It does not rely on statistical physics heuristics (shattering, the overlap gap property, replica methods) in any theorem statement, though these phenomena motivate the definitions. It does not conflate the presheaf framework with proof complexity or descriptive complexity — this is a paper about algebraic and categorical structure, not about logical definability. Its contribution is the formal object that makes a precise conjecture *stateable*, and the verification that this object correctly captures known tractability boundaries.

### 1.5. Relationship to prior work

We compare our constructions to existing work throughout the paper. Briefly: Definitions 3.1 and 4.1 generalize Ó Conghaile's presheaf ℋ_k [ÓC22] from CSPs to arbitrary computational problems, particularly MCSP. Definitions 5.1–5.2 reinterpret the Bulatov–Zhuk polymorphism condition [Bul17, Zhu20] as a categorical descent condition on this presheaf, while explicitly separating the algebraic existence of descent data from the computational problem of finding it. Definition 7.1 connects to Basu–Isik [BI20] by measuring the diagram complexity of cohomological computations. The circuit-level MCSP presheaf (Section 4.2), its cohomological analysis (Section 6.2), and the magnification conjecture (Section 7.3) are new contributions of this paper.

The topological dichotomy of Schnider and Weber [SW24] — that NP-complete Boolean CSPs are exactly the projection-universal ones, while tractable CSPs have topologically trivial solution spaces — provides independent confirmation that the sheaf-theoretic perspective captures the P/NP boundary for CSPs. The #P-hardness of sheaf cohomology computation (Schmid–Schreyer [SS98]) confirms that the framework's machinery has sufficient power to encode hard problems.

The Li–Schramm result [LS24], showing that the shortest-path problem exhibits the overlap gap property yet is polynomial-time solvable, is a critical test case. Our framework handles this correctly: the relevant distinction is not solution-space geometry (OGP, clustering) but algebraic structure (polymorphisms enabling descent). Shortest path possesses optimal substructure — a form of algebraic descent — that 3-SAT lacks. See Section 8.2 for a detailed discussion.

---

## 2. Preliminaries

We fix notation and recall the necessary background from category theory, constraint satisfaction, and circuit complexity. The reader fluent in all three areas may proceed to Section 3.

### 2.1. Category theory: sites, presheaves, and Čech cohomology

**Categories and functors.** A *category* C consists of a class of objects Ob(C), for each pair of objects X, Y a set of morphisms Hom_C(X, Y), and for each object X an identity morphism id_X, subject to associativity and identity laws for composition. A *functor* F : C → D maps objects to objects and morphisms to morphisms, preserving composition and identities. A *contravariant functor* (or equivalently, a functor C^{op} → D) reverses the direction of morphisms.

**Presheaves.** Let C be a small category. A *presheaf* on C (with values in Set) is a functor F : C^{op} → Set. For each object U ∈ Ob(C), the set F(U) is called the set of *sections* over U. For each morphism ι : V → U in C (thought of as an inclusion), the induced map F(ι) : F(U) → F(V) is called *restriction* and is denoted s ↦ s|_V. The presheaf axioms require F(id_U) = id_{F(U)} and F(ι ∘ κ) = F(κ) ∘ F(ι) for composable morphisms.

**Grothendieck sites and topologies.** A *Grothendieck topology* J on a small category C assigns to each object U a collection J(U) of *covering families* — sets of morphisms {U_i → U}_{i ∈ I} — subject to three axioms:

(i) *Identity*: For every object U, the singleton family {id_U : U → U} is in J(U).

(ii) *Stability (base change)*: If {U_i → U}_{i ∈ I} is in J(U) and g : V → U is any morphism, then the pullback family {U_i ×_U V → V}_{i ∈ I} exists and is in J(V).

(iii) *Transitivity (local character)*: If {U_i → U}_{i ∈ I} is in J(U), and for each i the family {V_{ij} → U_i}_{j ∈ J_i} is in J(U_i), then the composite family {V_{ij} → U}_{i,j} is in J(U).

A pair (C, J) is called a *site*.

**Sheaves.** A presheaf F on a site (C, J) is a *sheaf* if for every covering family {U_i → U}_{i ∈ I} in J(U) and every compatible family of sections s_i ∈ F(U_i) (meaning s_i|_{U_i ∩ U_j} = s_j|_{U_i ∩ U_j} for all i, j), there exists a unique section s ∈ F(U) with s|_{U_i} = s_i for all i. The sheaf condition decomposes into two parts: *separatedness* (at most one such s) and *gluing* (at least one such s).

**Čech cohomology for Set-valued presheaves.** Let F be a presheaf on a site (C, J) taking values in Set, and let 𝒰 = {U_i → U}_{i ∈ I} be a covering family. Since F takes values in Set rather than an abelian category, the standard cochain complex with alternating-sum coboundary maps is not available. We use the following combinatorial formulation.

The *Čech 0-cochains* are Č⁰(𝒰, F) = ∏_i F(U_i) — families of local sections, one per covering element. A 0-cochain (s_i)_{i ∈ I} is a *0-cocycle* if for all pairs i, j the restrictions agree: s_i|_{U_i ∩ U_j} = s_j|_{U_i ∩ U_j}. The set of 0-cocycles is Ž⁰(𝒰, F), and when F is separated, this is identified with the set of global sections F(U).

The *Čech 1-cocycles* (in the non-abelian sense) are families (g_{ij})_{i,j} where g_{ij} ∈ F(U_i ∩ U_j) satisfying a cocycle condition on triple overlaps. For Set-valued presheaves, the relevant notion reduces to: a *compatible family* is a 0-cochain (s_i) such that s_i|_{U_i ∩ U_j} = s_j|_{U_i ∩ U_j} for all i, j. The cohomological obstruction Ȟ¹ is then defined as:

- Ȟ¹(𝒰, F) = {∗} (the trivial pointed set) if every compatible family of local sections arises from a global section (the presheaf satisfies gluing over 𝒰).
- Ȟ¹(𝒰, F) ≠ {∗} if there exists a compatible family that does not arise from any global section (gluing fails over 𝒰).

The *Čech cohomology of the site* is Ȟ¹(U, F) = colim_{𝒰 ∈ J(U)} Ȟ¹(𝒰, F). We write Ȟ¹(U, F) ≠ {∗} to mean that for every covering 𝒰 ∈ J(U), gluing fails.

**Remark 2.1** (Limitations of Set-valued cohomology). Because F takes values in Set rather than in an abelian category, the standard tools of abelian sheaf cohomology — long exact sequences, Mayer-Vietoris, spectral sequences — are not directly available. Richer cohomological frameworks exist for non-abelian coefficients: Giraud's H¹ classifying torsors [Gir71], and the simplicial cohomology of Čech nerves. For the CSP presheaf, where sections form a set of partial homomorphisms with no natural group structure, the pointed-set formulation suffices to detect the primary obstruction (gluing failure). Extending to richer coefficient categories — for instance, taking sections in a groupoid of circuits modulo natural equivalences — is a direction for future work (see Section 8.4).

### 2.2. Constraint satisfaction problems

**Relational structures.** A *relational signature* σ is a set of relation symbols, each with an associated arity. A *σ-structure* A consists of a finite domain (or universe) A = dom(A) together with, for each relation symbol R ∈ σ of arity r, a relation R^A ⊆ A^r.

**CSPs.** For a fixed finite σ-structure B (the *template*), the *constraint satisfaction problem* CSP(B) asks: given a σ-structure A (the *instance*), does there exist a homomorphism h : A → B, i.e., a map h : dom(A) → dom(B) such that for every relation symbol R ∈ σ of arity r and every tuple (a₁, ..., a_r) ∈ R^A, we have (h(a₁), ..., h(a_r)) ∈ R^B?

Equivalently, CSP(B) can be formulated in terms of a *constraint language* Γ = {R^B : R ∈ σ}: given variables V and constraints (each a pair of a scope — a tuple of variables — and a relation from Γ), find an assignment V → dom(B) satisfying all constraints.

**Polymorphisms.** A *polymorphism* of a relational structure B is a homomorphism f : B^k → B for some k ≥ 1, where B^k is the direct product structure (relations applied coordinatewise). Equivalently, f : dom(B)^k → dom(B) is a polymorphism if for every relation R^B of arity r and every k tuples t₁, ..., t_k ∈ R^B, the tuple obtained by applying f coordinatewise also lies in R^B. The set of all polymorphisms of B, under composition, forms the *polymorphism clone* Pol(B).

**Weak near-unanimity (WNU) polymorphisms.** A polymorphism w : D^k → D (k ≥ 2) is *weak near-unanimity* if w(x, x, ..., x) = x for all x ∈ D (idempotency) and w(y, x, ..., x) = w(x, y, x, ..., x) = ··· = w(x, ..., x, y) for all x, y ∈ D. A *majority* operation is a WNU of arity 3: m(x, x, y) = m(x, y, x) = m(y, x, x) = x. A *Mal'tsev* operation satisfies p(x, y, y) = p(y, y, x) = x.

**The CSP dichotomy theorem** (Bulatov [Bul17], Zhuk [Zhu20]): For every finite relational structure B, CSP(B) is in P if Pol(B) contains a WNU polymorphism, and CSP(B) is NP-complete otherwise.

**k-Consistency.** For k ≥ 1, the *k-consistency* algorithm iteratively extends partial assignments on subsets of at most k variables, removing those that cannot be extended to subsets of size k + 1. The instance is *k-consistent* if this process terminates without removing all assignments from any scope. For structures with WNU polymorphisms of sufficient arity, bounded levels of consistency suffice to solve the CSP (this is the algorithmic content of the dichotomy theorem for bounded-width cases).

### 2.3. Circuit complexity and MCSP

**Boolean circuits.** A *Boolean circuit* C on n inputs is a directed acyclic graph with n source nodes (inputs x₁, ..., x_n), internal nodes (gates) labeled by Boolean operations (AND, OR, NOT), and one or more sink nodes (outputs). The *size* of C, denoted |C|, is the number of gates. C computes a function f_C : {0,1}^n → {0,1} in the natural way.

**Circuit restrictions.** Given a circuit C on n inputs and an inclusion of sub-cubes (S₁, α₁) ↪ (S₂, α₂) (i.e., the (S₁, α₁)-sub-cube is contained in the (S₂, α₂)-sub-cube), the *hardwired restriction* C|_{S₁,α₁} is the circuit obtained by fixing the inputs in S₂ \ S₁ to the values prescribed by α₁ and eliminating the resulting constant gates. This produces a circuit on |S₁| inputs with |C|_{S₁,α₁}| ≤ |C|.

**Truth tables.** The *truth table* of a function f : {0,1}^n → {0,1} is the binary string tt(f) = (f(0...00), f(0...01), ..., f(1...11)) of length N = 2^n.

**The Minimum Circuit Size Problem (MCSP).** The decision problem MCSP is: given a truth table T ∈ {0,1}^N (where N = 2^n) and a size bound s ∈ ℕ, does there exist a Boolean circuit of size at most s that computes the function with truth table T?

**Gap-MCSP.** For functions s₁(n) < s₂(n), the problem Gap-MCSP[s₁, s₂] asks to distinguish truth tables of functions with circuit complexity at most s₁(n) from those with circuit complexity at least s₂(n). Instances with circuit complexity between s₁ and s₂ may be answered either way.

**Sub-cubes.** For a subset S ⊆ [n] = {1, ..., n} and an assignment α : [n] \ S → {0,1} to the variables outside S, the *(S, α)-sub-cube* is the set of inputs {x ∈ {0,1}^n : x_i = α(i) for all i ∉ S}. This sub-cube has dimension |S| and cardinality 2^{|S|}. The *restriction* of a function f to the (S, α)-sub-cube is the function f_{S,α} : {0,1}^{|S|} → {0,1} defined by f_{S,α}(y) = f(x) where x_i = y_i for i ∈ S and x_i = α(i) for i ∉ S.

**Hardness magnification** (informally). Results of Oliveira–Santhanam [OS18] and collaborators show that for appropriate parameter settings, proving a lower bound of N^{1+ε} (for any ε > 0) on the circuit complexity of Gap-MCSP would imply NP ⊄ P/poly. The **locality barrier** [CHOPRS22] identifies why existing techniques fail to achieve even this modest bound: known lower-bound methods (random restrictions, the approximation method, switching lemmas) extend to circuits with small-fan-in oracle gates, and the target problems have efficient circuits with exactly such gates.

---

## 3. The Complexity Site

We define a Grothendieck site that captures the locality structure of computational problems. We give two instantiations: one for CSPs (generalizing the k-local structure underlying Ó Conghaile's presheaves [ÓC22]) and one for MCSP (capturing the sub-cube structure of truth tables).

### 3.1. The CSP site

Let A be a σ-structure with universe A = {a₁, ..., a_n}. For a parameter k ≥ 1, we define the *k-local CSP site* Site_k(A).

**Definition 3.1** (k-local CSP site). Fix a σ-structure A with |A| = n and an integer k ≥ 1. Define the category C_k(A) as follows:

- *Objects*: the empty set ∅, subsets S ⊆ A with 1 ≤ |S| ≤ k, and the full set A.
- *Morphisms*: inclusions ι_{S,T} : S ↪ T for S ⊆ T. (C_k(A) is a full subcategory of the poset (𝒫_k(A) ∪ {A, ∅}, ⊆) viewed as a category. The empty set ∅ is the initial object.)

Define the *k-local Grothendieck topology* J_k on C_k(A) as follows. A family {S_i ↪ T}_{i ∈ I} is a covering of T if:
- ⋃_{i ∈ I} S_i = T (the sub-objects cover T set-theoretically), and
- for every constraint scope (a_{j₁}, ..., a_{j_r}) in A with {a_{j₁}, ..., a_{j_r}} ⊆ T, there exists some i ∈ I with {a_{j₁}, ..., a_{j_r}} ⊆ S_i (every constraint within T is "seen" by some covering element).

The pair Site_k(A) = (C_k(A), J_k) is the k-local CSP site.

**Proposition 3.2.** *Site_k(A) is a well-defined Grothendieck site. That is, J_k satisfies the identity, stability, and transitivity axioms.*

*Proof.*

*(Identity.)* For any object T, the singleton family {id_T : T → T} covers T trivially: T = T, and every constraint scope within T is contained in T.

*(Stability.)* Let {S_i ↪ T}_{i ∈ I} be a covering of T and let g : T' ↪ T be an inclusion (i.e., T' ⊆ T). The pullback S_i ×_T T' is S_i ∩ T'. This intersection is either empty — in which case the pullback is the initial object ∅, which is an explicit object of C_k(A) with F(∅) = {∗} — or a nonempty subset of size |S_i ∩ T'| ≤ |S_i| ≤ k. In either case the pullback exists in C_k(A). We show {S_i ∩ T' ↪ T'}_{i ∈ I} covers T'. Set-theoretically, ⋃_i (S_i ∩ T') = (⋃_i S_i) ∩ T' = T ∩ T' = T'. For constraint scopes: if (a_{j₁}, ..., a_{j_r}) is a constraint scope with {a_{j₁}, ..., a_{j_r}} ⊆ T', then since T' ⊆ T, there exists i with {a_{j₁}, ..., a_{j_r}} ⊆ S_i, whence {a_{j₁}, ..., a_{j_r}} ⊆ S_i ∩ T'.

*(Transitivity.)* Let {S_i ↪ T}_{i ∈ I} cover T, and for each i let {V_{ij} ↪ S_i}_{j ∈ J_i} cover S_i. The composite family {V_{ij} ↪ T}_{i,j} covers T set-theoretically since ⋃_{i,j} V_{ij} = ⋃_i S_i = T. For constraint scopes: if scope σ ⊆ T, then σ ⊆ S_i for some i, and since {V_{ij}} covers S_i, we have σ ⊆ V_{ij} for some j. ∎

**Remark 3.3** (Site size). The category C_k(A) has at most 1 + ∑_{m=1}^{k} \binom{n}{m} + 1 = O(n^k) objects. For fixed k, this is polynomial in the instance size n.

### 3.2. The MCSP site

Let T ∈ {0,1}^N be a truth table of length N = 2^n. We define the *d-sub-cube site* Site_d(T).

**Definition 3.4** (d-sub-cube MCSP site). Fix a truth table T of length N = 2^n and a dimension parameter d ≥ 1. Define the category C_d(T) as follows:

- *Objects*: an initial object ∅_C (the empty sub-cube), pairs (S, α) where S ⊆ [n] with |S| ≤ d and α : [n] \ S → {0,1} (representing sub-cubes of dimension at most d), and the full cube ({[n]}, −) representing {0,1}^n.
- *Morphisms*: inclusions of sub-cubes. There is a morphism (S₁, α₁) → (S₂, α₂) whenever the sub-cube determined by (S₁, α₁) is contained in that of (S₂, α₂): this requires S₁ ⊆ S₂ and α₁|_{[n] \setminus S₂} = α₂|_{[n] \setminus S₂}. There is a unique morphism ∅_C → U for every object U.

Define the *d-local Grothendieck topology* J_d on C_d(T): a family {(S_i, α_i) ↪ (S, α)}_{i ∈ I} covers (S, α) if the union of the corresponding sub-cubes equals the sub-cube of (S, α).

The pair Site_d(T) = (C_d(T), J_d) is the d-sub-cube MCSP site.

**Proposition 3.5.** *Site_d(T) is a well-defined Grothendieck site.*

*Proof.* *(Identity.)* The singleton family {id_{(S,α)}} covers (S, α) trivially.

*(Stability.)* Let {(S_i, α_i) ↪ (S, α)}_{i ∈ I} cover (S, α) and let g : (S', α') ↪ (S, α) be an inclusion. The pullback (S_i, α_i) ×_{(S,α)} (S', α') is the intersection of the sub-cubes determined by (S_i, α_i) and (S', α'). This intersection is either empty — in which case the pullback is the initial object ∅_C, which is an explicit object of C_d(T) — or is itself a sub-cube (S_i ∩ S', β) for appropriate β, of dimension at most min(|S_i|, |S'|) ≤ d. In either case the pullback exists in C_d(T). The covering condition for (S', α') follows from the set-theoretic identity ⋃_i ((S_i\text{-sub-cube}) ∩ (S'\text{-sub-cube})) = (⋃_i S_i\text{-sub-cube}) ∩ (S'\text{-sub-cube}) = (S\text{-sub-cube}) ∩ (S'\text{-sub-cube}) = (S'\text{-sub-cube}).

*(Transitivity.)* Follows from set-theoretic transitivity of unions, as in Proposition 3.2. ∎

**Remark 3.6** (Site size and parameter regime). For fixed d, the number of sub-cubes of dimension at most d in {0,1}^n is ∑_{m=0}^{d} \binom{n}{m} · 2^{n-m}. For d = O(1), this is O(n^d · N), polynomial in N = 2^n. For the magnification application (Section 7), we require d = O(log n) = O(log log N), giving O(n^{O(\log n)} · N) = O(N^{1+o(1)}) objects — still sub-quadratic in N. When d must grow with n, the polynomial bound in N still holds provided d = O(log n / log log n).

---

## 4. The Complexity Presheaf

We define the complexity presheaf over the sites introduced in Section 3, prove functoriality, and establish that global section existence captures the decision problems of interest. For MCSP, we define a *circuit-level presheaf* whose sections are actual small circuits, giving it a rich multi-section structure analogous to the CSP presheaf.

### 4.1. The CSP presheaf

**Definition 4.1** (CSP complexity presheaf). Let A be a σ-structure with universe A, let B be a finite σ-structure (the template), and let k ≥ 1. The *CSP complexity presheaf* F_{A,B} on Site_k(A) is defined by:

- *Sections*: for each object S ∈ Ob(C_k(A)) with S ≠ ∅, set F_{A,B}(S) = Hom_σ(A[S], B), the set of σ-homomorphisms from the induced substructure A[S] to B. Set F_{A,B}(∅) = {∗} (a singleton, the unique empty map).

- *Restriction*: for an inclusion ι : S' ↪ S, the restriction map F_{A,B}(ι) : F_{A,B}(S) → F_{A,B}(S') sends a homomorphism h : A[S] → B to its restriction h|_{S'} : A[S'] → B.

**Proposition 4.2.** *F_{A,B} is a presheaf on Site_k(A).*

*Proof.* Identity: restricting to the same domain is the identity. Functoriality: for S'' ⊆ S' ⊆ S, (h|_{S'})|_{S''} = h|_{S''}, so F_{A,B}(ι_{S'',S'}) ∘ F_{A,B}(ι_{S',S}) = F_{A,B}(ι_{S'',S}). ∎

**Remark 4.3** (Relationship to Ó Conghaile). The presheaf F_{A,B} restricted to objects of size exactly k is precisely the presheaf ℋ_k(A, B) of Ó Conghaile [ÓC22]. Our definition extends it to a presheaf on the full site, including the global object A and the initial object ∅.

**Proposition 4.4** (Global sections and CSP satisfiability). *The CSP instance (A, B) is satisfiable if and only if F_{A,B}(A) ≠ ∅.*

*Proof.* F_{A,B}(A) = Hom_σ(A, B), which is nonempty iff a homomorphism A → B exists. ∎

### 4.2. The MCSP presheaf: circuit-level sections

The CSP presheaf has rich section sets: for each subset S of variables, there may be many distinct partial homomorphisms A[S] → B. This multiplicity gives the presheaf genuine gluing structure — the question of whether compatible local sections assemble into a global section is non-trivial.

For MCSP, the analogous richness requires that sections be *circuits themselves*, not merely the functions they compute. The multiplicity of small circuits computing a given sub-function is the structural analogue of the multiplicity of partial homomorphisms in the CSP presheaf. With this formulation, the gluing question becomes: given compatible small circuits for overlapping sub-cubes, can they be merged into a single small circuit for the union?

**Definition 4.5** (Circuit-level MCSP presheaf). Let T ∈ {0,1}^N encode f : {0,1}^n → {0,1}, let s ∈ ℕ be a circuit size bound, and let d ≥ 1. The *circuit-level MCSP complexity presheaf* F^{cir}_{T,s} on Site_d(T) is defined by:

- *Sections*: for each sub-cube object (S, α), set

  F^{cir}_{T,s}(S, α) = {[C] : C is a Boolean circuit with |C| ≤ s, C has |S| inputs, and f_C = f_{S,α}}

  where [C] denotes the equivalence class of C under *labeled DAG isomorphism* (relabeling internal nodes while preserving the input-output structure and gate labels). Two circuits are identified iff they have the same labeled DAG up to internal node naming. For the initial object, F^{cir}_{T,s}(∅_C) = {∗}.

- *Restriction*: for an inclusion (S₁, α₁) ↪ (S₂, α₂), the restriction map ρ : F^{cir}_{T,s}(S₂, α₂) → F^{cir}_{T,s}(S₁, α₁) sends [C] to [C|_{S₁,α₁}], the equivalence class of the hardwired restriction of C. That is, the inputs of C corresponding to coordinates in S₂ \ S₁ are fixed to the values prescribed by α₁, and the resulting constant gates are propagated and eliminated.

**Proposition 4.6** (Well-definedness). *The restriction map ρ is well-defined: if C computes f_{S₂,α₂} on {0,1}^{|S₂|} and |C| ≤ s, then C|_{S₁,α₁} computes f_{S₁,α₁} on {0,1}^{|S₁|} and |C|_{S₁,α₁}| ≤ s.*

*Proof.* Since the (S₁, α₁)-sub-cube is contained in the (S₂, α₂)-sub-cube, for every input y ∈ {0,1}^{|S₁|}, the extended input (y, α₁|_{S₂ \setminus S₁}) lies in {0,1}^{|S₂|} and maps to the same point of {0,1}^n. Hence C|_{S₁,α₁}(y) = C(y, α₁|_{S₂ \setminus S₁}) = f_{S₂,α₂}(y, α₁|_{S₂ \setminus S₁}) = f_{S₁,α₁}(y). The size bound holds because hardwiring inputs and propagating constants can only reduce circuit size. ∎

**Proposition 4.7.** *F^{cir}_{T,s} is a presheaf on Site_d(T).*

*Proof.* Identity: restricting a circuit to the same sub-cube (hardwiring no inputs) yields the same circuit. Functoriality: for inclusions (S₁, α₁) ↪ (S₂, α₂) ↪ (S₃, α₃), hardwiring first the coordinates of S₃ \ S₂ and then S₂ \ S₁ is the same as hardwiring all coordinates of S₃ \ S₁ at once. ∎

**Theorem 4.8** (Global sections and MCSP). *The MCSP instance (T, s) is a YES-instance if and only if F^{cir}_{T,s}({0,1}^n) ≠ ∅.*

*Proof.* F^{cir}_{T,s}({0,1}^n) = {[C] : |C| ≤ s and C computes f}, which is nonempty iff the circuit complexity of f is at most s. ∎

### 4.3. The presheaf structure of MCSP: richness and gluing

Unlike the functional-equivalence formulation (where each section set is either a singleton or empty), the circuit-level presheaf F^{cir}_{T,s} has genuinely rich section sets. For a sub-cube (S, α) of dimension d, the number of distinct small circuits computing f_{S,α} can be large — many structurally different circuits may implement the same sub-function. This multiplicity is the source of non-trivial presheaf structure.

The **gluing question** for F^{cir}_{T,s} is: given circuits C₁ ∈ F^{cir}_{T,s}(S₁, α₁) and C₂ ∈ F^{cir}_{T,s}(S₂, α₂) that are *compatible on the overlap* (their hardwired restrictions to the intersection sub-cube yield the same circuit equivalence class), does there exist a circuit C ∈ F^{cir}_{T,s}(S, α) for the union sub-cube whose restrictions to (S₁, α₁) and (S₂, α₂) are [C₁] and [C₂] respectively?

This is a non-trivial structural question about circuits. Two compatible local circuits — each small, each correct on its sub-cube, each restricting to the same circuit on the overlap — may fail to merge into a small global circuit. The obstruction is not merely that the global function is complex, but that the *specific circuits* chosen locally are structurally incompatible: they may use different gate arrangements, exploit different properties of their respective sub-cubes, and resist any common generalization of bounded size.

**Remark 4.9** (Comparison to CSP presheaf). The CSP presheaf F_{A,B} assigns to each subset S the set of partial homomorphisms A[S] → B — typically a large set with rich combinatorial structure. Two partial homomorphisms are compatible on the overlap iff they assign the same values to shared variables. The circuit-level MCSP presheaf F^{cir}_{T,s} parallels this: it assigns to each sub-cube the set of small circuits computing the correct sub-function, and compatibility on overlaps requires that hardwired restrictions match. In both cases, the gluing question asks whether compatible local objects (homomorphisms or circuits) can be assembled into a global object, and the answer is controlled by the algebraic/structural properties of the problem.

**Remark 4.10** (The functional-equivalence presheaf as a quotient). Let F^{fn}_{T,s} denote the presheaf obtained by further quotienting sections by functional equivalence: F^{fn}_{T,s}(S, α) = {f_{S,α}} if circuit-complexity(f_{S,α}) ≤ s, and ∅ otherwise. There is a natural transformation F^{cir}_{T,s} → F^{fn}_{T,s} that collapses each section set to at most one element. This quotient destroys the gluing structure: F^{fn}_{T,s} is a sub-presheaf of the terminal presheaf, and its Ȟ¹ reduces to the assertion "locally simple but globally complex." The circuit-level presheaf retains the structural information that F^{fn} discards. All results in the remainder of this paper use F^{cir}_{T,s} unless stated otherwise.

---

## 5. Descent and Polymorphisms

We define the descent condition in two stages — first the abstract algebraic condition, then the computational efficiency condition — prove the framework recovers the CSP dichotomy theorem, and verify descent for 2-SAT and XOR-SAT while proving its failure for 3-SAT.

### 5.1. Abstract and polynomial descent

**Definition 5.1** (Abstract descent). Let F be a presheaf on a site (C, J). We say F satisfies *abstract descent* at level k if for the k-local covering 𝒰_k of the global object, every compatible family of local sections extends to a global section. Equivalently, Ȟ¹(𝒰_k, F) = {∗}.

**Definition 5.2** (Polynomial descent). A presheaf F on a complexity site satisfies *polynomial descent* if there exists a constant k₀ such that:

(a) *Abstract descent holds at level k₀*: for every instance of size n, every compatible family of k₀-local sections extends to a global section.

(b) *Efficient local consistency*: whether a compatible family {s_i} over the k₀-local covering exists can be determined in time poly(n).

(c) *Efficient assembly*: whenever a compatible family {s_i} exists, the global section s can be computed from {s_i} in time poly(n).

The constant k₀ depends only on the template (for CSPs) or problem parameters (for MCSP), not on the instance.

**Remark 5.3** (Why the separation matters). For CSPs, conditions (a), (b), and (c) are simultaneously satisfied when a WNU polymorphism exists: the polymorphism provides both the *existence* of descent data (abstract descent) and the *algorithmic mechanism* for finding and assembling it (polynomial descent). For other problems, these may diverge. A problem might satisfy abstract descent at some level — compatible local sections always extend — without any efficient algorithm for finding the compatible family or performing the assembly. Conversely, a problem might admit efficient local consistency checking but lack abstract descent (compatible families exist but don't extend). This separation is conceptually important for understanding which aspect of tractability the sheaf condition captures.

**Remark 5.4** (Bounded level). Condition (a)'s requirement of a *uniform* constant k₀ is essential. Without it, one could trivially achieve descent by taking k = n (examining the entire instance), yielding an exponential-time algorithm rather than a polynomial one.

### 5.2. Recovery of the CSP dichotomy

**Theorem 5.5** (CSP dichotomy via descent). *Let B be a finite relational structure.*

*(i) If Pol(B) contains a WNU polymorphism, then the CSP complexity presheaf F_{A,B} admits polynomial descent for all instances A.*

*(ii) If Pol(B) contains no WNU polymorphism, then for every k ≥ 1, there exist instances A_k where the presheaf F_{A_k,B} on the k-local site has Ȟ¹ ≠ {∗}: abstract descent fails at every finite level.*

*(iii) Assuming P ≠ NP: if Pol(B) contains no WNU polymorphism, then F_{A,B} does not admit polynomial descent.*

*Proof sketch.*

*(i) WNU polymorphism implies polynomial descent.* Suppose w : dom(B)^m → dom(B) is a WNU polymorphism of B. The Bulatov–Zhuk theorem establishes that there exists a constant k₀, depending on B and the arity of w, such that (2, k₀)-consistency decides CSP(B) in polynomial time. We translate this into the descent framework:

*Condition (a):* For instances decided correctly by k₀-consistency, every compatible family of k₀-local sections extends to a global section. This follows from the algebraic theory: the WNU polymorphism ensures that the k₀-consistency algorithm's output is sound (it rejects only unsatisfiable instances) and complete (it accepts only satisfiable instances, for which a global section exists and can be extracted from the surviving local sections).

*Condition (b):* The k₀-consistency algorithm determines the existence of compatible families in time O(n^{k₀} · |dom(B)|^{k₀}).

*Condition (c):* The Bulatov–Zhuk algorithm constructs a global homomorphism from the consistent local data in polynomial time, using the WNU polymorphism to resolve conflicts: given m compatible local extensions, the WNU collapses them to a single consistent extension via absorption theory.

*(ii) Unbounded width.* If Pol(B) has no WNU polymorphism, then CSP(B) has unbounded width in the sense of Barto and Kozik [BK14]: for every k, there exist instances that are k-consistent yet unsatisfiable. Such an instance A_k provides a compatible family of k-local sections (the surviving partial assignments after k-consistency propagation) that does not extend to any global section (the instance is unsatisfiable). This is a nontrivial element of Ȟ¹ on the k-local site.

*(iii)* If polynomial descent held on some k₀-local site, conditions (a)–(c) would yield a polynomial-time algorithm for CSP(B), contradicting its NP-completeness under P ≠ NP. ∎

### 5.3. Two-SAT: descent via majority

**Theorem 5.6** (Sheaf property of 2-SAT). *For any 2-SAT instance φ with n variables, the CSP complexity presheaf F_φ on the 2-local site satisfies abstract descent (every compatible family extends to a global section when one exists) and polynomial descent (the global section is computable in time O(n + m) via the SCC decomposition of the implication graph).*

*Proof.* A 2-SAT instance φ defines a CSP over {0, 1} with template B₂ admitting the majority polymorphism m(a, b, c) = majority vote.

**The descent datum, concretely.** Consider a compatible family of 2-local sections: for each clause C_j = (ℓ_p ∨ ℓ_q), a partial assignment h_j : {x_p, x_q} → {0, 1} satisfying C_j, such that whenever two clauses share a variable x_k, they agree on x_k's value.

The *majority gluing* works as follows. For each variable x_k appearing in clauses C_{j₁}, ..., C_{j_r}, the compatible family assigns a single value v_k to x_k (compatibility forces agreement). This value is the restriction of each h_{j_i} to x_k. The global section h : {x₁, ..., x_n} → {0, 1} is defined by h(x_k) = v_k. By compatibility, h restricts correctly to each 2-local section.

**Why majority provides the gluing.** The majority polymorphism enters when compatibility must be *enforced*, not merely checked. In the implication graph G_φ, each clause (ℓ_p ∨ ℓ_q) creates implications ¬ℓ_p → ℓ_q and ¬ℓ_q → ℓ_p. The SCC decomposition of G_φ identifies which literals must take the same truth value. If no variable x_k and its negation ¬x_k lie in the same SCC, the instance is satisfiable. Processing SCCs in reverse topological order produces a consistent assignment in O(n + m) time.

The connection to majority: when three satisfying pairs for a clause are combined coordinatewise via majority, the result is again a satisfying pair. This closure under majority is what ensures that the local-to-global extension process (propagating implications through the graph) produces a globally consistent assignment at each step. The implication graph is the *descent structure* — it computes the global section from local data in linear time. ∎

### 5.4. XOR-SAT: Mal'tsev descent despite geometric complexity

**Theorem 5.7** (Sheaf property of XOR-SAT). *For any XOR-SAT instance φ (a system of linear equations over GF(2)), the CSP complexity presheaf F_φ admits polynomial descent via the Mal'tsev polymorphism p(x, y, z) = x ⊕ y ⊕ z, even when the solution space consists of exponentially many well-separated clusters at Hamming distance Ω(n).*

*Proof.* An XOR-SAT instance is a system of equations over GF(2): each clause x_{i₁} ⊕ ··· ⊕ x_{i_r} = b_j. The template admits the Mal'tsev polymorphism p(x, y, z) = x ⊕ y ⊕ z.

**The descent datum, concretely.** Consider two partial solutions h₁ on variable set S₁ and h₂ on S₂, compatible on S₁ ∩ S₂ (i.e., h₁(x) = h₂(x) for all x ∈ S₁ ∩ S₂). The Mal'tsev operation glues them as follows: choose any reference solution h₀ on S₁ ∩ S₂ (for instance, h₀ = h₁|_{S₁ ∩ S₂} = h₂|_{S₁ ∩ S₂}, which agree by compatibility). Define the glued assignment on S₁ ∪ S₂ by:

- h(x) = h₁(x) for x ∈ S₁ \ S₂
- h(x) = h₂(x) for x ∈ S₂ \ S₁
- h(x) = h₁(x) = h₂(x) for x ∈ S₁ ∩ S₂

This is well-defined by compatibility. We verify that h satisfies all constraints with scope in S₁ ∪ S₂. There are three cases:

*Scope entirely within S₁*: the constraint is satisfied because h agrees with h₁ on S₁, and h₁ satisfies all constraints within S₁.

*Scope entirely within S₂*: satisfied analogously via h₂.

*Scope crossing the boundary* (variables in both S₁ \ S₂ and S₂ \ S₁): in the k-local covering (Definition 3.1), every constraint scope is contained in some covering element S_j. If the scope crosses S₁ and S₂, there exists some S_j in the covering that contains the entire scope. The compatible family includes a local solution h_j on S_j satisfying this constraint. Since h agrees with h_j on S_j (by compatibility — h and h_j agree on S_j ∩ S₁ via h₁, and on S_j ∩ S₂ via h₂, and these overlaps cover S_j by the covering condition), h also satisfies the constraint.

The Mal'tsev polymorphism is what guarantees this assembly process is globally consistent. Given any three solutions u, v, w to a system of linear equations over GF(2), p(u, v, w) = u ⊕ v ⊕ w is also a solution, because A(u ⊕ v ⊕ w) = Au ⊕ Av ⊕ Aw = b ⊕ b ⊕ b = b. This closure property ensures that coordinatewise assembly of compatible partial solutions preserves constraint satisfaction across all scopes, not just those within individual covering elements. In particular, extending a compatible family from a k-local covering to a global assignment can be done inductively: at each step, the Mal'tsev operation combines the current partial solution with a new local section, and the result satisfies all constraints visible to either.

**Algorithmic content.** Gaussian elimination on the augmented matrix [A | b] runs in O(n³) time and either finds a solution or certifies unsatisfiability. The Mal'tsev polymorphism is the algebraic structure enabling this: it provides descent data (any three solutions combine into a fourth) that the algorithm exploits to navigate the solution space from any starting point.

**Algebraic structure versus geometric structure.** The solution space u₀ + ker(A) may consist of 2^{n - rank(A)} points forming exponentially many clusters at Hamming distance Ω(n). Yet descent works because the Mal'tsev operation navigates the solution space algebraically, independently of its geometric fragmentation. This directly addresses the lesson of Li and Schramm [LS24]: the OGP and clustering do not determine computational complexity. What determines complexity is whether an algebraic descent mechanism (here, the Mal'tsev polymorphism) exists. ∎

### 5.5. 3-SAT: descent failure

**Theorem 5.8** (Failure of descent for 3-SAT). *For the 3-SAT template (equivalently, CSP over {0,1} with all possible ternary relations preserved only by dictator polymorphisms), abstract descent fails at every finite level. Formally: for every k ≥ 1, there exist 3-SAT instances φ_k on n_k variables such that the complexity presheaf F_{φ_k} on the k-local site has Ȟ¹ ≠ {∗}.*

*Proof.* The polymorphism clone of the 3-SAT template contains only dictator (projection) polymorphisms. By the Barto–Kozik characterization [BK14], a CSP template has bounded width if and only if it admits a WNU polymorphism. Since the 3-SAT template has only dictator polymorphisms — which are not WNU — 3-SAT has unbounded width.

Concretely, for each k, one constructs a 3-SAT instance φ_k that is k-consistent yet unsatisfiable (see [BK14, Theorem 1.3] for the existence of such instances). The presheaf F_{φ_k} on the k-local site then has:

- F_{φ_k}(U_i) ≠ ∅ for every covering element U_i with |U_i| ≤ k.
- A compatible family exists: k-consistency means local sections on overlapping sets can be chosen consistently.
- F_{φ_k}(A) = ∅: no global section exists (the instance is unsatisfiable).

The compatible family is a nontrivial element of Ȟ¹: it does not arise from any global section. Since this holds for every k, no finite level of locality suffices for abstract descent. ∎

**Remark 5.9** (The structural difference). Theorems 5.6 (2-SAT), 5.7 (XOR-SAT), and 5.8 (3-SAT) formalize the difference between tractable and intractable CSPs in sheaf-theoretic terms. In 2-SAT, each clause creates a deterministic implication — one forced propagation step — and the implication graph (the descent structure) is traversable in linear time. In XOR-SAT, each constraint creates a linear relation, and the Mal'tsev polymorphism provides descent data that Gaussian elimination computes in cubic time, regardless of solution-space geometry. In 3-SAT, each clause creates a three-way branch with no algebraic mechanism to resolve the ambiguity, and no finite level of local consistency suffices.

---

## 6. Cohomological Obstructions

We formalize the obstruction to descent as a cohomological object, first for CSPs (recovering and reinterpreting Ó Conghaile's results) and then for the circuit-level MCSP presheaf (the new contribution).

### 6.1. Definition and interpretation

**Definition 6.1** (Cohomological obstruction). For a complexity presheaf F on a site (C, J) and an object U, the *cohomological obstruction to global solvability over U* is Ȟ¹(U, F) as defined in Section 2.1.

As discussed in Remark 2.1, since F takes values in Set, Ȟ¹ is a pointed set rather than a group. We emphasize the binary distinction: Ȟ¹ = {∗} (descent succeeds, every compatible family extends) versus Ȟ¹ ≠ {∗} (descent fails, some compatible family does not extend).

**Proposition 6.2** (Cohomological characterization of k-consistency). *For a CSP instance (A, B) and the CSP presheaf F_{A,B} on the k-local site:*

*(i) If the instance is satisfiable and k-consistency establishes satisfiability, then Ȟ¹(A, F_{A,B}) = {∗} on the k-local site.*

*(ii) If k-consistency fails to determine satisfiability (the instance is k-consistent but unsatisfiable), then Ȟ¹(A, F_{A,B}) ≠ {∗}.*

*(iii) If k-consistency detects unsatisfiability (some local section set becomes empty during propagation), then Ȟ⁰ of the refined presheaf (after consistency enforcement) is empty, and the obstruction is detected at the level of 0-cochains rather than 1-cocycles.*

*Proof.* This is a restatement of Ó Conghaile's results [ÓC22, Theorems 4.3 and 4.7] in the present notation. ∎

### 6.2. Nontrivial H¹ for the circuit-level MCSP presheaf

We establish the key result for the enriched MCSP presheaf: the existence of truth tables for which local simplicity coexists with global complexity, witnessed by nontrivial first Čech cohomology. We then analyze what the circuit-level formulation adds beyond the functional-equivalence formulation.

**Theorem 6.3** (Nontrivial H¹ for MCSP). *For each of the following circuit models, there exist truth tables T ∈ {0,1}^N and circuit size bounds s such that the MCSP presheaf on the d-sub-cube site (d = O(log n)) has nonempty local section sets for every sub-cube of dimension at most d, yet the global section set is empty:*

*(i) Bounded-depth circuits (AC⁰): The parity function PARITY_n, with s = n^c and the circuit model restricted to AC⁰ circuits of depth c.*

*(ii) General circuits: A random function f, with s = 2^{n/3} and d = n/4.*

*In both cases, Ȟ¹(F^{fn}_{T,s}) ≠ {∗} for the functional-equivalence presheaf. Whether Ȟ¹(F^{cir}_{T,s}) ≠ {∗} for the circuit-level presheaf — which requires the additional condition that a compatible family of circuits exists — is an open question (see below and Open Problem 2 in Section 8.4).*

*Proof.*

**Construction (i): Parity in bounded-depth circuits.** Let f = PARITY_n and fix the circuit model as AC⁰ circuits of depth c for a constant c ≥ 2. Set s = n^c and d = c · log n. For any sub-cube of dimension at most d, the restriction f_{S,α} is parity on at most d = c · log n bits. This function can be computed by a balanced binary tree of XOR gates — equivalently, a depth-O(log d) = O(log log n) circuit of size O(d) = O(c · log n) ≤ s. (For this circuit to lie within AC⁰ of depth c, we require O(log log n) ≤ c, which holds for all sufficiently large n since c is a fixed constant.) So F^{cir}_{T,s}(S, α) ≠ ∅ for every sub-cube of dimension at most d. By Håstad's switching lemma [Hås86], PARITY_n requires AC⁰ circuits of depth c to have size 2^{Ω(n^{1/(c-1)})}, which exceeds n^c for large n. Hence F^{cir}_{T,s}({0,1}^n) = ∅ in the AC⁰-of-depth-c model.

**Construction (ii): Random functions.** For a random function f : {0,1}^n → {0,1}, Shannon's counting argument shows circuit complexity at least 2^n/(2n) with probability 1 − o(1). Set s = 2^{n/3} and d = n/4. For any sub-cube of dimension d = n/4, the restriction f_{S,α} is a function on n/4 bits. Every function on m bits has circuit complexity at most O(2^m / m) by Lupanov's bound, so f_{S,α} has a circuit of size O(2^{n/4} / (n/4)) < 2^{n/3} = s for large n. Hence F^{cir}_{T,s}(S, α) ≠ ∅ for every sub-cube of dimension at most d. But f itself has circuit complexity ≥ 2^n/(2n) ≫ 2^{n/3} = s, so F^{cir}_{T,s}({0,1}^n) = ∅.

In both cases, every local section set F^{cir}_{T,s}(S, α) is nonempty, yet the global section set F^{cir}_{T,s}({0,1}^n) is empty, so Ȟ¹ ≠ {∗}.

We note, however, that this argument establishes nontrivial Ȟ¹ at the level of the functional-equivalence presheaf F^{fn}_{T,s}: the local sections exist (the sub-functions have small circuits) but no global section exists (the function has no small circuit). For F^{fn}, where each section set is a singleton or empty, any family of local sections is automatically compatible (there is at most one section per sub-cube, so the overlap condition is vacuously satisfied whenever restrictions preserve the unique section).

For the circuit-level presheaf F^{cir}_{T,s}, establishing nontrivial Ȟ¹ requires more: one must exhibit a *compatible* family of circuits — one circuit [C_{S,α}] per sub-cube, such that for every pair of overlapping sub-cubes, the hardwired restrictions of C_{S₁,α₁} and C_{S₂,α₂} to the intersection yield the same DAG-isomorphism class. Independently constructed circuits for overlapping sub-cubes will generically restrict to structurally different circuits on the intersection, even if they compute the same function there. Compatible families for F^{cir} may therefore be harder to construct, or may not exist in all cases where F^{fn}-compatible families do.

Whether compatible families for F^{cir} exist in the settings of Constructions (i) and (ii) is an open question whose resolution would clarify the relationship between the two presheaves (see Open Problems 1 and 2 in Section 8.4). If such families exist — for instance, by using a canonical circuit construction (such as Lupanov's method) that produces functorially compatible restrictions — then Ȟ¹(F^{cir}) ≠ {∗} captures strictly more information than Ȟ¹(F^{fn}) ≠ {∗}. If they do not, the obstruction detected by F^{cir} differs qualitatively from that of F^{fn}: the enriched presheaf would detect not only that no small global circuit exists, but that even the space of local circuits lacks the structural coherence needed to pose the gluing question. ∎

**Remark 6.4** (What the circuit-level presheaf adds). For the functional-equivalence presheaf F^{fn}_{T,s}, Theorem 6.3 reduces to the statement "f is locally simple but globally complex" — which is the *definition* of the MCSP gap, restated in sheaf notation. The nontrivial Ȟ¹ for F^{fn} carries no information beyond the bare MCSP decision.

The circuit-level presheaf F^{cir}_{T,s} is designed to capture richer structure. When a compatible family of circuits exists for F^{cir}, a nontrivial element of Ȟ¹ witnesses a *specific collection of structurally coherent local circuits* — each small, each correct, each restricting compatibly with its neighbors — that resists merging into a single small global circuit. Different compatible families may fail to merge for different structural reasons: gate-type incompatibilities, conflicting use of shared sub-circuits, or topological obstructions in the circuit DAG.

This richer obstruction — the *circuit mergeability obstruction* — encodes information about the structure of the function's circuit complexity that the bare MCSP decision does not. However, the enrichment cuts both ways. For F^{fn}, nontrivial Ȟ¹ requires only that every sub-cube has a small circuit while the global function does not — a condition that holds generically. For F^{cir}, nontrivial Ȟ¹ additionally requires that the local circuits can be chosen to agree *structurally* on overlaps: compatible families must exist before they can fail to extend globally. This makes Ȟ¹(F^{cir}) *harder to be nontrivial* than Ȟ¹(F^{fn}). It is logically possible that for some truth tables, Ȟ¹(F^{fn}) ≠ {∗} (locally simple, globally complex) but no compatible circuit family exists for F^{cir} at all — in which case the enriched presheaf's Ȟ¹ would be trivially {∗} (for the vacuous reason that the gluing question cannot even be posed). The implications for the conjecture are ambiguous: a "harder to trigger" obstruction might be easier to prove a lower bound against (fewer instances to handle), or it might be harder to connect to NP-completeness (fewer instances generating the obstruction). Resolving this requires understanding the structure of compatible families, which is the subject of Open Problems 1 and 2 in Section 8.4.

### 6.3. H¹ and the locality barrier: a cautious connection

The cohomological obstruction for MCSP has a structural feature that may be relevant to — but should not be conflated with — the locality barrier for hardness magnification.

**Definition 6.5** (Input-level non-locality of H¹). Say the obstruction Ȟ¹(F^{cir}_{T,s}) is *(k)-input-non-local* if for every constant k, there exist truth tables T₁, T₂ ∈ {0,1}^N with:

(i) F^{cir}_{T₁,s}({0,1}^n) ≠ ∅ (T₁ has circuit complexity ≤ s),

(ii) F^{cir}_{T₂,s}({0,1}^n) = ∅ (T₂ has circuit complexity > s),

(iii) For every sub-cube U of dimension at most k, the section sets F^{cir}_{T₁,s}(U) and F^{cir}_{T₂,s}(U) are both nonempty.

Condition (iii) says that examining any k bits of the truth table reveals a sub-cube on which both T₁ and T₂ have small circuits — the truth tables are locally indistinguishable with respect to the presheaf's section structure.

**Proposition 6.6.** *For s = n^c with c sufficiently large and d = c · log n, the obstruction Ȟ¹(F^{cir}_{T,s}) is (k)-input-non-local for every constant k.*

*Proof.* For constant k, every function on k bits has circuit complexity at most 2^k ≤ s = n^c for large n. Hence for *any* truth table T, every sub-cube of dimension at most k has a small circuit: F^{cir}_{T,s}(U) ≠ ∅ for all such U. Taking T₁ with circuit complexity ≤ s and T₂ with circuit complexity > s (both exist for appropriate s) satisfies all three conditions. ∎

**Remark 6.7** (Distinguishing input-level non-locality from the locality barrier). The locality barrier of Chen–Hirahara–Oliveira–Pich–Rajgopal–Santhanam [CHOPRS22] is a *proof-technique-level* phenomenon: it concerns the extensibility of lower-bound arguments to circuits augmented with oracle gates of bounded fan-in. Proposition 6.6 establishes an *input-level* property: constant-size sub-cubes cannot distinguish easy from hard truth tables.

These two notions are related but not identical. The locality barrier says that certain *proof methods* (random restrictions, switching lemmas, approximation methods) generalize to circuits with oracle gates, and therefore cannot prove lower bounds for problems computable by such circuits. Proposition 6.6 says that the *problem instances* are locally indistinguishable. The former is about the structure of proofs; the latter is about the structure of the problem.

We do not claim that Proposition 6.6 implies that sheaf-theoretic arguments evade the locality barrier. Rather, we observe that the non-local character of Ȟ¹ — its dependence on global interactions among distant sub-cubes — is a necessary (but not sufficient) condition for any argument exploiting it to be non-localizable. Whether it is also sufficient remains open.

---

## 7. Categorical Complexity and Magnification

We connect the presheaf framework to Basu–Isik's categorical complexity and state the main conjecture.

### 7.1. Diagram complexity of presheaf computations

**Definition 7.1** (Categorical complexity of a presheaf computation). Let C be a category equipped with a set of basic morphisms, and let the *diagram complexity* of an object X ∈ Ob(C) be the minimum number of basic morphisms in a diagram computation producing X, in the sense of Basu–Isik [BI20]. When C is the category of Boolean functions with single gates as basic morphisms, diagram complexity equals circuit size.

For a complexity presheaf F on a site, define:

(i) The *diagram complexity of global section existence*, denoted DC⁰(F), as the Basu–Isik diagram complexity of the Boolean function

GS_F : Instances → {0, 1}, GS_F(x) = [F_x(U_{global}) ≠ ∅].

In the Boolean function category, DC⁰(F) equals the minimum circuit size computing GS_F.

(ii) The *diagram complexity of obstruction detection*, denoted DC¹(F), as the Basu–Isik diagram complexity of

OD_F : Instances → {0, 1}, OD_F(x) = [Ȟ¹(F_x) ≠ {∗}].

**Remark 7.2.** For the CSP presheaf F_{A,B}, DC⁰ equals the circuit complexity of CSP(B). For the MCSP presheaf F^{cir}_{T,s}, DC⁰ equals the circuit complexity of MCSP. The measure DC¹ is potentially different from DC⁰: detecting *whether* Ȟ¹ is nontrivial could in principle differ in complexity from deciding whether a global section exists, particularly for the enriched presheaf where the obstruction carries structural information beyond the bare decision.

### 7.2. Connection to hardness magnification

The magnification theorems of Oliveira–Santhanam [OS18] and collaborators provide the bridge from modest lower bounds to major complexity separations.

**Theorem 7.3** (Hardness magnification, informal; [OS18, MMW19, CHOPRS22]). *For appropriate gap parameters s₁ ≪ s₂, if Gap-MCSP[s₁, s₂] cannot be computed by Boolean circuits of size N^{1+ε} for any ε > 0, then NP ⊄ P/poly.*

For the functional-equivalence presheaf, DC¹ is equivalent to the circuit complexity of Gap-MCSP (distinguishing truth tables with small circuits from those with large circuits). For the circuit-level presheaf, DC¹ potentially asks a richer question: detecting not just whether local sections exist without a global section, but whether compatible *circuits* exist locally without a compatible global circuit.

### 7.3. The conjecture

**Conjecture 7.4** (Sheaf-Theoretic Magnification). *For the circuit-level MCSP presheaf F^{cir}_{T,s} on the d-sub-cube site with appropriate parameters, the obstruction detection function*

OD : {0,1}^N → {0,1}, OD(T) = [Ȟ¹(F^{cir}_{T,s}) ≠ {∗}]

*cannot be computed by Boolean circuits of size N^{1+ε} for any ε > 0.*

Via Theorem 7.3, this conjecture would imply NP ⊄ P/poly and hence P ≠ NP.

**Assessment of what the sheaf reformulation provides.** We state frankly what the reformulation does and does not buy.

For the functional-equivalence presheaf F^{fn}, the function OD reduces to Gap-MCSP, and the conjecture is precisely the existing magnification conjecture restated with sheaf notation. In this case, the reformulation provides no proof-theoretic leverage — it is a change of language, not a change of mathematical content.

For the circuit-level presheaf F^{cir}, the situation is potentially different. The function OD asks not merely "is this truth table globally complex?" but "do compatible local circuits exist that resist merging?" This is a question about the *structure of the space of small circuits* — how small circuits for sub-functions relate to each other under restriction — rather than a question about a single function's complexity. Whether this structural enrichment yields genuine proof leverage is the central open question of the framework.

Three features of the circuit-level formulation suggest it may be worth pursuing. First, the presheaf F^{cir} has rich section sets (many structurally distinct circuits per sub-cube), bringing it into structural parallel with the CSP presheaf where the framework demonstrably works. Second, the mergeability obstruction — compatible local circuits that resist global assembly — is a constraint on *circuit transformations*, not just circuit existence, potentially enabling arguments about the process of computation rather than just its endpoint. Third, the (k)-input-non-locality (Proposition 6.6) means that any argument establishing Conjecture 7.4 for the enriched presheaf cannot rely on local input examination, which is a necessary (though not sufficient) condition for evading the locality barrier.

We do not claim these features make the conjecture more tractable. We claim only that the circuit-level presheaf formulation provides a different mathematical object to study — one whose structural properties are not yet fully characterized — and that the characterization of these properties is a well-posed mathematical program regardless of whether it ultimately leads to lower bounds.

---

## 8. Discussion

### 8.1. What the framework does and does not achieve

This paper constructs a unified mathematical object — the complexity presheaf — and proves it correctly captures the tractability boundary for finite-domain CSPs. The sheaf condition (polynomial descent via polymorphisms) is equivalent to the WNU condition from the CSP dichotomy theorem. The explicit verification for 2-SAT (majority descent with the gluing datum written out), XOR-SAT (Mal'tsev descent separating algebraic from geometric structure), and 3-SAT (descent failure at every finite level) demonstrates that the framework handles the known test cases correctly.

The MCSP instantiation is more speculative. The circuit-level presheaf is well-defined and has richer structure than the functional-equivalence formulation, but we have not yet established that this additional structure provides genuine proof-theoretic leverage. The concluding conjecture, for the functional-equivalence presheaf, reduces to the existing magnification conjecture. For the circuit-level presheaf, it asks a potentially richer question whose relationship to the existing conjecture requires further investigation.

### 8.2. The Li–Schramm test

Li and Schramm [LS24] proved that the shortest-path problem in sparse random graphs exhibits the overlap gap property — near-optimal paths in independently sampled graphs are nearly disjoint — yet shortest path is solvable in polynomial time. This demonstrates that solution-space geometry (OGP, clustering, shattering) does not determine computational complexity.

Our framework handles this correctly. Shortest path possesses *optimal substructure*: the shortest path from s to t decomposes as shortest sub-paths, and these sub-paths glue via the Bellman equation. In our language, the presheaf of locally optimal sub-paths satisfies polynomial descent — local optimality information assembles into global optimality — even though the path space may be geometrically wild.

For 3-SAT, no analogous descent mechanism exists. The three-way branching at each clause provides no algebraic operation to resolve the ambiguity. The framework correctly classifies shortest path as tractable and 3-SAT as intractable, despite both exhibiting complex solution-space geometry.

### 8.3. Relationship to known barriers

**Natural proofs.** The Razborov–Rudich barrier [RR97] shows that no "natural" proof can prove NP ⊄ P/poly, assuming pseudorandom functions exist. A natural proof identifies an efficiently testable property holding for most hard functions. The cohomological obstruction Ȟ¹ is plausibly non-natural: it may be *non-large* (random functions do not exhibit the specific pattern of local simplicity required, since random restrictions of random functions are themselves hard with high probability) and *non-constructive* (detecting Ȟ¹ nontriviality is closely related to MCSP, which may itself be hard). If the obstruction is non-natural, the barrier does not apply.

**Relativization and algebrization.** The Baker–Gill–Solovay [BGS75] and Aaronson–Wigderson [AW09] barriers block techniques that relativize or algebrize, respectively. The sheaf-theoretic framework uses categorical and topological structure specific to Boolean circuits, which does not obviously relativize or algebrize. However, this must be verified explicitly — it is not sufficient to observe that the definitions use non-standard mathematical language. A rigorous verification would require constructing oracle or algebraic-extension versions of the complexity site and presheaf and checking whether the theorems survive.

**Bounded arithmetic.** Pich and Santhanam [PS23, PS24] show that weak theories of bounded arithmetic cannot prove strong circuit lower bounds. A sheaf-theoretic proof would likely require formalization in a stronger theory — potentially escaping these provability barriers.

### 8.4. Open problems

1. **The pivotal question: separating the two presheaves.** As Theorem 6.3 and the subsequent discussion make clear, the central question for the MCSP instantiation is whether the circuit-level presheaf F^{cir} captures strictly more information than the functional-equivalence presheaf F^{fn}. Concretely: is there a truth table T and parameter s such that Ȟ¹(F^{fn}_{T,s}) ≠ {∗} but no compatible family of circuits exists for F^{cir}_{T,s} (or vice versa)? If the two presheaves yield identical Ȟ¹ for all instances, the enrichment is a mathematical curiosity with no proof-theoretic consequence. If they differ, the circuit-level formulation captures genuine structural information about the space of small circuits — information that is invisible to the bare MCSP decision problem and could potentially provide leverage for lower bounds. Resolving this question requires understanding when canonical circuit constructions (Lupanov's method, Shannon-style counting constructions) produce functorially compatible restrictions.

2. **Compatible family existence.** A prerequisite for the above: for the truth tables in Theorem 6.3 (parity in AC⁰, random functions in general circuits), do compatible families for F^{cir} exist? Lupanov's optimal circuit construction is systematic enough that hardwired restrictions may yield DAG-isomorphic circuits on overlapping sub-cubes. If so, this would establish Ȟ¹(F^{cir}) ≠ {∗} for concrete functions.

3. **Enriching the coefficient category.** The Set-valued presheaf formulation limits the cohomological toolkit. Taking sections in a groupoid of circuits (with morphisms given by natural circuit transformations — gate relabeling, subcircuit substitution) would give F values in a category where Giraud's non-abelian cohomology applies, classifying H¹ elements as torsors. This could provide finer invariants of the mergeability obstruction.

4. **Characterizing the mergeability obstruction.** For the circuit-level presheaf, what determines whether compatible local circuits merge into a small global circuit? When is the obstruction merely a size blowup (the merged circuit exists but is too large) versus a structural impossibility (no circuit of any size extends the local circuits compatibly)? The answer likely depends on the circuit class.

5. **Higher cohomology.** What role does H² play? In classical sheaf theory, H² classifies obstructions to lifting descent data. For the circuit-level presheaf, H² might capture second-order mergeability failures: situations where local circuits merge pairwise but the merged pairs resist further assembly.

6. **Extending beyond CSPs.** Can the framework capture graph isomorphism (via Ó Conghaile's isomorphism presheaf ℐ_k), optimization problems, or counting problems? For counting, the presheaf should take values in an abelian category, enabling the full cohomological toolkit.

7. **The completeness question.** Does descent failure on the sub-cube site *characterize* MCSP hardness, or merely detect it? A problem might have small circuit complexity yet fail descent on the sub-cube site (because the efficient circuit is not built by merging sub-cube circuits). Understanding completeness is essential for the framework's long-term viability.

---

## References

- [AW09] S. Aaronson and A. Wigderson. Algebrization: a new barrier in complexity theory. *ACM Transactions on Computation Theory*, 1(1):1–54, 2009.
- [Bas15] S. Basu. A complexity theory of constructible functions and sheaves. *Foundations of Computational Mathematics*, 15(1):199–279, 2015.
- [BI20] S. Basu and M. U. Isik. Categorical complexity. *Forum of Mathematics, Sigma*, 8:e34, 2020.
- [BGS75] T. Baker, J. Gill, and R. Solovay. Relativizations of the P =? NP question. *SIAM Journal on Computing*, 4(4):431–442, 1975.
- [BK14] L. Barto and M. Kozik. Constraint satisfaction problems solvable by local consistency methods. *Journal of the ACM*, 61(1):3:1–3:19, 2014.
- [Bul17] A. Bulatov. A dichotomy theorem for nonuniform CSPs. In *Proceedings of FOCS 2017*, pages 319–330, 2017.
- [CHOPRS22] L. Chen, S. Hirahara, I. C. Oliveira, J. Pich, N. Rajgopal, and R. Santhanam. Beyond natural proofs: hardness magnification and locality. *Journal of the ACM*, 69(4):25:1–25:49, 2022.
- [Gir71] J. Giraud. *Cohomologie non abélienne*. Grundlehren der mathematischen Wissenschaften 179. Springer, 1971.
- [Hås86] J. Håstad. Almost optimal lower bounds for small depth circuits. In *Proceedings of STOC 1986*, pages 6–20, 1986.
- [Hir23] S. Hirahara. NP-hardness of learning programs and partial MCSP. In *Proceedings of STOC 2023*, pages 968–981, 2023.
- [HI25] S. Hirahara and R. Ilango. MCSP is NP-hard under quasi-polynomial reductions. In *Proceedings of FOCS 2025*, 2025.
- [Isi19] M. U. Isik. Complexity classes and completeness in algebraic geometry. *Foundations of Computational Mathematics*, 19(2):245–280, 2019.
- [LP20] Y. Liu and R. Pass. On one-way functions and Kolmogorov complexity. In *Proceedings of FOCS 2020*, pages 1243–1254, 2020.
- [LS24] A. Li and T. Schramm. Some easy optimization problems have the overlap-gap property. arXiv:2411.01836, 2024.
- [MMW19] D. M. McKay, C. D. Murray, and R. R. Williams. Weak lower bounds on resource-bounded compression imply strong separations. In *Proceedings of STOC 2019*, pages 1215–1225, 2019.
- [ÓC22] C. Ó Conghaile. Cohomology in constraint satisfaction and structure isomorphism. In *Proceedings of MFCS 2022*, LIPIcs 241, pages 73:1–73:16, 2022.
- [OS18] I. C. Oliveira and R. Santhanam. Hardness magnification for natural problems. In *Proceedings of FOCS 2018*, pages 65–76, 2018.
- [PS23] J. Pich and R. Santhanam. Why are proof complexity lower bounds hard? *Journal of the ACM*, 70(1):1–42, 2023.
- [PS24] J. Pich and R. Santhanam. Circuit lower bounds in bounded arithmetics. *Annals of Pure and Applied Logic*, 175(1):103341, 2024.
- [RR97] A. A. Razborov and S. Rudich. Natural proofs. *Journal of Computer and System Sciences*, 55(1):24–35, 1997.
- [SS98] T. Schmid and F.-O. Schreyer. Computing sheaf cohomology. In *Algorithmic Algebra and Number Theory*, Springer, pages 283–294, 1998.
- [SW24] P. Schnider and S. Weber. A topological version of Schaefer's dichotomy theorem. In *Proceedings of SoCG 2024*, LIPIcs 293, 2024.
- [Zhu20] D. Zhuk. A proof of the CSP dichotomy conjecture. *Journal of the ACM*, 67(5):30:1–30:78, 2020.
