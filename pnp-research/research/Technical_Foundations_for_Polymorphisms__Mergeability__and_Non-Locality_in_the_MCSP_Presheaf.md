# Technical Foundations for Polymorphisms, Mergeability, and Non-Locality in the MCSP Presheaf

**Revised in response to review. Changes address all twelve recommended revisions: sharpened formal status of circuit polymorphisms (§2, §10), added caveats to the non-localizability argument (§6), expanded the Atserias–Müller magnification result (§7), specified circuit isomorphism for the groupoid enrichment (§4), tightened Shannon counting (§1), connected descent data to the groupoid option (§5), consolidated locality discussions (§6, §8), added compatible-family existence as a challenge (§10), added roadmap connectors, expanded the Friedman discussion (§9), integrated the §5 reference list, and corrected the Siggers operation arity (§2).**

**The three results this paper targets — absence of circuit polymorphisms, existence of non-extendable compatible families, and non-locality of the mergeability obstruction — draw on a precise constellation of results spanning Shannon's counting argument, the CSP dichotomy theorem, lifting theory, Lupanov's construction, non-abelian Čech cohomology, the locality barrier, and hardness magnification.** This report provides the mathematical foundations for each, organized by the three main theorems. No prior work directly connects sheaf theory to MCSP or meta-complexity, making this paper's framework genuinely novel. The closest antecedents are Friedman's incomplete cohomological approach to circuit depth (2005–2006), Basu's categorical complexity theory (2016/2020), and Abramsky's sheaf-theoretic contextuality for quantum computation. The most important recent development is Atserias–Müller's March 2025 uniform magnification result (arXiv:2503.24061), which provides independent evidence that the locality barrier may not be absolute.

---

## 1. Shannon counting, non-uniform advice, and the locally-simple-globally-complex gap

### Shannon's theorem and the tight bound

Shannon's 1949 result provides the counting backbone for Result A. Two related statements must be distinguished:

**The fraction statement (Shannon 1949).** A 1 − o(1) fraction of all 2^{2^n} Boolean functions on n bits require circuits of size at least 2^n/(2n) over the standard binary basis B₂ = {∧, ∨, ¬}.

**The threshold statement (Shannon–Lupanov).** For every ε > 0, the fraction of functions requiring circuits of size at least (1 − ε) · 2^n/n is at least 1 − 2^{−Ω(2^n)}, which is doubly exponentially close to 1.

The proof is a pigeonhole argument: a circuit of size s over a basis of b gates can be encoded in at most s · (2 + ⌈log₂(s + n)⌉) bits (each gate specifies a gate type and two input sources), so the number of distinct circuits of size at most s is at most **2^{O(s log s)}**. For the threshold statement, setting s = (1 − ε) · 2^n/n, the exponent O(s log s) = O(2^n/n · n) = O(2^n), but with a leading constant strictly less than 1 for sufficiently large n. Thus 2^{O(s log s)} < 2^{2^n}, and the fraction of functions not computed by any such circuit is at least 1 − 2^{O(s log s) − 2^n} = 1 − 2^{−Ω(2^n)}.

Lupanov's 1958 construction establishes the matching upper bound: every Boolean function on n bits can be computed by a circuit of size **(1 + O(log n / n)) · 2^n/n**. The construction uses a **(k,s)-representation**: partition variables into address variables (x₁,…,xₖ) and data variables (xₖ₊₁,…,xₙ), build all 2^k minterms of the address variables, organize the truth table as a 2^k × 2^{n−k} matrix, partition rows into blocks of size s, deduplicate column patterns within each block, and combine via a multiplexer. Setting k = n − 2 log n yields the dominant gate count of (1 + O(log n/n)) · 2^n/n.

### Extension to non-uniform advice

For Result A, the counting argument must be extended to show that a uniform polynomial-time merging algorithm with bounded advice cannot produce small circuits for most truth tables. In the presheaf context, the "algorithm A" is the hypothetical global gluing map, and the "k small circuits on sub-cubes" are the local sections being merged.

The formalization is direct: let A be a uniform poly-time algorithm taking k small circuits of size p(n) on sub-cubes plus a(n) advice bits and producing a circuit of size s'(n). Each input circuit is described by O(p(n) log p(n)) bits. The total number of distinct truth tables for which A succeeds is at most **2^{a(n) + O(k · p(n) log p(n))}**. When a(n) = poly(n), k = poly(n), p(n) = poly(n), this is 2^{poly(n)} ≪ 2^{2^n}, so almost all truth tables are uncoverable. This is the standard P/poly counting argument (underlying the Karp–Lipton theorem, 1980) applied to the merging setting.

### The locally-simple-globally-complex phenomenon

This gap drives H¹ nontriviality. For a random Boolean function f on n bits, fix all but m = O(log n) variables. The restricted function f|_ρ is on m bits and has circuit complexity at most O(2^m/m) = poly(n) by Lupanov's theorem. So on sub-cubes of dimension O(log n), **every function has polynomial-size circuits**. But globally, with probability 1 − 2^{−Ω(2^n)}, the function requires circuits of size (1 − ε) · 2^n/n. The sheaf-theoretic interpretation: local sections (small circuits on sub-cubes) exist, but they cannot be glued to a global section (a single polynomial-size circuit for the whole function).

*Roadmap.* The counting arguments in this section provide the quantitative engine for Results A and B. §2 next develops the algebraic parallel to the CSP dichotomy theorem, where the counting argument will establish that the "circuit polymorphism" concept — whose rigorous definition is a contribution of the main paper — captures a genuine algebraic impossibility.

---

## 2. The CSP dichotomy theorem and the polymorphism analogy for Result A

### The dichotomy theorem

The Feder–Vardi conjecture, proved independently by Bulatov (FOCS 2017, arXiv:1703.03021) and Zhuk (FOCS 2017 / JACM 67(5), 2020, arXiv:1704.01914), states: **for every finite domain D and finite constraint language Γ over D, CSP(Γ) is either in P or NP-complete**. The algebraic characterization: CSP(Γ) ∈ P if and only if the polymorphism clone Pol(Γ) contains a **Taylor operation** — equivalently, a weak near-unanimity (WNU) operation of some arity k ≥ 2, a cyclic operation (Barto–Kozik, 2012), or a Siggers operation. The standard Siggers characterization uses a 6-ary operation s satisfying s(x,y,x,z,y,z) ≈ s(y,x,z,x,z,y); some references instead use the equivalent 4-ary formulation due to Kearnes–Marković–McKenzie (2014): a 4-ary operation satisfying s(r,a,r,e) ≈ s(a,r,e,a). Both characterizations are equivalent for the tractability boundary.

A k-ary **polymorphism** of Γ is a function f: D^k → D that preserves every relation R ∈ Γ coordinate-wise: for any k tuples t₁,…,tₖ ∈ R, applying f coordinate-wise yields a tuple in R. The set Pol(Γ) forms a **clone** (closed under composition, containing all projections). The Jeavons–Cohen–Gyssens theorem (1997/1998) establishes that the complexity of CSP(Γ) depends only on Pol(Γ): languages with the same polymorphism clone have the same CSP complexity.

### Specific polymorphism types and their algorithmic leverage

The key types, each enabling distinct algorithmic approaches:

- **Majority operation** (ternary): m(x,x,y) = m(x,y,x) = m(y,x,x) = x. Enables solving via arc consistency / bounded-width propagation.
- **Mal'tsev operation** (ternary): m(x,y,y) = m(y,y,x) = x. Enables solving via Gaussian elimination (canonical example: systems of linear equations, where m(x,y,z) = x − y + z).
- **Semilattice operation** (binary): idempotent, commutative, associative. Enables solving by 1-minimality.
- **WNU operation** (k-ary, k ≥ 2): idempotent with w(y,x,…,x) = w(x,y,x,…,x) = ⋯ = w(x,…,x,y) for all x,y.

A 2024 unification paper by Barto, Brady, Bulatov, Kozik, and Zhuk ("Unifying the Three Algebraic Approaches to the CSP via Minimal Taylor Algebras," TheoretiCS 3, Art. 14, 2024) simplifies the two dichotomy proofs via minimal Taylor algebras, showing that a single algebraic notion — the minimal Taylor algebra — provides a unified route through all the different polymorphism types.

### 3-SAT has only dictator polymorphisms

The canonical NP-complete CSP is 3-SAT, whose constraint language Γ₃SAT has Pol(Γ₃SAT) = J_D, the clone of projections (dictator functions f(x₁,…,xₖ) = xᵢ). This follows from Schaefer's 1978 dichotomy over {0,1}: CSP(Γ) is in P iff Pol(Γ) contains one of six specific types (constant, AND, OR, majority, minority/XOR, semilattice); otherwise only projections remain and the problem is NP-complete. When Pol(Γ) = projections, the Galois connection yields Inv(Pol(Γ)) = all relations, meaning Γ can pp-define every Boolean relation, making CSP(Γ) maximally hard.

### The analogy for MCSP circuit polymorphisms

**Formal status: The "circuit polymorphism" concept is an analogy awaiting rigorous definition. The main paper must provide the formal definition; this section describes the intended parallel and the evidence that the counting argument will establish non-existence once the definition is in place.**

The parallel for Result A: a "circuit polymorphism" would be a polynomial-time algorithm that takes k compatible small circuits on overlapping sub-cubes and produces a small circuit on their union. Just as 3-SAT's projection-only polymorphisms signify NP-completeness — no nontrivial way to combine satisfying assignments — MCSP's conjectured lack of circuit polymorphisms would signify the impossibility of efficiently merging local circuit solutions.

Several definitional choices require justification in the main paper: Why polynomial-time? (Because the analogy targets the P/NP boundary, just as CSP polymorphisms characterize P vs. NP-complete.) Why bounded advice? (To match the non-uniform counting argument.) Why is this the right notion of "combining solutions"? (Because it captures the presheaf's descent condition: whether compatible local sections can be glued into a global section via an efficient procedure.)

The counting argument from §1 establishes non-existence *given* any reasonable formalization: any polynomial-time merging algorithm A with poly(n) advice covers at most 2^{poly(n)} truth tables, leaving 2^{2^n} − 2^{poly(n)} uncovered. The strength of this argument is its robustness — it applies to any definition of "circuit polymorphism" that produces circuits of polynomial size from polynomially many inputs of polynomial size, regardless of the precise algebraic formalization.

Alekseev and Filmus (ECCC TR25-074, June 2025) study **generalized polymorphisms** of predicates, proving that approximate polymorphisms (satisfying the condition for "most" inputs) are close to exact polymorphisms. This robustness result is relevant because it suggests that "approximate circuit polymorphisms" — algorithms that succeed on most compatible families rather than all — would also be constrained by the counting argument. If the MCSP presheaf paper defines circuit polymorphisms as exact objects, the Alekseev–Filmus result provides a backstop: relaxing to approximate polymorphisms would not rescue the merging operation.

*Roadmap.* §3 next examines lifting theorems as the closest existing parallel to circuit polymorphisms — the structural phenomenon of converting local complexity into global complexity via a mediating gadget, analogous to converting local circuit complexity to global complexity via the presheaf's gluing obstruction.

---

## 3. Lifting theorems as the closest parallel to circuit polymorphisms

### The Alekseev–Filmus–Smal classification (CCC 2024)

The paper **"Lifting Dichotomies"** by Yaroslav Alekseev, Yuval Filmus, and Alexander Smal (CCC 2024, LIPIcs vol. 300, pp. 9:1–9:18; journal version in computational complexity 34, Art. 18, 2025) provides a **complete classification of when polynomial query-to-communication lifting exists**, establishing dichotomies in four settings.

The lifted function f ∘ g^n maps (x,y) ∈ X^n × Y^n to f(g(x₁,y₁),…,g(xₙ,yₙ)), where Alice holds x and Bob holds y. The central theorem template: C_cc(f ∘ g^n) ≥ C_dt(f)^Ω(1), where the gadget g determines whether this polynomial relationship holds.

**The four settings and their dichotomies:**

1. **Decision tree depth → decision tree size.** A gadget g is weakly resistant iff it cannot be trivialized by fixing inputs. Dichotomy: weakly resistant gadgets give polynomial lifting; all others give no lifting.

2. **Conjunction DAG width → conjunction DAG size.** Same dichotomy as Setting 1, generalizing to Resolution proof complexity.

3. **Decision tree depth → parity decision tree depth/size.** This is actually a **trichotomy**: AND/OR gadgets (admitting affine projections to both AND and OR) give simultaneous depth and certificate lifting; OR-type gadgets give only depth lifting; all others give no lifting. A byproduct proves the **log-rank conjecture** for f ∘ OR ∘ XOR.

4. **Block sensitivity → deterministic/randomized communication complexity.** The critical setting for the polymorphism analogy. The classification uses reducibility via affine projections. **Polynomial lifting holds if and only if at least two of {AND, OR, XOR} reduce to g.** Otherwise, there exist infinite families with bs(fₙ) = Ω(n) but D(fₙ ∘ g) = O(1).

**Why lifting is the closest parallel.** Lifting theorems convert query complexity (a "local" measure) to communication complexity (a "global" measure) via a gadget. The presheaf framework analogously converts local circuit complexity (small circuits on sub-cubes) to global circuit complexity (no small global circuit) via the gluing obstruction. In both cases, a mediating structure (the gadget or the presheaf) determines whether local simplicity implies global simplicity. The dichotomy parallels the CSP dichotomy suggestively — either the gadget admits enough structure for lifting or it admits none — though the parallel is structural rather than formally tight: the CSP uses a single algebraic condition (Taylor polymorphism), while the lifting classification uses a combinatorial condition (reducibility of {AND, OR, XOR} to g).

### Key prior lifting results

The foundation was laid by Raz–McKenzie (1999) for monotone depth, Göös–Pitassi–Watson (FOCS 2015/2017) for deterministic and randomized lifting with the index gadget (requiring m = poly(n)), and Chattopadhyay–Filmus–Koroth–Meir–Pitassi (ICALP 2019) for lifting with discrepancy gadgets. A notable recent result is Lovett–Meka–Mertz–Pitassi–Zhang (ITCS 2022), who simplified deterministic lifting using connections to the sunflower lemma. The major open problem remains whether **constant-size gadgets suffice for linear lifting** in the communication setting.

Among 2025 developments, three are relevant: Alekseev–Itsykson (STOC 2025) lift bounded-depth Resolution over parities, extending the reach of lifting beyond standard proof systems; Efremenko–Itsykson (CCC 2025) introduce amortized closure techniques for Res(⊕) lifting, providing new algebraic tools; and de Rezende–Vinyals (CCC 2025) lift with colourful sunflowers, achieving improved monotone circuit lower bounds and further connecting the sunflower machinery to the lifting paradigm.

*Roadmap.* §4 turns from the algebraic setting (polymorphisms, lifting) to the geometric one: whether specific circuit constructions commute with restriction, which is the central technical challenge for Result B.

---

## 4. Lupanov's construction is not functorial under restriction

### Why functoriality fails

For Result B, the paper needs circuit constructions that commute with restriction (hardwiring variables). The analysis reveals that **Lupanov's construction is not functorial**, for three structural reasons:

**First**, the variable partition breaks. Lupanov's construction partitions n variables into address variables (x₁,…,xₖ with k = n − 2 log n) and data variables. Fixing a variable changes whether the optimal partition uses k−1 or n−k−1 remaining data variables; a fresh construction on n−1 variables would choose k' = (n−1) − 2 log(n−1) ≠ k − 1.

**Second**, the block decomposition is parameter-dependent. The block size s depends on n; for n−1 variables, different s' and m' are optimal. The blocks in the restricted circuit inherit the original (wrong) decomposition.

**Third**, the multiplexer structure depends on the full truth table. After fixing a data variable, the multiplexer μ_{n−k} has the wrong number of inputs. After fixing an address variable, it becomes degenerate. Neither matches the fresh Lupanov multiplexer for the restricted truth table.

### Balanced XOR trees also fail functoriality

Even the simplest canonical construction — the balanced binary XOR tree for parity — fails to commute with restriction. Consider the balanced tree on {x₁,x₂,x₃,x₄}: T₄ = (x₁ ⊕ x₂) ⊕ (x₃ ⊕ x₄). Fix x₁ = 0: the simplified circuit is x₂ ⊕ (x₃ ⊕ x₄), a **right-leaning tree** with structure [leaf, [leaf, leaf]]. But the canonical balanced tree on {x₂,x₃,x₄} is (x₂ ⊕ x₃) ⊕ x₄, with structure [[leaf, leaf], leaf]. These are **not DAG-isomorphic**. The result depends on *which* variable is fixed, not just how many remain: fixing variables in the left subtree produces a right-leaning tree; fixing in the right subtree produces a left-leaning tree.

### Three paths to compatibility

This non-functoriality has three resolutions, each with different implications for the presheaf framework:

**Option A (Equivalence classes):** Define the presheaf over functional equivalence classes rather than specific DAGs. This makes restriction trivially functorial because a Boolean function restricted to a sub-cube is just the sub-function — no structural mismatch arises. But the presheaf then satisfies the sheaf condition: Boolean functions are determined by their values on points, so compatible families always glue uniquely. This means H¹ = 0, making the presheaf uninteresting for complexity-theoretic purposes.

**Option B (Groupoid enrichment):** Replace the Set-valued presheaf with a **groupoid-valued presheaf**. The critical definitional question is: what counts as a morphism in the circuit groupoid? We propose the following precise definition.

**Definition (Circuit groupoid).** Fix a truth table T of length N = 2^n and a size bound s. For a sub-cube (S, α) of dimension d, the **circuit groupoid** Circ_{T,s}(S,α) has:
- *Objects:* Boolean circuits C of size ≤ s that compute T|_{(S,α)} (the restriction of T to the sub-cube).
- *Morphisms:* A morphism φ: C₁ → C₂ is a **size-preserving DAG isomorphism** — a bijection φ: V(C₁) → V(C₂) on the gate sets that preserves all four of: (i) **gate type** (AND, OR, NOT at each gate), (ii) **directed edge structure** (g feeds into h in C₁ iff φ(g) feeds into φ(h) in C₂, with input order preserved for non-commutative gates), (iii) **primary input identity** (if g is an input node reading variable xᵢ, then φ(g) reads xᵢ), and (iv) **output designation** (φ maps the output gate of C₁ to the output gate of C₂). Internal gate *labels* (names, indices, memory addresses) need not match — only the structural properties (i)–(iv) are required. Crucially, both C₁ and C₂ must have size ≤ s.

This is stricter than functional equivalence (which would collapse the groupoid to a set) but weaker than literal DAG equality (which would require the presheaf to be Set-valued). The size bound is essential: without it, one could always pad circuits to a canonical form, trivializing the groupoid.

With this definition, compatibility in the presheaf requires only **isomorphism** on overlaps, not equality. The cocycle condition φⱼₖ ∘ φᵢⱼ = φᵢₖ on triple overlaps ensures coherence. The restricted XOR tree and the canonical XOR tree on a sub-cube compute the same function and have the same number of XOR gates, so they are connected by a DAG isomorphism in the circuit groupoid (a relabeling of internal nodes that preserves the XOR structure). This is precisely the **descent datum** formalism of §5.

**Option C (All circuits presheaf):** The presheaf sends each sub-cube S to the set of *all* circuits computing f|_S with size ≤ s, with restriction maps given by literal hardwiring-and-simplification. This is always well-defined. The sheaf condition then asks whether local choices (one small circuit per sub-cube, structurally matching on overlaps) can be glued into a global small circuit.

**Proof strategy for Theorem B.1.** For Theorem B.1 (parity in AC⁰), the XOR tree construction offers the best starting point. While the balanced tree is not exactly functorial, one can define a **canonical variable-ordering-dependent construction** (e.g., the left-recursive chain x₁ ⊕ (x₂ ⊕ (⋯ ⊕ xₙ))) that, combined with a fixed variable ordering and re-indexing convention, yields compatible families under Option B. The key verification is that for parity (a simple, structured function), local circuits can be chosen to agree on overlaps up to the canonical rewriting rules for XOR gates. For hard random functions (Theorem B.2), Lupanov's construction provides local sections, but compatibility on overlaps requires the groupoid enrichment since Lupanov circuits on different sub-cubes will not be literally identical on overlaps.

*Roadmap.* §5 provides the formal cohomological framework — non-abelian Čech cohomology and the descent datum formalism — that underpins Option B and makes the circuit groupoid construction mathematically rigorous.

---

## 5. Non-abelian Čech cohomology formalizes the mergeability obstruction

### Giraud's framework for H¹

Jean Giraud's *Cohomologie non abélienne* (Springer Grundlehren 179, 1971; reprinted 2020) establishes that for a sheaf of (possibly non-abelian) groups G on a topological space or site X, **H¹(X, G) classifies isomorphism classes of G-torsors**. When G is non-abelian, H¹(X,G) is only a **pointed set** (distinguished element = trivial torsor), not a group. H² is defined via **gerbes** — stacks in groupoids that are locally non-empty and locally connected — classified by **liens** (bands), as developed by Breen (1990, 1994) using bitorsors and crossed modules, and later by Lurie in the ∞-categorical framework.

### Čech cohomology with non-abelian coefficients

For a cover U = {Uᵢ} and sheaf of groups G, Čech 1-cocycles are collections (gᵢⱼ) ∈ ∏ G(Uᵢ ∩ Uⱼ) satisfying the **cocycle condition gᵢⱼ · gⱼₖ = gᵢₖ** on triple overlaps. Two cocycles are cohomologous if g'ᵢⱼ = hᵢ · gᵢⱼ · hⱼ⁻¹ for some 0-cochain (hᵢ). The Čech cohomology Ȟ¹(X, G) = colim over covers of Z¹(U,G)/∼.

### Set-valued presheaves and compatible families

For a presheaf F: Op(X)^op → Set, a **compatible family** for cover U is a collection {sᵢ ∈ F(Uᵢ)} with sᵢ|_{Uᵢ∩Uⱼ} = sⱼ|_{Uᵢ∩Uⱼ}. The presheaf is a sheaf iff every compatible family extends uniquely to a global section. **H¹ nontriviality** in this setting means compatible families exist that do not extend — the gluing axiom fails. The Čech zeroth cohomology Ȟ⁰(U, F) = Eq(∏ᵢ F(Uᵢ) ⇉ ∏ᵢⱼ F(Uᵢ ∩ Uⱼ)) is exactly the set of compatible families.

### Groupoid-valued presheaves, descent, and the circuit groupoid

For a presheaf valued in groupoids, a **descent datum** consists of objects xᵢ ∈ F(Uᵢ), transition isomorphisms φᵢⱼ: xᵢ|_{overlap} → xⱼ|_{overlap}, subject to the cocycle condition φⱼₖ ∘ φᵢⱼ = φᵢₖ. The presheaf is a **stack** if every descent datum is effective (extends to a global object). **This is the formal framework underlying Option B of §4:** the circuit groupoid Circ_{T,s} defined there is a groupoid-valued presheaf on the sub-cube site, and a compatible family of circuits is precisely a descent datum — local circuits on each sub-cube together with DAG isomorphisms on overlaps satisfying the cocycle condition.

The nontriviality of H¹ for the circuit groupoid presheaf means: there exist descent data (local circuits with coherent transition isomorphisms) that are not effective (no global circuit of size ≤ s computes the whole truth table). This is the precise formalization of "the mergeability obstruction is genuine."

Lurie's *Higher Topos Theory* (2009) provides the ∞-categorical generalization: in an ∞-topos, non-abelian cohomology with coefficients in A is simply π₀ Map(X, A). Jardine's *Local Homotopy Theory* (2015) develops the model-structure approach on simplicial presheaves. Standard references for descent theory include Vistoli's lecture notes (2005) and the Stacks Project (tags 02ZC, 01FP, 0CJZ). Duskin (1982) provides the key result connecting bouquets in a topos to non-abelian H¹.

*Roadmap.* With the algebraic (§§2–3) and geometric (§§4–5) foundations in place, §6 turns to the central question for Result C: why the mergeability obstruction may evade the locality barrier, and what formal claims remain to be proved.

---

## 6. The locality barrier and why sheaf obstructions may evade it

### The formal definition

The locality barrier was established by Chen, Hirahara, Oliveira, Pich, Rajgopal, and Santhanam ("Beyond Natural Proofs: Hardness Magnification and Locality," ITCS 2020; JACM 69(4), Art. 25, 2022). They define **local circuit classes** [q, ℓ, a]–C: a language L is in this class if there exist oracle circuits {Eₙ} from class C with at most q(n) oracle gates, each of fan-in at most ℓ(n), with at most a(n) oracle gates on any input-to-output path, such that for some oracle O, {Eₙ^O} computes L. The oracle gates compute **arbitrary Boolean functions** on their ℓ(n) input bits.

A proof technique is **localizable** if lower bounds it proves extend to circuits augmented with bounded-fan-in oracle gates — i.e., the proof also rules out [q, ℓ, a]–C circuits for appropriate parameters.

### What the barrier proves

The paper demonstrates two complementary facts:

**Oracle upper bounds from magnification.** Hardness magnification theorems, in their contrapositive, unconditionally show that key problems admit efficient oracle circuits. For HM Frontier B: **MCSP[2^{n^{1/3}}, 2^{n^{2/3}}] ∈ Formula-O-XOR[N^{1.01}]** where oracle O has fan-in at most N^ε for any ε > 0. Similar results hold for all five frontiers.

**Existing techniques localize.** All known lower bound techniques — random restrictions (Furst–Saxe–Sipser, Håstad), the approximation method (Razborov), gate elimination, Tal's formula-XOR bounds — extend to circuits with bounded-fan-in oracle gates. The shared structural reason is that these techniques process circuits **gate-by-gate** in bottom-up fashion, and an oracle gate on m bits can be absorbed into the analysis by treating it as a single complex gate with approximation distance at most O(2^{m}) (see §8 for the detailed mechanism in Razborov's case). Therefore, **these techniques cannot distinguish circuits with and without oracle gates**, and hence cannot prove the lower bounds needed for magnification (since the problems unconditionally have efficient oracle circuits).

### Pich's localizability theorem

Ján Pich ("Localizability of the Approximation Method," computational complexity 33, Art. 12, 2024; arXiv:2212.09285) proves that Razborov's approximation method is **inherently localizable**. The main result (Theorem 1.1/3.13): if ρ_{O(d²)}(f, M) ≥ s for a legitimate model M of constant-depth circuits, then f cannot be computed by depth-d circuits of size s − poly(kdn(2^{m/d} + 1)) using k oracle gates of fan-in m. For general circuits (Theorem 3.7): if ρ(f, M) ≥ s and M is projective, then f cannot be computed by circuits of size s − O(km²) with k oracle gates of fan-in m anywhere in the circuit.

The mechanism: the approximation method replaces each gate with an approximation and tracks accumulated error. An oracle gate on m bits has approximation distance at most O(2^{8m/d}) by Razborov's barrier theorem, so it can be absorbed by replacing it with a majority-of-random-approximators construction. This gate-by-gate locality is exactly what makes the method unable to distinguish standard circuits from oracle-augmented circuits. (§8 provides the full five-step description of the method.)

### Structural arguments for why sheaf-theoretic obstructions may escape

**Formal status: The following four points are structural arguments supporting the conjecture that OD (the obstruction detection function for the circuit-level presheaf) is non-localizable. They are NOT a proof of non-localizability. The formal claim — OD ∉ [q, ℓ, a]–C for the parameters arising from magnification frontiers — remains the key open question for Result C.**

The central argument for Result C: the obstruction detection function OD — which determines whether H¹ is trivial or nontrivial — captures a **global mergeability failure** that appears structurally different from gate-by-gate analysis:

1. **OD examines global consistency.** It asks whether compatible local circuits can be simultaneously merged into a global circuit. This is a combinatorial/topological property of the *space* of circuits, not a property of individual gates. *This is a characterization of the obstruction's nature, not a theorem about non-localizability.*

2. **Oracle gates have limited reach.** A small fan-in oracle gate accesses at most ℓ(n) bits and thus cannot "see" the global structure governing whether 2^{n−O(log n)} local circuits on O(log n)-dimensional sub-cubes are globally compatible. The obstruction is spread across exponentially many overlapping sub-cubes. *However, this intuition must be formalized: one must rule out the possibility that an oracle gate could be designed to "hard-code" the global answer for its ℓ(n) input bits. The formal argument is that a single oracle gate sees at most 2^{ℓ(n)} possible input patterns, while the global consistency involves interactions among Θ(N^{2d}) sub-cube pairs — and for ℓ(n) = O(1), the oracle cannot distinguish more than 2^{O(1)} cases.*

3. **Non-locality of cohomological obstructions (by analogy).** In sheaf cohomology, H¹ ≠ 0 witnesses a *topological* obstruction — the impossibility of gluing local data globally. This is structurally analogous to Abramsky–Brandenburger's sheaf-theoretic contextuality (2011), where non-locality in quantum mechanics is captured by sections of a presheaf that cannot be extended globally. *This analogy is motivational, not a proof; the MCSP and quantum settings differ in fundamental ways (computational vs. physical non-locality).*

4. **Contrast with localizable techniques.** The approximation method, random restrictions, and gate elimination all work by analyzing individual gates or small subcircuits. They naturally extend to oracle gates because each oracle is "just another gate." The sheaf-theoretic approach instead examines the **relationship structure among exponentially many local solutions** — a property invisible to any bounded-fan-in computation. *This structural distinction is well-established, but converting it to a formal non-localizability proof requires showing that no gate-by-gate analysis can simulate the global consistency check, which is precisely what Theorem C.1 of the main paper must achieve.*

Pich explicitly leaves open (Problem 3.8) whether there exist concrete non-localizable lower bounds achieving HM frontiers, and suggests that the "weakness" (barely superlinear) of certain lower bounds may reflect their sensitivity to oracle augmentation.

*Roadmap.* §7 quantifies the stakes: the precise magnification theorems that connect barely-superlinear lower bounds for MCSP to major separations, including the recent Atserias–Müller result that provides the first uniform magnification for MCSP.

---

## 7. Hardness magnification establishes the stakes

### The magnification phenomenon

Hardness magnification, named by Oliveira and Santhanam (FOCS 2018, ECCC TR18-139), shows that barely-superlinear lower bounds for certain problems imply superpolynomial separations. The precise statements for MCSP:

**Oliveira–Santhanam (FOCS 2018), Theorem 1:** If (1, 1−n^{−k})-MCSP[n^k] ∉ Formula[N · (log N)^{O(1)}], then NP ⊄ Formula[poly]. If the same problem ∉ Formula[N^{1+ε}], then NP has functions requiring 2^{m^{δ'}}-size formulas.

**Oliveira–Pich–Santhanam (CCC 2019):** Gap-MCSP[2^{βn}/cn, 2^{βn}] ∉ Circuit[N^{1+ε}] ⟹ NP ⊄ Circuit[poly].

**Chen et al. (ITCS 2020/JACM 2022), five HM Frontiers:**
- **(A)** MKtP ∉ AC⁰-XOR[N^{1.01}] ⟹ EXP ⊄ NC¹
- **(B)** MCSP[2^{n^{1/3}}, 2^{n^{2/3}}] ∉ Formula-XOR[N^{1.01}] ⟹ NQP ⊄ NC¹
- **(C)** MCSP[2^{n^{1/2}}/10n, 2^{n^{1/2}}] ∉ N^{0.01}-Almost-Formula[N^{1.01}] ⟹ NP ⊄ NC¹
- **(D)** MCSP[2^{√n}] ∉ GapAND-Formula[N^{2.01}] ⟹ NQP ⊄ NC¹. This frontier is nearly tight: the known unconditional *lower* bound for GapAND-Formula is N^{2−ε}, so the gap between what is known (N^{2−ε}) and what magnification requires (N^{2.01}) is extremely small.
- **(E)** (n−k)-Clique ∉ AC⁰[m^{1.01}] ⟹ NP ⊄ NC¹

### The magnification–locality tension

The key tension: magnification theorems say barely-superlinear lower bounds suffice, but the locality barrier shows existing techniques cannot achieve even these bounds for the specific gap/approximate MCSP variants, because those variants admit efficient oracle circuits. **Magnification overcomes the natural proofs barrier** (Chen et al. prove that slightly superlinear MCSP lower bounds imply non-existence of P/poly-natural properties useful against P/poly), but the locality barrier fills the gap left by natural proofs.

### The Atserias–Müller uniform magnification result (March 2025)

**Atserias and Müller ("Simple general magnification of circuit lower bounds," arXiv:2503.24061, March/June 2025)** introduce a technically and conceptually simple approach to magnification that generalizes and strengthens known results. Their central innovation is the **distinguisher** — a sparse binary matrix D of dimensions n × m that retains key properties of error-correcting codes:

**Definition (Distinguisher).** An (n, m, ε, δ)-distinguisher is a binary n × m matrix D such that for any two strings x, y ∈ {0,1}^n with Hamming distance d_H(x,y) ≥ ε · n, the Hamming distance between xD and yD (computed over F₂) is at least δ · m. The **weight** of D is the maximum Hamming weight of any column.

**Main construction (Theorem).** There exists a polynomial-time algorithm that, given n, computes an (n, m, n^{−ε}, 1/8)-distinguisher of weight at most ⌈2n^ε⌉, where m ≤ n^7. The construction concatenates Reed–Solomon and Hadamard codes to achieve low weight with polynomial-time computability. An alternative "strongly explicit" construction (based on Naor–Naor derandomization) allows each entry of D to be computed in poly(log n) time.

**How the method works.** Given a problem Q ∈ NP, the distinguisher D is used to construct a **kernel** K — a compressed representation of Q obtained by hashing instances through D. The key insight: if x ∈ Q, then the projection k(x,u) = (xD)_u₁, …, (xD)_uᵣ lands in K for all random column selections u. If x is far from any element of Q (in Hamming distance ≥ εn), then k(x,u) ∈ K with low probability. The kernel K has much shorter input length than Q, so small circuits for K yield small circuits for approximating Q. Magnification then follows: if no small circuit solves Q exactly, and Q can be approximately solved via small circuits for K, then the circuit size for Q can be "magnified" from slightly superlinear to fixed polynomial.

**The key results:**

(1) **General magnification with sharp thresholds.** Fixed-polynomial formula-size lower bounds for NP are implied by slightly superlinear formula-size lower bounds for approximating any sufficiently sparse NP problem. The thresholds achieved are proved to be sharp (cannot be further improved).

(2) **Uniform magnification for MCSP.** P ≠ NP ⊕ P if there exists σ(ℓ) ≤ 2^{o(ℓ)} such that n^{−ε}-MCSP[σ] ∉ P-uniform-SIZE[n^{1+ε+o(1)}]. This is significant because **uniform circuits (those constructible in polynomial time) are not subject to the oracle upper bounds that drive the locality barrier**. The Chen et al. locality barrier specifically exploits the non-uniform freedom of oracle gates — an arbitrary function on ℓ(n) bits. For P-uniform circuits, no such arbitrary oracle is available, so the locality barrier's key mechanism (showing that MCSP admits efficient oracle circuits) does not directly apply. Santhanam–Williams (Computational Complexity, 2014) already showed P ⊄ P-uniform-SIZE[n^c] for all constants c, so the uniform setting is one where barely-superlinear lower bounds might be achievable by existing techniques.

(3) **Sharp lower bound.** n^{−ε}-MCSP[2^{√ℓ}] ∉ PFML[n^{2ε−o(1)}] for all 0 < ε ≤ 1, proved via random restrictions. This shows the magnification thresholds cannot be improved.

**Significance for the MCSP presheaf paper.** The Atserias–Müller result provides the first concrete evidence that the locality barrier is not absolute for MCSP lower bounds. Their uniform magnification avoids the non-uniform oracle mechanism entirely. This complements the sheaf-theoretic approach: the presheaf paper argues that the obstruction detection function OD is structurally non-local (§6), while Atserias–Müller show that uniform magnification provides a route where locality is not an obstacle in the first place. Both point toward the same conclusion: the locality barrier restricts a specific class of non-uniform techniques, not all possible approaches.

### Strongest known unconditional bounds for MCSP

The current state of the art:

- **De Morgan formulas:** N^{3−o(1)} (Cheraghchi–Kabanets–Lu–Myrisiotis, ICALP 2019)
- **Formulas over arbitrary basis / branching programs:** N^{2−o(1)} (CKLM 2019)
- **Depth-d AC⁰:** 2^{Ω(N^{1/(d+2.01)})} (CKLM 2019)
- **One-tape Turing machines:** Cannot compute MCSP[2^{μn}] in time N^{1.99} (Cheraghchi–Hirahara–Myrisiotis–Yoshida, STACS 2021)
- **General circuits (best explicit):** ~3.04n for the hardest explicit functions (Find–Golovnev–Hirsch–Kulikov, FOCS 2016)

The gap between these bounds and the magnification thresholds (typically N^{1+ε}) motivates the search for non-localizable techniques.

### Recent developments (2023–2026)

Several major results advance meta-complexity. **Hirahara (FOCS 2022)** proved NP-hardness of partial MCSP under randomized reductions. **Ilango–Ren–Santhanam (FOCS 2023)** showed SAT reduces to MCSP with a random oracle. **Huang–Ilango–Ren (STOC 2023 / SIAM J. Comput. 2025)** proved NP-hardness of approximating oracle circuit complexity via cryptographic techniques. **Chen–Li–Liang (ECCC TR24-182, 2024)** achieved the strongest near-maximum circuit lower bound: AMEXP_{/2^{n^ε}} requires circuit complexity ≥ 2^n/n.

---

## 8. Razborov's approximation method and the sunflower lemma

*This section provides the technical mechanism behind the localizability discussed in §6. The two sections are complementary: §6 explains what the locality barrier implies; this section explains why Razborov's method, specifically, is local.*

### How Razborov's method works

Razborov's 1985 method for monotone circuit lower bounds proceeds bottom-up through a monotone circuit C:

1. Replace each gate with an **approximator** — a bounded DNF or collection of "clique indicators" (sets of at most l vertices from a graph).
2. For AND gates: take all pairwise conjunctions of indicators from the two inputs. Discard conjunctions with > l vertices. Apply the **sunflower lemma** to reduce the number of remaining indicators to at most m by "plucking petals" (replacing sunflower sets with their core).
3. For OR gates: take the union of indicator sets.
4. Track accumulated approximation error against two test distributions: positive instances (k-cliques) and negative instances (complete (k−1)-partite graphs).
5. Show accumulated error exceeds what any small circuit can achieve, deriving the lower bound.

### Connection to the sunflower lemma

The sunflower lemma is the critical combinatorial tool: when more than (p−1)^l · l! sets of size at most l accumulate at an AND gate, the lemma guarantees a sunflower (collection of sets whose pairwise intersections are identical). Plucking the sunflower (replacing it with the core) keeps the indicator count bounded while introducing controlled error. The Alweiss–Lovett–Wu–Zhang breakthrough (Annals of Mathematics, 2020) improved the sunflower bound, and Cavalar–Kumar–Rossman (Algorithmica, 2022) applied it to achieve the strongest known monotone circuit lower bound: **exp(Ω(n^{1/2}/(log n)^{3/2}))** for an explicit function, breaking a 20-year record.

### Why the method is local (connecting to §6)

The method analyzes circuits **gate-by-gate** in bottom-up fashion. Each gate's approximation depends only on its immediate inputs' approximations. This is precisely what Pich's theorem (§6) formalizes: an oracle gate on m bits is "just another gate" whose output can be approximated (any function on m bits has a DNF of size at most 2^m, or approximation distance at most O(2^{8m/d}) in depth-d circuits). The approximation method automatically extends to oracle-augmented circuits because it never examines global circuit structure — and this is exactly the structural property that the sheaf-theoretic approach aims to avoid.

---

## 9. No prior work connects sheaf theory directly to MCSP

### The landscape of categorical approaches to complexity

Extensive search across arXiv, ECCC, Google Scholar, and DBLP from 2023–2026 found **no papers directly applying presheaf or sheaf methods to MCSP, meta-complexity, or Boolean circuit lower bounds**. The searches for "MCSP presheaf," "complexity presheaf," "sheaf framework meta-complexity," "Čech cohomology circuit complexity," and "polymorphism circuit complexity MCSP" returned no results.

The closest antecedents are:

**Friedman (2005–2006):** The most direct prior attempt, modeling circuit depth complexity via "cohomological complexity" — the sum of dimensions of Ext groups between sheaves on a Grothendieck topology derived from the circuit's structure. For AND-only circuits (monotone depth), Friedman's cohomological complexity recovers LP relaxation bounds. *Why the approach stalled:* Friedman's Grothendieck topology was built from the circuit itself (not from the problem's combinatorial structure), making the cohomology dependent on the circuit rather than on the function. Computing the cohomological complexity for general circuits (beyond AND-only) required understanding Ext groups in categories of sheaves on complex topologies — a computation that proved intractable. Additionally, no mechanism connected the cohomological lower bound to specific functions; the bound applied only to circuits of a given topology. Friedman acknowledged the impasse directly. The MCSP presheaf framework avoids these pitfalls by building the site from the problem's sub-cube structure (not from the circuit) and using H¹ of the presheaf (not Ext groups between sheaves) as the obstruction invariant.

**Basu (2013–2020):** Developed categorical complexity theory, defining complexity of objects and morphisms in arbitrary categories (Forum Math. Sigma, 2020). His constructible sheaf complexity theory (2015) defines sheaf-theoretic analogs of VP, VNP, and the polynomial hierarchy over the reals, with sheaf-theoretic P vs NP conjectures. However, this operates in the Blum–Shub–Smale model, not Boolean circuit complexity.

**Abramsky–Brandenburger (2011):** Sheaf-theoretic contextuality for quantum computation, where obstructions to global sections capture non-locality. Extended in 2023–2024 to accommodate causality and adaptivity. The structural parallel to the MCSP presheaf is the strongest of any prior work: both involve compatible local data (measurement outcomes / small circuits) that cannot be globalized (no global hidden-variable model / no global small circuit), with H¹ nontriviality as the formal witness. The MCSP presheaf paper uses this as motivational analogy rather than formal connection; making the connection precise would require identifying a shared categorical structure (perhaps a common type of Grothendieck topology or descent condition) between the quantum and circuit settings, which is a possible direction for future work but not within the scope of the current paper.

**Recent categorical developments:** Roberts (Compositionality, 2023) extends Lawvere's fixed-point theorem to substructural settings, connecting diagonal arguments to category theory. A topological version of Schaefer's dichotomy theorem (Papadimitriou–Yannakakis, SoCG 2024) connects CSP topology to dichotomy, though not via sheaves. A survey on category-theoretical frameworks in machine learning (Axioms, March 2025) develops topos-based learning with sheaf/stack structures, but not for circuit complexity.

The MCSP presheaf framework thus represents a genuinely novel research direction at the intersection of sheaf cohomology and meta-complexity, with no direct competitors in the literature.

---

## 10. Synthesis and implications for the three results

### Result A: the counting argument is robust, but the definition requires care

The no-nontrivial-polymorphism claim combines Shannon's counting argument (extended to advice settings) with the CSP dichotomy analogy. **The formalization depends on the main paper's definition of "circuit polymorphism."** The counting argument is robust: for any definition where the merging algorithm runs in polynomial time, takes polynomially many circuits of polynomial size, and produces a circuit of polynomial size, the argument shows non-existence for almost all truth tables. The Alekseev–Filmus–Smal lifting dichotomy (CCC 2024) provides the closest structural parallel. What the main paper must supply is a rigorous justification that the chosen definition is "the right one" — that it captures the algebraic content of the presheaf's descent condition, analogously to how CSP polymorphisms capture the algebraic content of constraint propagation.

### Result B: non-functoriality is the obstacle, but compatible-family existence is also non-trivial

The central technical challenge has two parts. **First**, no canonical circuit construction is functorial under restriction (§4). The groupoid-valued presheaf enrichment (Option B) resolves this by allowing compatibility up to size-preserving DAG isomorphism. **Second**, even with the groupoid enrichment, one must show that non-trivial compatible families *exist* for hard functions. The counting argument shows they cannot be globalized, but the existence of compatible local sections — small circuits on each sub-cube that are pairwise compatible on overlaps — is itself non-trivial and constitutes a separate constructive challenge. For parity (Theorem B.1), this is achievable via careful choice of XOR circuits. For random functions (Theorem B.2), Lupanov's construction provides local sections (small circuits on each sub-cube), but showing that these local sections can be chosen to satisfy the groupoid cocycle condition on overlaps requires either proving functoriality of a modified Lupanov construction (unlikely, per §4) or developing the groupoid enrichment sufficiently to show that the space of compatible families is non-empty (e.g., by showing that for each pair of overlapping sub-cubes, there exist Lupanov-type circuits that are DAG-isomorphic on the overlap). This is the "pivotal open question within the open question" identified in the Step 3 blueprint.

### Result C: the strongest theoretical support, but formal non-localizability is unproved

The claim that the mergeability obstruction evades the locality barrier is supported by multiple structural arguments (§6). Pich's theorem (2024) precisely characterizes what makes techniques localizable: gate-by-gate processing that naturally absorbs oracle gates. The sheaf-theoretic obstruction is fundamentally different — it examines global consistency across exponentially many overlapping sub-cubes. **However, the four structural arguments in §6 are supporting evidence, not a proof.** The key open question is formalizing a proof that OD ∉ [q, ℓ, a]–C for the parameters arising from magnification frontiers.

The Atserias–Müller uniform magnification result (March 2025) provides complementary and independent evidence that the locality barrier is not the final word: their uniform setting avoids the non-uniform oracle mechanism entirely, offering an alternative route where localizability is simply not the relevant obstruction. Together, the sheaf-theoretic non-locality argument and the Atserias–Müller uniform route suggest that MCSP lower bounds may be achievable by approaches that are structurally outside the scope of the Chen et al. locality barrier.

**The bottom line.** Each of the three results rests on solid but incomplete foundations. Result A needs a rigorous definition of circuit polymorphism; the counting argument then follows. Result B needs both the groupoid enrichment and a proof that compatible families exist for hard functions. Result C needs a formal non-localizability theorem, for which the structural arguments provide the roadmap. Either success or failure in any of these would advance the field: the framework is designed so that each negative result is as informative as each positive one.
