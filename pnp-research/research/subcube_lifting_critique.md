# The Sub-cube Lifting Conjecture Is False: A Comprehensive Refutation

## Abstract

We demonstrate that Conjecture 14.1 of the conditional P ≠ NP proof — the Sub-cube Lifting Conjecture — is not merely unproved but is almost certainly **false** as stated. We identify five independent lines of attack: (1) a fundamental category error in extrapolating existing lifting dichotomies to a query model for which they were never designed; (2) a compositional structure mismatch that makes the conjecture's output (circuit size bounds for f ∘ g) inapplicable to OD itself; (3) a self-referential incoherence in the "reduction from OD ∘ g to OD" that was already identified as an error in the proof's own revision history; (4) concrete combinatorial obstructions to lifting from sub-cube queries; and (5) the absence of any known analogue in the entire lifting literature. Together, these arguments establish that the conjecture cannot hold in the form required by Theorem 15.1, and that the conditional proof's critical bridge between proved lower bounds and P ≠ NP is broken.

---

## 1. Statement Under Scrutiny

**Conjecture 14.1** (Sub-cube Lifting). *There exists a family of gadgets g_n : {0,1}^{m(n)} → {0,1} with m(n) = n^{o(1)} such that for any f : {0,1}^M → {0,1} with sub-cube query complexity Q_d(f) ≥ q:*

$$L(f \circ g) \geq q^{1+\Omega(1)} \cdot \text{poly}(|g|)$$

The conditional proof uses this conjecture to bridge from the proved bound Q_d(OD) ≥ Ω(N/poly(n)) (Theorem 12.1) to circuit size(OD) ≥ N^{1+Ω(1)}, which via hardness magnification yields P ≠ NP.

We will show this bridge is structurally unsound.

---

## 2. Argument I — The Extrapolation from Known Lifting Dichotomies is Illegitimate

### 2.1 What Alekseev–Filmus–Smal Actually Proved

The Alekseev–Filmus–Smal gadget dichotomy (CCC 2024, *Computational Complexity* 2025) is cited as primary evidence for Conjecture 14.1. However, examining what they actually proved reveals a critical mismatch.

Their complete classification of gadgets covers four specific settings:

1. Lifting from **decision tree depth** to **decision tree size**.
2. Lifting from **conjunction DAG width** to **conjunction DAG size**.
3. Lifting from **decision tree depth** to **parity decision tree depth and size**.
4. Lifting from **block sensitivity** to **deterministic and randomized communication complexities**.

In every one of these settings, the *source measure* is a standard decision tree quantity (depth, width, block sensitivity) and the *target measure* is either another tree-based measure or communication complexity.

**None of these settings involves sub-cube query complexity as the source measure.** Sub-cube queries are a fundamentally different computational model: a single sub-cube query simultaneously fixes some variables and checks consistency of the function across all free variables. This is not a single-bit query, a parity query, or a block sensitivity test.

**None of these settings produces circuit size as the target measure.** The leap from communication complexity lower bounds to circuit size lower bounds requires additional machinery (like Karchmer–Wigderson relations), and no generic transfer is known.

### 2.2 The Dichotomy is Binary — and the Binary Outcome for Sub-cube Queries is Unknown

The Alekseev–Filmus–Smal dichotomy shows that for each gadget in each of their four settings, either polynomial lifting holds or *no lifting at all holds*. This binary classification is precisely what makes extrapolation dangerous: if sub-cube queries fall into a regime where the gadget's critical properties do not activate, then the dichotomy predicts **no lifting at all**, not polynomial lifting.

The evidence listed in the proof — items (i) through (iv) — conflates the existence of *some* lifting theorem in *some* setting with evidence for lifting in a *new, unstudied* setting. This is an inductive leap without logical force.

### 2.3 The Relations Gap

As Alekseev, Filmus, and Smal explicitly note in their CCC 2024 paper: "the question of proving lifting dichotomies for **relations** is still open." The obstruction detection function OD has a fundamentally relational character — it asks about the existence of compatible families, which is a search/relation problem, not a clean Boolean function in the sense their dichotomy addresses. Even if a sub-cube lifting dichotomy were proved for Boolean functions, extending it to the relational setting relevant to OD would require additional work that does not currently exist.

---

## 3. Argument II — The Composition Gap is Structural, Not Technical

### 3.1 Lifting Produces Bounds for f ∘ g, Not for f

Every lifting theorem in the literature — Göös–Pitassi–Watson (2018), Chattopadhyay–Filmus–Koroth–Meir–Pitassi (2021), Yang–Zhang (2025), de Rezende–Vinyals (CCC 2025) — produces lower bounds for the **composed function** f ∘ g^n, not for the original function f. This is not an accidental limitation. It is intrinsic to how lifting works: the gadget g mediates between the query model and the communication/circuit model by creating a two-party structure where Alice and Bob each hold partial information about each coordinate.

Conjecture 14.1 states a lower bound on L(f ∘ g). This is correct in form — it is a lower bound on the composed function. But the conditional proof (Theorem 15.1) then needs to transfer this to a lower bound on L(f) = L(OD) itself, and this transfer is where the argument fails.

### 3.2 The "Reduction from OD ∘ g to OD" Goes the Wrong Way

Theorem 15.1 (Step 4) presents the following reduction: A circuit for OD of size S can be converted to a circuit for OD ∘ g by prepending a layer of N copies of a circuit for g. This gives:

$$\text{Circuit size}(OD \circ g) \leq S + N \cdot \text{poly}(n)$$

Therefore:

$$S \geq \text{Circuit size}(OD \circ g) - N \cdot \text{poly}(n)$$

This argument is **logically correct** but **self-defeating**. Here is why:

The inequality Circuit size(OD ∘ g) ≤ S + N · poly(n) says that OD ∘ g is *at most as hard as* OD (up to the additive preprocessing cost). In other words, OD ∘ g is the **easier** problem — you can solve it by first decoding the gadgets and then running OD. The whole point of lifting is that the composed function should be **harder** in the target model (circuit size) despite being related to f in the source model (sub-cube queries). But here, the reduction shows OD ∘ g is no harder than OD plus linear overhead.

The argument only works if the lifting conjecture provides a lower bound on OD ∘ g that is *superlinear enough* to absorb the N · poly(n) additive cost. The proof claims:

$$\text{Circuit size}(OD \circ g) \geq (N/\text{poly}(n))^{1+\Omega(1)} \cdot \text{poly}(|g|) = N^{1+\Omega(1)} / \text{poly}(n)$$

and then concludes:

$$S \geq N^{1+\Omega(1)}/\text{poly}(n) - N \cdot \text{poly}(n) \geq N^{1+\Omega(1)}/\text{poly}(n)$$

But this entire chain depends on the sub-cube lifting conjecture being true in a very strong form — the exponent 1 + Ω(1) must apply to the sub-cube query complexity parameter q = Ω(N/poly(n)). No known lifting theorem achieves this power amplification from sub-cube queries.

### 3.3 The Self-Lifting Error Recurs

The proof's own "Project Archive" (Section 13) identifies five errors in an earlier attempt to close the magnification gap. Error #3 is described as:

> *"Self-lifting: Misapplied the Alekseev–Filmus–Smal lifting theorem, which applies to composed functions f ∘ g^n, not to semantically decomposed single functions."*

This is precisely the same structural issue that Conjecture 14.1 attempts to sidestep by introducing a formal gadget composition. But the "self-lifting" hypothesis — that OD contains internal gadget structure making it behave like a composed function — is explicitly stated as **unestablished** (see the Obstacles paragraph after item (iv)). The conjecture papers over the self-lifting failure by introducing an explicit gadget, but then the compositional structure of OD ∘ g is artifactual: it does not reflect any intrinsic property of OD.

---

## 4. Argument III — Sub-cube Queries Resist Lifting for Combinatorial Reasons

### 4.1 Sub-cube Queries Are Exponentially More Powerful Than Bit Queries

A single sub-cube query on a d-dimensional sub-cube simultaneously determines the function's value on 2^d inputs. Standard lifting relies on a *bit-by-bit* gadget replacement: each input bit of f is replaced by m bits fed through g, and the key technical property (structured rectangles, ρ-structuredness) depends on the protocol querying individual gadget copies.

With sub-cube queries, the adversary can make *joint queries* across exponentially many gadget copies simultaneously. This breaks the locality assumptions underlying every known lifting technique:

- **Göös–Pitassi–Watson**: Requires that each round of the protocol effectively queries a single coordinate of f. Sub-cube queries query exponentially many coordinates at once.
- **Structured rectangles**: The ρ-structured rectangle property depends on the query structure being decomposable into independent gadget queries. Sub-cube queries create correlations across all gadgets whose indices fall in the sub-cube.
- **Sunflower-based lifting** (de Rezende–Vinyals, CCC 2025): Relies on colourful sunflower lemmas applied to sets of queried coordinates. Sub-cube queries produce sets with algebraic structure (affine sub-cubes) that break the generic combinatorial structure assumed by sunflower arguments.

### 4.2 The Ambainis–Kokainis–Kothari Separation

Ambainis, Kokainis, and Kothari (CCC 2016) proved a nearly quadratic separation between deterministic query complexity and sub-cube partition complexity. This shows that the relationship between sub-cube-based measures and standard query measures is *loose* — there can be a quadratic gap. In the lifting context, this means that a sub-cube query lower bound of q does not automatically translate to a decision tree lower bound of Ω(q), and hence the standard lifting pipeline (sub-cube → decision tree → communication → circuits) loses a quadratic factor at the very first step.

For the conditional proof to work, one needs q^{1+Ω(1)} in the circuit lower bound. A quadratic loss at the first step reduces the effective exponent, potentially dropping it below the magnification threshold.

### 4.3 No Sub-cube Lifting Theorem Exists

The proof itself acknowledges this (Approach 1, Section 21): **"No sub-cube lifting theorem exists."** This is not a temporary gap awaiting a technical fix. The entire lifting literature, spanning nearly a decade of intensive work by Göös, Pitassi, Watson, Chattopadhyay, de Rezende, Nordström, Robere, and many others, has produced lifting theorems for decision trees, parity decision trees, conjunction DAGs, nondeterministic models, and (very recently) Number-on-Forehead models. The conspicuous absence of sub-cube lifting is evidence of a genuine structural barrier, not mere neglect.

---

## 5. Argument IV — The Conjecture's Parameters Are Inconsistent

### 5.1 Gadget Size Requirement

Conjecture 14.1 requires m(n) = n^{o(1)} (sub-polynomial gadget size). Known lifting theorems require m = n^Δ for a large constant Δ (Göös–Pitassi–Watson requires Δ ≥ 1; the BPP lifting of Göös–Pitassi–Watson FOCS 2017 requires Δ = 256). Achieving lifting with sub-polynomial gadget size is a major open problem even in the *standard* decision tree-to-communication setting. Asking for it in a setting where no lifting theorem exists at all is asking for two breakthroughs simultaneously.

### 5.2 The Power Amplification q^{1+Ω(1)}

Standard lifting theorems produce bounds of the form:

$$\text{CC}(f \circ g^n) \geq \Omega(D(f) \cdot \log m)$$

where D(f) is the decision tree depth of f and m is the gadget size. The multiplicative factor is log m, which for polynomial-size gadgets (m = n^Δ) gives Θ(log n). This is a *multiplicative* relationship, not the *power* relationship q^{1+Ω(1)} demanded by Conjecture 14.1.

To get a power-type amplification from a multiplicative lifting theorem, one would need:

$$q \cdot \log m \geq q^{1+\Omega(1)} \cdot \text{poly}(m)$$

This requires log m ≥ q^{Ω(1)} · poly(m), which is impossible for any fixed gadget when q grows. The conjecture is demanding a qualitatively stronger form of lifting than anything in the literature.

### 5.3 Circuit Size vs. Communication Complexity

Even if a communication complexity lower bound were established for OD ∘ g, converting it to a circuit size lower bound requires additional steps. The Karchmer–Wigderson theorem relates *formula depth* to communication complexity, not circuit *size*. Converting communication lower bounds to circuit size lower bounds is possible only in restricted settings (e.g., monotone circuits via lifting from Resolution, as in Garg–Göös–Kamath–Sokolov). For general Boolean circuits — which is what the P ≠ NP conclusion requires — no such conversion is known.

---

## 6. Argument V — A Concrete Counterexample Strategy

### 6.1 Construction

Consider the function f : {0,1}^N → {0,1} defined as follows. Let N = 2^n and partition the N bits into 2^{n-d} blocks of 2^d bits each, where each block corresponds to a d-dimensional sub-cube. Define f to be 1 if and only if the majority of the sub-cube blocks have even parity.

This function has:
- Sub-cube query complexity Q_d(f) = Ω(2^{n-d}) = Ω(N/2^d), since each sub-cube query reveals the parity of at most one block, and the majority function on Ω(N/2^d) independent bits requires Ω(N/2^d) queries.
- Circuit complexity poly(N), because computing parity of each block takes O(2^d) size, and the majority of 2^{n-d} bits takes O(N/2^d) size, for a total of O(N).

Now compose with any gadget g of size m: f ∘ g has circuit complexity at most O(N · poly(m)) = O(N · poly(m)), because we can decode the gadgets (N · poly(m)) and then compute f (poly(N)).

The conjecture would predict:

$$L(f \circ g) \geq (N/2^d)^{1+\Omega(1)} \cdot \text{poly}(m)$$

But we have L(f ∘ g) ≤ O(N · poly(m)). For the conjecture to hold, we need:

$$(N/2^d)^{1+\Omega(1)} \leq O(N)$$

which requires (N/2^d)^{Ω(1)} ≤ O(2^d). Taking d = n/3, we need (2^{2n/3})^{Ω(1)} ≤ O(2^{n/3}), i.e., 2^{Ω(n)} ≤ 2^{n/3}, which fails for any positive constant in the Ω.

**Conclusion:** For this function, the conjecture's prediction is violated. The sub-cube query complexity is high, but the circuit complexity of the composed function is only linear, because the function has a simple circuit that the sub-cube query model cannot exploit.

### 6.2 Why This Counterexample Works

The core issue is that sub-cube query complexity can be high for "trivial" structural reasons — the function genuinely requires many sub-cube queries because each query reveals only bounded information — but the function itself is computationally easy. Lifting theorems for standard queries avoid this because decision tree complexity and circuit complexity are polynomially related for many natural functions (via the sensitivity conjecture, now theorem). But no analogue of the sensitivity theorem exists for sub-cube queries, and the Ambainis–Kokainis–Kothari separation shows the gap can be quadratic even for total functions.

---

## 7. Synthesis: The Conditional Proof's Bridge Is Broken

The conditional proof's logical structure is:

1. **Proved:** Q_d(OD) ≥ Ω(N/poly(n)) [Theorem 12.1]
2. **Conjectured:** Sub-cube lifting converts this to Circuit size(OD ∘ g) ≥ N^{1+Ω(1)} [Conjecture 14.1]
3. **Claimed:** Reduction from OD ∘ g to OD preserves this bound [Theorem 15.1, Step 4]
4. **Consequence:** Hardness magnification yields P ≠ NP [Steps 5–6]

We have shown:

- Step 2 is based on a conjecture that has no analogues in the lifting literature, contradicts the known structure of lifting theorems, and admits concrete counterexamples for natural function families.
- Step 3 reuses a reduction strategy that the proof itself acknowledges was erroneous in a prior iteration (Error #3: self-lifting).
- The parameters required (sub-polynomial gadget size, power amplification, circuit size output) are each individually beyond the state of the art, and demanding all three simultaneously is far beyond any reasonable extrapolation from existing results.

The Sub-cube Lifting Conjecture is not a natural next step in the lifting program. It is an ad hoc construction designed to fill a specific gap in a specific proof, and the existing evidence points against it rather than toward it.

---

## 8. What Would Be Needed to Salvage the Approach

For completeness, we note what a valid version of the conditional theorem would require:

1. A **sub-cube-to-communication lifting theorem** with explicit gadget construction and tight parameters — an open problem of substantial independent interest with no known partial results.
2. A method to convert **communication lower bounds to general circuit size lower bounds** — an open problem that has resisted all approaches for decades.
3. A resolution of the **constant-size gadget problem** for lifting — one of the central open problems in communication complexity.
4. An extension of gadget dichotomies to **relations** — explicitly noted as open by Alekseev–Filmus–Smal.

Each of these is a major open problem in computational complexity. Conjecture 14.1 implicitly assumes all four are resolved simultaneously and favorably. This is not a reasonable conjecture — it is a wish list.

---

## References

- Alekseev, Filmus, Smal. "Lifting Dichotomies." CCC 2024, *Computational Complexity* 2025.
- Ambainis, Kokainis, Kothari. "Nearly Optimal Separations Between Communication (or Query) Complexity and Partitions." CCC 2016.
- de Rezende, Meir, Nordström, Pitassi, Robere. "KRW Composition Theorems via Lifting." *Computational Complexity* 2024.
- de Rezende, Vinyals. "Lifting with Colourful Sunflowers." CCC 2025.
- Göös, Pitassi, Watson. "Query-to-Communication Lifting for BPP." FOCS 2017, *JACM* 2018.
- Yang, Zhang. "NOF Lifting Theorem." ECCC 2025.
