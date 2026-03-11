# Progressive Idris: Unifying the Spectrum from Scripts to Proofs

**A Position Paper for the Programming Languages Research Community**

---

## Abstract

We observe that the landscape of statically typed programming languages forms a spectrum from dynamically typed scripting (Python), through simple types (Go), affine ownership types (Rust), typeclass-rich polymorphism (Haskell), to full dependent types with formal verification (Idris 2, Lean 4, Coq). Each point on this spectrum corresponds to a distinct language with its own ecosystem, community, and culture. We argue that this fragmentation is unnecessary — that quantitative dependent type theory (QTT), as implemented in Idris 2, provides a coherent foundation that subsumes all these points as special cases. We propose _Progressive Idris_: an extension of Idris 2 in which type annotations are fully optional, constraints propagate via bidirectional type inference, and programmers move freely along the entire spectrum within a single language and codebase. Unlike gradual typing, Progressive Idris uses no runtime contracts or boundary checks — type annotations either propagate statically or produce compile-time errors. We sketch the core design, identify open problems, and argue that this approach could resolve the adoption barrier that has prevented dependently typed languages from achieving mainstream use.

---

## 1. Introduction

### 1.1 The Fragmentation Problem

Modern programming language design has produced remarkable results at every level of the type system spectrum. Python demonstrates that dynamic typing enables rapid prototyping and broad accessibility. Rust proves that affine types can enforce memory safety without garbage collection in systems-level code. Haskell shows that typeclasses and higher-kinded polymorphism enable powerful, reusable abstractions. Idris 2 and Lean 4 demonstrate that dependent types can encode arbitrary program invariants and enable machine-checked proofs of correctness.

Yet these results exist in isolation. A team building safety-critical software in Idris cannot leverage Haskell's extensive library ecosystem. A Rust developer who wants to formally verify a protocol must leave their language entirely. A Python prototype that matures into production code must be rewritten — not refactored — in a language with stronger guarantees. The essential complexity of a problem is conserved across all these languages; what changes is where the complexity is managed — in types, in tests, in documentation, or in bug reports.

We argue that this fragmentation is not an inevitable consequence of theoretical trade-offs, but a historical accident. The theoretical foundation for a unified language already exists.

### 1.2 The Core Thesis

Quantitative type theory (Atkey 2018, as realized in Idris 2) provides a single calculus in which:

- Unrestricted variables recover conventional functional programming (Haskell-like)
- Linear variables (multiplicity 1) recover resource tracking (Rust-like ownership)
- Erased variables (multiplicity 0) recover compile-time-only computation (Zig-like comptime)
- Dependent function types recover full specification and verification (Lean/Coq-like proofs)

What is missing is not the foundational theory but the _language design_ that makes this spectrum accessible without requiring all users to understand its full depth. Progressive Idris aims to provide exactly this design.

### 1.3 The Insight: Propagation, Not Boundaries

Gradual typing (Siek & Taha 2006) proposed adding optional types to dynamic languages, with runtime contracts enforcing type invariants at the boundaries between typed and untyped code. This approach has well-documented limitations: performance degradation at boundaries (Takikawa et al. 2016), blame tracking complexity, and cultural fragmentation between typed and untyped codebases.

We propose a fundamentally different mechanism: _type propagation_. In Progressive Idris, when a programmer adds a type annotation, the constraint propagates through the program's call graph via standard bidirectional type inference. Connected code must either satisfy the constraint (verified statically) or the programmer must acknowledge the gap with an explicit typed hole. There are no runtime contracts, no boundary wrappers, no performance penalty. Types are contagious — adding an annotation exerts static pressure on all connected code, naturally expanding the verified region of the program.

This design yields a critical property: **annotation monotonicity**. Adding a type annotation can only increase the static guarantees of a program, never change its runtime behavior. This is stronger than the gradual guarantee of Siek et al., which permits runtime blame errors when annotations are added.

---

## 2. Background and Related Work

### 2.1 Quantitative Type Theory

Atkey (2018) introduced quantitative type theory (QTT), which extends dependent type theory with a semiring of _quantities_ (or _multiplicities_) annotating each variable binding. In the system adopted by Idris 2 (Brady 2021), the multiplicities are:

- **0** (erased): the variable is used only at the type level and has no runtime representation. This enables type-level computation analogous to Zig's `comptime` or C++'s `constexpr`, but within a dependently typed framework.
- **1** (linear): the variable must be used exactly once. This enforces resource protocols — memory that must be freed, channels that must be closed, tokens that must be consumed.
- **ω** (unrestricted): the variable may be used any number of times. This is the default mode, recovering conventional functional programming.

The key insight for our purposes is that these multiplicities are not separate language features but _parameters_ of a single unified system. A program that uses only ω-variables is a Haskell program. A program that uses 1-variables for resource-sensitive operations is a Rust-like program. A program that uses 0-variables for type-level computation is doing compile-time metaprogramming. These are not different languages — they are different configurations of one calculus.

### 2.2 Gradual Typing and Its Limitations

The gradual typing research program (Siek & Taha 2006, 2007; Tobin-Hochstadt & Felleisen 2008) demonstrated that static and dynamic typing can coexist in a single language via a _dynamic type_ that is compatible with all static types, mediated by runtime casts.

Practical experience with gradual typing has revealed persistent challenges:

- **Performance**: Typed Racket demonstrated that boundary contracts can impose order-of-magnitude slowdowns (Takikawa et al. 2016). The "gradual typing performance problem" remains an active research area.
- **Semantic gaps**: The interaction between dynamic types and parametric polymorphism produces surprising behaviors (Ahmed et al. 2011).
- **Cultural fragmentation**: In TypeScript, the `any` type serves as a permanent escape hatch, leading to codebases where gradual typing's benefits are never fully realized.

Our proposal avoids these issues entirely by eliminating the dynamic-static boundary as a runtime concept.

### 2.3 Existing Partial Solutions

Several existing systems address portions of the spectrum we target:

- **Liquid Haskell** (Vazou et al. 2014) adds refinement types to Haskell, enabling lightweight verification via SMT solvers. This demonstrates the value of optional, incremental verification but is limited to refinement predicates and sits outside the core language.
- **F*** (Swamy et al. 2016) combines dependent types with an effect system and SMT-backed proof automation. It represents the closest existing approximation to our vision, but requires upfront commitment to its type discipline and has a steep learning curve.
- **Typed Racket** (Tobin-Hochstadt & Felleisen 2008) demonstrates sound gradual typing but with the performance and boundary costs noted above.
- **Idris 2** (Brady 2021) provides the theoretical foundation (QTT) but requires full type discipline from the start — there is no "untyped" mode of operation.
- **Lean 4** (de Moura et al. 2021) offers strong tooling and tactic-based proving but similarly requires engagement with the type system from the outset.

None of these systems allow a programmer to begin with fully untyped code and incrementally add type annotations up to and including dependent types and formal proofs, within a single language, with no runtime cost. Progressive Idris addresses this gap directly.

---

## 3. Design Overview

### 3.1 The Spectrum

We envision Progressive Idris as a language in which the same syntactic constructs can be used at any level of type precision, with a unified underlying theory based on QTT. The levels are not discrete modes but a continuum; we identify representative points for exposition:

**Level 0 — Untyped.** No annotations. The compiler infers what it can (following Hindley-Milner-style inference where applicable) and treats genuinely unconstrained values as having an implicit type `Infer` (not `Dynamic` — there is no runtime representation of the dynamic type). Code at this level resembles Python or an ML with optional annotations.

```
def sort(xs) =
  -- implementation here
```

**Level 1 — Simply typed.** Basic type annotations on function signatures. Standard parametric polymorphism. Resembles Go or early TypeScript.

```
def sort(xs : List(Int)) -> List(Int) =
  -- implementation here
```

**Level 2 — Polymorphic with typeclasses.** Higher-kinded types, typeclass (interface) constraints, monad abstractions. Resembles Haskell.

```
def sort(xs : List(a)) -> List(a) given Ord(a) =
  -- implementation here
```

**Level 3 — Linear and resource-aware.** Multiplicity annotations for resource tracking. Resembles Rust's ownership model, but expressed within QTT.

```
def sort(1 xs : List(Int)) -> List(Int) =
  -- in-place sort, xs consumed linearly
```

**Level 4 — Dependently typed with specifications.** Return types that depend on input values. Lightweight specifications discharged by SMT or simple tactics.

```
def sort(xs : List(Int)) -> (ys : List(Int) ** Sorted(ys)) =
  -- compiler verifies sortedness via SMT or tactic
```

**Level 5 — Fully verified.** Complete formal proofs of arbitrary properties. Machine-checked correctness theorems. Resembles Idris 2, Lean 4, or extracted Coq.

```
def sort(xs : List(Int)) -> (ys : List(Int) ** (Sorted(ys), Perm(xs, ys))) =
  let ys = mergesort(xs)
  (ys ** (mergesort_sorted xs, mergesort_perm xs))
```

All levels coexist in a single compilation unit. Functions at different levels can call each other, subject to the propagation rules described below.

### 3.2 Type Propagation

The central mechanism is _type propagation via bidirectional type inference_, extended to handle the full QTT. When a programmer adds a type annotation, the consequences are:

**Outward propagation (to callers).** If a function's parameter is annotated with type `T`, every call site must supply an argument of type `T`. If the caller is unannotated, the compiler propagates the constraint backward, inferring the necessary types at the call site. If propagation reaches code that is inconsistent with the constraint, a compile-time error is reported.

**Inward propagation (to implementations).** If a function's return type includes a dependent type or proof obligation, that obligation propagates inward to the function body. The implementation must produce a value of the specified type, including any proofs.

**Typed holes as deferred obligations.** When propagation reaches a point where the programmer is not ready to satisfy the constraint, they write `?`, producing a _typed hole_. The compiler records the hole's type and context, reports it as a warning, and continues compilation. The program is executable — holes are replaced with `error("unfilled hole: ...")` at runtime, analogous to Idris 2's existing hole mechanism but positioned as a standard part of the development workflow rather than an escape hatch.

**No runtime boundaries.** Crucially, there are no runtime casts or contracts at the interface between annotated and unannotated code. The unannotated code is either constrained by propagation (and verified statically) or explicitly marked with a hole (and flagged as incomplete). This eliminates the performance and semantic issues of gradual typing entirely.

### 3.3 The `Infer` Type

Unannotated code is not dynamically typed. Instead, unannotated bindings have type `Infer`, which the compiler resolves via inference. `Infer` is an instruction to the compiler, not a runtime type:

- If inference succeeds, `Infer` is replaced with the inferred type, and the code is statically checked at whatever level the inferred type requires.
- If inference fails (insufficient information), the binding remains polymorphic (universally quantified), like an unannotated top-level binding in Haskell.
- If inference produces a contradiction, a compile-time error is reported.

This means that even fully "untyped" code receives as much static checking as the compiler can infer — in practice, significantly more than a dynamically typed language provides. The programmer gets safety for free, and annotations add precision beyond what inference alone can determine.

### 3.4 Multiplicity Inference

QTT multiplicities (0, 1, ω) are inferred by default. The compiler analyzes usage patterns:

- If a variable is used exactly once, it _may_ be treated as linear, enabling optimizations (in-place mutation, deterministic resource cleanup).
- If a variable is used multiple times, it is unrestricted.
- If a variable appears only in types and not in runtime expressions, it is erased.

Explicit multiplicity annotations override inference, enabling the programmer to _enforce_ linearity (triggering compile errors if a linear variable is used more than once) rather than merely permitting it. This mirrors the distinction between Rust's ownership rules (enforced) and a compiler's internal alias analysis (inferred).

---

## 4. Key Properties

### 4.1 Annotation Monotonicity

**Property.** Adding a type annotation to a well-typed program either: (a) preserves well-typedness and does not change runtime behavior, or (b) reveals a type error that was previously undetected.

This is strictly stronger than the gradual guarantee, which allows runtime blame errors when annotations are added. In our system, annotations only add static knowledge; they never introduce runtime failure (except via typed holes, which are explicit and tracked).

### 4.2 Progressive Verification

**Property.** For any program at level _n_ on the spectrum, there exists a sequence of annotation additions that lifts it to level _n+1_ without requiring a rewrite.

This is the key usability property. A Python-like prototype can be incrementally transformed into a fully verified program by adding annotations — never by rewriting in a different language or paradigm.

### 4.3 Ecosystem Unification

**Property.** Libraries written at any level on the spectrum are callable from code at any other level, with type propagation mediating the interface.

A library author who provides rich type annotations exports stronger guarantees to callers. A library with minimal annotations is still usable — callers simply receive weaker static guarantees about its behavior. This allows a single package ecosystem to serve all communities, from scripters to verifiers.

---

## 5. Open Problems

We identify several significant challenges that must be addressed for this design to be realized:

### 5.1 Inference for Dependent Types

Hindley-Milner inference is decidable and complete for simple types but does not extend to dependent types, where type checking involves arbitrary computation and unification is undecidable in general. A practical language must define a _predictable subset_ of dependent types for which inference works well, with clear error messages when the programmer must provide annotations. The experience of Lean 4's elaborator and Idris 2's unifier may guide this design, but significant work remains to make inference feel seamless in the progressive context.

### 5.2 SMT Integration for Automated Proof

For refinement types and lightweight specifications (Levels 4-5), SMT solvers can discharge many proof obligations automatically. However, SMT-based verification introduces unpredictability — small changes to code can cause previously automatic proofs to fail. F*'s experience demonstrates both the power and the fragility of this approach. Progressive Idris must manage SMT integration carefully, perhaps by distinguishing "SMT-dischargeable" obligations from those requiring manual proof and providing clear feedback about which category a given obligation falls into.

### 5.3 Compilation and Performance

Compiling dependently typed languages to efficient native code remains challenging. Erasing types and proofs (multiplicity 0) is well-understood, but generating competitive code from the remaining runtime terms requires optimization passes that current dependently typed compilers lack. Koka's Perceus reference counting (Reinking et al. 2021) and Lean 4's compilation to C with reference counting provide promising models, but significant engineering effort is needed to match the code quality of GHC or LLVM-backed compilers.

### 5.4 Error Messages Across the Spectrum

A compiler that serves both novice scripters and expert type theorists must provide error messages appropriate to the programmer's current level of engagement. In Progressive Idris, a Level 0 user should see Python-like error messages about arity and basic mismatches. A Level 5 user should see detailed proof state. The challenge is not merely presentation but _selecting what to report_ — at Level 0, many potential issues are deliberately left unchecked, and the compiler must not overwhelm the programmer with warnings about properties they have not yet chosen to enforce.

### 5.5 Typed Holes and Deferred Obligations

The design relies heavily on typed holes (`?`) as the mechanism for deferring proof obligations. For this to work well, the tooling must provide:

- A clear inventory of all holes in a project, with their types and contexts
- Guidance on how to fill each hole (suggested tactics, counterexamples for invalid holes)
- Integration with CI/CD to track holes as technical debt metrics

The hole mechanism must also interact correctly with SMT automation — an obligation that the SMT solver can discharge should not require a manual hole, but an obligation that the solver _sometimes_ discharges (depending on heuristics) must be handled predictably.

### 5.6 Module System and Separate Compilation

Dependent types complicate separate compilation because type checking may require evaluating terms from other modules. A practical progressive language must support incremental compilation that scales to large codebases. Lean 4's approach to this problem (compiling to C, using `.olean` intermediate files) may be instructive.

---

## 6. Toward Implementation

We suggest that the shortest path to a prototype of Progressive Idris is modification of Idris 2 itself rather than a from-scratch effort:

**Idris 2** provides the theoretical foundation (QTT) and already supports typed holes, multiplicity annotations, and elaborator reflection. Extending it with `Infer`-based optional annotations, enhanced type inference for unannotated code, and SMT integration for automated proof discharge would yield a prototype of Progressive Idris. The primary engineering challenges are: making the elaborator robust to missing annotations, designing the inference/propagation boundary, and producing quality error messages for partially-annotated code.

**Lean 4** offers superior performance and tooling but lacks native multiplicity tracking. Adding QTT-style multiplicities to Lean's type theory would be a more fundamental change, though Lean's metaprogramming capabilities might allow prototyping some progressive features as a macro layer. A "Progressive Lean" variant is conceivable but would require deeper foundational changes than the Idris 2 path.

---

## 7. Conclusion

The programming language community has developed powerful type-theoretic foundations — dependent types, linear types, algebraic effects — but has failed to make them accessible to the majority of working programmers. We argue that the barrier is not theoretical but ergonomic: existing dependently typed languages demand full engagement with the type system from the first line of code, creating an all-or-nothing adoption dynamic that most practitioners reject.

Progressive dependent types, as realized in Progressive Idris, offer an alternative: a single language spanning the full spectrum from untyped scripting to formally verified code, with type annotations that propagate constraints statically rather than enforcing them at runtime boundaries. The theoretical foundation (QTT) exists. The host language (Idris 2) exists. The design space, while challenging, is tractable. The potential reward — a unified ecosystem that serves all programming communities — justifies the effort.

We invite the programming languages research community to investigate the Progressive Idris design space further, with particular attention to the open problems of dependent type inference, SMT integration, compilation performance, and cross-spectrum error reporting.

---

## References

- Ahmed, A., Findler, R. B., Siek, J. G., & Wadler, P. (2011). Blame for all. _POPL '11_.
- Atkey, R. (2018). Syntax and semantics of quantitative type theory. _LICS '18_.
- Brady, E. (2021). Idris 2: Quantitative type theory in practice. _ECOOP '21_.
- de Moura, L., & Ullrich, S. (2021). The Lean 4 theorem prover and programming language. _CADE-28_.
- Reinking, A., Xie, D., de Moura, L., & Leijen, D. (2021). Perceus: Garbage free reference counting with reuse. _PLDI '21_.
- Siek, J. G., & Taha, W. (2006). Gradual typing for functional languages. _Scheme and Functional Programming Workshop_.
- Swamy, N., et al. (2016). Dependent types and multi-monadic effects in F*. _POPL '16_.
- Takikawa, A., Feltey, D., Greenman, B., New, M. S., Vitek, J., & Felleisen, M. (2016). Is sound gradual typing dead? _POPL '16_.
- Tobin-Hochstadt, S., & Felleisen, M. (2008). The design and implementation of Typed Scheme. _POPL '08_.
- Vazou, N., Seidel, E. L., Jhala, R., Vytiniotis, D., & Peyton Jones, S. (2014). Refinement types for Haskell. _ICFP '14_.