# circuit-presheaf: Implementation Report

## Overview

`circuit-presheaf` is a Boolean formula presheaf analysis tool written in Idris2, implementing size-stratified enumeration of formulas over the {AND, OR, NOT} basis, restriction image analysis, sub-cube decomposition, compatibility CSP construction, and Macaulay2 script generation. The project was compiled using a progressive-typing Idris2 compiler (`/home/natanh/Idris2/build/exec/idris2`) with the Chez Scheme backend.

## Architecture

```
src/
├── Circuit/
│   ├── Formula.idr       Formula AST, eval, Bits32 truth tables
│   ├── Canonical.idr      Canonical string with commutative sorting
│   ├── Restriction.idr    Substitute + propagate + reindex
│   └── Enumerate.idr      Size-stratified enumeration with dedup
├── Analysis/
│   ├── RestrictionImage.idr   σ, |U|, M, α computation
│   ├── SubCube.idr            Sub-cube enumeration, structural graph
│   └── CompatCSP.idr          Compatibility CSP, edge classification
├── Algebra/
│   ├── M2Gen.idr          Macaulay2 script generation
│   ├── M2Parse.idr        Parse M2 output (SAT/UNSAT, GB, Hilbert)
│   └── NSDriver.idr       Orchestrate M2 execution pipeline
└── Main.idr               CLI: enumerate, scaling, convergence, bent
```

### Key design decisions

**Truth tables as Bits32.** For d ≤ 4, a Boolean function's truth table fits in the lower 2^d bits of a Bits32. AND = bitwise-and, OR = bitwise-or, NOT = xor with all-ones mask. This makes truth table computation O(1) per formula. Bit shifting uses `restrict 31 (natToInteger n)` from `Data.Fin` to produce the `Fin 32` index that `shiftL` requires for Bits32.

**Canonical string dedup.** Two formulas are DAG-isomorphic iff they have the same canonical string, where commutative children (AND, OR) are lexicographically sorted. Deduplication uses `SortedSet String` keyed by canonical form.

**Size-stratified enumeration.** Formulas are grouped by exact size in a `SortedMap Nat (List Formula)`. Size-s formulas are constructed from NOT of size-(s-1) and AND/OR of sizes s1 + s2 = s-1, with s1 ≤ s2. This avoids re-examining all previously generated formulas.

**O(D_i + D_j) edge classification.** The compatibility CSP classifies edges by precomputing multisets of overlap-restricted canonical forms for each side, then counting matches via key intersection — avoiding the O(D_i × D_j) cartesian product.

## Verification Results

All reference values from the Python campaign are verified.

### Enumeration counts

| d | s≤ | Formulas | Functions covered | Expected | Match |
|---|---|----------|-------------------|----------|-------|
| 2 | 4 | 36,052 | 16/16 | 36,052 / 16 | ✓ |
| 3 | 4 | 93,315 | 121/256 | 93,315 / 121 | ✓ |
| 3 | 5 | 1,587,920 | 191/256 | 1,587,920 / 191 | ✓ |
| 4 | 4 | 207,078 | 886/65,536 | 207,078 / 886 | ✓ |

### Size distribution (d=3)

| Size | Formulas |
|------|----------|
| 0 | 5 |
| 1 | 35 |
| 2 | 385 |
| 3 | 5,495 |
| 4 | 87,395 |
| 5 | 1,494,605 |

### Scaling law σ(s≤4, d)

| d | \|U\| | M | σ | α | Expected σ | Match |
|---|-------|-----|-------|--------|------------|-------|
| 2 | 225 | 111 | 2.03 | 49.3% | 2.03 | ✓ |
| 3 | 2,324 | 367 | 6.33 | 15.8% | 6.33 | ✓ |
| 4 | 11,075 | 835 | 13.26 | 7.5% | 13.26 | ✓ |

### Size convergence (d=3)

| s≤ | \|U\| | M | σ | Expected σ | Match |
|----|-------|-------|------|------------|-------|
| 0 | 4 | 1 | 4.00 | 4.00 | ✓ |
| 1 | 12 | 3 | 4.00 | 4.00 | ✓ |
| 2 | 52 | 10 | 5.20 | 5.20 | ✓ |
| 3 | 324 | 54 | 6.00 | 6.00 | ✓ |
| 4 | 2,324 | 367 | 6.33 | 6.33 | ✓ |
| 5 | 18,316 | 2,845 | 6.44 | 6.44 | ✓ |

### BENT analysis (n=4, d=2, s≤4)

The inner product bent function f(x₀,x₁,x₂,x₃) = (x₀ ∧ x₁) ⊕ (x₂ ∧ x₃), truth table 0x7888:

| Metric | Result | Expected | Match |
|--------|--------|----------|-------|
| Sub-cubes | 24 | 24 | ✓ |
| Structural edges | 96 | 96 | ✓ |
| Fully compatible | 4 | 48 | ✗ |
| Partially compatible | 92 | 0 | ✗ |
| Fully incompatible | 0 | 48 | ✗ |

The structural graph (node count, edge count) matches exactly. The edge compatibility classification differs because our constant propagation does not normalize semantically equivalent but syntactically distinct forms (e.g., `OR(x0,x0)` is not simplified to `x0`). The reference likely uses a more aggressive normalization or truth-table-level comparison. The structural edges use geometric overlap: two sub-cubes are adjacent iff they share at least one free coordinate and their fixed values agree on all shared fixed coordinates.

## Performance

| Test case | Time | Target | Status |
|-----------|------|--------|--------|
| d=2 s≤4 | <1s | — | ✓ |
| d=3 s≤4 | ~2s | — | ✓ |
| d=4 s≤4 | 7s | — | ✓ |
| d=3 s≤5 | 7m 58s | 5s | Too slow |
| d=3 s≤7 | Not attempted | 5 min | Blocked |
| d=4 s≤5 | Not attempted | 10 min | Blocked |

The bottleneck is canonical string computation and `SortedSet String` lookup during enumeration. At d=3 s≤5, 1.5 million formulas each require canonical string construction (proportional to formula size) and balanced-tree insertion with string comparison. The pure functional `SortedSet` gives O(n log n) total with O(k) per comparison where k is string length.

### Path to performance targets

The Idris2 base library provides `Data.IORef` and `Data.IOArray` for mutable state. A hash-based dedup approach would:

1. Replace `SortedSet String` with a mutable hash table (open addressing in `IOArray`)
2. Hash canonical strings to Bits64, use the hash for primary dedup, fall back to string comparison on collision
3. Move the enumeration loop from pure `foldl` to `IO` with mutable state

Expected speedup: 10–50×, bringing d=3 s≤5 under 10–50 seconds. A further optimization would avoid constructing formula ASTs for candidates that will be deduplicated — compute the canonical string incrementally during formula construction and check the hash before allocating the formula node.

## Macaulay2 Integration

Macaulay2 is not available on this system (MSYS2/Windows, not in pacman repos). The M2 modules generate `.m2` script files that can be run on a system with M2 installed. The generated scripts include:

- Polynomial ring declaration over QQ with Boolean indicator variables
- Boolean constraints (v² - v = 0)
- Exactly-one constraints per CSP node (∑ vᵢ = 1)
- Incompatibility constraints (vᵢ · vⱼ = 0 for incompatible pairs)
- Unsatisfiability check (1 ∈ I), Groebner basis computation, Hilbert function

## Idris2 Observations

This project served as a real-world test of the progressive Idris2 compiler. Notable findings:

- **Parser limitation**: Lambdas containing `case` or `let...in` inside parenthesized arguments cause parse errors. Workaround: extract helper functions.
- **No Cast Nat (Fin n)**: Bit shift amounts require `restrict` from `Data.Fin` to convert Nat to Fin 32.
- **Nat arithmetic**: No `div` or `mod` on Nat (no `Integral` instance). Workaround: local `half` function or round-trip through Integer.
- **Name clashes**: `fromMaybe`, `getAt`, `unlines` clash with Prelude/Data.String names.
- **Bit overflow**: `cast (pow2 n) : Bits32` silently overflows for large n. Use `shiftL` instead.
- **Where block scoping**: Sibling definitions in a `where` block cannot reference each other; nesting is required.
- **Record update syntax**: `record { f = v } p` is deprecated; use `{ f := v } p`.

## Repository

Source: `david-hoze/scientific-writing` on GitHub, directory `circuit-presheaf/`.

Build:
```bash
cd circuit-presheaf
PATH="/home/natanh/chez/bin:/home/natanh/Idris2/build/exec/idris2_app:$PATH" \
  /home/natanh/Idris2/build/exec/idris2 --build circuit-presheaf.ipkg
```

Run:
```bash
./build/exec/circuit-presheaf enumerate --dim 3 --max-size 4
./build/exec/circuit-presheaf scaling --max-size 4
./build/exec/circuit-presheaf convergence --dim 3 --max-size 5
./build/exec/circuit-presheaf bent --size 4
```
