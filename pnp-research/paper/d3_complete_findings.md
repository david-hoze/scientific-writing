# The Complete d = 3 Circuit Complexity Landscape

## The Periodic Table of 3-Input Boolean Functions in {AND, OR, NOT}

---

## The Full Spectrum

The integer truth-table enumeration resolved the circuit complexity of all 256 Boolean functions on 3 inputs in the {AND, OR, NOT} basis (all gates counted):

| Min size | # functions | Cumulative | Notable functions |
|---|---|---|---|
| 0 | 5 | 5 | x₀, x₁, x₂, ZERO, ONE |
| 1 | 9 | 14 | AND, OR, NOT |
| 2 | 26 | 40 | NAND, NOR, implications |
| 3 | 44 | 84 | AND₃, OR₃, threshold functions |
| 4 | 37 | 121 | XOR, XNOR, MAJ₃, MUX |
| 5 | 70 | 191 | Mixed-complexity functions |
| 6 | 35 | 226 | Unanimity, near-threshold |
| 7 | 6 | 232 | — |
| 8 | 16 | 248 | — |
| 9 | 6 | 254 | Last "ordinary" functions |
| 10 | 0 | 254 | (gap) |
| 11 | 1 | 255 | **PAR3 (3-bit parity)** |
| 12 | 1 | 256 | **NPAR3 (NOT parity)** |

**PAR3 requires 11 gates** — making it the single hardest 3-input function. It is isolated by a gap of 2 full size levels (zero functions at size 10) from the next-hardest functions at size 9. NPAR3 = NOT(PAR3) requires 12 gates (trivially, by adding one NOT).

---

## Structural Profiles by Size Tier

### Tier 1: Size 4 (XOR/XNOR family)

| Function | #circuits | Φ | Category |
|---|---|---|---|
| XOR₀₁ | 1 | 3 | **STIFF** |
| XOR₁₂ | 1 | 3 | **STIFF** |
| XNOR₀₁ | 1 | 3 | **STIFF** |
| XNOR₁₂ | 1 | 3 | **STIFF** |
| MAJ₃ | 6 | 4 | MODERATE |

At size 4, XOR/XNOR are the rigid bottlenecks with singleton circuit domains and fluidity Φ = 3.

### Tier 2: Size 5 (all moderate)

All 70 newly covered functions at size 5 are MODERATE (Φ = 4–16). Zero are stiff. This is the "softening" tier where XOR-family functions (now having circuits at sizes 4 and 5) gain fluidity.

At this point, the XOR bottleneck has dissolved.

### Tier 3: Size 6 (new non-XOR stiffness)

| Function | #circuits | Φ | Category |
|---|---|---|---|
| f01111110 (diversity) | 9 | 2 | **STIFF** |
| f10000001 (unanimity) | 9 | 2 | **STIFF** |
| (33 others) | 1–4 | 4–14 | MODERATE |

Two genuinely non-XOR functions are STIFF at size 6, with fluidity Φ = 2 — stiffer than XOR was at its frontier. These are the unanimity/diversity detectors: functions that test whether all three inputs agree.

### Tier 4: Sizes 7–9 (moderate, thinning)

Only 28 functions remain at sizes 7–9, in decreasing numbers (6, 16, 6). Based on the pattern from previous tiers, these are expected to be moderate.

### Tier 5: Size 11 (PAR3 — predicted extreme stiffness)

PAR3 stands alone at size 11. Building circuit objects at this size is computationally infeasible in the current environment. However, the structural prediction is strong:

**PAR3 is almost certainly STIFF at size 11, likely with Φ ≤ 3.**

The evidence:

1. **Extreme scarcity.** Only 1 new function appears at size 11, after a gap of zero functions at size 10. This implies very few circuit implementations exist at size 11.

2. **Restriction structure.** PAR3(x₀,x₁,x₂) = x₀ ⊕ x₁ ⊕ x₂. Fixing any one input produces XOR or XNOR of the remaining two — which at size 4 has exactly 1 circuit. The restriction of an 11-gate PAR3 circuit, after hardwiring one input and propagating constants, should reduce to a circuit computing XOR/XNOR. Whether this reduced circuit is the unique size-4 XOR or a larger non-minimal structure determines the restriction image. But with very few PAR3 circuits available, the restriction image is necessarily small.

3. **Pattern confirmation.** Every function family that has been the hardest at its size tier has been stiff at first coverage: XOR at 4, unanimity at 6. PAR3 at 11 would continue this pattern.

---

## The Recurrence of Frontier Stiffness

| Size | Stiff functions | Function family | Φ |
|---|---|---|---|
| 4 | 4 | XOR/XNOR | 3 |
| 5 | 0 | (XOR softened) | — |
| 6 | 2 | Unanimity/Diversity | 2 |
| 7–10 | (unknown, likely 0) | (previous stiff softened) | — |
| 11 | 1 (predicted) | PAR3 | ≤3 (predicted) |

The pattern is: stiffness appears at each coverage frontier, driven by a different function family each time. It dissolves at the next size level as additional circuits provide structural flexibility. Then it reappears at the next frontier, from a different family.

This is not a single bottleneck that persists forever — it is a *moving frontier of bottlenecks* that regenerates at each new complexity tier.

---

## Implications for the Propagation Conjecture

### For high-complexity truth tables at d = 3

A truth table T on n bits with high global circuit complexity will have many sub-cubes whose 3-input sub-functions are at or near the coverage frontier. At any size budget s, some fraction of sub-functions will be "just barely coverable" — having min-size exactly s or s−1 — and these frontier functions will be stiff or moderate.

The critical quantity is: what fraction of sub-functions land at the coverage frontier for a random high-complexity T? If this fraction is Ω(1), then the misalignment rate M(T, s) is bounded away from zero (because stiff frontier functions misalign with their neighbors), and the Propagation Conjecture holds.

### The PAR3 prediction as a test

If PAR3 is confirmed as stiff at size 11 (which requires either a more efficient enumeration or a theoretical argument about the restriction structure of near-optimal parity circuits), it would establish:

1. **Frontier stiffness recurs across three independent function families** (XOR, unanimity, parity) — not a property of any one family.

2. **The complexity gap amplifies stiffness.** PAR3's 2-level gap (sizes 10–11 are empty/singleton) means it is even more isolated than XOR was at size 4 (which had 37 other functions at the same level). Greater isolation implies fewer circuits and stronger stiffness.

3. **Parity is the universal structural anchor.** In higher dimensions (d > 3), parity on d bits will be the hardest d-input function, and its circuit complexity in {AND, OR, NOT} grows as Θ(4^{d/2}) (Khrapchenko-type bounds). This means parity sub-functions become exponentially rigid as d grows — exactly the scaling behavior needed for the Propagation Conjecture to produce Ω(1/n^c) misalignment.

### The Khrapchenko connection

Khrapchenko's theorem (1971) gives a lower bound of n² on the formula complexity of n-bit parity over {AND, OR}. In our gate-counting model (where NOT is a gate), the minimum circuit size for n-bit parity appears to be:

| n | Min size | Formula |
|---|---|---|
| 2 | 4 | Known (XOR) |
| 3 | 11 | New data |
| 4 | ? | Predicted ~30+ |

The super-linear growth from 4 to 11 (factor 2.75 for one additional input) is consistent with the quadratic lower bound. If this scaling continues, d-bit parity at d = O(log n) would have circuit complexity Ω(log² n) — polynomially growing in the dimension, ensuring that parity sub-functions are deep in the coverage frontier for any polynomial size budget.

---

## Complete Data Files

The following scripts and data support these findings:

- `d3_catalog.py`: Size ≤ 4 enumeration, 121/256 functions, structural profiles
- `d3_size5.py`: Size ≤ 5 enumeration, 191/256 functions, moderate tier
- `d3_size6.py`: Size ≤ 6 targeted, 226/256 functions, unanimity stiffness
- `par3_audit.py`: Size ≤ 7 + integer enumeration to size 12, PAR3 min-size = 11
- All scripts include circuit enumeration, restriction image computation, and fluidity analysis
