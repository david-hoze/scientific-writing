# Sub-Cubes and Compatibility CSP

## Sub-cubes

For n-bit truth table T and sub-cube dimension d:
- A sub-cube is `(freeCoords : List (Fin n), fixedValues : List (Fin n, Bool))`
- Enumerate all sub-cubes: choose d coords to be free, fix the rest to all 2^(n-d) combinations
- The sub-function: evaluate T on the sub-cube's inputs
- Structural intersection graph: edges between sub-cubes sharing >= 1 free coordinate

## Compatibility CSP

- **Nodes** = sub-cubes
- **Domains** = canonical forms of formulas computing the sub-function at size <= s
- **Edges** = structural edges (shared free coordinates)
- **Constraints** = STRUCT-MATCH: restrictions to overlap must be DAG-isomorphic

For each edge, compute compatible pairs: `(c_i, c_j)` where `can(restrict(c_i, overlap)) == can(restrict(c_j, overlap))`.

### Edge classification

- **Fully compatible**: all pairs are compatible
- **Partially compatible**: some pairs compatible, some not
- **Fully incompatible**: no pairs are compatible

## Source files

- `src/Analysis/SubCube.idr`
- `src/Analysis/CompatCSP.idr`
