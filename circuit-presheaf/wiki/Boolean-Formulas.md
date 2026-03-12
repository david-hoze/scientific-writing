# Boolean Formulas

Boolean formulas over the basis {AND, OR, NOT} with `d` input variables.

## AST

A formula is a tree:
- **Leaves**: inputs `x0..x_{d-1}` or constants `0, 1`
- **Internal nodes**: AND (fan-in 2), OR (fan-in 2), or NOT (fan-in 1)

**Size** = number of internal nodes (all gates count, including NOT).

## Truth table optimization

For d <= 4, represent truth tables as bit integers:
- `Bits8` for d=2
- `Bits16` for d=3
- `Bits32` for d=4

Hardcoded input atoms:

| d=3 | Value |
|-----|-------|
| x0 | 0xAA |
| x1 | 0xCC |
| x2 | 0xF0 |
| const0 | 0x00 |
| const1 | 0xFF |

| d=4 | Value |
|-----|-------|
| x0 | 0xAAAA |
| x1 | 0xCCCC |
| x2 | 0xF0F0 |
| x3 | 0xFF00 |
| const0 | 0x0000 |
| const1 | 0xFFFF |

Gate operations become bitwise: `AND(a,b)` = bitwise-and, `OR(a,b)` = bitwise-or, `NOT(a)` = bitwise-xor with all-ones mask. Truth table computation is O(1) per formula.

## Canonical strings

Two formulas are **DAG-isomorphic** if they have the same **canonical string**:

```
can(Input i)     = "x" ++ show i
can(Const b)     = if b then "c1" else "c0"
can(NOT c)       = "N(" ++ can(c) ++ ")"
can(AND l r)     = "A(" ++ min(can(l),can(r)) ++ "," ++ max(can(l),can(r)) ++ ")"
can(OR l r)      = "O(" ++ min(can(l),can(r)) ++ "," ++ max(can(l),can(r)) ++ ")"
```

The min/max sorts commutative children. This is the ONLY equivalence.

## Source files

- `src/Circuit/Formula.idr` - AST, evaluation, size function
- `src/Circuit/Canonical.idr` - Canonical string function, dedup via `SortedSet String`
