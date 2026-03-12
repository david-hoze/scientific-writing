# Restriction

Hardwire input `x_i` to value `v`, then propagate constants bottom-up.

## Propagation rules

| Pattern | Result |
|---------|--------|
| `NOT(Const b)` | `Const (not b)` |
| `AND(Const False, _)` or `AND(_, Const False)` | `Const False` |
| `AND(Const True, x)` or `AND(x, Const True)` | `x` |
| `OR(Const True, _)` or `OR(_, Const True)` | `Const True` |
| `OR(Const False, x)` or `OR(x, Const False)` | `x` |

After propagation, re-index remaining inputs: any index > i decrements by 1.

## Derived concepts

- **Restriction image** of function `f` at size `s`, direction `(i,v)`: the set of canonical strings of `restrict(F, i, v)` across all formulas F of size <= s computing f.
- **Restriction universe** `U(s,d)`: union of all restriction images across all functions and directions.
- **Max image** `M(s,d)`: the largest restriction image for any single function/direction.

## Source file

- `src/Circuit/Restriction.idr`
