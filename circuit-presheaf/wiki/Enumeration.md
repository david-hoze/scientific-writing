# Enumeration

Size-stratified formula generation.

## Algorithm

Generate size-s formulas by combining:
- NOT of any size-(s-1) formula
- AND or OR of formulas with sizes s1 + s2 = s-1, iterating s1 from 0 to (s-1)/2

Formulas are grouped by exact size in a `SortedMap Nat (List Formula)`. Only combine across size levels -- never re-examine all previously generated formulas. Deduplicate by canonical string.

Also maintains a map from truth table (as bit integer) to (size -> list of formulas) for analysis.

## Source file

- `src/Circuit/Enumerate.idr`
