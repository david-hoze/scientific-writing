# Test Vectors

Known values from the Python campaign. All must match before proceeding to new computations.

## Enumeration counts

| d | s<= | formulas | functions covered |
|---|-----|----------|-------------------|
| 2 | 4 | 36,052 | 16/16 |
| 3 | 4 | 93,315 | 121/256 |
| 3 | 5 | 1,587,920 | 191/256 |
| 4 | 4 | 207,078 | 886/65,536 |

## Scaling law sigma(s,d)

| d | s<=4 \|U\| | s<=4 M | s<=4 sigma |
|---|------------|--------|------------|
| 2 | 225 | 111 | 2.03 |
| 3 | 2,324 | 367 | 6.33 |
| 4 | 11,075 | 835 | 13.26 |

## Size convergence at d=3

| s<= | \|U\| | M | sigma |
|-----|-------|---|-------|
| 0 | 4 | 1 | 4.00 |
| 1 | 12 | 3 | 4.00 |
| 2 | 52 | 10 | 5.20 |
| 3 | 324 | 54 | 6.00 |
| 4 | 2,324 | 367 | 6.33 |
| 5 | 18,316 | 2,845 | 6.44 |

## Top share alpha(d)

- alpha(d=2) = 49.3%
- alpha(d=3) = 15.8%
- alpha(d=4) = 7.5%

## BENT at n=4, d=2, s<=4

- 24 sub-cubes
- 96 structural edges
- 48 incompatible (50%)
- 48 compatible

## Minimum formula sizes (d=3)

XOR=4, AND=1, AND3=2, OR3=2, NAND3=3, MAJ3=4, PAR3=11

## Canonical compression (d=3, s<=4)

Ratio of raw formulas to canonical DAG classes ranges from 1.1 to 2.0 across all functions, median 2.0.
