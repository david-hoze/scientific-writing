#!/usr/bin/env python3
"""
Compute Nullstellensatz degree over GF(2) via linear algebra.

Over GF(2):
- x^2 = x for all variables (Boolean constraints are free)
- All monomials are multilinear (squarefree)
- Arithmetic is mod 2

NS degree d over GF(2): 1 = sum_a p_a(x) * f_a(x) mod (x_i^2 + x_i)
where deg(p_a * f_a) <= d, all coefficients in GF(2).

Generators (no Boolean constraints needed):
- Selection: sum(x_{i,j}) + 1 = 0 for each node  (since -1 = 1 in GF(2))
- Incompatibility: x_{i,a} * x_{j,b} = 0 for incompatible pairs

Usage: python3 ns_degree_gf2.py CSP_DUMP_FILE [SUBSET]
"""

import sys
import numpy as np
from itertools import combinations
from math import comb

sys.path.insert(0, 'scripts')
from ns_from_csp import parse_csp, compute_profiles

def enumerate_multilinear_monomials(n_vars, max_degree):
    """Enumerate all squarefree monomials up to max_degree."""
    monoms = []
    for d in range(max_degree + 1):
        for combo in combinations(range(n_vars), d):
            exp = [0] * n_vars
            for idx in combo:
                exp[idx] = 1
            monoms.append(tuple(exp))
    return monoms

def multiply_multilinear(m1, m2, n_vars):
    """Multiply two multilinear monomials. Returns None if result has x^2 (=0 info)."""
    result = [0] * n_vars
    for i in range(n_vars):
        result[i] = m1[i] + m2[i]
        if result[i] > 1:
            return None  # x_i^2 = x_i, handled by reduction
    return tuple(result)

def reduce_monomial(m):
    """Reduce monomial mod (x_i^2 + x_i): replace x_i^k with x_i for k >= 1."""
    return tuple(min(e, 1) for e in m)

def monomial_degree(m):
    return sum(m)

def check_ns_degree_gf2(generators, n_vars, d):
    """Check if NS degree over GF(2) is <= d."""
    # Target monomials: all multilinear monomials of degree <= d
    target_monoms = enumerate_multilinear_monomials(n_vars, d)
    target_idx = {m: i for i, m in enumerate(target_monoms)}
    n_target = len(target_monoms)

    columns = []

    for gen_idx, (gen_coeffs, gen_deg) in enumerate(generators):
        mult_deg = d - gen_deg
        if mult_deg < 0:
            continue

        # Multiplier monomials: multilinear, degree <= mult_deg
        mult_monoms = enumerate_multilinear_monomials(n_vars, mult_deg)

        for mult_monom in mult_monoms:
            col = np.zeros(n_target, dtype=np.int8)
            for gen_monom, gen_coeff in gen_coeffs.items():
                # Multiply mult_monom * gen_monom
                prod = list(mult_monom)
                valid = True
                for i in range(n_vars):
                    prod[i] += gen_monom[i]
                # Reduce mod x^2 = x
                reduced = tuple(min(e, 1) for e in prod)
                if monomial_degree(reduced) <= d and reduced in target_idx:
                    # In GF(2), check if the original product had any x^2 terms
                    # x^2 = x, so we just reduce
                    col[target_idx[reduced]] = (col[target_idx[reduced]] + gen_coeff) % 2
            columns.append(col)

    if not columns:
        return False

    A = np.column_stack(columns).astype(np.int8)
    n_target_actual = A.shape[0]

    # Target: constant monomial = 1
    zero_monom = tuple([0] * n_vars)
    b = np.zeros(n_target_actual, dtype=np.int8)
    b[target_idx[zero_monom]] = 1

    # Solve Ax = b over GF(2) using Gaussian elimination
    return solve_gf2(A, b)

def solve_gf2(A, b):
    """Solve Ax = b over GF(2) using Gaussian elimination.
    Returns True if system is consistent."""
    m, n = A.shape
    # Augmented matrix [A | b]
    aug = np.zeros((m, n + 1), dtype=np.int8)
    aug[:, :n] = A % 2
    aug[:, n] = b % 2

    pivot_row = 0
    for col in range(n):
        # Find pivot
        found = -1
        for row in range(pivot_row, m):
            if aug[row, col] == 1:
                found = row
                break
        if found == -1:
            continue

        # Swap rows
        aug[[pivot_row, found]] = aug[[found, pivot_row]]

        # Eliminate
        for row in range(m):
            if row != pivot_row and aug[row, col] == 1:
                aug[row] = (aug[row] + aug[pivot_row]) % 2

        pivot_row += 1

    # Check consistency: any row with all zeros in A but 1 in b?
    for row in range(pivot_row, m):
        if aug[row, n] == 1:
            return False  # inconsistent

    return True

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 ns_degree_gf2.py CSP_DUMP_FILE [SUBSET]")
        sys.exit(1)

    csp_file = sys.argv[1]
    nodes, edges = parse_csp(csp_file)

    if len(sys.argv) >= 3:
        keep = set(int(x) for x in sys.argv[2].split(","))
        nodes = {k: v for k, v in nodes.items() if k in keep}
        edges = [(a, b, gi, gj) for (a, b, gi, gj) in edges if a in keep and b in keep]

    profiles, profile_keys = compute_profiles(nodes, edges)

    # Build variable index
    var_idx = {}
    v = 0
    for ni in sorted(profiles.keys()):
        for pidx in sorted(profiles[ni].keys()):
            var_idx[(ni, pidx)] = v
            v += 1
    n_vars = v

    print(f"Variables: {n_vars}")
    for ni in sorted(profiles.keys()):
        print(f"  node {ni}: {len(profiles[ni])} profiles")

    # Build generators (over GF(2), no Boolean constraints needed)
    generators = []

    # Selection constraints: sum(x_{i,j}) + 1 = 0 (since -1 = 1 in GF(2))
    for ni in sorted(profiles.keys()):
        coeffs = {}
        for pidx in sorted(profiles[ni].keys()):
            v_idx = var_idx[(ni, pidx)]
            m = [0] * n_vars
            m[v_idx] = 1
            coeffs[tuple(m)] = 1  # coefficient 1 in GF(2)
        # constant term: +1 (since -1 = 1 in GF(2))
        m0 = tuple([0] * n_vars)
        coeffs[m0] = (coeffs.get(m0, 0) + 1) % 2
        generators.append((coeffs, 1))

    # Incompatibility constraints: x_{i,a} * x_{j,b} = 0
    n_incompat = 0
    for eidx, (ni, nj, gi, gj) in enumerate(edges):
        for pi in sorted(profile_keys[ni].keys()):
            for pj in sorted(profile_keys[nj].keys()):
                ki = profile_keys[ni][pi].get(eidx, "?")
                kj = profile_keys[nj][pj].get(eidx, "?")
                if ki != "?" and kj != "?" and ki != kj:
                    vi = var_idx[(ni, pi)]
                    vj = var_idx[(nj, pj)]
                    coeffs = {}
                    m = [0] * n_vars
                    m[vi] = 1
                    m[vj] = 1
                    coeffs[tuple(m)] = 1
                    generators.append((coeffs, 2))
                    n_incompat += 1

    print(f"Generators: {len(generators)} ({n_incompat} incompatibility)")

    # Check NS degree over GF(2)
    for d in range(1, 8):
        n_monoms = sum(comb(n_vars, k) for k in range(d + 1))
        n_mult_cols = sum(
            sum(comb(n_vars, k) for k in range(max(0, d - deg) + 1))
            for _, deg in generators
        )
        print(f"\nDegree {d}: {n_monoms} multilinear monomials, ~{n_mult_cols} columns")

        if n_monoms > 100000 or n_mult_cols > 500000:
            print(f"  Too large, skipping")
            continue

        feasible = check_ns_degree_gf2(generators, n_vars, d)
        print(f"  NS degree (GF(2)) <= {d}: {feasible}")
        if feasible:
            print(f"\n*** NS DEGREE OVER GF(2) = {d} ***")
            break

if __name__ == "__main__":
    main()
