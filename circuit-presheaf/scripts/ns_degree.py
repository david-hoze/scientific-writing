#!/usr/bin/env python3
"""
Compute Nullstellensatz degree via linear algebra.

NS degree d: 1 = Σ_α p_α(x) · f_α(x), deg(p_α · f_α) ≤ d.

For each degree d, this is a linear feasibility problem over
the coefficients of p_α. We enumerate all monomials up to the
required degree, express the product p_α · f_α in monomial basis,
and check if the constant term can be made 1 while all other
coefficients are 0.

Usage: python3 ns_degree.py CSP_DUMP_FILE
"""

import sys
import numpy as np
from collections import defaultdict
from itertools import combinations_with_replacement

sys.path.insert(0, 'scripts')
from ns_from_csp import parse_csp, compute_profiles

def enumerate_monomials(n_vars, max_degree):
    """Enumerate all monomials up to max_degree in n_vars variables.
    Returns list of tuples (exponent vectors)."""
    monoms = []
    for d in range(max_degree + 1):
        for combo in combinations_with_replacement(range(n_vars), d):
            exp = [0] * n_vars
            for idx in combo:
                exp[idx] += 1
            exp_tuple = tuple(exp)
            if exp_tuple not in monoms:
                monoms.append(exp_tuple)
    return monoms

def multiply_monomials(m1, m2):
    """Multiply two monomials (add exponents)."""
    return tuple(a + b for a, b in zip(m1, m2))

def monomial_degree(m):
    return sum(m)

def build_constraint_system(generators, n_vars, max_ns_degree):
    """Build the linear system for NS degree d.

    generators: list of (coefficient_dict, degree) where coefficient_dict
                maps monomial -> coefficient.
    """
    # Monomials in the target space (degree ≤ max_ns_degree)
    target_monoms = enumerate_monomials(n_vars, max_ns_degree)
    target_idx = {m: i for i, m in enumerate(target_monoms)}
    n_target = len(target_monoms)

    # For each generator f_α of degree deg_α, the multiplier p_α
    # has degree ≤ max_ns_degree - deg_α
    columns = []  # each column corresponds to one coefficient in one p_α
    col_info = []

    for gen_idx, (gen_coeffs, gen_deg) in enumerate(generators):
        mult_deg = max_ns_degree - gen_deg
        if mult_deg < 0:
            continue  # can't use this generator at this degree

        # Monomials for the multiplier
        mult_monoms = enumerate_monomials(n_vars, mult_deg)

        for mult_monom in mult_monoms:
            # Compute the product p_α_monom * f_α
            col = np.zeros(n_target)
            for gen_monom, gen_coeff in gen_coeffs.items():
                prod_monom = multiply_monomials(mult_monom, gen_monom)
                if monomial_degree(prod_monom) <= max_ns_degree:
                    if prod_monom in target_idx:
                        col[target_idx[prod_monom]] += gen_coeff
            columns.append(col)
            col_info.append((gen_idx, mult_monom))

    if not columns:
        return None, target_idx

    # Build matrix A where Ax = b, b = e_0 (constant monomial = 1)
    A = np.column_stack(columns)
    return A, target_idx

def check_ns_degree(generators, n_vars, d):
    """Check if NS degree ≤ d by solving the linear system."""
    A, target_idx = build_constraint_system(generators, n_vars, d)
    if A is None:
        return False

    n_target = A.shape[0]
    # Target: constant monomial coefficient = 1, all others = 0
    zero_monom = tuple([0] * n_vars)
    b = np.zeros(n_target)
    b[target_idx[zero_monom]] = 1.0

    # Solve Ax = b via least squares
    x, residuals, rank, sv = np.linalg.lstsq(A, b, rcond=None)

    # Check if residual is near zero
    residual = np.linalg.norm(A @ x - b)
    return residual < 1e-8

def csp_to_generators(profiles, profile_keys, edges, n_vars, var_idx):
    """Convert CSP to polynomial generators.

    Returns list of (coefficient_dict, degree) where coefficient_dict
    maps monomial (tuple) -> coefficient.
    """
    generators = []

    # Boolean constraints: x_i^2 - x_i = 0 for each variable
    for v in range(n_vars):
        coeffs = {}
        # x_i^2 term
        m2 = [0] * n_vars
        m2[v] = 2
        coeffs[tuple(m2)] = 1.0
        # -x_i term
        m1 = [0] * n_vars
        m1[v] = 1
        coeffs[tuple(m1)] = -1.0
        generators.append((coeffs, 2))

    # Selection constraints: sum(x_{i,j}) - 1 = 0 for each node
    for ni in sorted(profiles.keys()):
        coeffs = {}
        for pidx in sorted(profiles[ni].keys()):
            v = var_idx[(ni, pidx)]
            m = [0] * n_vars
            m[v] = 1
            coeffs[tuple(m)] = 1.0
        # -1 constant term
        m0 = tuple([0] * n_vars)
        coeffs[m0] = -1.0
        generators.append((coeffs, 1))

    # Incompatibility constraints: x_{i,a} * x_{j,b} = 0
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
                    coeffs[tuple(m)] = 1.0
                    generators.append((coeffs, 2))

    return generators

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 ns_degree.py CSP_DUMP_FILE [SUBSET]")
        print("  SUBSET: comma-separated node indices (e.g., 0,2,4,5,6)")
        sys.exit(1)

    csp_file = sys.argv[1]
    nodes, edges = parse_csp(csp_file)

    # Optional subset
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

    # Build generators
    generators = csp_to_generators(profiles, profile_keys, edges, n_vars, var_idx)
    print(f"Generators: {len(generators)}")

    # Check NS degree
    for d in range(1, 8):
        # Estimate matrix size
        from math import comb
        n_monoms = comb(n_vars + d, d)
        n_mult_cols = sum(comb(n_vars + max(0, d - deg), max(0, d - deg))
                         for _, deg in generators)
        print(f"\nDegree {d}: {n_monoms} monomials, ~{n_mult_cols} columns")

        if n_monoms > 50000 or n_mult_cols > 200000:
            print(f"  Too large, skipping (would need {n_monoms}×{n_mult_cols} matrix)")
            continue

        feasible = check_ns_degree(generators, n_vars, d)
        print(f"  NS degree <= {d}: {feasible}")
        if feasible:
            print(f"\n*** NS DEGREE = {d} ***")
            break

if __name__ == "__main__":
    main()
