#!/usr/bin/env python3
"""
Resolution width scaling analysis.

Extracts sub-CSPs from a full CSP dump (selecting node subsets) and measures
CDCL resolution width on each, to understand how width scales with
constraint structure.

Usage:
  python3 width_scaling.py CSP_DUMP_FILE
"""

import sys
import os
from itertools import combinations
from collections import defaultdict

sys.path.insert(0, os.path.dirname(__file__))
from ns_from_csp import parse_csp, compute_profiles
from resolution_width import build_cnf
from cdcl_width import CDCLSolver


def extract_sub_csp(nodes, edges, node_subset):
    """Extract a sub-CSP containing only the given node subset."""
    sub_nodes = {ni: nodes[ni] for ni in node_subset if ni in nodes}
    sub_edges = [(ni, nj, gi, gj) for ni, nj, gi, gj in edges
                 if ni in node_subset and nj in node_subset]
    return sub_nodes, sub_edges


def measure_instance(nodes, edges, label=""):
    """Measure CDCL resolution width for a CSP instance."""
    profiles, profile_keys = compute_profiles(nodes, edges)
    total_profiles = sum(len(p) for p in profiles.values())

    if total_profiles == 0:
        return None

    num_vars, clauses, var_map = build_cnf(profiles, profile_keys, edges)

    solver = CDCLSolver(num_vars, clauses)
    result = solver.solve(max_conflicts=100000)

    init_widths = [len(c) for c in clauses]
    max_init = max(init_widths) if init_widths else 0

    return {
        'label': label,
        'nodes': len(nodes),
        'edges': len(edges),
        'profiles': total_profiles,
        'vars': num_vars,
        'clauses': len(clauses),
        'result': result,
        'decisions': solver.decisions,
        'conflicts': solver.conflicts,
        'learned': len(solver.learned),
        'max_width': solver.max_learned_width,
        'max_init_width': max_init,
        'learned_widths': [len(c) for c in solver.learned],
    }


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    csp_file = sys.argv[1]

    # Try different path formats
    for path in [csp_file, csp_file.replace('/c/', 'C:/')]:
        if os.path.exists(path):
            csp_file = path
            break

    nodes, edges = parse_csp(csp_file)
    all_nodes = sorted(nodes.keys())

    print(f"Full CSP: {len(nodes)} nodes, {len(edges)} edges")
    print(f"Node domains: {', '.join(f'{ni}:{nodes[ni]}' for ni in all_nodes)}")
    print()

    # Full instance
    r = measure_instance(nodes, edges, "full")
    print(f"{'Instance':<30s} {'Nodes':>5s} {'Edges':>5s} {'Prof':>5s} {'Vars':>5s} {'Cls':>5s} "
          f"{'Result':>8s} {'Dec':>5s} {'Conf':>5s} {'Lrn':>5s} {'MaxW':>5s} {'MaxInit':>7s}")
    print("-" * 110)

    def print_result(r):
        if r is None:
            return
        print(f"{r['label']:<30s} {r['nodes']:>5d} {r['edges']:>5d} {r['profiles']:>5d} "
              f"{r['vars']:>5d} {r['clauses']:>5d} {r['result']:>8s} {r['decisions']:>5d} "
              f"{r['conflicts']:>5d} {r['learned']:>5d} {r['max_width']:>5d} {r['max_init_width']:>7d}")

    print_result(r)

    # Skip forced nodes (domain size 1 or 2) to find hard sub-instances
    forced_nodes = [ni for ni in all_nodes if nodes[ni] <= 1]
    small_nodes = [ni for ni in all_nodes if nodes[ni] <= 2]
    print(f"\nForced nodes (dom=1): {forced_nodes}")
    print(f"Small nodes (dom<=2): {small_nodes}")

    # Remove forced nodes
    non_forced = [ni for ni in all_nodes if nodes[ni] > 1]
    if len(non_forced) < len(all_nodes):
        sub_n, sub_e = extract_sub_csp(nodes, edges, non_forced)
        r = measure_instance(sub_n, sub_e, f"no-forced({non_forced})")
        print_result(r)

    # Remove small nodes
    non_small = [ni for ni in all_nodes if nodes[ni] > 2]
    if len(non_small) < len(non_forced):
        sub_n, sub_e = extract_sub_csp(nodes, edges, non_small)
        r = measure_instance(sub_n, sub_e, f"no-small({non_small})")
        print_result(r)

    # All 4-node subsets (excluding forced nodes when possible)
    print(f"\n--- 4-node subsets ---")
    results_4 = []
    for subset in combinations(all_nodes, 4):
        sub_n, sub_e = extract_sub_csp(nodes, edges, list(subset))
        if len(sub_e) == 0:
            continue
        r = measure_instance(sub_n, sub_e, f"sub{list(subset)}")
        if r and r['result'] == 'UNSAT':
            results_4.append(r)
            print_result(r)

    # All 5-node subsets
    print(f"\n--- 5-node subsets (UNSAT only) ---")
    results_5 = []
    for subset in combinations(all_nodes, 5):
        sub_n, sub_e = extract_sub_csp(nodes, edges, list(subset))
        if len(sub_e) == 0:
            continue
        r = measure_instance(sub_n, sub_e, f"sub{list(subset)}")
        if r and r['result'] == 'UNSAT':
            results_5.append(r)
            print_result(r)

    # All 6-node subsets
    print(f"\n--- 6-node subsets (UNSAT only) ---")
    results_6 = []
    for subset in combinations(all_nodes, 6):
        sub_n, sub_e = extract_sub_csp(nodes, edges, list(subset))
        if len(sub_e) == 0:
            continue
        r = measure_instance(sub_n, sub_e, f"sub{list(subset)}")
        if r and r['result'] == 'UNSAT':
            results_6.append(r)
            print_result(r)

    # Summary
    print(f"\n--- Summary ---")
    for label, results in [("4-node", results_4), ("5-node", results_5), ("6-node", results_6)]:
        if results:
            widths = [r['max_width'] for r in results]
            unsat = [r for r in results if r['result'] == 'UNSAT']
            print(f"{label}: {len(unsat)} UNSAT instances, "
                  f"max width: min={min(widths)}, max={max(widths)}, avg={sum(widths)/len(widths):.1f}")


if __name__ == "__main__":
    main()
