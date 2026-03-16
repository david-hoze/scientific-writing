#!/usr/bin/env python3
"""
n=5 Structural CSP Scanner

Investigates structural obstructions for 5-variable Boolean functions.
Bypasses Idris2 by reimplementing the full pipeline in Python:
  1. Formula enumeration (d=3 variables, up to size s)
  2. Sub-cube geometry (n=5, d=3 → 40 sub-cubes, 480 edges)
  3. Sub-function extraction, domain computation, canonical grouping
  4. Profile reduction + backtracking solver
  5. Sampling strategies for the 2^32 function space

Usage:
  python3 n5_scan.py --enumerate --dim 3 --size 4
  python3 n5_scan.py --scan --dim 3 --size 4 --strategy lifted --n4-unsat 686,139,...
  python3 n5_scan.py --scan --dim 3 --size 4 --strategy random --count 1000
  python3 n5_scan.py --solve --tt 0xDEADBEEF --dim 3 --size 4
"""

import argparse
import sys
import time
import random
import csv
from collections import defaultdict
from itertools import combinations


# ============================================================
# Phase 0: Formula AST, evaluation, canonicalization, enumeration
# ============================================================

class Formula:
    """Abstract syntax tree for Boolean formulas."""
    pass

class Input(Formula):
    __slots__ = ['idx']
    def __init__(self, idx):
        self.idx = idx
    def __repr__(self):
        return f"x{self.idx}"

class Const(Formula):
    __slots__ = ['val']
    def __init__(self, val):
        self.val = val
    def __repr__(self):
        return str(int(self.val))

class Not(Formula):
    __slots__ = ['child']
    def __init__(self, child):
        self.child = child
    def __repr__(self):
        return f"N({self.child})"

class And(Formula):
    __slots__ = ['left', 'right']
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __repr__(self):
        return f"A({self.left},{self.right})"

class Or(Formula):
    __slots__ = ['left', 'right']
    def __init__(self, left, right):
        self.left = left
        self.right = right
    def __repr__(self):
        return f"O({self.left},{self.right})"


def formula_size(f):
    """Count internal gates (leaves = 0)."""
    if isinstance(f, (Input, Const)):
        return 0
    elif isinstance(f, Not):
        return 1 + formula_size(f.child)
    elif isinstance(f, (And, Or)):
        return 1 + formula_size(f.left) + formula_size(f.right)


def canonical(f):
    """Canonical string representation (commutative children sorted)."""
    if isinstance(f, Input):
        return f"x{f.idx}"
    elif isinstance(f, Const):
        return "1" if f.val else "0"
    elif isinstance(f, Not):
        return f"N({canonical(f.child)})"
    elif isinstance(f, And):
        cl, cr = canonical(f.left), canonical(f.right)
        a, b = (cl, cr) if cl <= cr else (cr, cl)
        return f"A({a},{b})"
    elif isinstance(f, Or):
        cl, cr = canonical(f.left), canonical(f.right)
        a, b = (cl, cr) if cl <= cr else (cr, cl)
        return f"O({a},{b})"


def truth_table(d, f):
    """Compute truth table as integer (bit i = eval(f, input i))."""
    result = 0
    for i in range(1 << d):
        bits = [(i >> j) & 1 for j in range(d)]
        if eval_formula(f, bits):
            result |= (1 << i)
    return result


def eval_formula(f, bits):
    """Evaluate formula on a bit vector."""
    if isinstance(f, Input):
        return bool(bits[f.idx])
    elif isinstance(f, Const):
        return f.val
    elif isinstance(f, Not):
        return not eval_formula(f.child, bits)
    elif isinstance(f, And):
        return eval_formula(f.left, bits) and eval_formula(f.right, bits)
    elif isinstance(f, Or):
        return eval_formula(f.left, bits) or eval_formula(f.right, bits)


def substitute(f, var_idx, val):
    """Replace Input(var_idx) with Const(val)."""
    if isinstance(f, Input):
        return Const(val) if f.idx == var_idx else f
    elif isinstance(f, Const):
        return f
    elif isinstance(f, Not):
        return Not(substitute(f.child, var_idx, val))
    elif isinstance(f, And):
        return And(substitute(f.left, var_idx, val), substitute(f.right, var_idx, val))
    elif isinstance(f, Or):
        return Or(substitute(f.left, var_idx, val), substitute(f.right, var_idx, val))


def propagate(f):
    """Constant folding and simplification."""
    if isinstance(f, (Input, Const)):
        return f
    elif isinstance(f, Not):
        c = propagate(f.child)
        if isinstance(c, Const):
            return Const(not c.val)
        if isinstance(c, Not):
            return c.child
        return Not(c)
    elif isinstance(f, And):
        l, r = propagate(f.left), propagate(f.right)
        if isinstance(l, Const):
            return r if l.val else Const(False)
        if isinstance(r, Const):
            return l if r.val else Const(False)
        return And(l, r)
    elif isinstance(f, Or):
        l, r = propagate(f.left), propagate(f.right)
        if isinstance(l, Const):
            return Const(True) if l.val else r
        if isinstance(r, Const):
            return Const(True) if r.val else l
        return Or(l, r)


def reindex(removed_idx, f):
    """Shift input indices down past removed_idx."""
    if isinstance(f, Input):
        if f.idx > removed_idx:
            return Input(f.idx - 1)
        return f
    elif isinstance(f, Const):
        return f
    elif isinstance(f, Not):
        return Not(reindex(removed_idx, f.child))
    elif isinstance(f, And):
        return And(reindex(removed_idx, f.left), reindex(removed_idx, f.right))
    elif isinstance(f, Or):
        return Or(reindex(removed_idx, f.left), reindex(removed_idx, f.right))


def restrict_formula(f, var_idx, val):
    """Restrict formula: substitute, propagate, reindex."""
    return reindex(var_idx, propagate(substitute(f, var_idx, val)))


def enumerate_formulas(d, max_size):
    """
    Enumerate all structurally distinct formulas up to max_size gates
    over d input variables. Returns dict: truth_table -> [Formula].
    """
    by_size = {}   # size -> [Formula]
    seen = set()   # canonical strings
    by_tt = defaultdict(list)  # truth_table -> [Formula]

    def add(f):
        cs = canonical(f)
        if cs in seen:
            return
        seen.add(cs)
        s = formula_size(f)
        if s not in by_size:
            by_size[s] = []
        by_size[s].append(f)
        tt = truth_table(d, f)
        by_tt[tt].append(f)

    # Size 0: constants + inputs
    add(Const(False))
    add(Const(True))
    for i in range(d):
        add(Input(i))

    # Sizes 1 to max_size
    for s in range(1, max_size + 1):
        # NOT of size-(s-1)
        for f in by_size.get(s - 1, []):
            add(Not(f))

        # AND/OR of (s1, s2) with s1 + s2 = s - 1
        for s1 in range(s):
            s2 = (s - 1) - s1
            if s2 < s1:
                continue
            fs1 = by_size.get(s1, [])
            fs2 = by_size.get(s2, [])
            if not fs1 or not fs2:
                continue

            if s1 == s2:
                # Self-pairing
                for i, f in enumerate(fs1):
                    for g in fs1[i:]:
                        add(And(f, g))
                        add(Or(f, g))
            else:
                # Cross-pairing
                for f in fs1:
                    for g in fs2:
                        add(And(f, g))
                        add(Or(f, g))

    return dict(by_tt), len(seen)


# ============================================================
# Phase 1: Sub-cube geometry
# ============================================================

def generate_subcubes(n, d):
    """Generate all d-dimensional sub-cubes of {0,..,n-1}."""
    subcubes = []
    for free_coords in combinations(range(n), d):
        free_set = set(free_coords)
        fixed_coords = [c for c in range(n) if c not in free_set]
        for fixed_bits in range(1 << (n - d)):
            fixed_values = {}
            for i, coord in enumerate(fixed_coords):
                fixed_values[coord] = bool((fixed_bits >> i) & 1)
            subcubes.append({
                'free': list(free_coords),
                'fixed': fixed_values,
            })
    return subcubes


def sub_function_tt(n, full_tt, subcube):
    """Extract d-variable truth table from n-variable truth table."""
    free = subcube['free']
    fixed = subcube['fixed']
    d = len(free)
    result = 0
    for free_bits in range(1 << d):
        full_index = 0
        for i, coord in enumerate(free):
            if (free_bits >> i) & 1:
                full_index |= (1 << coord)
        for coord, val in fixed.items():
            if val:
                full_index |= (1 << coord)
        bit = (full_tt >> full_index) & 1
        if bit:
            result |= (1 << free_bits)
    return result


def structural_edges(subcubes):
    """Compute all structural edge pairs (i, j) with i < j."""
    edges = []
    n_sc = len(subcubes)
    for i in range(n_sc):
        for j in range(i + 1, n_sc):
            if are_adjacent(subcubes[i], subcubes[j]):
                edges.append((i, j))
    return edges


def are_adjacent(sc1, sc2):
    """Two sub-cubes are adjacent if they share ≥1 free coord and fixed values agree."""
    free1 = set(sc1['free'])
    free2 = set(sc2['free'])
    if not (free1 & free2):
        return False
    # Check fixed value compatibility
    for coord, v1 in sc1['fixed'].items():
        if coord in sc2['fixed'] and sc2['fixed'][coord] != v1:
            return False
    for coord, v2 in sc2['fixed'].items():
        if coord in sc1['fixed'] and sc1['fixed'][coord] != v2:
            return False
    return True


def overlap_pattern(sc_i, sc_j):
    """
    Compute the overlap pattern between two adjacent sub-cubes.
    Returns: list of (local_idx_in_i, fixed_value_from_j) for non-shared free coords of sc_i,
             sorted in descending order of local_idx.
    """
    free_i = set(sc_i['free'])
    free_j = set(sc_j['free'])
    non_shared = free_i - free_j

    restrictions = []
    for coord in non_shared:
        local_idx = sc_i['free'].index(coord)
        if coord in sc_j['fixed']:
            val = sc_j['fixed'][coord]
        else:
            # coord is free in j but not shared → this shouldn't happen
            # for adjacent sub-cubes with same d
            continue
        restrictions.append((local_idx, val))

    restrictions.sort(reverse=True)  # descending for stable index arithmetic
    return restrictions


# ============================================================
# Phase 1b: Pre-compute overlap group lookup tables
# ============================================================

def precompute_overlap_groups(d, max_size, subcubes, edges):
    """
    Pre-compute canonical overlap groups using pattern deduplication.

    Key optimization: many edges share the same restriction pattern
    (same local indices fixed to same values). For d=3, there are at most
    ~19 distinct patterns vs 480 edges. We compute canonical groups once
    per pattern, then look up results for each edge.

    Returns:
        formula_domains: {truth_table: [Formula]}
        edge_tables: list of dicts, one per edge
    """
    # First enumerate all formulas for d variables up to max_size
    formula_domains, total_formulas = enumerate_formulas(d, max_size)

    print(f"  Enumerated {total_formulas} distinct formulas, "
          f"{len(formula_domains)} distinct truth tables")

    # Collect all distinct restriction patterns across all edges
    # pattern_key = tuple of (local_idx, value) pairs (already sorted desc)
    edge_patterns_i = []  # per edge: pattern key for side i
    edge_patterns_j = []  # per edge: pattern key for side j
    all_patterns = set()

    for eidx, (si, sj) in enumerate(edges):
        sc_i = subcubes[si]
        sc_j = subcubes[sj]
        pi = tuple(overlap_pattern(sc_i, sc_j))
        pj = tuple(overlap_pattern(sc_j, sc_i))
        edge_patterns_i.append(pi)
        edge_patterns_j.append(pj)
        all_patterns.add(pi)
        all_patterns.add(pj)

    print(f"  {len(all_patterns)} distinct restriction patterns "
          f"(vs {len(edges)} edges)")

    # Pre-compute canonical groups for each (pattern, truth_table) pair
    # pattern_groups[pattern][sub_tt] = {canonical_key: [formula_indices]}
    pattern_groups = {}

    for pidx, pattern in enumerate(sorted(all_patterns)):
        pattern_groups[pattern] = {}

        for sub_tt, domain in formula_domains.items():
            groups = defaultdict(list)
            for fidx, f in enumerate(domain):
                restricted = f
                for local_idx, val in pattern:
                    restricted = restrict_formula(restricted, local_idx, val)
                cs = canonical(restricted)
                groups[cs].append(fidx)
            pattern_groups[pattern][sub_tt] = dict(groups)

        if (pidx + 1) % 5 == 0:
            print(f"  Computed groups for {pidx + 1}/{len(all_patterns)} patterns")

    print(f"  Computed all pattern canonical groups")

    # Build edge tables by looking up pre-computed pattern groups
    edge_tables = []
    for eidx in range(len(edges)):
        pi = edge_patterns_i[eidx]
        pj = edge_patterns_j[eidx]
        edge_tables.append({
            'groups_i': pattern_groups[pi],
            'groups_j': pattern_groups[pj],
        })

    return formula_domains, edge_tables


# ============================================================
# Phase 2: CSP construction and solving for a single function
# ============================================================

def build_csp(full_tt, n, subcubes, edges, formula_domains, edge_tables):
    """
    Build the structural CSP for a given n-variable truth table.

    Returns:
        nodes: {node_idx: domain_size}
        csp_edges: [(ni, nj, groups_i, groups_j), ...]
            where groups_i[key] = [formula_indices], groups_j[key] = [formula_indices]
        sub_tts: [sub_tt_for_each_node]
    """
    n_nodes = len(subcubes)

    # Extract sub-function truth tables
    sub_tts = [sub_function_tt(n, full_tt, sc) for sc in subcubes]

    # Domain sizes
    nodes = {}
    for ni in range(n_nodes):
        st = sub_tts[ni]
        if st in formula_domains:
            nodes[ni] = len(formula_domains[st])
        else:
            nodes[ni] = 0  # No formulas compute this function at this size budget

    # Build edge data using pre-computed tables
    csp_edges = []
    for eidx, (si, sj) in enumerate(edges):
        sti = sub_tts[si]
        stj = sub_tts[sj]

        gi = edge_tables[eidx]['groups_i'].get(sti, {})
        gj = edge_tables[eidx]['groups_j'].get(stj, {})

        csp_edges.append((si, sj, gi, gj))

    return nodes, csp_edges, sub_tts


def compute_profiles(nodes, csp_edges):
    """Profile reduction: collapse interchangeable domain elements."""
    node_edge_keys = defaultdict(list)

    for eidx, (ni, nj, gi, gj) in enumerate(csp_edges):
        inv_i = {}
        for key, indices in gi.items():
            for idx in indices:
                inv_i[idx] = key
        node_edge_keys[ni].append((eidx, inv_i))

        inv_j = {}
        for key, indices in gj.items():
            for idx in indices:
                inv_j[idx] = key
        node_edge_keys[nj].append((eidx, inv_j))

    profiles = {}
    profile_keys = {}

    for ni, dom_size in nodes.items():
        edge_keys = sorted(node_edge_keys[ni], key=lambda x: x[0])
        prof_groups = defaultdict(list)
        for dom_idx in range(dom_size):
            profile = tuple(
                (eidx, inv.get(dom_idx, "?"))
                for eidx, inv in edge_keys
            )
            prof_groups[profile].append(dom_idx)

        sorted_profiles = sorted(prof_groups.keys())
        profiles[ni] = {}
        profile_keys[ni] = {}
        for pidx, prof in enumerate(sorted_profiles):
            profiles[ni][pidx] = prof_groups[prof]
            profile_keys[ni][pidx] = {eidx: key for eidx, key in prof}

    return profiles, profile_keys


def solve_csp(profiles, profile_keys, csp_edges, max_backtracks=500000):
    """
    Backtracking solver with forward checking on profile-reduced CSP.

    Returns: ('SAT', assignment) or ('UNSAT', None) or ('UNKNOWN', None)
    """
    # Check for empty domains
    for ni, profs in profiles.items():
        if len(profs) == 0:
            return 'UNSAT', None

    node_order = sorted(profiles.keys(), key=lambda ni: len(profiles[ni]))
    backtracks = [0]

    # Pre-build adjacency index: for each node, list of (eidx, is_src)
    node_adj = defaultdict(list)
    for eidx, (ni, nj, _, _) in enumerate(csp_edges):
        node_adj[ni].append((eidx, True))
        node_adj[nj].append((eidx, False))

    def is_consistent(assignment, ni, pi):
        for eidx, is_src in node_adj[ni]:
            ei, ej = csp_edges[eidx][0], csp_edges[eidx][1]
            if is_src and ej in assignment:
                ki = profile_keys[ni][pi].get(eidx, "?")
                kj = profile_keys[ej][assignment[ej]].get(eidx, "?")
                if ki != "?" and kj != "?" and ki != kj:
                    return False
            elif not is_src and ei in assignment:
                ki = profile_keys[ei][assignment[ei]].get(eidx, "?")
                kj = profile_keys[ni][pi].get(eidx, "?")
                if ki != "?" and kj != "?" and ki != kj:
                    return False
        return True

    def backtrack(idx, assignment):
        if backtracks[0] >= max_backtracks:
            return None
        if idx == len(node_order):
            return dict(assignment)
        ni = node_order[idx]
        for pi in sorted(profiles[ni].keys()):
            if is_consistent(assignment, ni, pi):
                assignment[ni] = pi
                result = backtrack(idx + 1, assignment)
                if result is not None:
                    return result
                del assignment[ni]
                backtracks[0] += 1
                if backtracks[0] >= max_backtracks:
                    return None
        return None

    result = backtrack(0, {})
    if result is not None:
        return 'SAT', result
    elif backtracks[0] >= max_backtracks:
        return 'UNKNOWN', None
    else:
        return 'UNSAT', None


def analyze_function(full_tt, n, subcubes, edges, formula_domains, edge_tables,
                     max_backtracks=500000):
    """Full analysis of one n-variable truth table. Returns result dict."""
    nodes, csp_edges, sub_tts = build_csp(full_tt, n, subcubes, edges,
                                           formula_domains, edge_tables)

    # Count coverage
    n_nodes = len(subcubes)
    n_covered = sum(1 for ni in range(n_nodes) if nodes.get(ni, 0) > 0)
    n_empty = n_nodes - n_covered
    total_domain = sum(nodes.values())

    if total_domain == 0:
        return {
            'tt': full_tt,
            'result': 'TRIVIAL',
            'total_domain': 0,
            'n_covered': 0,
            'n_empty': n_nodes,
            'profiles': 0,
            'edges_partial': 0,
            'edges_incompat': 0,
        }

    # If some nodes have empty domains, it's trivially UNSAT
    # But still analyze the covered subgraph for genuine obstructions
    has_empty = n_empty > 0

    # Build covered-only subgraph for genuine analysis
    if has_empty:
        covered_nodes = {ni: ds for ni, ds in nodes.items() if ds > 0}
        covered_set = set(covered_nodes.keys())
        covered_edges = [(ni, nj, gi, gj) for ni, nj, gi, gj in csp_edges
                         if ni in covered_set and nj in covered_set]
    else:
        covered_nodes = nodes
        covered_edges = csp_edges

    # Count edge types (on covered subgraph)
    n_partial = 0
    n_incompat = 0
    for ni, nj, gi, gj in covered_edges:
        keys_i = set(gi.keys())
        keys_j = set(gj.keys())
        if not (keys_i & keys_j):
            if keys_i and keys_j:
                n_incompat += 1
        elif keys_i != keys_j or any(k not in keys_j for k in keys_i):
            n_partial += 1

    # Profile reduction on covered subgraph
    profiles, profile_keys = compute_profiles(covered_nodes, covered_edges)
    total_profiles = sum(len(p) for p in profiles.values())

    # Solve covered subgraph
    status, assignment = solve_csp(profiles, profile_keys, covered_edges, max_backtracks)

    # Result classification:
    # - TRIVIAL: all domains empty
    # - EMPTY_UNSAT: some domains empty, covered subgraph is SAT
    # - EMPTY+GENUINE_UNSAT: some domains empty AND covered subgraph is UNSAT
    # - SAT: all domains non-empty, CSP is SAT
    # - UNSAT: all domains non-empty, CSP is genuinely UNSAT
    # - UNKNOWN: solver exhausted
    if has_empty:
        if status == 'SAT':
            result = 'EMPTY_UNSAT'
        elif status == 'UNSAT':
            result = 'EMPTY+GENUINE'
        else:
            result = 'EMPTY+UNKNOWN'
    else:
        result = status

    return {
        'tt': full_tt,
        'result': result,
        'total_domain': total_domain,
        'n_covered': n_covered,
        'n_empty': n_empty,
        'profiles': total_profiles,
        'edges_partial': n_partial,
        'edges_incompat': n_incompat,
    }


# ============================================================
# Phase 3: Sampling strategies
# ============================================================

def lift_n4_to_n5(tt4):
    """
    Lift an n=4 truth table to n=5 by making variable 4 irrelevant.
    f(x0,x1,x2,x3,x4) = f4(x0,x1,x2,x3).
    """
    # n=4 TT is 16 bits. n=5 TT is 32 bits.
    # Bit i of n=5 TT: extract lower 4 bits of i, look up in n=4 TT
    result = 0
    for i in range(32):
        low4 = i & 0xF  # x0..x3
        bit = (tt4 >> low4) & 1
        if bit:
            result |= (1 << i)
    return result


def lift_n4_with_xor(tt4, mask=0x10):
    """Lift n=4 by XORing with x4: f(x0..x4) = f4(x0..x3) XOR x4."""
    base = lift_n4_to_n5(tt4)
    # x4 truth table: bit i is set iff bit 4 of i is set
    x4_tt = 0
    for i in range(32):
        if (i >> 4) & 1:
            x4_tt |= (1 << i)
    return base ^ x4_tt


def lift_n4_with_and(tt4):
    """Lift n=4 by ANDing with x4: f = f4(x0..x3) AND x4."""
    base = lift_n4_to_n5(tt4)
    x4_tt = 0
    for i in range(32):
        if (i >> 4) & 1:
            x4_tt |= (1 << i)
    return base & x4_tt


def sample_random(count, seed=42):
    """Random n=5 truth tables."""
    rng = random.Random(seed)
    return [rng.getrandbits(32) for _ in range(count)]


def sample_threshold(n=5):
    """Threshold functions: f = 1 iff popcount(x) >= k."""
    results = []
    for k in range(n + 2):
        tt = 0
        for i in range(1 << n):
            if bin(i).count('1') >= k:
                tt |= (1 << i)
        results.append(('THR_' + str(k), tt))
    return results


def sample_symmetric(n=5):
    """Symmetric functions: output depends only on Hamming weight."""
    # There are 2^(n+1) symmetric functions on n variables
    results = []
    for pat in range(1 << (n + 1)):
        tt = 0
        for i in range(1 << n):
            w = bin(i).count('1')
            if (pat >> w) & 1:
                tt |= (1 << i)
        results.append(('SYM_' + format(pat, f'0{n+1}b'), tt))
    return results


def sample_majority_variants(n=5):
    """MAJ variants: majority, weighted majority, etc."""
    results = []
    # Standard MAJ5
    tt = 0
    for i in range(1 << n):
        if bin(i).count('1') >= 3:
            tt |= (1 << i)
    results.append(('MAJ5', tt))

    # Negated MAJ5
    results.append(('NEG_MAJ5', ((1 << (1 << n)) - 1) ^ tt))

    return results


# ============================================================
# Main scan loop
# ============================================================

def run_enumerate(args):
    """Phase 0: Just enumerate formulas and report statistics."""
    d = args.dim
    s = args.size
    print(f"Enumerating formulas: d={d}, max_size={s}")
    t0 = time.time()
    domains, total = enumerate_formulas(d, s)
    elapsed = time.time() - t0
    print(f"  Total distinct formulas: {total}")
    print(f"  Distinct truth tables: {len(domains)}")
    print(f"  Time: {elapsed:.2f}s")

    # Distribution by domain size
    size_dist = defaultdict(int)
    for tt, forms in domains.items():
        size_dist[len(forms)] += 1
    print(f"\n  Domain size distribution:")
    for ds in sorted(size_dist.keys()):
        print(f"    {ds} formulas: {size_dist[ds]} functions")

    # Max domain
    max_dom_tt = max(domains.keys(), key=lambda t: len(domains[t]))
    print(f"\n  Largest domain: TT={max_dom_tt} with {len(domains[max_dom_tt])} formulas")


def run_solve(args):
    """Solve CSP for a single n=5 function."""
    n = 5
    d = args.dim
    s = args.size
    full_tt = args.tt

    print(f"Solving: TT=0x{full_tt:08X} ({full_tt}), n={n}, d={d}, s<={s}")
    print(f"\nPhase 1: Pre-computation...")
    t0 = time.time()

    subcubes = generate_subcubes(n, d)
    edges = structural_edges(subcubes)
    print(f"  {len(subcubes)} sub-cubes, {len(edges)} edges")

    formula_domains, edge_tables = precompute_overlap_groups(d, s, subcubes, edges)
    t1 = time.time()
    print(f"  Pre-computation: {t1-t0:.2f}s")

    print(f"\nPhase 2: Build and solve CSP...")
    result = analyze_function(full_tt, n, subcubes, edges, formula_domains, edge_tables)
    t2 = time.time()

    print(f"\n  Result: {result['result']}")
    print(f"  Coverage: {result['n_covered']}/{result['n_covered']+result['n_empty']} "
          f"nodes ({result['n_empty']} empty)")
    print(f"  Total domain: {result['total_domain']}")
    print(f"  Profiles: {result['profiles']}")
    print(f"  Partial edges: {result['edges_partial']}")
    print(f"  Incompatible edges: {result['edges_incompat']}")
    print(f"  Solve time: {t2-t1:.2f}s")


def run_scan(args):
    """Scan multiple functions using sampling strategy."""
    n = 5
    d = args.dim
    s = args.size
    strategy = args.strategy

    print(f"n=5 Scan: d={d}, s<={s}, strategy={strategy}")
    print(f"\nPhase 1: Pre-computation...")
    t0 = time.time()

    subcubes = generate_subcubes(n, d)
    edges = structural_edges(subcubes)
    print(f"  {len(subcubes)} sub-cubes, {len(edges)} edges")

    formula_domains, edge_tables = precompute_overlap_groups(d, s, subcubes, edges)
    t1 = time.time()
    print(f"  Pre-computation: {t1-t0:.2f}s")

    # Build target list based on strategy
    targets = []

    if strategy == 'lifted':
        n4_unsat = [int(x) for x in args.n4_unsat.split(',')]
        print(f"\n  Lifting {len(n4_unsat)} n=4 UNSAT functions to n=5")
        for tt4 in n4_unsat:
            targets.append((f"lift_{tt4}", lift_n4_to_n5(tt4)))
            targets.append((f"lift_xor_{tt4}", lift_n4_with_xor(tt4)))
            targets.append((f"lift_and_{tt4}", lift_n4_with_and(tt4)))

    elif strategy == 'random':
        count = args.count or 1000
        tts = sample_random(count, seed=args.seed or 42)
        targets = [(f"rnd_{i}", tt) for i, tt in enumerate(tts)]

    elif strategy == 'structured':
        targets.extend(sample_threshold(n))
        targets.extend(sample_symmetric(n))
        targets.extend(sample_majority_variants(n))

    elif strategy == 'all':
        # Combine strategies
        # 1. Lifted
        if args.n4_unsat:
            n4_unsat = [int(x) for x in args.n4_unsat.split(',')]
            for tt4 in n4_unsat:
                targets.append((f"lift_{tt4}", lift_n4_to_n5(tt4)))
                targets.append((f"lift_xor_{tt4}", lift_n4_with_xor(tt4)))
                targets.append((f"lift_and_{tt4}", lift_n4_with_and(tt4)))

        # 2. Structured
        targets.extend(sample_threshold(n))
        targets.extend(sample_symmetric(n))
        targets.extend(sample_majority_variants(n))

        # 3. Random
        count = args.count or 500
        tts = sample_random(count, seed=args.seed or 42)
        targets.extend([(f"rnd_{i}", tt) for i, tt in enumerate(tts)])

    print(f"\n  Total targets: {len(targets)}")

    # Scan
    results = []
    t_start = time.time()
    counts = defaultdict(int)

    max_bt = args.max_backtracks or 500000

    for idx, (label, full_tt) in enumerate(targets):
        r = analyze_function(full_tt, n, subcubes, edges, formula_domains, edge_tables,
                            max_backtracks=max_bt)
        r['label'] = label
        results.append(r)
        counts[r['result']] += 1

        if r['result'] in ('UNSAT', 'EMPTY+GENUINE'):
            tag = 'GENUINE UNSAT' if r['result'] == 'UNSAT' else 'EMPTY+GENUINE'
            print(f"  *** {tag}: {label} (TT=0x{full_tt:08X}, "
                  f"cov={r['n_covered']}/{r['n_covered']+r['n_empty']}, "
                  f"dom={r['total_domain']}, prof={r['profiles']}, "
                  f"incompat={r['edges_incompat']})")

        if (idx + 1) % 50 == 0:
            elapsed = time.time() - t_start
            rate = (idx + 1) / elapsed
            c = counts
            print(f"  [{idx+1}/{len(targets)}] "
                  f"SAT={c['SAT']} UNSAT={c['UNSAT']} "
                  f"EMPTY={c['EMPTY_UNSAT']} EMPTY+GEN={c['EMPTY+GENUINE']} "
                  f"UNK={c['UNKNOWN']+c['EMPTY+UNKNOWN']} TRIV={c['TRIVIAL']} "
                  f"({rate:.1f} fn/s)")

    elapsed = time.time() - t_start
    print(f"\n{'='*60}")
    print(f"SCAN COMPLETE: {len(targets)} functions in {elapsed:.1f}s")
    print(f"  SAT (all covered):           {counts['SAT']}")
    print(f"  GENUINE UNSAT (all covered): {counts['UNSAT']}")
    print(f"  EMPTY_UNSAT (trivial):       {counts['EMPTY_UNSAT']}")
    print(f"  EMPTY+GENUINE:               {counts['EMPTY+GENUINE']}")
    print(f"  UNKNOWN:                     {counts['UNKNOWN']+counts['EMPTY+UNKNOWN']}")
    print(f"  TRIVIAL (all empty):         {counts['TRIVIAL']}")

    # Write CSV
    if args.output:
        with open(args.output, 'w', newline='') as csvfile:
            writer = csv.DictWriter(csvfile, fieldnames=[
                'label', 'tt', 'result', 'n_covered', 'n_empty',
                'total_domain', 'profiles', 'edges_partial', 'edges_incompat'])
            writer.writeheader()
            for r in results:
                writer.writerow({
                    'label': r.get('label', ''),
                    'tt': f"0x{r['tt']:08X}",
                    'result': r['result'],
                    'n_covered': r['n_covered'],
                    'n_empty': r['n_empty'],
                    'total_domain': r['total_domain'],
                    'profiles': r['profiles'],
                    'edges_partial': r['edges_partial'],
                    'edges_incompat': r['edges_incompat'],
                })
        print(f"\nResults written to: {args.output}")

    # Print genuine UNSAT summary
    genuine = [r for r in results if r['result'] in ('UNSAT', 'EMPTY+GENUINE')]
    if genuine:
        print(f"\n{'='*60}")
        print(f"GENUINE UNSAT FUNCTIONS ({len(genuine)}):")
        for r in genuine:
            print(f"  {r.get('label','')}: TT=0x{r['tt']:08X} "
                  f"cov={r['n_covered']}/{r['n_covered']+r['n_empty']} "
                  f"dom={r['total_domain']} prof={r['profiles']} "
                  f"incompat={r['edges_incompat']}")


def main():
    parser = argparse.ArgumentParser(description="n=5 Structural CSP Scanner")
    sub = parser.add_subparsers(dest='command')

    # enumerate
    p_enum = sub.add_parser('enumerate', help='Enumerate formulas')
    p_enum.add_argument('--dim', type=int, default=3)
    p_enum.add_argument('--size', type=int, default=4)

    # solve
    p_solve = sub.add_parser('solve', help='Solve single function')
    p_solve.add_argument('--tt', type=lambda x: int(x, 0), required=True,
                         help='Truth table (decimal or 0x hex)')
    p_solve.add_argument('--dim', type=int, default=3)
    p_solve.add_argument('--size', type=int, default=4)

    # scan
    p_scan = sub.add_parser('scan', help='Scan multiple functions')
    p_scan.add_argument('--dim', type=int, default=3)
    p_scan.add_argument('--size', type=int, default=4)
    p_scan.add_argument('--strategy', choices=['lifted', 'random', 'structured', 'all'],
                        default='all')
    p_scan.add_argument('--n4-unsat', type=str, default=None,
                        help='Comma-separated n=4 UNSAT truth tables')
    p_scan.add_argument('--count', type=int, default=None,
                        help='Number of random samples')
    p_scan.add_argument('--seed', type=int, default=42)
    p_scan.add_argument('--max-backtracks', type=int, default=500000)
    p_scan.add_argument('--output', type=str, default=None,
                        help='Output CSV file')

    args = parser.parse_args()

    if args.command == 'enumerate':
        run_enumerate(args)
    elif args.command == 'solve':
        run_solve(args)
    elif args.command == 'scan':
        run_scan(args)
    else:
        parser.print_help()


if __name__ == "__main__":
    main()
