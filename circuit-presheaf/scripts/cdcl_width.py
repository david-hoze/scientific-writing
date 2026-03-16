#!/usr/bin/env python3
"""
CDCL SAT solver with proof recording for resolution width measurement.

For UNSAT instances, records the resolution proof (sequence of conflict clauses)
and reports the maximum clause width. This gives an upper bound on the minimum
resolution width needed to refute the formula.

Usage:
  python3 cdcl_width.py CSP_DUMP_FILE
"""

import sys
import os
from collections import defaultdict

sys.path.insert(0, os.path.dirname(__file__))
from ns_from_csp import parse_csp, compute_profiles
from resolution_width import build_cnf


class CDCLSolver:
    """CDCL SAT solver with proof width tracking."""

    def __init__(self, num_vars, clauses):
        self.num_vars = num_vars
        self.clauses = [list(c) for c in clauses]
        self.assignment = {}    # var -> (value, decision_level, reason_clause_idx)
        self.trail = []         # assignment order
        self.decision_level = 0
        self.watches = defaultdict(list)  # lit -> [clause_idx]
        self.learned = []       # learned clauses
        self.max_learned_width = 0
        self.max_intermediate_width = 0  # max width of intermediate resolvents
        self.conflicts = 0
        self.decisions = 0

        # Initialize watched literals (watch first two literals)
        for ci, clause in enumerate(self.clauses):
            if len(clause) >= 1:
                self.watches[clause[0]].append(ci)
            if len(clause) >= 2:
                self.watches[clause[1]].append(ci)

    def value(self, lit):
        """Get value of a literal. Returns True/False/None."""
        var = abs(lit)
        if var not in self.assignment:
            return None
        val = self.assignment[var][0]
        return val if lit > 0 else not val

    def assign(self, var, val, level, reason=None):
        """Assign a variable."""
        self.assignment[var] = (val, level, reason)
        self.trail.append(var)

    def unassign_to_level(self, level):
        """Undo assignments above the given level."""
        while self.trail and self.assignment[self.trail[-1]][1] > level:
            var = self.trail.pop()
            del self.assignment[var]

    def propagate(self):
        """Unit propagation. Returns conflicting clause index or None."""
        changed = True
        while changed:
            changed = False
            for ci, clause in enumerate(self.clauses):
                unset = []
                satisfied = False
                for lit in clause:
                    v = self.value(lit)
                    if v is True:
                        satisfied = True
                        break
                    elif v is None:
                        unset.append(lit)

                if satisfied:
                    continue
                if len(unset) == 0:
                    return ci  # conflict
                if len(unset) == 1:
                    # Unit clause: propagate
                    lit = unset[0]
                    var = abs(lit)
                    val = lit > 0
                    self.assign(var, val, self.decision_level, ci)
                    changed = True

            # Also check learned clauses
            for ci_offset, clause in enumerate(self.learned):
                ci = len(self.clauses) + ci_offset
                unset = []
                satisfied = False
                for lit in clause:
                    v = self.value(lit)
                    if v is True:
                        satisfied = True
                        break
                    elif v is None:
                        unset.append(lit)

                if satisfied:
                    continue
                if len(unset) == 0:
                    return ci  # conflict
                if len(unset) == 1:
                    lit = unset[0]
                    var = abs(lit)
                    val = lit > 0
                    self.assign(var, val, self.decision_level, ci)
                    changed = True

        return None

    def get_clause(self, ci):
        """Get clause by index (original or learned)."""
        if ci < len(self.clauses):
            return self.clauses[ci]
        return self.learned[ci - len(self.clauses)]

    def analyze_conflict(self, conflict_ci):
        """Analyze conflict and learn a clause. Returns (learned_clause, backtrack_level)."""
        # Simple 1-UIP conflict analysis
        clause = set(self.get_clause(conflict_ci))

        # Resolve until only one literal at the current decision level
        while True:
            current_level_lits = [lit for lit in clause
                                  if abs(lit) in self.assignment
                                  and self.assignment[abs(lit)][1] == self.decision_level]
            if len(current_level_lits) <= 1:
                break

            # Find most recent assigned literal at current level
            most_recent = None
            most_recent_idx = -1
            for lit in current_level_lits:
                var = abs(lit)
                idx = self.trail.index(var)
                if idx > most_recent_idx:
                    most_recent_idx = idx
                    most_recent = lit

            # Get its reason clause
            var = abs(most_recent)
            reason = self.assignment[var][2]
            if reason is None:
                break  # decision variable, can't resolve further

            # Resolve
            reason_clause = set(self.get_clause(reason))
            # Remove the pivot variable
            pivot_pos = abs(most_recent)
            clause.discard(most_recent)
            clause.discard(-most_recent)
            reason_clause.discard(most_recent)
            reason_clause.discard(-most_recent)
            clause |= reason_clause
            # Track intermediate resolvent width
            self.max_intermediate_width = max(self.max_intermediate_width, len(clause))

        learned_clause = list(clause)

        # Find backtrack level (second highest decision level in learned clause)
        levels = set()
        for lit in learned_clause:
            var = abs(lit)
            if var in self.assignment:
                levels.add(self.assignment[var][1])

        if len(levels) <= 1:
            bt_level = 0
        else:
            sorted_levels = sorted(levels, reverse=True)
            bt_level = sorted_levels[1]

        return learned_clause, bt_level

    def pick_variable(self):
        """Pick an unassigned variable. Returns var or None."""
        for v in range(1, self.num_vars + 1):
            if v not in self.assignment:
                return v
        return None

    def solve(self, max_conflicts=1000000):
        """Run CDCL solver. Returns 'SAT', 'UNSAT', or 'UNKNOWN'."""
        # Initial propagation
        conflict = self.propagate()
        if conflict is not None:
            # Conflict at level 0 — UNSAT
            learned, _ = self.analyze_conflict(conflict)
            self.max_learned_width = max(self.max_learned_width, len(learned))
            return 'UNSAT'

        while True:
            if self.conflicts >= max_conflicts:
                return 'UNKNOWN'

            var = self.pick_variable()
            if var is None:
                return 'SAT'

            # Decision
            self.decision_level += 1
            self.decisions += 1
            self.assign(var, False, self.decision_level, None)

            # Propagate
            conflict = self.propagate()

            while conflict is not None:
                self.conflicts += 1

                if self.decision_level == 0:
                    return 'UNSAT'

                # Analyze conflict
                learned_clause, bt_level = self.analyze_conflict(conflict)
                width = len(learned_clause)
                self.max_learned_width = max(self.max_learned_width, width)
                self.learned.append(learned_clause)

                # Backtrack
                self.unassign_to_level(bt_level)
                self.decision_level = bt_level

                # The learned clause should be unit after backtracking
                conflict = self.propagate()

            # Check if we need to try the other branch
            # (In full CDCL, backtracking + learned clause handles this)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)

    csp_file = sys.argv[1]
    nodes, edges = parse_csp(csp_file)
    profiles, profile_keys = compute_profiles(nodes, edges)

    total_profiles = sum(len(p) for p in profiles.values())
    print(f"Nodes: {len(nodes)}, Edges: {len(edges)}")
    print(f"Profile variables: {total_profiles}")

    num_vars, clauses, var_map = build_cnf(profiles, profile_keys, edges)
    print(f"CNF: {num_vars} vars, {len(clauses)} clauses")

    # Solve
    solver = CDCLSolver(num_vars, clauses)
    result = solver.solve()

    print(f"\nResult: {result}")
    print(f"Decisions: {solver.decisions}")
    print(f"Conflicts: {solver.conflicts}")
    print(f"Learned clauses: {len(solver.learned)}")
    print(f"Max learned clause width: {solver.max_learned_width}")
    print(f"Max intermediate resolvent width: {solver.max_intermediate_width}")

    # Initial clause width stats
    init_widths = [len(c) for c in clauses]
    print(f"Initial clause widths: min={min(init_widths)}, max={max(init_widths)}, avg={sum(init_widths)/len(init_widths):.1f}")

    if solver.learned:
        widths = [len(c) for c in solver.learned]
        print(f"Learned clause widths: min={min(widths)}, max={max(widths)}, avg={sum(widths)/len(widths):.1f}")

        # Width distribution
        width_dist = defaultdict(int)
        for w in widths:
            width_dist[w] += 1
        print("\nWidth distribution:")
        for w in sorted(width_dist.keys()):
            print(f"  width {w}: {width_dist[w]} clauses")


if __name__ == "__main__":
    main()
