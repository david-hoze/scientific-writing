# Session Log

## 2026-03-16 — Session 1: Initialization

**Context:** Fresh start in reorganized pnp-research directory. All files consolidated from Desktop. ChatGPT API operational (chatgpt.py with urllib, no openai package needed).

**State:** Paper submission-ready. Path C pipeline operational but hits M2 scaling wall. Empty-domain bug identified. Twisted cycles show bounded NS degree (=3). BENT min-size has zero partial compatibility.

**Active problem:** Path C sub-instance analysis — extract BENT subgraphs with partial compatibility at min+1 domains.

**Next:** Assess Python scripts, run Path C experiments.

## 2026-03-16 — Session 2: Three Bug Fixes + M2 Integration

**Context:** Continued from Session 1. Working in circuit-presheaf Idris2 project.

**Bugs fixed:**
1. **Nat range bug:** `[1..0]` for Nat in Idris2 gives `[1, 0]` (counts down), not `[]`. `enumerate d 0` was generating size-1 formulas. All prior BENT numbers were shifted by 1 size level. Fixed with `case maxSize of Z => res0; S _ => ...`.
2. **Empty-domain classification:** `classifyEdge` returned FullyCompat when `totalPairs == 0` (empty domain). Now returns FullyIncompat. Added `emptyDomainNodes` tracking.
3. **Normalization depth:** Added double-negation elimination to `propagate`. Added semantic (truth-table) overlap comparison alongside structural (canonical). Semantic gives clean binary compat/incompat split.

**New features:**
- `buildCSPSemantic`: function-level compatibility classification
- `bent-sub` CLI command: BFS-based sub-instance extraction, skips empty-domain nodes
- M2Gen: trivial UNSAT detection for empty domains; Hilbert function probes at degrees 1-5
- NSDriver: uses `bash -c "M2 ..."` wrapper for MSYS2 compatibility
- M2 operational via shell wrapper at `/ucrt64/bin/M2` → build-tree binary

**Corrected BENT results (n=4, d=2, TT=0x7888):**
| s≤ | empty | semantic C/I | structural C/P/I |
|----|-------|-------------|-----------------|
| 0  | 12    | 20/76       | 20/0/76         |
| 1  | 6     | 60/36       | 40/20/36        |
| 2  | 4     | 68/28       | 20/48/28        |
| 3  | 4     | 68/28       | 0/68/28         |
| 4  | 0     | 96/0        | 4/92/0          |

**M2 experiment results:**
- s≤1, 18 non-empty nodes (74 vars, 60 edges): **SAT** — compatible families exist
- s≤1, 7 nodes (33 vars): SAT, h(0..5) = 1, 24, 224, 1008, 2160, 1728
- s≤4 full CSP: ~36K formulas, domains too large for M2. Semantic CSP trivially SAT (96/0/0).

**Key finding:** BENT structural CSP on non-empty nodes is SAT at s≤1. UNSAT at s≤1 comes entirely from empty domains. BENT likely has compatible families at all sizes — it is NOT an obstruction function.

**Next:** Find functions that produce UNSAT structural CSPs with non-empty domains. Address d=3 s≤5 performance (8min vs 5s target — needs mutable hash-based dedup in enumeration).

## 2026-03-16 — Session 3: Function Scan + M2 Scaling Wall

**Context:** Continuing from Session 2. Goal: find n=4 obstruction witness candidates.

**New CLI commands added:**
- `scan`: screens all 65536 n=4 functions for sub-function diversity at d=2
- `scan --top K`: shows top-K functions + named candidates
- `test --tt TT [--size S]`: CSP table across sizes 0..S for arbitrary truth table
- `test-m2 --tt TT --size S [--nodes N] --m2gen FILE.m2`: build CSPData + M2 script

**Idris2 parser fixes:** Extracted all `let...in` inside lambdas (map, traverse_) to named `let` bindings — Idris2 parser can't handle inline `let...in` inside lambda arguments.

**Scan results (n=4, d=2):**

| Distinct sub-funcs | # functions |
|---|---|
| 16 (maximum) | 48 |
| 15 | 292 |
| 14 | 3060 |
| ... | ... |
| 2 (parity) | 6 |
| 1 (constant) | 2 |

Named candidates: BENT = 6/16, Parity = 2/16, Majority = 3/16, Threshold-2 = 6/16.

**Top-48 functions (16/16 distinct sub-functions):** All show identical structural pattern:

| s≤ | empty | struct C/P/I | semantic C/I |
|----|-------|-------------|-------------|
| 0 | 17-19 | 2-8/0/88-94 | 2-8/88-94 |
| 1 | 12-14 | 10-16/4-6/74-82 | 14-22/74-82 |
| 2 | 2 | 40-42/38-40/16 | 80/16 |
| 3 | 2 | 0/80/16 | 80/16 |
| 4 | 0 | **0/96/0** | 96/0 |

**Key finding: at s≤4, ALL 96 edges are partial-compatible (0 fully compatible, 0 incompatible).** Compare BENT which had 4/92/0. These are the strongest obstruction candidates.

**M2 experiment results:**
- All 9 tested candidates: **SAT at s≤1** (12 nodes, 36-44 vars, 22 edges)
- s≤2 sub-instances: 93-317 vars → M2 Groebner basis **times out** (>2 min at 107 vars, >5 min at 93 vars)

**Critical bottleneck:** M2 Groebner basis computation doesn't scale beyond ~80 variables. All interesting structural CSP instances at s≤2+ have 100+ variables.

**Analysis:**
- At n=4, d=2, overlap dimension between sub-cubes is at most 1 variable → weak constraints
- At s≤1, domain sizes are 3-4 formulas → SAT trivially
- At s≤2+, domain sizes grow (10-60 formulas after dedup) → M2 can't handle
- Semantic CSP is trivially SAT whenever all domains non-empty (proved in Session 2)
- UNSAT structural CSP requires structural constraints to be globally inconsistent

**Two paths forward:**
1. **SAT solver encoding**: Convert structural CSP to Boolean SAT problem. Modern SAT solvers handle millions of variables. Would replace M2 for SAT/UNSAT detection.
2. **d=3 optimization**: The d=2 overlap (≤1 variable) may be too weak for obstructions. d=3 gives 3-dimensional sub-cubes with ≤2-variable overlaps — stronger constraints. But enumeration at d=3 is 8 min for s≤5.

**Next:** Either implement SAT solver encoding (faster than M2 for SAT/UNSAT), or optimize d=3 enumeration, or both.

## 2026-03-16 — Session 3b: Structural Obstructions Found!

**Context:** Continuing Session 3. Implemented backtracking CSP solver, scan-solve, and ran d=3 experiments.

**CSP Solver:**
- Backtracking with forward checking, O(1)-per-edge canonical key comparison
- `invertGroups` maps domIdx → canonical key; `checkEdge` compares keys
- Sorted by domain size (smallest-first heuristic), 1M fuel limit

**d=2 results (definitive):**
- ALL n=4 functions SAT at d=2, s≤4 (tested top-10, all found SAT in ~2s each)
- Overlap dimension ≤1 at d=2 → constraints too weak for obstructions

**d=3 results (BREAKTHROUGH):**
- At d=3, s≤4: 121/256 3-variable functions covered
- Scan-solve found **hundreds** of UNSAT instances among eligible functions
- Two classes of UNSAT:
  1. **Edge-incompatible UNSAT (FI > 0)**: Functions where ≥1 edge has ZERO compatible pairs despite both domains non-empty. Semantic CSP is SAT (24/0) but structural is UNSAT. Example: TT=0x8b (139), structural = 1/21/2, all domains non-empty.
  2. **Graph-coloring UNSAT (FI = 0)**: Functions where EVERY edge has some compatible pairs, but no globally consistent selection exists. Example: TT=0x2ae (686), structural = 2/22/0, 414 dedup elements.
  3. **Unverified UNSAT (FI = 0, large domains)**: Functions with >10K dedup elements where 1M fuel may be insufficient. Example: TT=0xac (172), 20691 elements. Needs verification.

**Key findings:**
1. **First genuine structural obstructions at n=4, d=3** — no compatible family exists for hundreds of functions at s≤4 size budget
2. Semantic CSP is trivially SAT for ALL eligible functions (24/0) — this confirms the obstruction is purely structural (DAG-isomorphism constraints)
3. Both "trivial" (edge-level) and "non-trivial" (graph-coloring) obstructions found
4. The non-trivial case (TT=0x2ae with 2/22/0 structural) is especially interesting — all edges have compatible pairs locally but no global selection works

**Caveats:**
- These obstructions are at size budget s≤4. At s≤5+, more formulas become available and some obstructions may dissolve
- Functions with large dedup domains (>10K) need fuel verification
- Scan still running (~1/3 through 65536 functions)

**Next steps:**
1. Complete the scan and tally total UNSAT count
2. Verify FI=0 UNSAT results with larger fuel or exact counts
3. Check persistence: do obstructions at s≤4 persist at s≤5? (requires 8-min d=3 enumeration)
4. Study NS degree of UNSAT instances — this is the Path C goal
5. Characterize which functions have obstructions (structure, complexity class)
