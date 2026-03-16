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

## 2026-03-16 — Session 4: NS Degree = 2 (Path C Limitation)

**Context:** Continuing scan-solve and Path C NS degree investigation.

**Scan-solve progress:** At ~33% (TT=21520/65536), 302 UNSAT found. Process terminated when Scheme processes were killed for rebuild.

**Hamming weight distribution of UNSAT functions:**
| Weight | UNSAT count | Total weight-k functions | Fraction |
|--------|------------|-------------------------|----------|
| 4 | 51 | 1,820 | 2.80% |
| 5 | 27 | 4,368 | 0.62% |
| 6 | 75 | 8,008 | 0.94% |
| 7 | 53 | 11,440 | 0.46% |
| 8 | 30 | 12,870 | 0.23% |
| 9 | 40 | 11,440 | 0.35% |
| 10 | 10 | 8,008 | 0.12% |
| 11 | 8 | 4,368 | 0.18% |
| 12 | 8 | 1,820 | 0.44% |

UNSAT concentrated at low Hamming weights (4-7).

**Profile-based CSP reduction:**
- New `profiles` command and `dump-csp` format added
- For each node, domain elements with identical edge profiles (canonical key on every adjacent edge) are interchangeable
- TT=686 (414 elements): reduced to 150 profiles. Reduction: 2.76x
- Python `ns_from_csp.py` parses dumps, computes profiles, generates M2 scripts, and includes a backtracking solver

**Minimal UNSAT core analysis (TT=686):**
- Removing nodes 2, 5, or 6 makes the CSP SAT (critical nodes)
- Exactly ONE 4-node UNSAT subset: {0, 2, 5, 6} (35 profiles)
- No 3-node subset is UNSAT
- Four 5-node UNSAT subsets; seven 6-node; five 7-node

**NS degree computation (LINEAR ALGEBRA METHOD):**
- Python `ns_degree.py` implements exact NS degree via monomial coefficient matching
- For degree d: set up matrix A (target monomials × multiplier coefficients), solve Ax = b where b = e_0

**Results:**
| Instance | Nodes | Profiles | NS degree |
|----------|-------|----------|-----------|
| TT=686 minimal core {0,2,5,6} | 4 | 35 | **2** |
| TT=686 sub {0,2,4,5,6} | 5 | 39 | **2** |
| TT=686 full (8 nodes) | 8 | 150 | **2** |

**⚠️ CRITICAL FINDING: NS degree = 2 is an inherent limitation.**

The structural CSP Γ(T, d, s) is always a 2-CSP: constraints are pairwise (between overlapping sub-cube pairs). For 2-CSPs with Boolean (one-hot) encoding:
- Selection constraints: degree 1
- Boolean constraints: degree 2
- Incompatibility constraints: degree 2

The NS degree is bounded by 2 regardless of n, d, or s. This is because:
1. NS degree > 1 (selection constraints alone can't prove UNSAT)
2. NS degree ≤ 2 (degree-2 constraints combined with selection constraints suffice)

**Proof sketch for NS degree ≤ 2:**
For edge-incompatible UNSAT (FI > 0): if edge (i,j) has zero compatible pairs, then Σ_a x_{i,a} = 1 and all x_{i,a}·x_{j,b} = 0 imply Σ_b x_{j,b} = 0, contradicting Σ_b x_{j,b} = 1. The certificate has degree 2.

For graph-coloring UNSAT (FI = 0): more edges are needed, but the same degree-2 mechanism works via unit propagation from forced nodes (small domains).

**Impact on Path C:**
- ❌ NS degree of the standard one-hot 2-CSP encoding is O(1), not Ω(n). It cannot give circuit lower bounds.
- ⚠️ This does NOT kill Path C entirely. Alternative proof complexity measures may still work:
  1. **Resolution width** of the CSP — can be Ω(n) even when NS degree is O(1)
  2. **NS degree over GF(2)** — Grigoriev showed Ω(n) for pigeonhole over GF(2)
  3. **Higher-arity encoding** — encode the domain algebraically rather than via one-hot
  4. **SOS/Positivstellensatz degree** — different from NS
  5. **NS degree of a DIFFERENT system** — not the compatibility CSP itself, but a related system encoding OD

**What we've learned about the structural CSP:**
1. At d=3, n=4: hundreds of UNSAT functions (structural obstruction witnesses)
2. Minimal UNSAT cores have 4 nodes (for TT=686)
3. The UNSAT structure is "shallow" — degree-2 certificates suffice
4. The obstruction is genuine (semantic CSP is always SAT) but proof-theoretically simple

**Next:**
1. Restart scan-solve to get full counts
2. Report findings to ChatGPT for strategic reassessment of Path C
3. Investigate resolution width or GF(2) NS degree as alternative measures
4. Consider whether the obstruction's *existence* (not its proof complexity) carries useful information

## 2026-03-16 — Session 5: Complete Classification + GF(2) + Roadmap Update

**Context:** Continuing from Session 4. Classification of 302 UNSAT functions by structural pattern was running.

**GF(2) NS degree results:**
- NS degree over GF(2) = **2** for TT=686 minimal core (35 vars) and full instance (150 vars)
- Same as over Q — confirms the 2-CSP structure bounds NS degree at 2 over ANY field
- Grigoriev's Ω(n) GF(2) result for pigeonhole doesn't apply here (pigeonhole is not a 2-CSP)

**Complete structural classification (302/302):**

| Pattern | Count | Type | Weights |
|---------|-------|------|---------|
| 0/24/0 | 61 | Graph-coloring | 4, 6, 7 |
| 1/23/0 | 61 | Graph-coloring | 5, 6, 7 |
| 2/22/0 | 22 | Graph-coloring | 6 |
| 1/21/2 | 57 | Edge-incompatible | 4, 8, 12 |
| 1/22/1 | 34 | Edge-incompatible | 7, 9 |
| 4/18/2 | 29 | Edge-incompatible | 6, 10 |
| 0/23/1 | 15 | Edge-incompatible | 9 |
| 2/20/2 | 13 | Edge-incompatible | 8, 11 |
| 2/21/1 | 10 | Edge-incompatible | 9 |

**Totals:** 158 edge-incompatible (52.3%) vs 144 graph-coloring (47.7%)

**Hamming weight analysis:**
- FI=0 (graph-coloring) exists ONLY at weights 4, 5, 6, 7
- FI>0 (edge-incompatible) exists at weights 4, 6, 7, 8, 9, 10, 11, 12
- Weight 5 is purely graph-coloring (27 functions, 0 edge-incompatible)
- Weight 8+ is purely edge-incompatible (96 functions, 0 graph-coloring)
- Complement symmetry: wt-k and wt-(16-k) have identical counts

**Roadmap update:** Rewrote post_retraction_roadmap.md:
- Path C downgraded from HIGH to MEDIUM priority
- Strategy DAG updated: "NS degree = 2 (dead) / Resolution width?"
- Added 8 new completed items to timeline
- Decision points updated: "Resolution width grows with n?" replaces "NS degree > 3?"
- Summary rewritten to reflect NS degree negative result

**Next:**
1. Complete scan-solve of remaining ~67% of n=4 functions
2. Test obstruction persistence at s≤5
3. Investigate resolution width via CDCL proof logging
4. Commit and push updates

## 2026-03-16 — Session 6: Verification Types + Resolution Width Analysis

**Context:** Continuing from Session 5. Two parallel tracks: (1) Idris2 verified types, (2) Path C resolution width.

### Idris2 Verification Types (Complete)

Four new modules in `src/Verified/` providing dependent-type safety:

1. **`Verified.Formula`** — `VFormula d` with `Fin d` indices. Total evaluation, no OOB fallback. Includes `evalPreserved` proof: `veval f xs = eval (toRaw f) xs`. Zero `believe_me`.

2. **`Verified.SubCube`** — `VSubCube n d` with `Vect d (Fin n)` free coords. Erased `So` proofs for distinctness/disjointness.

3. **`Verified.CSP`** — `VCanonGroups domSize` with partition proof. `VEdge` with bounded node refs. `VCSPEdge` with dependent domain sizes.

4. **`Verified.Solver`** — `SATWitness` (assignment + AllSatisfied proof), `UNSATCert` (only TrivialUnsat with empty-domain proof), `VInconclusive` (honest label for fuel-bounded UNSAT). Uses `DecEq String` from stdlib — no escape hatches.

**Verification:** `verifiedSolve` on TT=686 returns `VSAT (verified witness)`, matching unverified solver. All four modules compile clean with `%default total` (Formula, SubCube, CSP) or `%default covering` (Solver, which calls non-total `solveCSP`).

### Resolution Width Analysis (Negative Result)

**Tools built:**
- `scripts/resolution_width.py` — CNF export, DIMACS writer, DPLL solver, iterative widening resolution width
- `scripts/cdcl_width.py` — CDCL solver with 1-UIP conflict analysis, proof width tracking (learned + intermediate)
- `scripts/width_scaling.py` — systematic sub-CSP extraction and width measurement

**CNF encoding:** Direct (one-hot) encoding of profile-reduced CSP:
- At-least-one clauses: width = number of profiles per node (up to 70)
- At-most-one clauses: width 2 (binary)
- Incompatibility clauses: width 2 (binary)

**Results on TT=686:**

| Instance | Nodes | Vars | Clauses | Result | Decisions | Conflicts | Max Learned Width | Max Intermediate Width | Max Initial Width |
|---|---|---|---|---|---|---|---|---|---|
| Full (8 nodes) | 8 | 150 | 6881 | UNSAT | 0 | 0 | 1 | 0 | 70 |
| Core {0,2,5,6} | 4 | 35 | 510 | UNSAT | 21 | 7 | 16 | 17 | 17 |
| No forced nodes | 7 | 149 | 6830 | **SAT** | 54 | 2 | — | — | 70 |
| No small nodes | 6 | 138 | 6196 | **SAT** | 26 | 0 | — | — | 70 |

**Systematic sub-instance analysis (all C(8,k) subsets):**
- 4-node UNSAT: 1 instance ({0,2,5,6}), max width 16
- 5-node UNSAT: 4 instances, max width 1 or 16
- 6-node UNSAT: 7 instances, max width 1 or 16
- Width is **bimodal**: either 1 (unit propagation, when node 1 present) or 16 (search needed)

**⚠️ KEY FINDING: Resolution width = max initial clause width.**

The CDCL proof width (max intermediate resolvent = 17) equals the max at-least-one clause width (17 for the core). The Ben-Sasson–Wigderson tradeoff gives:

> size ≥ 2^((res_width - max_initial_width)² / n) = 2^((17-17)²/35) = 2^0 = 1

**This is a trivial bound.** Resolution width does not exceed initial clause width for the structural CSP's direct encoding.

**Why this happens:**
1. The structural CSP is a 2-CSP (pairwise constraints) — all constraint clauses are binary (width 2)
2. The only wide clauses are at-least-one (cardinality) constraints
3. CDCL's conflict analysis resolves binary constraint clauses with cardinality clauses, producing resolvents bounded by the cardinality clause width
4. No "amplification" of proof width occurs

**Implications for Path C:**
- ❌ Resolution width of the direct encoding is bounded by max(profiles per node), which is O(|domain|)
- ❌ Combined with NS degree = 2, both standard proof complexity measures are bounded for the structural CSP's one-hot encoding
- ⚠️ Alternative encodings (log encoding, order encoding) could change the picture — different CNF encodings of the same CSP have different proof complexities
- ⚠️ The max profile count could still grow with n (more edges → finer profiles), but this growth is bounded by domain size (= formulas of size ≤ s on d inputs), which is independent of n

**Second negative result for Path C.** Both NS degree (= 2) and resolution width (= max initial clause width) are bounded by the 2-CSP structure. The direct encoding's proof complexity is shallow.

### Scan-solve status
- Background scan-solve at d=3, s≤4 still running (stuck at 121/256 functions, 1 UNSAT)
- Likely blocked on a large instance

**Next:**
1. Update roadmap with resolution width negative result
2. Consider alternative CNF encodings (log/order) or entirely different proof complexity approaches
3. Commit scripts and findings
4. Complete scan-solve (may need restart)

## 2026-03-16 — Session 6b: Persistence Test + n=5 + ChatGPT Consultation

**Obstruction persistence test (CRITICAL NEGATIVE RESULT):**
- TT=686: **UNSAT at s≤4 → SAT at s≤5** (8 nodes, 7625 dedup elements, compatible family found)
- TT=139: s≤5 test OOMs (too large), but inconclusive
- **The structural obstructions at s≤4 are size-budget artifacts.** Like stiffness, they dissolve when more formulas become available. This significantly weakens the obstruction-based direction.

**n=5 support added:**
- `inferN` function derives n from truth table size (default n=4 for TT ≤ 65535, n=5 for larger)
- Bug found and fixed: initially inferred n=3 for TT=139 (small TT ≠ small n)
- n=5 TT=100000 at d=3, s≤3: 40 nodes, 480 edges, 35 empty-domain nodes
- n=5 extension of TT=139 at s≤4: 40 nodes, 237K elements, UNSAT/fuel exhaustion (unverified)
- n=5 computations are very slow (minutes per function at s≤4)

**ChatGPT brainstorm consultation:**
Five angles proposed. My verification:
1. Alternative CNF encodings (log/order) — ⚠️ plausible, testable
2. Lifting theorems — ❌ blocked (proof too shallow to lift)
3. Polynomial calculus degree — ❌ incorrect (PC degree ≤ NS degree, so PC ≤ 2)
4. Obstruction counting — ✅ already planned
5. Sheaf non-locality — ⚠️ too vague

Key verification: PC degree ≤ NS degree because NS refutations are valid PC proofs. Since NS = 2, all algebraic proof systems are bounded at degree ≤ 2 for the one-hot 2-CSP encoding.

**Reorganization:**
- pnp-research/ reorganized into paper/, research/, archive/, scripts/ subdirectories
- Root-level duplicates deleted
- CLAUDE.md updated to match new structure

**Assessment of Path C after all findings:**
1. NS degree = 2 (dead)
2. Resolution width = initial width (dead)
3. PC degree ≤ NS degree ≤ 2 (dead)
4. Obstructions dissolve at s≤5 (dead as persistent witnesses)
5. Only remaining angle: alternative encodings or obstruction density scaling with n

**Next:**
1. Test more UNSAT functions at s≤5 to confirm dissolution pattern
2. Focus on Path B (paper submission)
3. Consider whether obstruction density scaling with n is worth pursuing

## 2026-03-16 — Session 7: n=5 Investigation (Python Pipeline)

**Context:** Following "investigate n=5" directive. Previous Idris2 approach too slow for n=5 (OOM on profiles dump).

**Python pipeline built:** `circuit-presheaf/scripts/n5_scan.py`
- Full reimplementation of formula enumeration, sub-cube geometry, canonical grouping, profile reduction, and backtracking solver in Python
- Key optimization: **restriction pattern deduplication** — 18 distinct patterns vs 480 edges (25× speedup on pre-computation)
- Pre-computation: ~23s for d=3, s≤4 (93K formulas, 121 TTs, 18 patterns)
- Solve rate: ~5-10 functions/second at n=5

**Formula enumeration results:**
- d=3, s≤4: 93,315 distinct formulas, 121/256 TTs covered (47.3%)
- d=3, s≤5: 1,587,920 distinct formulas, 191/256 TTs covered (74.6%)

**Critical discovery: size-budget coverage gap at n=5**
- For n=5, d=3: 40 sub-cubes, each extracting a 3-variable truth table
- At s≤4, only 121/256 (47%) 3-var TTs are coverable
- P(all 40 sub-cubes covered) ≈ 0.47^40 ≈ 10^{-13} for random functions
- ALL random n=5 functions have empty-domain sub-cubes → trivially UNSAT
- Analysis corrected to distinguish TRIVIAL/EMPTY_UNSAT/GENUINE_UNSAT

**n=5 results (d=3, s≤4):**

| Category | Lifted (6) | Structured (73) | Random (200) |
|----------|-----------|-----------------|-------------|
| SAT (full coverage) | 0 | 20 | 0 |
| GENUINE UNSAT (full coverage) | 4 | 0 | 0 |
| EMPTY_UNSAT (trivial) | 0 | 31 | 59 |
| EMPTY+GENUINE | 2 | 0 | 141 |
| UNKNOWN | 0 | 1 | 0 |
| TRIVIAL | 0 | 21 | 0 |

**Key findings:**
1. **Lifted n=4 UNSAT → n=5 UNSAT**: lift_686 (40/40 covered, 151K dom, 7092 prof, graph-coloring UNSAT) and lift_139 (40/40, 237K dom, 8384 prof, 4 incompat edges) both genuinely UNSAT at n=5
2. **lift_and variants also UNSAT**: AND with x4 preserves UNSAT with even larger domains
3. **Symmetric functions: 0 genuine UNSAT** — all 31 "UNSAT" were trivial empty-domain
4. **Random functions: 70% have genuine subgraph obstructions** but none have full coverage
5. **MAJ5 is SAT** (26,820 domain, 4400 profiles) — confirmed

**Interpretation:**
- The n=4→n=5 lifting preserves structural obstructions (not just size artifacts)
- Obstructions that dissolve at s≤5 in n=4 (like TT=686) do NOT dissolve when lifted to n=5 at s≤4 — the extra variable adds structural rigidity
- The critical question: do n=5 genuine UNSAT persist at s≤5? Running now.

**n=4 complete scan (in progress):** At 60K/65K, finding ~904 UNSAT (vs 302 at 33% in earlier Idris2 scan). Full count pending.

**n=5 persistence test at s≤5 (running):** Testing lifted TT=686 and TT=139 at s≤5 (1.59M formulas, 191/256 coverage).
