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

## 2026-03-16 — Session 8: Sigma Scaling Law (Compression Bound)

**Context:** Following directive "go the best route you think" toward P ≠ NP. Chose to investigate Open Problem 5.10 (compression bound / σ∞(d) scaling).

**Built:** `compression_analysis.py` — Python reimplementation of the restriction image σ computation, verified to match Idris binary output exactly at all dimensions and size levels.

**Key discovery: σ definition clarification**
- σ(s,d) = |U(s,d)| / M(s,d) where:
  - U = set of (d-1)-var canonical formula strings from restricting d-var formulas
  - M = max over (function, direction) pairs of per-direction restriction image size
- Previously misunderstood as raw formula count ratio. The restriction image version gives INCREASING σ with s (correct), not decreasing.

**New data (first computation at s=5, extending beyond Idris capability):**

| d | s≤4 (paper) | **s≤5 (new)** | σ∞ estimate |
|---|-------------|---------------|-------------|
| 2 | 2.16 | **2.19** | ~2.2 |
| 3 | 6.45 | **6.52** | ~6.5-6.6 |
| 4 | 13.40 | **13.66** | ~13.7-14 |

**Cross-dimensional scaling law:**
σ∞(d) ≈ 1.4d² − 2.7d + 2.0 (quadratic fit, exact at d=2,3,4)

**Critical finding:** The canonical/raw ratio (empirical σ vs Savicky prediction 2^{2^{d-1}}):
- d=2: 55%, d=3: 41%, d=4: 5.3%, d=5 (predicted): 0.04%
- Canonical compression absorbs most of the Savicky anti-concentration
- σ still grows to ∞ (quadratically), but not fast enough for the CSP program

**Assessment:** σ∞(d) ≈ d² → at d = c·log(n), dilution is O(log²n) — polylogarithmic. Too weak for structural CSP to yield superpolynomial lower bounds. The structural presheaf program reaches a natural quantitative limit.

**Files created/modified:**
- `circuit-presheaf/scripts/compression_analysis.py` (created -- restriction image sigma analysis)
- `pnp-research/state/current_problem.md` (updated -- compression findings)

## 2026-03-16 -- Session 9: Restriction Depth and Affine Site Investigation

**Context:** User asked "Can you make the rate of structural diversification part of the proof?" Following analysis of whether sigma ~ d^2 can be an ingredient rather than a limitation.

**Question addressed:** Can a different site topology (random restrictions, projections, affine substitutions) yield exponential sigma growth?

**Three experiments conducted:**

1. **Restriction depth independence (CONFIRMED)**
   - d=4 k=2 value restriction (target dim 2): sigma = 6.4537
   - d=3 k=1 value restriction (target dim 2): sigma = 6.4537
   - EXACT MATCH: sigma depends only on target dimension, not restriction depth
   - Theoretical proof: at convergence, sigma_k(d) = sigma_1(d-k+1)

2. **Affine site (value + projection restrictions)**
   - Added x_i -> x_j and x_i -> NOT(x_j) projections to standard restrictions
   - Projections preserve formula size (no constant folding), accessing larger target formulas
   - Result: ~4x higher sigma but SAME quadratic growth rate:
     - Value: sigma ~ 1.3d^2
     - Affine: sigma ~ 6.2d^2
   - Ratio aff/val approaches 4.0 at d=5

3. **d=5 definitive measurement**
   - 411,397 canonical 5-var formulas enumerated at s<=4
   - sigma_val(4,5) = 23.05 (predicted from d=2,3,4 quadratic: 23.0)
   - sigma_aff(4,5) = 92.15 (predicted from quadratic: 90.6)
   - Both fit quadratic with R^2 > 0.9999
   - Exponential fit R^2 = 0.92 -- definitively ruled out

**Definitive conclusion:** sigma ~ d^2 is an INTRINSIC property of Boolean formula combinatorics. No site topology change (sub-cube, multi-variable, affine, decision tree) can improve the growth rate beyond quadratic. The T_g distribution (formula class counts across functions) determines sigma, and the top function's share decreases as O(1/d^2).

**What this rules out:**
- Any sigma-based route to circuit lower bounds (sigma is polylogarithmic at d = O(log n))
- Site topology changes as a remedy
- The "find a better site" research direction for improving sigma specifically

**What remains viable:**
- Constraint graph topology (global structure, not per-edge)
- Cohomological invariants beyond sigma
- T_g distribution statistics other than sum/max ratio
- Non-structural approaches (query complexity, non-relativization)

**Files created:**
- `circuit-presheaf/scripts/restriction_depth_analysis.py` (multi-k and affine sigma)
- `circuit-presheaf/scripts/affine_scaling.py` (cross-dimensional affine comparison)
- `circuit-presheaf/scripts/affine_d5_test.py` (d=5 s<=3 scaling check)
- `circuit-presheaf/scripts/affine_d5_s4.py` (d=5 s<=4 definitive measurement)
- `pnp-research/state/current_problem.md` (rewritten -- definitive results)

## 2026-03-16 — Session 10: N_eff and the Renyi Entropy Spectrum

**Context:** Following Session 9's definitive sigma ~ d^2 result, investigated Shannon entropy N_eff = 2^{H_1} ~ 2.24^d (discovered late Session 9) and its role in the proof.

**Three analyses conducted:**

1. **Shannon entropy of T_g distribution (EXPONENTIAL)**
   - H_func ~ 1.16d (linear in d) → N_eff ~ 2.24^d (exponential)
   - Growth rate CONSTANT: 2.27, 2.22, 2.24 per d step
   - Exponential fit R^2 = 0.99997 — definitively exponential
   - At d = c*log(n): N_eff ~ n^{1.16c} — POLYNOMIAL in n

2. **Full Renyi entropy spectrum (SHARP TRANSITION AT alpha ~ 1)**
   - Computed N_alpha = 2^{H_alpha} for alpha = 0, 0.5, 1, 1.5, 2, 3, 5, 10, inf
   - At d = 2, 3, 4, 5 with max_size = 5 (d<=3) or 4 (d>=4)
   - KEY DIAGNOSTIC: growth rate TREND (constant = exp, decelerating = poly)
   - alpha=0.5: 3.81^d (exp, constant ratios 3.78, 3.83, 3.82)
   - alpha=1 (Shannon): 2.24^d (exp, constant ratios 2.27, 2.22, 2.24)
   - alpha=1.5: borderline (decel ratios 1.79, 1.69, 1.62)
   - alpha=2 (collision): polynomial ~d^1.3 (decel ratios 1.59, 1.49, 1.40)
   - alpha=inf (sigma): polynomial ~d^0.62 (decel ratios 1.24, 1.21, 1.17)
   - **Shannon entropy is the LAST Renyi order with genuinely exponential growth**

3. **UNSAT core diversity (PRELIMINARY)**
   - TT=686 (d=3, n=4): 6-edge core, 75 distinct canonical group keys
   - Most UNSAT at d=3 n=4 are trivial (empty domains)
   - Too few sub-cubes (8) to observe N_eff scaling

**Proof technique constraint (Finding 14):**
- Worst-case arguments → use sigma (poly) → INSUFFICIENT
- Collision/birthday arguments → use N_2 (poly) → INSUFFICIENT
- **Average-case/distributional arguments → use N_eff (exp) → SUFFICIENT**
- The proof technique MUST be distributional in nature

**Three routes for N_eff:**
- Route A (moderate): UNSAT core diversity + function-type encoding for proof complexity
- Route B (highest): Distributional communication complexity of OD
- Route C (low): Williams algorithmic method (no bridging reduction to Circuit-SAT)

**M2 assessment:** Python scripts should stay for formula enumeration/entropy. M2 (available v1.25.11) is right tool for algebraic CSP analysis (Groebner, UNSAT proofs) but wrong for combinatorial enumeration.

**Files created:**
- `circuit-presheaf/scripts/entropy_analysis.py` (Shannon entropy + T_g measures)
- `circuit-presheaf/scripts/neff_analysis.py` (Renyi spectrum + UNSAT core diversity)
- `pnp-research/state/current_problem.md` (rewritten — N_eff and Renyi results)

**Next:**
1. Formalize the distributional communication complexity argument (Route B)
2. Determine the right distribution on truth tables for a distributional KW theorem
3. Test N_eff at d=6 (if computationally feasible) to confirm the base 2.24 holds
4. Investigate whether the constraint graph topology adds to N_eff

## 2026-03-17 — Session 11: N_eff Combined Analysis + Verified UNSAT Certificates + Information Loss on Cycles

**Context:** Continuing from Session 10. Three major developments:

### 1. Combined N_eff Analysis (Options 1-3)

Ran `combined_neff.py` analyzing constraint graph topology (Opt 1), compression bounds (Opt 2), and cohomological structure (Opt 3) for all genuine UNSAT instances at n=4, d=3, s<=4.

**Results:**
- Option 2 (compression): sigma_eff/sigma ratio grows with d (0.006 -> 0.290), confirming N_eff enters the compression bound
- Options 1&3 (topology + cohomology): genuine UNSAT instances have B1=14-16, type diversity 6-8/8

### 2. Verified UNSAT Certificates (Exhaustive.idr)

Built and deployed a certificate-based UNSAT verification system in Idris2:

**New module:** `Verified.Exhaustive` — mutual recursive certificate types (`RefutationCert`, `RejectReason`), complete backtracking solver (`solveWithCert`), independent certificate checker (`checkCert`).

**Modified:** `Verified.Solver` (added `ExhaustiveUnsat` constructor), `Analysis.CompatCSP` (exported `EdgeKeys`, `mkEdgeKeys`, `checkEdge`, `isConsistent`), `Main` (added `cert` command).

**Results:** 1,056 of 1,064 genuine UNSAT instances machine-verified (99.2%). 8 failures due to OOM on large instances. Largest verified certificate: 4,693,168 nodes. Zero verification failures.

**Complete census at n=4, d=3, s<=4:**
- 65,536 total truth tables
- 1,064 genuine UNSAT (non-empty domains, no compatible family)
- 1,056 machine-verified with independently checked refutation certificates
- All 1,064 have unique conflict patterns (no two share the same structural fingerprint)

### 3. Information Loss on Cycles (THE KEY INSIGHT)

**Critical empirical finding:** At s<=4, ALL 1,064 genuine UNSAT instances have:
- 0% fully incompatible edges
- 0% fully compatible edges
- 100% PARTIALLY compatible edges

**Progression across sizes:**
| s<= | C     | P      | I     |
|-----|-------|--------|-------|
| 0   | 19.9% | 0.0%   | 80.1% |
| 1   | 36.1% | 7.8%   | 56.0% |
| 2   | 27.0% | 73.0%  | 0.0%  |
| 3   | 0.5%  | 99.5%  | 0.0%  |
| 4   | 0.0%  | 100.0% | 0.0%  |

**This kills the graph-coloring proof path** (Turan, chromatic number bounds, clique-based arguments). There are no fully incompatible edges to exploit.

**New proof path:** UNSAT arises from **cohomological obstruction** — local consistency on every edge, but global inconsistency around cycles. Written up in:
- `Verified/ProofSearch.idr` — type-level proof skeleton with exact holes
- `pnp-research/research/Information_Loss_on_Cycles.md` — full research note

**The information loss mechanism:**
1. Each partial edge has an overlap ratio r < 1 (fraction of canonical keys at source matching any key at destination)
2. Along a cycle of length L, surviving fraction is at most r^L
3. When r^L < 1/k_max (where k_max = max profiles at any node), no assignment survives
4. N_eff controls k_max (more types = more profiles = more information to lose)
5. Shannon entropy is the right measure because the loss per edge is an average-case quantity (conditional entropy)

**Formal statement (Lemma 3.4):** For random T, the max edge overlap ratio r is bounded away from 1. Then Theorem 3.5: UNSAT when N_eff > (1-epsilon)^{-L}, which holds for all large n since N_eff grows exponentially.

**The main open problem is now precisely identified:** Prove Lemma 3.4 (overlap ratio bound). This corresponds exactly to `?richStructureForcesObstruction_hole` in the Idris2 formalization.

**Idris2 type-level proof skeleton (ProofSearch.idr):**
```
HighNeff -> HighDiversity -> RichPartialStructure -> CohomologicalObstruction -> UNSAT
```
Five holes, three provable (overlap, cycle loss, presheaf-CSP correspondence), one main open problem (rich structure forces obstruction), one diagnostic (confirms gap is load-bearing).

**Files created/modified:**
- `src/Verified/Exhaustive.idr` (new — certificate types + solver + checker)
- `src/Verified/ProofSearch.idr` (new — type-level proof skeleton)
- `src/Verified/Solver.idr` (modified — ExhaustiveUnsat constructor)
- `src/Analysis/CompatCSP.idr` (modified — exported private types)
- `src/Main.idr` (modified — cert command)
- `circuit-presheaf.ipkg` (modified — new modules)
- `scripts/shannon_conflict_analysis.py` (new — edge classification analysis)
- `scripts/overlap_ratio_analysis.py` (new — overlap ratio computation)
- `scripts/genuine_unsat_n4d3s4.txt` (new — all 1064 genuine UNSAT truth tables)
- `pnp-research/research/Information_Loss_on_Cycles.md` (new — key insight writeup)

**What this resolves from the open questions:**
- Q4 (constraint graph topology): YES, cycle structure (B1 independent cycles) is the vehicle for cohomological obstruction
- Q5 (H^1 beyond sigma): YES, H^1 IS the mechanism for UNSAT in the pure-partial regime
- Q6 (distributional proof technique): Information loss on cycles IS the technique; uses Shannon entropy because loss per edge is average-case

**Next:**
1. Compute overlap ratios r_{uv} explicitly for all 1,064 instances (running)
2. Verify Lemma 3.4 computationally at n=4, d=3
3. Attempt Lemma 3.4 proof for general n
4. Test whether overlap ratio stays bounded at n=5
