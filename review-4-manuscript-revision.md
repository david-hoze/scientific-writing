# Technical details for quantum computing architecture manuscript revision

Cat qubit architectures achieve **44× overhead reduction** over surface codes for 100 logical qubits (758 vs. 33,700 physical qubits), IBM's bivariate bicycle codes reach a **24:1 physical-to-logical ratio** in memory mode, and phononic bandgap engineering for bosonic-mode cavities remains an unexplored but promising research direction. Below is a section-by-section technical breakdown with the specific numbers, thresholds, and citations needed for manuscript revision.

---

## 1. Magic-state distillation is dramatically cheaper with biased noise

The core advantage of cat qubit architectures for magic states is that they use **Toffoli (CCZ) magic states** rather than T states, and the exponential bit-flip suppression enables physical-level state preparation that bypasses expensive multi-round distillation.

**Chamberland et al., PRX Quantum 3, 010329 (2022)** presents a two-tiered magic state approach for concatenated cat codes. The "bottom-up" protocol prepares Toffoli states at the physical hardware level exploiting noise bias, requiring only a compact circuit of physical CNOT and Toffoli gates with repeated measurement for fault tolerance. The "top-down" protocol adds a novel distillation layer for higher-fidelity states. Resource estimates: **~1,000–2,000 ATS components** for classically intractable random circuits (~100 logical qubits, ~1,000 Toffoli gates), and **~18,000 ATS components** for Hubbard model simulation (~100 logical qubits, ~10⁶ Toffoli gates). The paper explicitly notes the magic state factory consumes a small fraction of total resources — a stark contrast to surface-code architectures where factories consume **50–75%** of total qubits.

**Gouzien et al., PRL 131, 040602 (2023)** analyzes a full repetition cat code architecture for 256-bit elliptic curve discrete logarithm (Shor's algorithm): **126,133 cat qubits**, **19 average photons** per cat state, **9-hour runtime**, assuming κ₁/κ₂ = 10⁻⁵ and 500 ns cycle time. Magic states (Toffoli/CCZ) are prepared offline in dedicated factories via projective measurements and consumed through gate teleportation. The total count includes logical algorithm qubits, routing qubits for all-to-all connectivity, and magic state factory qubits.

**Ruiz et al., "Unfolded distillation," arXiv:2507.12511 (2025)** is the most significant recent advance. It "unfolds" the X stabilizer group of the 3D quantum Reed-Muller code into 2D, exploiting cat qubit noise bias to eliminate the need for 3D structure. Key numbers: only **53 cat qubits** per magic state (vs. 463 for Gidney's cultivation for unbiased qubits), **5.5 error correction rounds** for logical error rate **3 × 10⁻⁷**, requiring noise bias η ≳ 5 × 10⁶ (or η ≳ 80 with slightly increased volume). This represents an **8.7× qubit reduction** and **>10× circuit volume reduction** over the best unbiased-noise schemes. Crucially, it requires only nearest-neighbor 2D gates — no physical Toffoli gates needed.

**Daguerre et al., PRX 15, 041008 (2025)** demonstrated code switching between [[15,1,3]] quantum Reed-Muller and [[7,1,3]] Steane codes on a **Quantinuum ion-trap processor**, achieving magic state infidelity of at most **5.1(2.7) × 10⁻⁴** — below the leading physical two-qubit error rate (1.38 × 10⁻³) by a factor of at least 2.7. While demonstrated on trapped ions, the methodology eliminates multi-round distillation entirely, producing magic states directly via code switching with **82.6% acceptance probability**.

**Defensible factory overhead fraction for cat qubit architectures:** Standard surface-code architectures dedicate 50–75% of physical qubits to magic state factories. Cat qubit architectures reduce this to an estimated **10–30%** (based on Chamberland and Gouzien architectural analyses), potentially falling to **<10%** with unfolded distillation.

---

## 2. IBM bivariate bicycle codes deliver a 14× efficiency gain over surface codes

**The [[144, 12, 12]] gross code** (Bravyi et al., Nature 627, 778, 2024) encodes **12 logical qubits** in 144 data qubits with code distance 12. Including syndrome extraction ancillas, the total is **288 physical qubits** — giving a physical-to-logical ratio of **24:1** in memory mode. For comparison, a distance-13 surface code requires ~338 physical qubits for a single logical qubit (ratio 338:1). The gross code is thus roughly **14× more efficient** in encoding rate. It features weight-6 stabilizers, a biplanar torus topology where each qubit connects to only 6 others, and a pseudo-threshold of **~0.7%** under circuit-level noise.

Other codes in the bivariate bicycle family include [[72, 8, 8]], [[288, 12, 18]] ("two-gross"), and [[360, 12, ≤24]], offering different distance-vs-overhead trade-offs.

**IBM's Starling processor** targets **200 logical qubits** capable of running **100 million quantum gates**, using approximately **~10,000 physical qubits** — a full-system ratio of **~50:1**. The architecture comprises ~17 gross code blocks (17 × 12 = 204 logical qubits), with ~4,900 qubits for code blocks and the remaining ~5,000 for logical processing units (LPUs), bridges, and magic state factories. The roadmap places Starling at 2029, preceded by Kookaburra (2026, first qLDPC memory), Cockatoo (2027, inter-module entanglement), and a 2028 magic state injection demo. Module fabrication yield is notable: **~40%** for gross code modules, **~20%** for two-gross modules.

**The Tour de Gross paper (arXiv:2506.03094, Yoder et al., 2025)** presents the complete "bicycle architecture" — a modular framework with an explicit fault-tolerant instruction set for both [[144,12,12]] and [[288,12,18]] codes. Its headline result: an **order of magnitude (10×) larger logical circuits** can be implemented with a given number of physical qubits compared to surface code architectures. The paper develops Pauli-Based Computation compilation, in-module and inter-module Pauli measurements, shift automorphisms for Clifford gates, and T-state injection via distillation with a Relay-BP decoder.

**Defensible range for 100 logical qubits with bivariate bicycle codes:**

|Metric|Code blocks only|Full system (incl. LPU, magic states)|
|---|---|---|
|Blocks needed|9 (encoding 108 logical qubits)|9–10|
|Data qubits|1,296|1,296|
|Data + syndrome ancillas|2,592|2,592|
|Total physical qubits|~2,600|**~5,000–6,000**|
|Physical:logical ratio|26:1|**~50–60:1**|

The lower bound of ~2,600 counts only code block qubits. The upper bound of ~6,000 extrapolates from Starling's ~50:1 full-system ratio. A manuscript should cite the **26:1 ratio for memory** and **~50:1 for computation** with appropriate caveats about LPU and magic state overhead. Note that IBM's Matthias Steffen has stated "several hundred physical qubits to create 10 logical qubits," consistent with the ~50:1 computation ratio.

---

## 3. Phononic shielding for bosonic modes is an identified literature gap

**No published work has specifically proposed or demonstrated phononic bandgap shielding for superconducting electromagnetic cavities or CPW resonators used for bosonic codes.** This represents a clear gap in the literature and a potentially novel contribution.

Phononic bandgap engineering for transmon qubits has shown dramatic results. Chen et al. (Science Advances 10, eado6240, 2024) demonstrated a **100× increase in TLS relaxation time** (to >5 ms) by embedding a transmon in a phononic crystal metamaterial on SOI. Odeh et al. (Nature Physics 21, 406–411, 2025) extracted phononic band edges at **5.05–5.4 GHz** (~350 MHz bandwidth) and observed non-Markovian qubit relaxation within the bandgap. The mechanism works by suppressing TLS → phonon decay: if TLS defects cannot emit phonons, they remain saturated and stop absorbing qubit energy.

**The fundamental challenge for 3D cavities** is geometric. Phononic bandgap metamaterials are fabricated by patterning periodic structures in thin-film substrates (typically SOI). For bulk 3D cavities (e.g., Nb SRF cavities achieving photon lifetimes of **~2 seconds**, Q ~ 2×10¹⁰ at 1.3 GHz per Romanenko et al. 2020), TLS reside in the thin niobium pentoxide (~5 nm, loss tangent ~0.1) on cavity walls, and phonons propagate in the bulk metal — not in a patternable substrate. Existing phononic crystal designs are not directly applicable.

**On-chip planar resonators are the promising target.** For stripline quantum memories (Yale/AWS, 2024: T₁ = **1.0–1.4 ms**, T₂* = **2.0–2.7 ms** on Ta/sapphire) and CPW resonators (best Qᵢ ~ 10⁶–10⁷), the TLS are at metal-substrate and substrate-air interfaces — the same interfaces where phononic bandgap engineering works for transmons. In principle, the same substrate phononic metamaterial could suppress TLS → phonon emission for on-chip resonators. A 2024 discovery of interface piezoelectricity at Al-Si interfaces (K² ≈ 3×10⁻⁵%) directly limits qubit Q to 10⁴–10⁸ depending on surface participation, and the discoverers explicitly recommend "phononic metamaterials" for mitigation.

**Key differences between protecting a transmon vs. a high-Q cavity:**

- **Surface participation ratio:** Transmons have p_MS ~ 10⁻³–10⁻⁴; 3D cavities have p_MS ~ 10⁻⁵–10⁻⁶ (TLS loss is diluted by volume). On-chip resonators fall in between.
- **Dominant loss channels:** Transmons are limited by TLS at junction interfaces and capacitor pads. 3D Nb cavities are limited by Nb₂O₅ TLS (mitigated by 340–450°C vacuum heat treatment). On-chip resonators are limited by surface/interface TLS — the channel most amenable to phononic engineering.
- **For cat qubits specifically:** Phase-flip time T_Z scales as 1/(2n̄κ₁), so cavity quality (κ₁) directly determines the dominant error rate. Improving cavity Q via phononic engineering would directly reduce phase-flip errors.

---

## 4. Cryo-CMOS latency fits within cat qubit timing budgets

**Syndrome extraction cycle times** for superconducting qubits are well-established at **~1.1 µs** (Google Willow, Nature 2024; IBM/ETH Zurich, 2022). The cycle comprises 4 two-qubit gates (~20–40 ns each), dispersive readout (~300–500 ns), and active reset (~50–300 ns). For cat qubits using repetition codes, the estimated cycle time is **~0.5–1.5 µs**, comparable to transmon surface codes since syndrome extraction still uses transmon ancilla readout.

**Cryo-CMOS decoder latencies demonstrated to date:**

- **Riverlane Collision Clustering ASIC** (Nature Electronics, 2025): **240 ns** for a 1,057-qubit surface code, 0.06 mm², 8 mW
- **Riverlane FPGA implementation:** **810 ns** for 881-qubit surface code
- **Pinball cryo-CMOS predecoder** (22nm FDSOI, 4K): **~90 ns** in high-performance mode (0.8V, 100 MHz) or **800 ns** in low-power mode (0.48V), consuming <0.56 mW; supports 37,313 logical qubits at d=3 under 1.5 W cryogenic power budget; achieves up to **3,781× bandwidth reduction** vs. sending all syndromes to room temperature
- **Neural network CIM decoder (simulated at 4K):** **197–252 ns** for distances 3–9

For comparison, room-temperature decoders have much higher latency: Google Willow's real-time decoder averages **63 µs** at distance-5 (pipelined over ~57 syndrome rounds). The cryo-CMOS advantage is **100–1,000× latency reduction** by eliminating the room-temperature roundtrip, though cable propagation delay itself is only ~15–20 ns.

**Timing budget for cat qubit error correction** given T_φ ~ 14–70 µs:

|T_φ|Cycle time|Available cycles|Assessment|
|---|---|---|---|
|14 µs|1 µs|~14|Marginal; requires very low physical error rate per cycle|
|20 µs|1 µs|~20|Workable for small codes (d = 3–5); demonstrated by AWS Ocelot|
|50 µs|1 µs|~50|Good margin for moderate codes|
|70 µs|1 µs|~70|Comfortable for distance 5–7+|

AWS Ocelot demonstrated a distance-5 repetition cat code with **1.65(3)% logical error per cycle**, confirming operation below threshold. Cat qubit bit-flip times now exceed **1 hour** (Alice & Bob's galvanic cat at 11 photons, 2025) and **~1 second** (AWS Ocelot at 4 photons), making bit-flips negligible.

**A critical architectural simplification:** Cat qubits use a **1D repetition code**, not a 2D surface code, because bit-flips are exponentially suppressed at the hardware level. This reduces decoding to a 1D chain problem — simple majority-vote or 1D MWPM — dramatically relaxing classical processing requirements. A cryo-CMOS predecoder at 90–800 ns is well within the ~1 µs cycle budget, and the 1D decoder is far simpler than full 2D surface code decoding.

---

## 5. LDPC-cat code thresholds and the path to 758 qubits for 100 logicals

**Ruiz et al., Nature Communications 16, 1040 (2025)** introduces LDPC-cat codes that exploit the classical BPT bound (kd = O(n), more favorable than the quantum bound kd² = O(n)) because only phase-flip correction is needed — bit-flips are handled by cat qubit hardware.

**Code parameters:** The best code family is **[165 + 8ℓ, 34 + 2ℓ, 22]**, achieving an overhead reduction factor of **kd/n = 5.5** over the repetition code. For 100 logical qubits (ℓ = 33), the code requires only **758 cat qubits** — a **44× reduction** over surface codes (33,700 qubits) and **2.8× reduction** over repetition cat codes (2,100 qubits) at the same ε_L ≤ 10⁻⁸ target.

**Threshold phase-flip error rates** follow from the logical error scaling formulas:

- **LDPC-cat code:** p_Z^L = 0.1 × (1613 × κ₁/κ₂)^{0.94⌊(d+1)/2⌋}, giving a threshold at **κ₁/κ₂ ≈ 1/1613 ≈ 6.2 × 10⁻⁴**
- **Repetition code:** p_Z^L = 0.07 × (486 × κ₁/κ₂)^{0.94⌊(d+1)/2⌋}, giving a threshold at **κ₁/κ₂ ≈ 1/486 ≈ 2.1 × 10⁻³**

The LDPC-cat threshold is ~3.4× lower than the repetition code threshold, attributed to deeper syndrome circuits (weight-4 stabilizers requiring 4 CNOT gates vs. weight-2 for the repetition code).

**The p_Z dependence on T_φ and cycle time** follows from the cat qubit noise model:

**p_Z ≈ 2n̄ × κ₁ × t_cycle = t_cycle / T_Z**

where T_Z = 1/(2n̄κ₁) is the phase-flip coherence time and n̄ = |α|² is the mean photon number. Equivalently, p_Z ≈ 2n̄ × (κ₁/κ₂) × κ₂ × t_cycle. Since κ₂ sets the gate speed (t_cycle ~ few/κ₂), the effective phase-flip error scales as **~n̄ × κ₁/κ₂**. At the paper's operating point (κ₁/κ₂ = 10⁻⁴, n̄ = 11 photons), the circuit-level error rates are: SPAM infidelity **1.1 × 10⁻³**, CNOT infidelity **1.6 × 10⁻²**, idling error **1.1 × 10⁻³** per cycle.

**Achieved logical error rates** at κ₁/κ₂ = 10⁻⁴, n̄ = 11: logical phase-flip error **6.4 × 10⁻¹⁰** per cycle per logical qubit, logical bit-flip error **1.8 × 10⁻⁹** per cycle. The required physical bit-flip time is **T_X ~ 13 minutes**, which Alice & Bob's galvanic cat (>1 hour) already exceeds. The current experimental κ₁/κ₂ = **6.5 × 10⁻³** needs a **~65× improvement** to reach the 10⁻⁴ target — a significant but plausible hardware challenge given the trajectory of cat qubit development.

---

## Conclusion: quantitative anchors for the manuscript

Three numbers anchor the resource story. For **cat qubit architectures**, 758 physical qubits suffice for 100 logical qubits at distance 22 using LDPC-cat codes — but this requires κ₁/κ₂ = 10⁻⁴, roughly 65× beyond current hardware. For **IBM's bivariate bicycle codes**, 100 logical qubits require ~2,600 physical qubits in memory mode (26:1) or ~5,000–6,000 including computational overhead (~50:1), with the Tour de Gross paper confirming a 10× advantage over surface codes for logical circuit capacity. The **magic state bottleneck** that dominates surface-code architectures (50–75% of qubits) shrinks to an estimated 10–30% for cat qubit architectures, with the 2025 unfolded distillation protocol (53 qubits per magic state) potentially pushing this below 10%.

On the experimental frontier, phononic bandgap engineering for on-chip bosonic-mode resonators — as opposed to transmons, where 100× TLS lifetime improvement has been demonstrated — is an identified literature gap with clear physical motivation. Cryo-CMOS predecoders operating at 90–800 ns latency at 4K fit comfortably within the ~1 µs cat qubit syndrome cycle, and the 1D repetition code structure of cat qubit error correction dramatically simplifies the decoding problem relative to 2D surface codes.