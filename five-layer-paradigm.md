# The Five-Layer Paradigm: A Convergent Architecture for Hardware-Efficient Fault-Tolerant Quantum Computing

**A proposal for integrating biased-noise bosonic qubits, classical LDPC outer codes, cryogenic CMOS control, and environmental shielding into a unified, physically viable quantum computing architecture**

---

## Abstract

Fault-tolerant quantum computing remains bottlenecked by the extraordinary overhead of quantum error correction: conventional approaches demand millions of physical qubits to produce a modest number of reliable logical qubits. In this article, we propose that five independently maturing research frontiers — biased-noise bosonic qubits, bosonic error-correcting codes, classical low-density parity-check (LDPC) outer codes, cryogenic CMOS integrated control electronics, and nano-engineered environmental shielding — can be unified into a single, physically viable architecture on a superconducting circuit platform. We argue that the key architectural insight is the _multiplicative_ nature of layered error suppression: each layer reduces the burden on the next, compounding overhead savings that cannot be achieved by any single technique in isolation. We survey the experimental milestones of 2024–2025 that make this integration timely, analyze the physical compatibility constraints between layers, and present a concrete modular design targeting 100 fault-tolerant logical qubits for Clifford operations. At projected hardware parameters (κ₁/κ₂ = 10⁻⁴, a ~65× improvement over current state of the art), this requires approximately 750–1,000 physical cat qubits; at near-term demonstrated parameters, the estimate rises to ~2,000–5,000 — in both regimes a substantial reduction relative to surface-code-only architectures. Magic-state distillation for non-Clifford gates will require additional overhead not quantified here. We include a sensitivity analysis showing how overhead scales with key hardware parameters, and conclude with an assessment of remaining challenges, particularly the ~200× gap between idle and operational noise bias, recent theoretical results suggesting fundamental limits on cat qubit coherence from chaos-assisted tunneling, and the need for improved phase-flip coherence times.

---

## 1. Introduction

The central challenge of quantum computing is not the construction of individual qubits — it is the protection of quantum information from environmental noise long enough to complete useful computations. Since Shor's pioneering work on quantum error correction (QEC) in 1995, the field has understood that fault-tolerant quantum computation is possible in principle, provided physical error rates fall below a threshold and sufficient redundancy is available. The practical question has always been: _how much redundancy?_

For the surface code — the most studied and experimentally advanced QEC code — the answer is sobering. Achieving logical error rates of 10⁻¹² for cryptographically relevant algorithms is estimated to require on the order of 10³–10⁴ physical qubits per logical qubit, depending on the physical error rate and code distance. When accounting for thousands of logical qubits plus magic-state distillation factories, the total physical qubit count for a cryptographically relevant machine reaches 10⁶–10⁷. Google's Willow processor demonstrated below-threshold surface code operation in late 2024, a landmark achievement, yet the path from that demonstration to a machine with thousands of logical qubits remains daunting. The fundamental issue is that the surface code treats all errors as equally likely, correcting both bit-flip (X) and phase-flip (Z) errors symmetrically, and encodes only a single logical qubit per code instance with an encoding rate that diminishes as distance increases.

This article proposes a different philosophy: rather than relying on a single, monolithic error correction code to handle arbitrary noise, we advocate for a _layered_ architecture in which multiple error suppression mechanisms operate at different levels of the system stack, each tailored to eliminate a specific class of errors. The key principle is that **each layer should reshape the residual noise into a form that the subsequent layer can correct most efficiently**, creating a multiplicative cascade of overhead savings.

We identify five layers that are independently mature and physically compatible on a superconducting circuit platform:

1. **Biased-noise bosonic qubits** (Layer 1): Physical qubits engineered so that one error type (bit-flips) is exponentially suppressed relative to the other (phase-flips).
2. **Bosonic error-correcting encoding** (Layer 2): Quantum information encoded in the continuous-variable Hilbert space of a microwave resonator, providing hardware-level partial error protection.
3. **Classical LDPC outer code** (Layer 3): An efficient error correction code that exploits the noise bias to correct only the dominant remaining error channel, achieving high encoding rates with local connectivity.
4. **Cryogenic CMOS integrated control** (Layer 4): Classical control electronics operating at millikelvin temperatures inside the cryostat, eliminating the wiring bottleneck that limits scalability.
5. **Nano-engineered environmental shielding** (Layer 5): Phononic bandgap metamaterials and radiation mitigation structures that suppress the dominant decoherence mechanisms at their physical source.

We argue that this five-layer architecture is not merely a theoretical aspiration but is grounded in experimental demonstrations from 2024–2025, and that its integration on a single superconducting platform faces primarily engineering barriers — though recent theoretical work on chaos-assisted tunneling in driven nonlinear oscillators [38] suggests that some limits on cat qubit coherence may be more fundamental than previously appreciated.

---

## 2. Layer 1: Biased-Noise Bosonic Qubits

### 2.1 The Principle of Noise Bias

Standard superconducting qubits — transmons, flux qubits, fluxonium — experience bit-flip and phase-flip errors at comparable rates. This symmetry between error channels is, from an error correction perspective, maximally inconvenient: the QEC code must simultaneously protect against both X and Z errors, which requires two-dimensional redundancy (the surface code's d × d patch, for instance).

Biased-noise qubits break this symmetry by engineering the physical system such that one error type is exponentially suppressed. The dissipatively stabilized cat qubit, pioneered by Mirrahimi, Leghtas, and colleagues, achieves this by confining the quantum state of a microwave resonator to a manifold spanned by two coherent states |+α⟩ and |−α⟩ through a two-photon driven-dissipative process. In this encoding, a bit-flip requires a transition between the two coherent states — a process whose rate decreases exponentially with the mean photon number |α|², as it requires traversing a phase-space barrier. Phase-flip errors, by contrast, arise from single-photon loss events and occur at a rate proportional to |α|², increasing linearly with the confinement strength.

The result is an asymmetric noise channel. Alice & Bob demonstrated bit-flip lifetimes exceeding ten seconds in their published Nature work [2], with the Boson 4 chip extending this to approximately seven minutes [21]. In September 2025, Alice & Bob reported preliminary (not yet peer-reviewed) results on a Galvanic Cat design — the same architecture used on the Helium 2 multi-qubit chip — suggesting bit-flip times approaching one hour (33–60 minutes, 95% CI), with individual qubits on the Helium 2 chip achieving 189–252 minutes, though the impact of Z-gate drive on bit-flip incidence was not measured in this preliminary assessment [22]. During idle operation, noise bias ratios for dissipative cat qubits approach 10⁸ at high mean photon numbers. AWS's Ocelot chip independently demonstrated five cat qubits on a tantalum-on-silicon platform with exponential bit-flip suppression [3]. In a separate single-qubit experiment, AWS demonstrated that phase coherence and exponential bit-flip suppression can be simultaneously preserved, achieving an effective T₁ ≈ 70 µs limited by oscillator coherence [23]; the multi-qubit Ocelot chip achieves lower phase-flip times of ~14–17 µs under two-photon dissipation at |α|² = 4 [3]. Hann et al. subsequently proposed a hybrid cat-transmon architecture providing the theoretical scaling framework for the Ocelot approach [39].

Critically, the Kerr-cat qubit variant studied by Siddiqi's group at UC Berkeley revealed that _operational_ bias during gates — the metric relevant for error correction — is significantly lower than idle bias, measuring approximately 250 under dihedral randomized benchmarking [4]. The companion work by Hajr et al. demonstrated a high-coherence Kerr-cat qubit in a 2D architecture with bit-flip times exceeding 1 ms (peaking at ~950 µs at |α|² = 8) [24]; the idle bias ratio inferred from T₁/T_φ measurements in such systems is in the range of 10³–10⁴ for Kerr-cat qubits, and substantially higher (>10⁵) for dissipative cat qubits at comparable mean photon numbers [2, 21]. This represents a discrepancy of one to two orders of magnitude with the operational bias of ~250 measured during gates. This gap arises because gate operations introduce additional error channels (leakage, heating, non-bias-preserving rotations) that are absent during idle evolution. Recent work has identified specific physical mechanisms: Adinolfi et al. measured leakage populations exceeding 9% during parametric driving [40], while Martínez et al. demonstrated that chaos-assisted tunneling can mediate transitions between cat wells even when static Hamiltonian analyses predict exponential suppression [38]. Benhayoune-Khadraoui et al. showed that multimode resonances in realistic circuits sharply degrade coherence above critical drive amplitudes, though careful electromagnetic environment engineering can partially recover performance [41]. The distinction between idle and operational bias is essential for realistic architecture design, and the error correction overhead estimates in this paper are computed using the more conservative operational bias values.

### 2.2 Consequences for Error Correction

The error correction payoff of biased noise is transformative. With noise bias ratios above approximately 100, tailored QEC codes achieve error thresholds exceeding 5%, compared to roughly 1% for standard depolarizing noise. More fundamentally, because only one error type requires active correction, the dimensionality of the error correction problem collapses from two to one: instead of a 2D surface code, a 1D repetition code suffices for the dominant error channel.

This dimensional reduction propagates through the entire architecture, affecting connectivity requirements, decoder complexity, and ultimately qubit count. It is the foundation upon which the subsequent layers build.

---

## 3. Layer 2: Bosonic Error-Correcting Encoding

### 3.1 Encoding in Continuous-Variable Hilbert Space

The cat qubit encoding described above is itself an instance of a bosonic code — quantum information is stored in the infinite-dimensional Hilbert space of a harmonic oscillator rather than in a two-level system. This is significant because the redundancy needed for error detection is built into the physics of the resonator mode itself, rather than requiring additional physical qubits.

Several bosonic codes have been experimentally demonstrated beyond the break-even point, where the encoded logical qubit outlives any unencoded physical qubit in the same device: binomial codes (Yale, 2019), cat codes (multiple groups, 2023–2025), and Gottesman-Kitaev-Preskill (GKP) codes (Yale/Google, 2025). Each bosonic code family offers different trade-offs.

Cat codes, as described above, provide exponential bit-flip suppression at the cost of linear phase-flip degradation. GKP codes encode a qubit in a periodic grid structure in phase space, offering protection against small displacement errors in both quadratures and providing analog syndrome information — continuous-valued measurement outcomes that carry soft reliability data exploitable by outer decoders. The University of Sydney demonstrated the first universal quantum gate set for GKP-encoded logical qubits in a trapped-ion system in 2025, while Xanadu generated GKP states on an integrated silicon nitride photonic chip at room temperature.

### 3.2 The Inner Code as a Noise-Shaping Layer

The conceptual role of the bosonic inner code within our architecture is _noise shaping_: transforming the raw, continuous noise of the physical system into a structured, discrete error channel that the outer code can handle optimally. For cat qubits, this means converting the analog noise of photon loss into a pure phase-flip (Z-error) channel. For GKP qubits, it means converting small displacement errors into correctable qubit-level Pauli errors plus soft analog residual information.

This noise-shaping function is what distinguishes the five-layer architecture from simple code concatenation. The inner bosonic code does not merely reduce the error rate — it _restructures_ the noise, enabling the outer code to be vastly simpler than it would otherwise need to be.

### 3.3 Why Cat Codes Over GKP?

The choice of cat codes over GKP codes as the inner bosonic layer deserves explicit justification, as GKP codes offer complementary advantages. GKP codes correct small displacement errors in _both_ quadratures symmetrically and provide analog syndrome information (soft data) that can significantly boost outer decoder performance [7, 8, 36]. The surface-GKP concatenation has been shown to substantially reduce the overhead of standard surface codes [33], and recent demonstrations of GKP qudit error correction beyond break-even [37] underscore the maturity of this alternative.

Cat codes are preferred in this architecture for three reasons. First, the extreme noise bias (exponential bit-flip suppression) enables the outer code to be _classical_ rather than quantum, eliminating the need for CSS or stabilizer code structure and the associated overhead in syndrome extraction and non-local connectivity. GKP codes, with symmetric error correction, still require a quantum outer code. Second, cat codes permit purely 2D local connectivity for the outer LDPC code, whereas GKP-qLDPC architectures typically require non-local connections. Third, the dissipative stabilization mechanism for cat codes has been demonstrated on multi-qubit chips (AWS Ocelot, Alice & Bob Boson 4), whereas multi-qubit GKP systems remain less mature on superconducting platforms, though dissipative GKP protection in high-impedance superconducting circuits has recently been demonstrated [42]. The cost of this choice is the loss of analog soft information and the need to manage the phase-flip penalty associated with increasing confinement strength.

It is worth noting an alternative philosophy: Xu et al. have explored bosonic codes that achieve fault tolerance _without_ concatenation with an outer code, using the bosonic Hilbert space itself to provide sufficient protection [43]. This "direct bosonic coding" approach could in principle eliminate the outer code layer entirely, though it currently requires non-local connectivity and remains theoretically less mature than the concatenated approach adopted here.

---

## 4. Layer 3: Classical LDPC Outer Code

### 4.1 From Quantum to Classical Error Correction

The most consequential advantage of the biased-noise bosonic inner code is that it enables the outer error correction layer to be a _classical_ code rather than a quantum one. Because bit-flip errors are exponentially suppressed by the cat qubit physics, the outer code needs to correct only phase-flip (Z) errors — a task equivalent to classical binary error correction. This eliminates the need for full CSS or stabilizer codes with their associated overhead in syndrome extraction, non-local connectivity, and decoder complexity.

Alice & Bob's LDPC-cat code, published in Nature Communications in January 2025, is the most complete realization of this principle. The architecture uses a classical LDPC code tailored for the biased noise channel, with the following properties:

- **Weight-4 stabilizers**: Each stabilizer check involves only four cat qubits, identical to the locality constraints of the surface code.
- **2D local connectivity**: All required interactions are between nearest neighbors on a planar grid, requiring no long-range couplers, reconfigurable atom arrays, or flip-chip routing tricks.
- **High encoding rate**: The LDPC code achieves approximately 5× the encoding rate of the repetition code that would otherwise be used for phase-flip correction.
- **Cellular-automaton decoder**: Syndrome decoding is performed by a local cellular automaton that operates in linear time, avoiding the computational bottleneck of minimum-weight perfect matching or union-find decoders.

The headline result: **100 logical qubits encoded in 758 physical cat qubits**, with logical error rates below 10⁻⁸ per cycle, assuming a physical phase-flip error probability of 0.1% per gate cycle.

It is important to note the hardware assumptions underlying this result. The LDPC-cat code simulations assume a single-photon loss ratio κ₁/κ₂ = 10⁻⁴, where κ₁ is the single-photon loss rate and κ₂ is the two-photon dissipation rate. The best experimentally demonstrated value is κ₁/κ₂ ≈ 6.5 × 10⁻³ (Réglade et al., 2024), roughly 65× worse than the assumption. Similarly, the target logical error rate of 10⁻⁸ requires bit-flip times of approximately 13 minutes in multi-qubit systems under active gate operations — a regime demonstrated only for single isolated qubits (the Galvanic Cat result), not in multi-qubit processors. Table 2 in Section 7.2 distinguishes demonstrated from projected parameters, and Section 9.5 provides a sensitivity analysis showing how the physical qubit count varies with these hardware parameters.

### 4.2 Comparison with Quantum LDPC Codes

It is instructive to compare the LDPC-cat approach with the full quantum LDPC codes pursued by IBM and others. IBM's bivariate bicycle (BB) code encodes 12 logical qubits in 144 data qubits at distance 12, achieving approximately 10× overhead reduction relative to the surface code. This is a remarkable theoretical achievement, but it comes with substantial hardware demands: degree-6 connectivity (each qubit coupled to six others), weight-6 stabilizers, thickness-2 Tanner graph routing requiring flip-chip fabrication, and computationally intensive decoders.

The LDPC-cat architecture achieves comparable or superior overhead reduction while requiring _simpler_ hardware: lower-weight stabilizers, purely local 2D connectivity, and a classical rather than quantum decoder. The price paid is the need for cat qubit hardware — superconducting cavities with two-photon dissipation — rather than standard transmons. Given that AWS has already fabricated a five-cat-qubit chip with below-threshold concatenated error correction, this trade-off appears increasingly favorable.

An important intermediate option is offered by bias-tailored quantum LDPC codes, which exploit noise asymmetry while retaining full quantum code structure. The Romanesco codes of Leroux and Iverson [35] are Clifford-deformed bivariate bicycle codes on bipartite hexagonal lattices that, in the large bias limit, reduce to two independent classical cellular automaton codes — the same code family used in the LDPC-cat construction [1]. This connection suggests that the classical LDPC outer code adopted here may be understood as the infinite-bias limit of a more general family of bias-tailored quantum codes, with the Romanesco codes providing a natural interpolation for intermediate bias regimes. Roffe et al. [44] independently developed bias-tailored quantum LDPC codes via lifted products, and Xu et al. [45] demonstrated tailored XZZX codes achieving high thresholds under biased noise. These works collectively indicate that even if the extreme bias regime (>10⁴) proves difficult to maintain during operations, efficient codes exist for moderate bias levels (~10²–10³).

### 4.3 The Connectivity Advantage

The connectivity simplification deserves special emphasis. Non-local connectivity is arguably the most challenging engineering requirement in scaling quantum processors. IBM's qLDPC roadmap requires L-coupler inter-chip links spanning up to one meter; Harvard/QuEra's qLDPC proposals rely on reconfigurable atom arrays; Quantinuum's trapped-ion implementation used all-to-all connectivity inherent to their platform. Each of these represents a major engineering effort.

The LDPC-cat architecture sidesteps this entirely. By collapsing the error correction problem to a single error type, it enables codes with the same connectivity as the surface code — the most hardware-friendly topology known — while achieving the overhead savings previously thought to require complex non-local architectures.

---

## 5. Layer 4: Cryogenic CMOS Integrated Control

### 5.1 The Wiring Bottleneck

Every qubit in a superconducting quantum processor currently requires multiple coaxial cables running from room-temperature electronics at ~300 K through successive thermal stages to the mixing chamber at ~10 mK. For a 1,000-qubit processor, this implies thousands of cables — each carrying heat, introducing noise, and consuming physical space in the cryostat. Scaling to millions of qubits under this paradigm is widely recognized as impractical.

Cryogenic CMOS control electronics address this by moving signal generation and multiplexing _inside_ the cryostat, replacing N analog cables with approximately log(N) digital lines. The approach has matured rapidly.

### 5.2 Millikelvin-Compatible Electronics

Intel's Pando Tree chip, presented at the IEEE VLSI Symposium in 2024, operates at 10–20 mK as a frequency demultiplexer serving up to 64 qubit terminals. Working in tandem with the Horse Ridge II chip at 4 K for signal generation, the system requires only ~log(N) input signals to control N qubits. IMEC demonstrated a cryo-CMOS multiplexer operating below 15 mK alongside superconducting qubits with single-qubit gate fidelities above 99.9%. IBM demonstrated full two-qubit cross-resonance gate control from a 14nm FinFET cryo-CMOS ASIC at 4 K, dissipating approximately 23 mW per qubit. TU Delft demonstrated an integrated cryo-CMOS system (DAC, demultiplexer, crossbar array) at 66 mK directly adjacent to quantum devices. Most recently, Bartee et al. demonstrated a ~100,000-transistor FDSOI CMOS chip heterogeneously integrated with spin qubits at millikelvin temperatures, performing universal logic operations including two-qubit entangling gates — the most complete cryo-CMOS integration to date [46].

### 5.3 Synergy with the Five-Layer Architecture

Cryo-CMOS integration is _more_ viable in the five-layer architecture than in conventional approaches, for a simple reason: the system is smaller. A machine requiring ~758 cat qubits needs far fewer control signals than one requiring millions of transmons. The thermal budget at the mixing chamber stage — typically 10–20 µW at 10 mK — is the binding constraint. With fewer qubits to control, each qubit can be allocated a larger share of the thermal budget, relaxing the power efficiency requirements on the cryo-CMOS circuits.

Furthermore, the classical nature of the LDPC decoder is compatible with cryo-CMOS implementation. Rather than shuttling syndrome data to room-temperature classical processors for decoding, a cryo-CMOS decoder operating at 4 K can perform the cellular-automaton LDPC decoding with microsecond-scale latency — well within the phase-flip coherence window. This closes the feedback loop entirely within the cryostat, eliminating a major latency bottleneck.

The distributed thermal architecture — heavy signal processing at 4 K where cooling power is abundant (watts), ultra-low-power demultiplexing at millikelvin where cooling power is scarce (microwatts) — maps naturally onto the five-layer system's modest qubit count and simple decoder requirements.

### 5.4 Alternative Classical Control Paradigms

While this article focuses on cryo-CMOS as the most mature classical control technology, several alternative paradigms merit consideration and may prove complementary or superior for specific functions within the five-layer architecture [25]. Single-flux-quantum (SFQ) logic operates natively at millikelvin temperatures with ultra-low power dissipation (~1 nW per gate) and can potentially scale to over 42,000 qubits in SIMD configurations [26]. Shen et al. demonstrated the first photonic link from SFQ circuits to room temperature [27], and Arnold et al. achieved all-optical superconducting qubit readout at millikelvin temperatures requiring no active or passive cryogenic microwave equipment [28]. A wireless terahertz cryogenic interconnect that minimizes heat-to-information transfer has also been demonstrated [29]. The five-layer architecture's modest qubit count relaxes the scalability demands on any of these approaches, and a hybrid strategy — cryo-CMOS for signal generation at 4 K, SFQ for ultra-low-power switching at millikelvin — may prove optimal.

---

## 6. Layer 5: Nano-Engineered Environmental Shielding

### 6.1 Phononic Bandgap Metamaterials

Decoherence in superconducting qubits arises substantially from interactions with two-level system (TLS) defects at material interfaces — amorphous oxide layers, substrate surfaces, and junction barriers. These TLS defects couple to qubits both directly (via electric fields) and indirectly (via phonon emission and absorption). Phononic bandgap metamaterials — periodically patterned structures that forbid phonon propagation at specific frequencies — can suppress the phonon-mediated decoherence channel.

Chen and Painter demonstrated acoustic bandgap structures in superconducting circuits that increased TLS defect relaxation times by two orders of magnitude, with strongly coupled TLS defects achieving T₁ values exceeding 5 milliseconds (compared to ~3 µs for the transmon qubit itself). This dramatic extension of TLS lifetimes indicates effective suppression of the phonon-mediated decoherence channel, though translating TLS lifetime improvements into proportional qubit coherence gains remains an active area of research. Odeh and Sipahigil at UC Berkeley placed a superconducting qubit on a phononic bandgap metamaterial fabricated on silicon-on-insulator, observing non-Markovian relaxation dynamics — a direct signature of modified phonon density of states — and TLS lifetime extension to 34 µs inside the bandgap.

For the five-layer architecture, phononic shielding directly attacks the dominant remaining error source: single-photon loss in the cat qubit cavity, which drives phase-flip errors. By suppressing TLS-mediated photon loss, phononic structures improve the phase-flip coherence time T_φ, which in turn reduces the physical phase-flip error rate per correction cycle. Recent work by Zhou et al. has identified interface piezoelectricity as a significant dissipation channel limiting qubit quality factors to ~10⁴–10⁸ [47], providing additional physical motivation for phononic engineering approaches. Because the outer LDPC code's overhead scales with this error rate, even modest improvements compound into significant reductions in total qubit count.

### 6.2 Cosmic Ray and Radiation Mitigation

High-energy particles — cosmic ray muons, environmental gamma rays, and secondary particles — impact superconducting quantum chips every few seconds, depositing energy that generates phonon bursts and quasiparticle cascades affecting qubits across the entire chip. Google demonstrated in 2022 that these events produce correlated error bursts that surface codes cannot correct, setting a fundamental floor on achievable logical error rates [14].

Multiple mitigation strategies are now under active development:

- **Underground operation**: Fermilab's QUIET laboratory, located 100 meters underground, provides a 99% reduction in cosmic ray muon flux and is currently commissioning superconducting qubit experiments. Italy's Gran Sasso laboratory demonstrated a 30× reduction in quasiparticle burst rates for underground superconducting resonators.
- **On-chip phonon containment**: Normal-metal phonon downconversion layers, first demonstrated by the Syracuse/UW-Madison collaboration, achieved a 100× reduction in correlated quasiparticle poisoning. Gap-engineered quasiparticle traps near Josephson junctions push charge-parity switching rates below 1 Hz.
- **Tantalum substrates**: Tantalum-film qubits recover from quasiparticle bursts approximately 100× faster than aluminum, as demonstrated by Li et al. in a direct comparison study.

### 6.3 Compatibility and Compounding

Phononic bandgap structures are inherently planar and fabricated using standard lithographic processes on the qubit chip substrate. They are compatible with both 2D coplanar waveguide resonators and 3D cavity architectures (where the phononic metamaterial sits on the coupling chip). No fundamental fabrication conflict exists between phononic shielding and the other four layers.

The compounding effect is significant. Consider a scenario in which phononic shielding reduces the TLS-mediated photon loss rate by a factor of 5 (conservative relative to the 100× TLS lifetime improvement demonstrated by Chen and Painter). This reduces the phase-flip error rate by a corresponding factor, which in turn allows the outer LDPC code to achieve the same logical error rate at a lower distance — potentially reducing the physical qubit count by 30–50%. When combined with radiation mitigation that eliminates correlated error floors, the five-layer system approaches the fundamental limits of what superconducting hardware can achieve.

---

## 7. The Integrated Architecture

### 7.1 Module Design

We propose a modular architecture in which each module consists of:

- **8–16 cat qubit data cells**: Each cell comprises a high-Q microwave resonator (coplanar or 3D) hosting a dissipatively stabilized cat state, coupled to a transmon ancilla for syndrome measurement. The resonator substrate incorporates phononic bandgap patterning for TLS suppression.
- **Local cryo-CMOS demultiplexer**: An ultra-low-power CMOS chip at ~15 mK receiving multiplexed signals from a 4 K signal generation stage, providing individual qubit control via frequency-division multiplexing. Power dissipation target: <20 µW per qubit.
- **On-chip radiation hardening**: Tantalum films for fast quasiparticle recovery, normal-metal phonon downconversion layers, and gap-engineered quasiparticle traps at Josephson junctions.
- **Inter-module couplers**: Nearest-neighbor microwave links to adjacent modules, providing the 2D local connectivity required by the LDPC outer code.

### 7.2 System-Level Parameters

Based on demonstrated or near-term experimental parameters:

|Parameter|Value|Source|Status|
|---|---|---|---|
|Bit-flip time (idle, single qubit)|~7 min (Boson 4); ~33–60 min (Galvanic Cat, preliminary)|Alice & Bob (2024, 2025)|Demonstrated (single qubit)|
|Phase-flip time|~490 ns (Alice & Bob); ~14–17 µs (AWS Ocelot, multi-qubit); ~70 µs (AWS, single qubit)|Réglade et al. (2024); Putterman et al. (2025) [3, 23]|Demonstrated|
|Phase-flip time target (with phononic shielding)|~50 µs|Projected|Target|
|Noise bias (idle)|10³–10⁴ (Kerr-cat); >10⁵ (dissipative cat)|Hajr et al. (2024); Réglade et al. (2024)|Demonstrated|
|Noise bias (operational, during gates)|~250|Siddiqi group benchmarking (2024)|Demonstrated|
|κ₁/κ₂ (current best)|6.5 × 10⁻³|Réglade et al. (2024)|Demonstrated|
|κ₁/κ₂ (required for 758-qubit target)|10⁻⁴|LDPC-cat proposal (2025)|Target (65× improvement needed)|
|LDPC code rate|~13%|LDPC-cat proposal (2025)|Simulated|
|Stabilizer weight|4|LDPC-cat proposal (2025)|Simulated|
|Physical qubits per logical qubit|~7.6 (at target κ₁/κ₂)|758 cat qubits / 100 logical qubits|Projected|
|Logical error rate target|≤10⁻⁸ per cycle|LDPC-cat proposal (2025)|Target|
|Cryo-CMOS power per qubit (mK)|~18.5 µW|CEA/Quobly (ISSCC 2025)|Demonstrated|
|Phononic TLS defect lifetime enhancement|~100×|Chen & Painter (2024)|Demonstrated (TLS, not qubit)|

### 7.3 Comparison with Alternative Architectures

|Architecture|Physical qubits for 100 logical qubits|Connectivity|Decoder|Platform maturity|Demonstrated logical qubits|
|---|---|---|---|---|---|
|Surface code (transmon)|~100,000–1,000,000|2D local|MWPM/UF|High|Memory only (Google Willow)|
|Yoked surface code (transmon) [30]|~30,000–300,000|2D local|MWPM/UF|High|N/A (simulated)|
|qLDPC bivariate bicycle (transmon) [17]|~10,000–20,000|Degree-6, non-local|BP/OSD|Medium|N/A (simulated)|
|Neutral-atom qLDPC [31]|~448 demonstrated (96 logical)|Reconfigurable, non-local|Various|Medium-High|96 (Bluvstein et al., 2025)|
|Trapped-ion qLDPC [32]|~600–2,500 (estimated)|All-to-all|BP/OSD|Medium-High|48 error-corrected; 94 GHZ (Helios, 2025)|
|LDPC-cat (five-layer)|~758 (at target κ₁/κ₂)|2D local|Classical CA|Low-Medium|0 (simulated)|
|GKP + surface code (cavity) [33]|~2,000–10,000|2D local|Soft MWPM|Low–Medium|1 (break-even)|
|GKP + qLDPC (cavity)|~500–5,000|Non-local|Soft BP|Low–Medium|0 (simulated)|

The five-layer architecture achieves among the lowest projected physical qubit counts while requiring the simplest connectivity and decoder — a combination that no other architecture matches. However, several important caveats apply. The 758-qubit figure assumes hardware parameters (κ₁/κ₂ = 10⁻⁴) not yet demonstrated at scale, and the "demonstrated logical qubits" column makes the maturity gap explicit: neutral-atom platforms have already achieved 96 logical qubits [31], and Quantinuum's Helios processor has demonstrated 48 error-corrected logical qubits at a 2:1 physical-to-logical encoding ratio and 94 logical qubits fully entangled in a GHZ state [32], while the LDPC-cat code has not yet been experimentally realized. Yoked surface codes [30] substantially narrow the overhead gap while remaining on the most mature transmon platform. The trade-off is the requirement for bosonic cat qubit hardware, which is less mature than transmon technology but advancing rapidly.

---

## 8. Experimental Validation Milestones

The five-layer architecture is grounded in experimental demonstrations that have already validated its core components:

**Layer 1–2 (biased-noise bosonic encoding):** AWS's Ocelot chip (Nature, February 2025) demonstrated the first below-threshold concatenated bosonic error correction, with five cat data qubits and four transmon ancillas implementing a distance-5 repetition code. Logical phase-flip error rates decreased from distance 3 to distance 5, confirming that code scaling works with biased-noise bosonic qubits. Separately, in a single-qubit experiment, AWS demonstrated simultaneous phase coherence preservation and exponential bit-flip suppression, achieving T₁,eff ≈ 70 µs [23]; the multi-qubit Ocelot chip achieves ~14–17 µs [3]. Alice & Bob's Boson 4 chip demonstrated bit-flip lifetimes exceeding seven minutes, and preliminary (not yet peer-reviewed) results on a Galvanic Cat design reported bit-flip times approaching one hour in September 2025 [22], though the impact of gate drives on this figure remains to be characterized.

**Layer 3 (classical LDPC outer code):** The LDPC-cat code was published in Nature Communications in January 2025 with detailed numerical simulations. While not yet experimentally demonstrated, Alice & Bob's five-milestone roadmap targets the first error-corrected logical qubit below threshold (Helium series) in 2025–2026, using approximately 16 physical cat qubits.

**Layer 4 (cryo-CMOS control):** IBM demonstrated cryo-CMOS-controlled two-qubit gates (PRX Quantum, 2024). IMEC demonstrated millikelvin cryo-CMOS multiplexing alongside superconducting qubits with >99.9% fidelity (Nature Electronics, 2023). Intel's Pando Tree operates at 10–20 mK with 64-terminal demultiplexing (IEEE VLSI, 2024). The Fraunhofer QSolid project is developing interposer technology for direct co-integration.

**Layer 5 (environmental shielding):** Phononic bandgap TLS suppression demonstrated by Chen/Painter (Science Advances, 2024) and Odeh/Sipahigil (Nature Physics, 2025). Cosmic ray correlated error characterization by Google (2021, 2025) and RIKEN (2025). Underground operation demonstrated at Gran Sasso (Nature Communications, 2021) and Fermilab QUIET (commissioning 2024–2025). On-chip phonon downconversion by Syracuse/UW-Madison (Nature Communications, 2022).

No group has yet demonstrated all five layers simultaneously on a single device. The nearest integration is AWS's Ocelot, which combines Layers 1–2 with a simple repetition outer code. The critical path to full integration involves combining the bosonic inner code with the full LDPC outer code (Alice & Bob's Helium series) and subsequently adding cryo-CMOS control and phononic shielding — both of which have been demonstrated separately with superconducting qubits on compatible fabrication platforms.

---

## 9. Challenges and Limitations

### Part A: Demonstrated Limitations

### 9.1 Phase-Flip Coherence

The dominant limitation of the five-layer architecture is the short absolute phase-flip time of current cat qubits. Alice & Bob's best single-qubit devices achieve approximately 490 ns (Réglade et al., 2024), while AWS's Ocelot achieves T₁,eff ≈ 14–17 µs at |α|² = 4 under two-photon dissipation [3, 23]. The outer LDPC code must complete a full syndrome extraction cycle within a fraction of the phase-flip time. Even at the Ocelot's more favorable coherence, this demands gate speeds of ~100–500 ns and limits the achievable code distance. Improving phase-flip times by an additional order of magnitude — to ~50 µs or beyond — would qualitatively change the scalability landscape, and this is precisely where Layer 5 (phononic shielding) and materials advances (e.g., tantalum-on-silicon substrates achieving T₁ > 1 ms for transmons [34]) could contribute most.

### 9.2 The Idle-vs-Operational Bias Gap

The discrepancy between idle noise bias (10³–10⁵, depending on the cat qubit variant and operating point [2, 24]) and operational noise bias during gates (~250, as measured by Qing et al. [4]) represents the most critical challenge for the architecture. The LDPC-cat code's overhead calculations depend sensitively on which bias value applies during a full error correction cycle. At high idle bias levels, the bit-flip channel is effectively negligible and the classical LDPC outer code is nearly optimal. At an operational bias of ~250, residual bit-flip errors during gates are non-negligible and must be accounted for, potentially requiring additional bit-flip protection or limiting the maximum mean photon number |α|² to manage the phase-flip penalty.

Recent experimental and theoretical work has clarified the physical origins of this gap. Three mechanisms have been identified:

1. **Gate-induced leakage.** Adinolfi et al. measured leakage populations exceeding 9% — twelve times higher than in the undriven system — during parametric driving of a Kerr-cat qubit, with bit-flip times ultimately saturating for reasons not yet fully understood [40]. Controlled single-photon dissipation partially mitigates leakage but does not eliminate the problem.

2. **Chaos-assisted tunneling.** Martínez, García-Mata, and Wisniacki demonstrated that chaotic dynamics in the driven nonlinear oscillator can mediate tunneling between cat wells even when static effective Hamiltonian analyses predict exponential suppression [38]. This represents a potentially _fundamental_ rather than purely engineering limit on Kerr-cat coherence during parametric driving, and its implications for dissipative cat qubits (which use a different stabilization mechanism) remain to be fully explored.

3. **Multimode resonances.** Benhayoune-Khadraoui, Lledó, and Blais showed that buffer modes and higher junction array modes in realistic circuits induce multiphoton resonances that sharply degrade coherence above critical drive amplitudes [41]. Careful electromagnetic environment engineering can partially recover performance, suggesting that this mechanism is addressable through improved circuit design.

Beyond gate design improvements — particularly bias-preserving CNOT implementations [5] — the path forward may involve hybrid stabilization approaches combining Kerr confinement with engineered two-photon dissipation, as employed in Alice & Bob's Galvanic Cat design. The overhead estimates presented in Section 7.2 use the more conservative operational bias values, but we note that the LDPC-cat simulations of Ruiz et al. [1] assume a noise model in which bit-flip errors are fully exponentially suppressed, effectively corresponding to the idle bias regime. A circuit-level simulation incorporating realistic operational bias remains an important open task.

To make the sensitivity to operational bias explicit:

|Operational bias|Bit-flip contribution to logical error|Estimated overhead multiplier|
|---|---|---|
|~250 (current, Kerr-cat)|Non-negligible; additional bit-flip protection needed|~2–3×|
|~500|Marginal; single repetition code layer may suffice|~1.3–1.5×|
|~1,000|Subdominant; classical LDPC outer code nearly optimal|~1×|
|>10,000 (idle regime)|Negligible; architecture operates as designed|1× (baseline)|

These multipliers are approximate and depend on code parameters not fully explored here. The key observation is that achieving operational bias ≥1,000 — a factor of 4 improvement over current Kerr-cat demonstrations — would largely close the gap between the idealized and realistic overhead estimates.

### 9.3 Absence of Topological Protection

The five-layer architecture deliberately omits true topological protection — degenerate ground states separated by a spectral gap in a many-body system. Theoretical work by Magdalena de la Fuente et al. (2024) has constructed families of topological stabilizer codes on continuous-variable bosonic degrees of freedom, but these models have gapless spectra and no experimental realization. The cat qubit's bit-flip suppression is symmetry-protected (by the Z₂ parity symmetry of the driven Hamiltonian) rather than topologically protected, and can be broken by symmetry-breaking perturbations such as single-photon loss.

For practical purposes, the exponential suppression provided by the cat qubit — scaling as e^{−2|α|²} — is sufficient for all near-term and medium-term applications. At |α|² = 11 (Alice & Bob's demonstrated operating point), bit-flip rates are suppressed by factors exceeding 10⁸. Whether this suffices for the most demanding long-term applications (e.g., factoring 2048-bit RSA keys, requiring ~10¹⁵ logical operations) remains an open question that depends on progress in improving phase-flip times.

### Part B: Integration Risks

### 9.4 Frequency Management at Scale

A system with hundreds of bosonic modes — each requiring a distinct resonant frequency — plus transmon ancillas and cryo-CMOS output tones, faces a challenging frequency allocation problem. Superconducting cavity modes typically span 4–10 GHz, transmon ancillas occupy 4–6 GHz, and cryo-CMOS output ranges extend to ~17 GHz. Spurious coupling, frequency collisions, and phase noise from cryo-CMOS local oscillators become non-trivial systems engineering challenges. IBM's observation of drive-induced Z-rotations at spurious-tone levels of −35 to −40 dBc suggests that extremely clean signal generation is necessary.

Mitigations include frequency-division multiplexing with on-chip bandpass filtering, modular architectures that isolate frequency environments between modules, and the use of broadband parametric drives (as in the dissipative cat stabilization scheme) that are inherently less sensitive to frequency crowding than resonant gate protocols.

### 9.5 Sensitivity to Hardware Parameters

The headline figure of 758 physical cat qubits for 100 logical qubits depends on hardware parameters not yet demonstrated at scale. To make the dependence explicit, we present an approximate sensitivity analysis based on the scaling relations in Ruiz et al. [1]:

|Scenario|κ₁/κ₂|Operational bias|Bit-flip time (multi-qubit)|Est. physical qubits for 100 logical qubits|
|---|---|---|---|---|
|Current hardware|6.5 × 10⁻³|~250|~seconds (inferred)|>5,000 (extrapolated)|
|Near-term target|10⁻³|~500|~1–2 min|~2,000–3,000|
|Medium-term target|10⁻⁴|~1,000|~10 min|~758 (Ruiz et al.)|
|With phononic shielding|10⁻⁴ (+ 5× T_φ improvement)|~1,000|~10 min|~400–500|

These estimates are approximate and depend on additional factors (decoder performance, measurement error rates, ancilla quality) not varied here. The key takeaway is that at current hardware parameters, the five-layer architecture still offers substantial overhead reduction relative to the surface code, but the full two-orders-of-magnitude advantage requires ~65× improvement in κ₁/κ₂.

### 9.6 No Full-Stack Experimental Demonstration

The most honest limitation is that no group has operated all five layers on a single device. Each pairwise combination has been demonstrated or shown fabrication-compatible, but integration challenges — thermal management, electromagnetic compatibility, fabrication process conflicts — may emerge at the full-stack level. The history of quantum computing is replete with examples of components that work individually but interact deleteriously when combined.

### 9.7 Magic State Distillation

The 758-qubit estimate covers quantum memory and Clifford operations but does not account for the overhead of non-Clifford gates (T gates), which are required for universal quantum computation. Magic state distillation or injection protocols will require additional physical qubits beyond those counted in the LDPC-cat code's encoding. In conventional surface code architectures, magic state factories can account for a substantial fraction of the total qubit budget. For the five-layer architecture, the interaction between biased noise and magic state preparation has not been fully analyzed. Daguerre et al. recently demonstrated logical magic states via code switching on Quantinuum's trapped-ion platform, achieving infidelity ≤5.1 × 10⁻⁴ [48] — whether analogous techniques can be adapted to the biased-noise cat qubit setting, and at what overhead cost, remains an open and important question. The qubit counts reported in this paper should therefore be understood as lower bounds on the total resources required for universal fault-tolerant computation.

---

## 10. Toward Experimental Realization

We propose the following phased integration roadmap:

**Phase 1 (2025–2027): LDPC-cat below threshold.** Demonstrate an LDPC outer code over cat qubits with logical error rates decreasing as code distance increases. This validates the combination of Layers 1–3. Alice & Bob's Helium series targets this milestone with ~16 cat qubits.

**Phase 2 (2027–2028): Cryo-CMOS integration.** Add millikelvin cryo-CMOS demultiplexing to an LDPC-cat processor, demonstrating that gate fidelities are preserved under cryo-CMOS control. This integrates Layer 4. The Fraunhofer QSolid interposer technology and CEA/Quobly's low-power readout IC (18.5 µW/qubit) are candidate technologies.

**Phase 3 (2028–2029): Environmental shielding.** Fabricate cat qubit cavities on phononically engineered substrates with on-chip radiation hardening (tantalum films, quasiparticle traps). Measure the improvement in phase-flip times and the reduction in correlated error rates. This integrates Layer 5.

**Phase 4 (2029–2030): Full five-layer system.** Combine all layers in a modular architecture with nearest-neighbor inter-module microwave links. Target: 100 logical qubits with logical error rates ≤10⁻⁸, using ~750–1,000 physical cat qubits.

This timeline is ambitious but consistent with Alice & Bob's published roadmap (100 logical qubits by 2030) and the demonstrated pace of progress in cryo-CMOS and phononic engineering.

---

## 11. Broader Implications

### 11.1 Reducing the Physical Qubit Burden

The five-layer architecture illustrates a broader trend in quantum computing: the recognition that fault tolerance overhead can be reduced not only by improving physical error rates but by engineering the _structure_ of residual noise to match the strengths of available codes. A machine with ~750 cat qubits and 100 logical qubits (at projected parameters; ~2,000–5,000 at near-term parameters) is a qualitatively different engineering challenge from a surface-code machine requiring hundreds of thousands of transmons for the same logical capacity. The former fits in a single dilution refrigerator; the latter approaches a data-center-scale cryogenic infrastructure that does not yet exist. However, the overhead reduction comes at the cost of requiring less mature hardware (cat qubits rather than transmons) and relying on noise properties not yet validated at scale during operations.

### 11.2 Accessibility and Timeline

By reducing the qubit count by up to two orders of magnitude (under projected hardware parameters; see Section 9.5 for sensitivity analysis), the five-layer approach potentially compresses the timeline to useful fault-tolerant quantum computing. A ~750-qubit superconducting chip is within the fabrication capabilities demonstrated by AWS (Ocelot), Alice & Bob (Boson 4), and IBM (Eagle/Heron). The bottleneck shifts from manufacturing scale to component quality — particularly phase-flip coherence and κ₁/κ₂ ratio — and systems integration.

### 11.3 Implications for the Quantum LDPC Code Community

The five-layer architecture highlights an important question about the relationship between hardware-level noise engineering and code-level overhead reduction. If biased-noise bosonic qubits can reduce the error correction problem to a classical one on superconducting platforms, the optimal code choice becomes platform-dependent in a way that current theoretical treatments do not always emphasize. The answer, as recent experiments demonstrate, is nuanced and increasingly informed by hardware demonstrations.

Full quantum LDPC codes have now been implemented on real hardware: Quantinuum demonstrated a [[25, 4, 3]] qLDPC code on trapped ions encoding 4 logical qubits with fidelity exceeding break-even [32], and Bluvstein et al. demonstrated 96 logical qubits on a 448-atom neutral-atom array [31] — nearly matching the five-layer architecture's 100-logical-qubit target, albeit at higher physical qubit count. These demonstrations establish that qLDPC codes are not merely theoretical constructs but practical tools on platforms with reconfigurable or all-to-all connectivity.

The five-layer approach does not render qLDPC codes obsolete — it redirects their application. For platforms without engineered noise bias (transmons, trapped ions, neutral atoms), full quantum LDPC codes remain essential. The mathematical insights from qLDPC theory also inform the design of classical LDPC codes for biased channels, as demonstrated by the Romanesco codes of Leroux and Iverson [35], which are bias-tailored qLDPC codes built from fractal codes that generalize the cellular automaton codes used in this architecture. The question is not whether qLDPC codes are valuable — they clearly are — but whether the five-layer approach offers a more hardware-efficient path specifically for superconducting platforms.

---

## 12. Conclusion

We have presented the case for a five-layer quantum computing architecture that integrates biased-noise bosonic qubits, bosonic error-correcting encoding, classical LDPC outer codes, cryogenic CMOS control, and nano-engineered environmental shielding into a unified, physically viable system. The architecture's central insight — that multiplicative, layered error suppression can reduce fault-tolerance overhead by up to two orders of magnitude under projected hardware parameters — is supported by experimental demonstrations across all five layers, though no full-stack integration has yet been achieved, and the full overhead reduction requires ~65× improvement in the single-photon loss ratio κ₁/κ₂ relative to current state of the art.

The most important conceptual contribution of this approach is the recognition that the _structure_ of residual noise, not merely its magnitude, determines the efficiency of error correction. By engineering the physical system to produce maximally biased noise, the five-layer architecture converts the quantum error correction problem into a classical one, unlocking overhead savings that no amount of improvement to conventional surface codes can match. However, the gap between idle and operational noise bias during gates (Section 9.2) — with current operational bias of ~250 compared to idle values orders of magnitude higher — represents a significant challenge that must be addressed through improved bias-preserving gate designs and a deeper understanding of the fundamental limits imposed by driven nonlinear dynamics.

The remaining challenges are real: phase-flip coherence must improve, the idle-to-operational bias gap must narrow (with recent theoretical work on chaos-assisted tunneling [38] and multimode resonances [41] clarifying both the difficulty and potential paths forward), frequency management at scale requires careful engineering, magic state distillation overhead for non-Clifford gates remains unquantified, and true topological protection remains unavailable for bosonic systems. But the majority of these are engineering challenges with identifiable paths forward. The experimental milestones of 2024–2025 — AWS's below-threshold concatenated bosonic code (with T₁,eff ≈ 70 µs in a single-qubit experiment [23] and ~14–17 µs on the multi-qubit Ocelot chip [3]), Alice & Bob's minute-scale bit-flip times (with preliminary results on a Galvanic Cat design approaching one hour), IMEC's millikelvin cryo-CMOS integration, and Berkeley's phononic bandgap qubit — collectively demonstrate that each layer of the architecture works. What remains is to build them together, while acknowledging that competing approaches — particularly neutral-atom qLDPC systems (with 96 demonstrated logical qubits [31]), trapped-ion platforms (with 48 error-corrected logical qubits on Quantinuum's Helios [32]), and GKP-based concatenation — are advancing on parallel timelines and may reach comparable milestones through different means.

---

## References

1. Ruiz, D., Guillaud, J., Leverrier, A., Mirrahimi, M. & Vuillot, C. LDPC-cat codes for low-overhead quantum computing in 2D. _Nature Communications_ **16**, 1040 (2025).
2. Réglade, U. et al. Quantum control of a cat qubit with bit-flip times exceeding ten seconds. _Nature_ **629**, 778–783 (2024).
3. Putterman, H. et al. (AWS). Hardware-efficient quantum error correction via concatenated bosonic qubits. _Nature_ **638**, 927–934 (2025).
4. Qing, H. et al. Quantum benchmarking of high-fidelity noise-biased operations on a detuned-Kerr-cat qubit. _PNAS_ **123**, e2520479123 (2026). [arXiv:2411.04442 (2024)].
5. Puri, S. et al. Bias-preserving gates with stabilized cat qubits. _Science Advances_ **6**, eaay5901 (2020).
6. Raveendran, N. et al. Finite rate QLDPC-GKP coding scheme that surpasses the CSS Hamming bound. _Quantum_ **6**, 767 (2022).
7. Berent, L., Burkhard, L. & Locher, D. F. Analog information decoding of bosonic quantum LDPC codes. _PRX Quantum_ **5**, 020349 (2024).
8. Borah, S. et al. Fault tolerant decoding of QLDPC-GKP codes with circuit-level soft information. arXiv:2505.06385 (2025).
9. van Dijk, J. P. G. et al. Multiplexed superconducting qubit control at millikelvin temperatures with a low-power cryo-CMOS multiplexer. _Nature Electronics_ **6**, 670–677 (2023).
10. Pando Tree. Intel Labs. IEEE VLSI Symposium (2024).
11. Bardin, J. C. et al. Using cryogenic CMOS control electronics to enable a two-qubit cross-resonance gate. _PRX Quantum_ **5**, 010326 (2024).
12. Chen, Y. & Painter, O. Phonon engineering of atomic-scale defects in superconducting quantum circuits. _Science Advances_ **10**, eado6240 (2024).
13. Odeh, M. & Sipahigil, A. Non-Markovian dynamics of a superconducting qubit in a phononic bandgap. _Nature Physics_ **21**, 406–411 (2025).
14. McEwen, M. et al. Resolving catastrophic error bursts from cosmic rays in large arrays of superconducting qubits. _Nature Physics_ **18**, 107–111 (2022).
15. Li, R. et al. Synchronous detection of cosmic rays and correlated errors in superconducting qubit arrays. _Nature Communications_ **16**, 4823 (2025).
16. Xu, Q. et al. Constant-overhead fault-tolerant quantum computation with reconfigurable atom arrays. arXiv:2308.08648 (2023).
17. IBM Quantum. Tour de Gross: A modular quantum computer based on bivariate bicycle codes. arXiv:2506.03094 (2025).
18. Magdalena de la Fuente, J. C. et al. Topological stabilizer models on continuous variables. arXiv:2411.04993 (2024).
19. Walshe, B. et al. Linear-optical quantum computation with arbitrary error-correcting codes. arXiv:2408.04126 (2024).
20. Huang, H. et al. High-performance quantum interconnect between bosonic modules beyond transmission loss constraints. arXiv:2512.24926 (2025).
21. Alice & Bob. Boson 4 chip: bit-flip lifetimes exceeding seven minutes. Company technical disclosure (2024).
22. Alice & Bob. Preliminary results vastly surpassing previous bit-flip time record. Company press release, September 25, 2025. https://alice-bob.com/newsroom/alice-bob-surpasses-bit-flip-stability-record/
23. Putterman, H. et al. Preserving phase coherence and linearity in cat qubits with exponential bit-flip suppression. _Phys. Rev. X_ **15**, 011070 (2025). [arXiv:2409.17556].
24. Hajr, A. et al. High-coherence Kerr-cat qubit in 2D architecture. _Phys. Rev. X_ **14**, 041049 (2024). [arXiv:2404.16697].
25. Reilly, D. J. et al. Classical interfaces for controlling cryogenic quantum computing technologies. _APL Quantum_ **2**, 041501 (2024).
26. McDermott, R. et al. DigiQ: A scalable digital controller for quantum computers using SFQ logic. _IEEE Trans. Appl. Supercond._ **32**, 1300205 (2022).
27. Shen, H. et al. Photonic link from single-flux-quantum circuits to room temperature. _Nature Photonics_ **18**, 371–378 (2024).
28. Arnold, G. et al. All-optical superconducting qubit readout. _Nature Physics_ **21** (2025).
29. Nature Electronics. A wireless terahertz cryogenic interconnect that minimizes heat-to-information transfer. _Nature Electronics_ (2025).
30. Gidney, C. et al. Yoked surface codes. _Nature Communications_ (2025).
31. Bluvstein, D. et al. A fault-tolerant neutral-atom architecture for universal quantum computation. _Nature_ **649**, 39–46 (2025).
32. Quantinuum. Experimental implementation of non-local qLDPC codes. arXiv:2406.02666 (2024); Helios processor: 98 barium ion qubits, 48 error-corrected logical qubits (2025). arXiv:2511.05465.
33. Noh, K. & Chamberland, C. Low-overhead fault-tolerant quantum error correction with the surface-GKP code. _PRX Quantum_ **3**, 010315 (2022).
34. Bland, S. et al. Millisecond lifetimes and coherence times in 2D transmon qubits. _Nature_ **647**, 343–348 (2025).
35. Leroux, C. & Iverson, J. K. Romanesco codes: Bias-tailored qLDPC codes from fractal codes. arXiv:2506.00130 (2025).
36. Walshe, B. et al. Advances in bosonic quantum error correction with Gottesman-Kitaev-Preskill codes. _Progress in Quantum Electronics_ **96**, 100518 (2024). [arXiv:2308.02913].
37. Brock, S. et al. Quantum error correction of qudits beyond break-even. _Nature_ **641**, 612–618 (2025).
38. Martínez, M., García-Mata, I. & Wisniacki, D. A. Fundamental limits to cat-code qubits from chaos-assisted tunneling. arXiv:2510.15175 (2025).
39. Hann, C. T. et al. Hybrid cat-transmon architecture for scalable, hardware-efficient quantum error correction. _PRX Quantum_ (2025). [arXiv:2410.23363].
40. Adinolfi, R., Haxell, D. Z. et al. Enhancing Kerr-cat qubit coherence with controlled dissipation. arXiv:2511.01027 (2025).
41. Benhayoune-Khadraoui, A., Lledó, C. & Blais, A. How the Kerr-cat qubit dies — and how to rescue it. arXiv:2507.06160 (2025).
42. Sellem, L. et al. Dissipative protection of a GKP qubit in a high-impedance superconducting circuit driven by a microwave frequency comb. _Phys. Rev. X_ **15**, 011011 (2025).
43. Xu, Q. et al. Letting the tiger out of its cage: Bosonic coding without concatenation. _Phys. Rev. X_ **15**, 041025 (2025).
44. Roffe, J. et al. Bias-tailored quantum LDPC codes. _Quantum_ **7**, 1005 (2023).
45. Xu, Q. et al. Tailored XZZX codes for biased noise. _Phys. Rev. Research_ **5**, 013035 (2023).
46. Bartee, M. et al. Spin-qubit control with a milli-kelvin CMOS chip. _Nature_ **643**, 382–387 (2025).
47. Zhou, X. et al. Observation of interface piezoelectricity in superconducting devices on silicon. _Nature Communications_ (2026).
48. Daguerre, L. et al. Logical magic state preparation with non-stabilizer codes on a trapped-ion quantum computer. Quantinuum technical report (2025).

---

_Author: David Hoze. The author declares no competing interests and no affiliation with any company whose hardware is discussed in this paper. Correspondence should be addressed to the author._