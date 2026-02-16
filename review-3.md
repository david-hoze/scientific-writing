# Peer review verification of the Five-Layer Paradigm manuscript

**The manuscript's headline claim of ~758 physical cat qubits for 100 fault-tolerant logical qubits is traceable to verified published work (Ruiz et al., Nature Communications 2025), but rests on assumptions—particularly κ₁/κ₂ = 10⁻⁴ during operations—that remain experimentally unvalidated.** Most cited experimental results check out, though several require attribution corrections and important caveats. The competitive landscape has shifted substantially in late 2025, and several critical recent works appear to be missing from the manuscript.

---

## 1. Alice & Bob claims are verified but carry underappreciated caveats

**Boson 4 bit-flip lifetime of 7 minutes: Confirmed.** Alice & Bob reported bit-flip times exceeding **430 seconds** on the Boson 4 chip, launched on Google Cloud Marketplace on May 15, 2024. The underlying physics builds on Boson 3 results published in Nature 629, 778–783 (2024), titled "Quantum control of a cat-qubit with bit-flip times exceeding ten seconds." No standalone Boson 4 peer-reviewed paper has been published yet—the preprint was listed as "coming soon" on Alice & Bob's product page. The peer reviewer should flag this: the 7-minute figure currently relies on company-reported data rather than peer-reviewed publication.

**Galvanic Cat 33–60 minutes: Confirmed as preliminary.** The September 25, 2025 press release reports a **95% confidence interval of 33–60 minutes** at mean photon number 11, based on 61 consecutive one-hour quantum jump traces. However, three critical caveats deserve emphasis in any manuscript citing this result. First, the impact of Z gate drive on bit-flip incidence was not measured. Second, the 2030 target of 13-minute bit-flip time must hold **during two-qubit CNOT gates**, which is far more demanding than idle conditions—this has not been demonstrated. Third, the blog post also revealed three qubits on the Helium 2 multi-qubit chip achieved 189, 252, and 189 minutes respectively, which is a stronger result that the manuscript may want to cite instead.

**LDPC-cat code paper (Ruiz et al.): Fully verified.** Published in Nature Communications 16, 1040 (2025), DOI: 10.1038/s41467-025-56298-8, arXiv: 2401.09541. The **758 physical cat qubits** figure corresponds to a [429, 100, 22] code requiring 429 data qubits + 329 ancilla qubits. The paper assumes physical phase-flip error probability ε ≈ 0.1%, yielding total logical error probability ε_L = 2.5 × 10⁻⁹ per cycle per logical qubit. This corresponds to κ₁/κ₂ = 10⁻⁴ with CNOT gate infidelity of **1.6 × 10⁻²** and SPAM infidelities of **1.1 × 10⁻³**. A competing-interests note exists: multiple authors are affiliated with Alice & Bob and hold financial interests.

**The κ₁/κ₂ = 10⁻⁴ target is aspirational, not achieved.** Current best experimental ratios for dissipative cat qubits are approximately κ₁/κ₂ ≈ 10⁻² (κ₁/2π ≈ 1.7 kHz, κ₂/2π ≈ 170 kHz). The Galvanic Cat results suggest the idle bit-flip performance may be sufficient, but no experiment has demonstrated this ratio is maintained during gate operations. The manuscript should clearly distinguish between idle and operational regimes when citing this target.

---

## 2. AWS Ocelot claims need attribution corrections

**Putterman et al. Nature paper: Verified with nuances.** Published as "Hardware-efficient quantum error correction via concatenated bosonic qubits" in Nature 638, 927–934 (2025), DOI: 10.1038/s41586-025-08642-7. The chip uses **five cat data qubits** plus four ancilla transmons in a distance-5 repetition code—confirmed. Below-threshold operation was demonstrated: logical phase-flip error decreased from d = 3 to d = 5, with total logical error rates of 1.75(2)% per cycle for d = 3 and 1.65(3)% per cycle for d = 5.

**The noise bias ratio ~20,000:1 requires clarification.** This figure appears to originate from AWS re:Invent conference presentations rather than the Nature paper itself. The actual Ocelot chip achieves bit-flip times approaching one second with phase-flip times of ~20 µs, yielding a ratio closer to **~50,000:1** at certain operating points. The single-qubit PRX paper reports T₁,eff ≈ 70 µs with bit-flip times >0.1 s, giving ~1,400:1. The ratio is exponentially tunable with photon number, so citing a single number without specifying the operating point is imprecise.

**T₁,eff ≈ 70 µs is from the PRX paper, not the Nature paper.** This is an important attribution error the manuscript should correct. The figure comes from "Preserving Phase Coherence and Linearity in Cat Qubits with Exponential Bit-Flip Suppression," published as Phys. Rev. X 15, 011070 (2025), arXiv: 2409.17556. The Ocelot Nature paper reports phase-flip times of ~20 µs on the multi-qubit chip—reflecting different device generations or operating conditions. The companion architecture paper by Hann et al., "Hybrid cat-transmon architecture for scalable, hardware-efficient quantum error correction" (PRX Quantum 2025, arXiv: 2410.23363), is also essential to cite as it provides the theoretical framework for scaling Ocelot.

---

## 3. UC Berkeley Kerr-cat claims: one verified, one questionable

**Qing et al. operational bias ~250: Confirmed.** Published in PNAS (January 2026), DOI: 10.1073/pnas.2520479123, arXiv: 2411.04442, titled "Quantum Benchmarking of High-Fidelity Noise-Biased Operations on a Detuned-Kerr-Cat Qubit." Key results include Z(π/2) gate fidelity of **99.18 ± 0.066%**, X(π/2) gate fidelity of **92.5 ± 0.23%**, and bit-flip time of 1.2 ms (3× improvement over resonant Kerr-cat). The paper's central finding is that conventional estimates systematically overstate the noise bias—the measured operational bias approaches ~250, dramatically lower than idle-inferred values.

**Hajr et al. idle bias >50,000: Not verified from the paper.** Published as Physical Review X 14, 041049 (2024), arXiv: 2404.16697, titled "High-Coherence Kerr-Cat Qubit in 2D Architecture." The paper reports bit-flip times >1 ms (peaking at ~950 µs at α² = 8) and demonstrates a scalable 2D superconducting circuit with QND readout fidelity of 99.6%. However, the paper does **not explicitly claim an idle bias >50,000**. The reported bit-flip and phase-flip times yield ratios in the hundreds to low thousands range for the Kerr-cat system. The >50,000 figure may be conflated with dissipative cat qubit results (e.g., Alice & Bob's Boson 3/4 achieving bit-flip times >10 seconds) or with the AWS PRX paper's results. The manuscript should verify and correct this attribution.

---

## 4. Cryo-CMOS demonstrations all verified, plus a key missing result

**Intel Pando Tree: Confirmed.** Presented at IEEE VLSI 2024 (June 16–20, Hawaii) by Sushil Subramanian and Stefano Pellerano. Fabricated in Intel 22nm low-power FinFET, operating at **10–20 mK** alongside Tunnel Falls spin qubits. Supports up to **64 qubit terminals** using logarithmic wiring scaling (1 signal line + 9 digital control signals). Intel claims it was the first manufacturer to demonstrate distribution of cryo control electronics at different temperature stages inside a dilution refrigerator.

**IMEC multiplexer: Confirmed.** Published as "Multiplexed superconducting qubit control at millikelvin temperatures with a low-power cryo-CMOS multiplexer" in Nature Electronics 6, 900–909 (2023), DOI: 10.1038/s41928-023-01033-8. Key specs: operates below **15 mK**, 28nm bulk CMOS, static power **0.6 µW**, single-qubit gate fidelities >99.9%.

**IBM cryo-CMOS: Confirmed with clarification.** Published as "Using Cryogenic CMOS Control Electronics to Enable a Two-Qubit Cross-Resonance Gate" in PRX Quantum 5, 010326 (2024), DOI: 10.1103/PRXQuantum.5.010326. However, the IBM ASIC operates at **4 K** (not millikelvin)—the qubits are at the mixing chamber but the control electronics are at the 4 K stage. Power dissipation is ~23 mW per qubit. Single-qubit error: 8 × 10⁻⁴; two-qubit error: 1.4 × 10⁻².

**Missing result: Bartee et al. (Nature 2025).** "Spin-qubit control with a milli-kelvin CMOS chip," Nature 643, 382–387 (2025), from Microsoft/University of Sydney/UNSW. This demonstrates ~100,000-transistor FDSOI CMOS heterogeneously integrated with spin qubits at millikelvin temperatures, performing universal logic operations including **two-qubit entangling gates**—arguably the most significant recent cryo-CMOS milestone. The manuscript should cite this.

---

## 5. Phononic bandgap results verified; emerging evidence strengthens the case

**Chen & Painter: Confirmed.** "Phonon engineering of atomic-scale defects in superconducting quantum circuits," Science Advances 10, eado6240 (2024), DOI: 10.1126/sciadv.ado6240. Demonstrated **100× increase in TLS relaxation time** within the acoustic bandgap, with longest TLS T₁ exceeding **5 ms**. Measured 55 TLSs across two chips. Authors include Putterman and Painter from the AWS/Caltech group.

**Odeh & Sipahigil: Confirmed.** "Non-Markovian dynamics of a superconducting qubit in a phononic bandgap," Nature Physics 21, 406–411 (2025), DOI: 10.1038/s41567-024-02740-5. From UC Berkeley. Observed Purcell-engineered TLS lifetime of **34 µs** and non-Markovian qubit relaxation dynamics within the bandgap. Accompanied by a Nature Physics News & Views piece.

**Missing: Zhou et al. (Nature Communications 2026).** "Observation of interface piezoelectricity in superconducting devices on silicon" (Sipahigil group) identifies interface piezoelectricity as a major dissipation channel limiting qubit Q to ~10⁴–10⁸, which directly motivates phononic engineering approaches. The manuscript should cite this as it strengthens the rationale for the environmental shielding layer.

---

## 6. Cosmic ray mitigation claims verified; significant new data available

**Google correlated errors: Confirmed but misdated.** McEwen et al. was published online December 13, 2021 in Nature Physics 18, 107–111 (2022), DOI: 10.1038/s41567-021-01432-8. The manuscript citing "2021/2025" should use the 2022 publication date. No Google-authored 2025 follow-up was found, but two important 2025 papers from other groups exist: Harrington et al. (MIT Lincoln Lab, Nature Communications 2025) measured cosmic-ray-induced correlated errors at a rate of **1/(592 s)** accounting for 17.1% of all correlated events; and Li et al. (Nature Communications 2025) achieved the first direct observation of muon-specific quasiparticle bursts, finding QP recombination ~100× faster in tantalum than aluminum.

**Fermilab QUIET: Confirmed and operational.** Opened May 30, 2024, located ~100 meters underground (225 meters water equivalent). Superconducting qubits were first deployed in October 2024. The facility achieves **99.5% muon flux reduction** and includes identical above-ground equipment (LOUD facility) for direct comparison. No peer-reviewed publications from QUIET data yet.

**Gran Sasso: Confirmed with important nuance.** The foundational paper is Cardani et al., Nature Communications 12 (2021), showing 30× quasiparticle burst rate reduction underground. However, a more recent direct comparison (De Dominicis et al., arXiv: 2405.18355) found average qubit T₁ ≈ 80 µs at **both** above-ground and underground locations, concluding that intrinsic noise remains dominant in current transmon qubits. Radiation effects become important only as other noise sources are suppressed—a finding with implications for the manuscript's environmental shielding layer.

---

## 7. The competitive landscape has evolved dramatically

**Harvard/QuEra neutral atoms have advanced substantially.** The November 2025 Nature paper (Bluvstein, Geim et al., Nature 649, 39–46, DOI: 10.1038/s41586-025-09848-5) demonstrated **448 neutral atoms** with 2.14× below-threshold surface code performance, lattice surgery, transversal teleportation with 3D [[15,1,3]] codes, and up to **96 simultaneous logical qubits** using high-rate [[16,6,4]] codes. Algorithmic Fault Tolerance (Zhou et al., arXiv: 2406.17653) claims 10–100× overhead reduction. These results represent the most comprehensive fault-tolerant demonstration to date and directly challenge the five-layer paradigm's resource efficiency claims.

**IBM bivariate bicycle codes remain simulation-only but show strong scaling.** The [[144,12,12]] gross code (Nature 627, 2024) achieves **24 physical qubits per logical qubit** at distance 12. The "Tour de Gross" architecture paper (arXiv: 2506.03094, June 2025) demonstrates an order of magnitude larger logical circuits per physical qubit count versus surface codes. IBM Quantum Loon (November 2025) experimentally demonstrated c-coupler connectivity needed for qLDPC codes. The 2029 IBM Starling target is **200 logical qubits** with 100 million gates.

**Quantinuum has achieved remarkable milestones with Helios.** The November 2025 Helios processor (98 physical qubits, ¹³⁷Ba⁺ ions) demonstrated two-qubit gate fidelity of **99.921%**, **48 error-corrected logical qubits** at 2:1 encoding ratio, and **94 logical qubits** fully entangled in a GHZ state. Earlier, Hong et al. demonstrated a [[25,4,3]] qLDPC code on the H2 processor. Daguerre et al. (July 2025) achieved logical magic state infidelity ≤5.1 × 10⁻⁴ via code switching—a key milestone for universal fault tolerance.

**Google Willow: Confirmed.** Published in Nature 638, 920–926 (2025), demonstrating distance-7 surface code with logical error rate **0.143% per cycle** and error suppression factor Λ = 2.14 per code distance increment. No post-selection used. However, Willow demonstrated memory only—no logical gates. Google estimates distance-27 (~1,457 physical qubits) for 10⁻⁶ logical error rate, which is ~14.6 physical qubits per logical qubit at that target—still far more than the cat qubit claim.

The manuscript should note that the **comparison is not apples-to-apples**: the 758-qubit claim assumes κ₁/κ₂ = 10⁻⁴ (not yet demonstrated operationally), while competing results are experimental. At equivalent demonstrated error rates, the resource advantages narrow considerably.

---

## 8. GKP codes and theoretical works reveal important gaps in the manuscript

**The Sydney GKP universal gate set is by Matsos et al., not Tsunoda et al.** Published as Nature Physics 21, 1664–1669 (2025), DOI: 10.1038/s41567-025-03002-8. First universal logical gate set for GKP qubits, demonstrated in a trapped ¹⁷¹Yb⁺ ion at room temperature. Single-qubit process fidelity up to **0.960**; two-qubit CZ gate fidelity **0.680**. This represents a competing bosonic code approach with fundamentally different error correction characteristics—GKP corrects both shift error types symmetrically, unlike cat codes' asymmetric bias.

**Brock et al. GKP qudit: Confirmed.** "Quantum error correction of qudits beyond break-even," Nature 641, 612–618 (2025). First QEC for qudits (d > 2) beyond break-even, with GKP qutrit lifetime exceeding best physical qutrit by factor **1.82 ± 0.03** and ququart by **1.87 ± 0.03**. Uses the same Yale 3D cavity platform as the 2023 Sivak et al. GKP qubit result.

**Romanesco codes (Leroux & Iverson, arXiv: 2506.00130): Verified and directly relevant.** These are Clifford-deformed bivariate bicycle codes on a bipartite hexagonal lattice, explicitly designed for biased noise from inner bosonic codes. In the large bias limit, they reduce to two independent classical cellular automaton codes—the same code family used in Ruiz et al.'s LDPC-cat construction. They achieve distance scaling better than possible with 2D topological quantum codes. This paper is **essential to cite** as it provides a natural quantum LDPC alternative to the classical LDPC outer code in the five-layer paradigm.

**Other missing theoretical works the manuscript should consider:**

- **Roffe et al. (Quantum 7, 1005, 2023)**: Bias-tailored quantum LDPC codes using lifted products, directly relevant to the outer code layer
- **Xu et al. (Phys. Rev. Research 5, 013035, 2023)**: Tailored XZZX codes for biased noise with remarkably high thresholds
- **Berent et al. (PRX Quantum 5, 020349, 2024)**: Analog information decoding of bosonic qLDPC codes, introducing "quasi-single-shot" protocols
- **Tamiya et al. (Nature Physics, 2025)**: Polylogarithmic time overhead fault tolerance combining qLDPC and concatenated Steane codes
- **Xu et al. (Phys. Rev. X 15, 041025, 2025)**: "Letting the tiger out of its cage: Bosonic coding without concatenation"—explores direct bosonic codes achieving fault tolerance without outer code layers, a potential alternative to the five-layer approach
- **Sellem et al. (Phys. Rev. X 15, 011011, 2025)**: Dissipative GKP protection in high-impedance superconducting circuits
- **qLDPC-GKP concatenation (arXiv: 2505.06385, May 2025)**: Circuit-level noise model for concatenated qLDPC-GKP with analog decoding

---

## 9. The idle-to-operational bias gap is a critical vulnerability

The manuscript's architecture fundamentally depends on maintaining extreme noise bias during gate operations, yet the experimental evidence reveals a **two-orders-of-magnitude gap** between idle and operational regimes. Qing et al.'s PNAS 2026 paper is the most rigorous characterization to date, measuring operational bias of ~250 against idle bit-flip times of 1.2 ms. This gap arises from three identified mechanisms.

**Leakage dominates bit-flip errors during operations.** Adinolfi, Haxell et al. (arXiv: 2511.01027, November 2025, with Grimm) measured leakage population >9%—twelve times higher than in the undriven system. They demonstrated controlled single-photon dissipation can partially mitigate this, but the bit-flip time ultimately saturated for reasons that remain unclear. Bhandari et al. (PRX Quantum 6, 030338, 2025) proposed symmetrically threaded SQUIDs as next-generation Kerr-cat qubits to suppress low-order multiphoton dissipation.

**Chaos-assisted tunneling imposes a fundamental bound.** Martínez, García-Mata, and Wisniacki (arXiv: 2510.15175, October 2025) demonstrated that chaotic dynamics mediate tunneling between cat wells even when static effective Hamiltonian analyses predict exponential suppression. This represents a **fundamental** rather than engineering limit on Kerr-cat coherence.

**Multimode resonances degrade performance in realistic circuits.** Benhayoune-Khadraoui, Lledó, and Blais (arXiv: 2507.06160, July 2025, Sherbrooke) showed that buffer modes and higher junction array modes induce multiphoton resonances that sharply degrade coherence above critical drive amplitudes. The paper is notably titled "How the Kerr-Cat Qubit Dies—And How to Rescue It," demonstrating that careful electromagnetic environment engineering can recover performance.

**The path to κ₁/κ₂ = 10⁻⁴ during operations is plausible but unproven.** Alice & Bob's Galvanic Cat demonstrates sufficient idle bit-flip suppression (33–60 minutes exceeds the 13-minute target by 4×), and the hybrid Hamiltonian-dissipative stabilization approach (combining Kerr confinement with engineered two-photon dissipation) shows theoretical promise. However, no experiment has demonstrated κ₁/κ₂ = 10⁻⁴ during two-qubit gate operations. The manuscript should acknowledge this gap explicitly and discuss the specific experimental milestones needed to validate the architecture's central assumption.

---

## Conclusion: the architecture is scientifically grounded but incompletely validated

The five-layer paradigm synthesizes genuine advances across multiple subfields into a coherent vision. The individual components—cat qubit bit-flip suppression, LDPC-cat codes, phononic engineering, cryo-CMOS control—are each supported by real experimental progress. The 758-qubit headline figure traces to a rigorous, peer-reviewed Nature Communications paper with clearly stated assumptions.

Three issues demand attention in review. First, the **operational bias gap** (idle ~thousands vs. gate ~250) is inadequately addressed if the paper treats κ₁/κ₂ = 10⁻⁴ as near-term achievable without substantial caveats. The recent chaos-assisted tunneling and multimode resonance results suggest this is not purely an engineering challenge. Second, the **competitive landscape has shifted**: Quantinuum's Helios (48 logical qubits at 2:1 ratio, experimentally demonstrated) and Harvard/QuEra's 96 simultaneous logical qubits with high-rate codes challenge the resource efficiency narrative. Third, several **critical 2025 theoretical works**—especially Romanesco codes, analog bosonic qLDPC decoding, and bosonic coding without concatenation—should be discussed as they directly bear on the proposed architecture's outer code layer and design philosophy. The most important factual correction needed is the attribution of T₁,eff ≈ 70 µs to the AWS PRX paper rather than the Ocelot Nature paper, and the >50,000 idle bias claim for Hajr et al. appears unsupported by the published data.