# Verifying the "Five-Layer Paradigm" article's technical claims

The article contains **several significant errors** alongside many accurate technical claims. Of the 10 claims examined, three have factually wrong citation details, one contains a potentially misleading interpretation of experimental data, and one dramatically overstates qubit overhead by roughly three orders of magnitude. The remaining claims check out against published sources. Below is a claim-by-claim breakdown.

## AWS Ocelot: right science, wrong author attribution

The paper's title, "Hardware-efficient quantum error correction via concatenated bosonic qubits," is correct, as are the volume (638), year (2025), and core technical claims about five cat data qubits and below-threshold concatenated error correction. However, the citation **"Lescanne, R. et al."** is flatly wrong. The actual first author is **Harald Putterman** (AWS Center for Quantum Computing / Caltech), and the paper has 120+ authors. Raphaël Lescanne is CTO of Alice & Bob—a completely different organization—and has no connection to this paper. The page range is also off by one: the correct span is **927–934**, not 927–933. This is a serious misattribution that conflates two rival cat-qubit research groups.

## Alice & Bob's bit-flip lifetime claims hold up—with caveats

The Nature 629, 778–783 (2024) citation is accurate. That paper, led by U. Réglade and A. Bocquet at Alice & Bob, demonstrated cat-qubit bit-flip times **exceeding ten seconds**, consistent with the article's claim. The progression to "exceeding seven minutes" on the Boson 4 chip (~430 seconds) is also confirmed, announced around May 2024. The "preliminary results at the hour scale reported in September 2025" checks out as well: on September 25, 2025, Alice & Bob announced **33–60 minute bit-flip times** (95% CI, averaging ~44 minutes) on their Galvanic Cat qubit design. The key caveat is that the hour-scale results remain **unpublished preliminary data** from a blog post, not a peer-reviewed paper. The article's phrasing "demonstrated bit-flip lifetimes exceeding one hour" overstates the certainty of what are explicitly preliminary, non-peer-reviewed measurements.

## LDPC-cat code has the wrong article number

The actual citation for the Ruiz et al. paper is Nature Communications 16, **1040** (2025)—not 957 as claimed. The title ("LDPC-cat codes for low-overhead quantum computing in 2D"), authors (Diego Ruiz, Jérémie Guillaud, Anthony Leverrier, Mazyar Mirrahimi, Christophe Vuillot), and technical claim of **100 logical qubits from 758 physical cat qubits** at a phase-flip error probability of ~0.1% are all correct. This remains a theoretical architectural proposal, not an experimental demonstration.

## Intel Pando Tree and Google Willow check out cleanly

The Intel Pando Tree claim is **fully accurate**. The chip was presented at the 2024 IEEE Symposium on VLSI Technology & Circuits (June 2024), operates at **10–20 mK**, and delivers 64-terminal demultiplexing—the largest interconnect demultiplexing capability demonstrated at millikelvin temperatures. It uses Intel's 22nm low-power FinFET process.

Google's Willow processor claim is likewise **fully confirmed**. Google published "Quantum error correction below the surface code threshold" in Nature 638, 920–926 (online December 9, 2024). The 105-qubit Willow chip demonstrated below-threshold surface code operation at distance 7, achieving a logical error rate of **0.143% ± 0.003% per cycle** and an error suppression factor Λ = 2.14 ± 0.02 between distances 5 and 7.

## Three claims with subtle but important inaccuracies

**Odeh & Sipahigil phononic bandgap paper.** The journal (Nature Physics), volume (21), year (2025), and scientific content (non-Markovian dynamics of a superconducting qubit on a phononic bandgap metamaterial) are all correct. However, the page numbers are wrong: the actual pages are **406–411**, not 218–224. Additionally, the paper consistently refers to a "superconducting qubit," not specifically a "transmon," though this is a minor terminological point.

**UC Berkeley Kerr-cat qubit.** The arXiv ID (2411.04442), first author (Bingcheng Qing), Siddiqi group affiliation, and noise bias value of approximately **250** are all confirmed. Two nuances matter: the paper measures this bias via **dihedral randomized benchmarking (DRB)**, not gate-set tomography as the article implies, and uses the term **"noise bias"** rather than "operational bias." The paper has since been published in PNAS 123(5), e2520479123 (2026).

**Chen & Painter phononic engineering paper.** The citation (Science Advances 10, eado6240, 2024) is correct. But the claim about "T1 values exceeding 5 milliseconds" is **potentially misleading**. The 5 ms T1 refers to the relaxation time of **two-level system (TLS) defects** strongly coupled to a transmon, not the transmon qubit's own coherence time. The transmon itself measured only **T1 ~ 3 μs**. If the article presents this as a qubit coherence achievement, it misrepresents the result by a factor of ~1,700×.

## Surface code overhead claim is off by ~1,000×

The assertion that surface codes require **10⁶–10⁷ physical qubits per logical qubit** at 10⁻¹² logical error rates is the article's most significant factual error. Mainstream estimates consistently place the per-logical-qubit overhead at roughly **10³ physical qubits** (thousands, not millions). A distance-25 to distance-30 surface code, sufficient for 10⁻¹² error rates at physical error rates near 10⁻³, requires on the order of 1,000–2,000 physical qubits per logical qubit. The article appears to have confused the **total system qubit count** for cryptographic algorithms (which does reach 10⁶–10⁸ when accounting for thousands of logical qubits plus magic-state distillation factories) with the per-logical-qubit ratio. Key references supporting this correction include Fowler et al. (2012), which estimates 10³–10⁴ physical qubits per logical qubit for error rates below 10⁻¹⁵, and Gidney & Ekerå (2021), which estimates ~20 million total noisy qubits for 2048-bit RSA factoring.

## IBM bivariate bicycle code is accurately described

The arXiv:2506.03094 (June 2025) preprint, "Tour de gross: A modular quantum computer based on bivariate bicycle codes," is confirmed as an IBM Quantum paper by Yoder, Schoute, Rall, and colleagues. The **[[144, 12, 12]] code** parameters—encoding 12 logical qubits in 144 data qubits at distance 12—are exactly correct, originating from Bravyi et al., Nature 627, 778–782 (2024). The code achieves roughly **10× better encoding efficiency** than comparable surface codes.

## Conclusion

The article's scientific narrative about cat-qubit error correction and the five-layer paradigm rests on a largely accurate technical foundation, but the execution contains errors that undermine credibility. The most consequential mistakes are the **misattribution of the AWS Ocelot paper to Lescanne** (an Alice & Bob researcher), the **~1,000× overstatement of surface code overhead** per logical qubit, and the **misleading presentation of 5 ms TLS defect lifetimes as qubit coherence**. Three citation page/article numbers are also wrong (Claims 1, 3, 5). These errors suggest either careless reference management or, in the case of the surface code overhead, a fundamental misunderstanding of how qubit resource estimates are structured. The core scientific claims about cat-qubit bias, LDPC-cat architectures, and cryogenic control electronics are sound.