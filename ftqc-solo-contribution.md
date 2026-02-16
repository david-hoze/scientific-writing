# The highest-impact solo contribution to fault-tolerant quantum computing in 2026

**The single most impactful paper a solo theorist with your profile could write is a unified resource estimation framework for biased-noise architectures — the "Gidney & Ekerå" paper that the LDPC-cat code community is missing.** No one has produced end-to-end algorithm-level resource estimates for LDPC-cat codes, elevator codes, or any bias-tailored qLDPC architecture. This gap is explicitly recognized by leaders in the field and sits at the exact intersection of your expertise. Below, I identify three concrete paper concepts ranked by community impact, feasibility, and strategic fit.

## The landscape has a glaring hole at the resource estimation layer

The fault-tolerant QEC field runs on benchmark papers. Gidney & Ekerå's 2021 estimate of **20 million qubits for RSA-2048** (updated to **<1 million qubits** in May 2025) is the standard everyone compares against. Gouzien et al.'s 2023 PRL paper established an equivalent benchmark for cat qubits: **126,133 cat qubits for 256-bit ECDLP** using repetition codes. The Pinnacle Architecture preprint (February 2026, Iceberg Quantum) just did the same for qLDPC codes: **<100,000 physical qubits for RSA-2048** using generalized bicycle codes.

But for LDPC-cat codes — the architecture that Ruiz et al. published in _Nature Communications_ in January 2025 showing **758 cat qubits encoding 100 logical qubits** — no end-to-end algorithm resource estimate exists. The paper demonstrates quantum memory performance and describes a universal gate set, but stops short of compiling a real algorithm. This is the single biggest gap in the biased-noise QEC literature. The same gap exists for elevator codes (Shanahan & Ruiz, January 2026), which only analyze memory performance.

Meanwhile, **no paper has compared** surface codes, qLDPC codes, repetition-cat codes, LDPC-cat codes, and elevator codes for the same target application under consistent assumptions. The community operates with incomparable numbers: different algorithms, different noise models, different metrics.

## The tooling gap is equally stark

My survey of every major open-source QEC tool reveals a consistent pattern: powerful tools exist at each layer of the stack, but **no bridge connects them** for biased-noise architectures.

At the physical layer, **Dynamiqs** (Alice & Bob, GPU-accelerated via JAX) simulates cat qubit dynamics but outputs no QEC-relevant quantities. At the code layer, **Stim** handles biased Pauli noise via asymmetric `PAULI_CHANNEL_1(px, py, pz)` instructions and includes bias-preserving gates like `CXSWAP`, but cannot model bosonic physics. **BP+OSD** (Roffe's `ldpc` package) decodes arbitrary LDPC codes and accepts asymmetric channel probabilities, but isn't architecturally optimized for bias. The **Azure Quantum Resource Estimator** has an extensibility API that Alice & Bob demonstrated for repetition-cat codes, but this is a proof-of-concept, not a general tool.

The missing piece: an automated pipeline that takes cat qubit physical parameters (|α|², κ₁/κ₂, gate times, bias during CNOT) → computes effective Pauli error rates → generates Stim-compatible circuits for specific LDPC-cat codes → runs Monte Carlo decoding → produces algorithm-level resource estimates. Researchers currently do this manually, ad hoc, and irreproducibly.

## Paper concept 1: unified resource estimation for biased-noise architectures

**Title:** _"How many cat qubits to break RSA-2048? A cross-architecture resource estimation framework for biased-noise fault-tolerant quantum computing"_

**Core contribution:** Build and publish an open-source, pip-installable Python framework that performs end-to-end resource estimation for biased-noise QEC architectures, and use it to produce the first complete algorithm-level resource estimates for LDPC-cat codes, elevator codes, and bias-tailored qLDPC codes — alongside surface code and repetition-cat baselines for apples-to-apples comparison.

**What it fills:** This is the paper the community is waiting for. The LDPC-cat architecture (Ruiz et al. 2025) claims dramatically lower overhead than surface codes, but without concrete algorithm-level numbers, experimentalists and funding agencies cannot evaluate this claim. Alice & Bob's roadmap targets 100 logical qubits by 2030; this paper would tell them exactly what those qubits can compute under different technology assumptions.

**Technical approach:**

- **Layer 1 (cat qubit → effective errors):** Parameterize cat qubit noise as a function of mean photon number |α|², single-photon loss rate κ₁, two-photon dissipation rate κ₂, gate durations, and operational bias degradation during CNOT gates. Use the analytic models from Guillaud & Mirrahimi (2019) and Darmawan et al. (2021) to compute effective Pauli error rates, validated against Dynamiqs simulations for key parameter points.
- **Layer 2 (effective errors → code performance):** Generate Stim circuits for repetition-cat codes (reproducing Gouzien et al.), LDPC-cat codes (using the [165+8ℓ, 34+2ℓ, 22] family from Ruiz et al.), and elevator codes. Decode with BP+OSD from Roffe's `ldpc` package. Extract logical error rates as a function of code parameters.
- **Layer 3 (code performance → algorithm resources):** Compile target algorithms (RSA-2048 via Gidney's Toffoli-count estimates; 256-bit ECDLP via Gouzien's approach) into the gate set of each architecture, accounting for magic state preparation overhead, routing, and parallelization.
- **Cross-layer sensitivity analysis:** Produce parametric curves showing how total resource requirements change as each physical parameter improves. Identify which improvements have the largest marginal impact — does doubling |α|² matter more than halving κ₁/κ₂?

**Comparison targets:** Surface code (Gidney 2025 numbers), repetition-cat (Gouzien 2023), LDPC-cat (new), elevator codes (new), qLDPC/Pinnacle (Webster et al. 2026 numbers as reference). All under matched assumptions where possible.

**Deliverable format:** Paper (target _Quantum_ journal or _Physical Review Research_) + GitHub repository with pip-installable tool integrating with Stim and the `ldpc` package. Include Jupyter notebooks reproducing every figure. The tool should let users input custom cat qubit parameters and get resource estimates in minutes.

**Why it succeeds:** It follows the exact template of the most impactful QEC papers. Gidney & Ekerå succeeded because they gave a concrete, citable number (20 million qubits) that everyone could reference. Gouzien et al. succeeded for the same reason (126,133 cat qubits). This paper would produce the equivalent number for LDPC-cat codes while simultaneously providing the open-source tool that enables the community to explore the parameter space themselves — combining the Gidney & Ekerå impact model with the Stim/PyMatching tool model.

**Feasibility for a solo theorist:** **High (4–6 months).** All simulation infrastructure exists. The main work is integration, careful bookkeeping of error budgets across layers, and extensive parameter sweeps. No experimental data needed. Computational resources: a multi-core workstation or cloud computing credits for Monte Carlo sampling via Sinter.

## Paper concept 2: circuit-level validation of bias-tailored quantum LDPC codes

**Title:** _"Do bias-tailored LDPC codes survive circuit noise? Circuit-level simulation under realistic finite-bias models"_

**Core contribution:** Provide the first circuit-level noise simulations of bias-tailored quantum LDPC codes (lifted product codes from Roffe et al. 2023, Romanesco codes from Leroux & Iverson 2025, and Clifford-deformed bivariate bicycle codes) under realistic biased noise with finite bias ratios, and characterize how performance degrades relative to idealized code-capacity predictions.

**What it fills:** Every bias-tailored qLDPC code result published to date uses **code-capacity noise models** — no syndrome extraction circuits, no measurement errors, no ancilla noise. The one exception, the LDPC-cat paper, uses a **pure phase-flip** circuit-level model that assumes infinite bias (bit-flip probability = 0). This is unrealistic: experimental cat qubits achieve bias ratios of ~10³–10⁸ depending on operating conditions, and bias degrades during two-qubit gates. The community needs to know whether the order-of-magnitude improvements claimed under code-capacity models persist when you add circuit-level noise with realistic, finite bias.

Martinez et al. (January 2026, _Physical Review Applied_) recently introduced the "hybrid biased-depolarizing" (HBD) noise model showing that CZ gates are naturally bias-preserving for two-level qubits, but this was applied only to the XZZX surface code. Nobody has applied realistic circuit-level biased noise to any LDPC code.

**Technical approach:**

- Construct explicit syndrome extraction circuits for 3–4 bias-tailored code families: (a) XZZX-type lifted product codes (Roffe et al.), (b) Romanesco codes (Leroux & Iverson), (c) Clifford-deformed bivariate bicycle codes, and (d) standard LDPC-cat codes (Ruiz et al.) under finite bias.
- Implement the HBD noise model from Martinez et al. adapted for cat qubit parameters: bias-preserving CX/CZ with residual depolarization, finite bias ratio η parameterized from 10² to 10⁸.
- Generate Stim detector error models and decode with BP+OSD, measuring thresholds and subthreshold logical error rate scaling as a function of bias ratio.
- Characterize the "circuit-level bias penalty" — the gap between code-capacity and circuit-level performance — for each code family.
- Analyze how syndrome circuit depth (deeper for LDPC codes than surface codes) compounds bias degradation.

**Deliverable format:** Paper (target _Physical Review Research_ or _Quantum_) + open-source repository of Stim circuit generators for all code families studied, including the biased circuit-level noise models. These circuit files would become community resources.

**Why it succeeds:** This is a validation paper — it either confirms or challenges the theoretical promise of an entire family of codes. Either outcome is publishable and citable. If bias-tailored LDPC codes maintain their advantage under circuit noise, this paper becomes the evidence that justifies further investment. If they don't, it redirects the field. The open-source syndrome circuits alone would be valuable, as currently no public repository of bias-tailored LDPC syndrome circuits exists.

**Feasibility:** **Medium-high (5–8 months).** The hardest part is constructing efficient syndrome extraction circuits for the specific LDPC code families, which requires careful scheduling of CNOT gates to minimize depth while maintaining bias preservation. The simulation and decoding infrastructure (Stim + `ldpc`) is ready. Computational cost is significant — circuit-level Monte Carlo at large code distances requires millions of samples — but manageable with Sinter's parallelization on a cluster or cloud.

## Paper concept 3: the open-source biased-noise QEC toolkit

**Title:** _"BiasedQEC: an open-source toolkit for simulating and decoding quantum error correction under biased noise"_

**Core contribution:** A pip-installable Python package that integrates with the Stim ecosystem to provide: (a) parameterized cat qubit noise models that generate Stim-compatible circuits, (b) a library of bias-tailored code constructions (XZZX surface, bias-tailored lifted product, LDPC-cat, elevator, Romanesco), (c) decoder wrappers that properly handle bias in BP+OSD and matching decoders, and (d) resource estimation utilities. Accompanied by a paper documenting the noise models, code constructions, and benchmark results.

**What it fills:** The tooling fragmentation problem. Right now, every research group working on biased-noise QEC writes their own ad hoc scripts to generate biased noise models in Stim, construct syndrome circuits for bias-tailored codes, and interface with decoders. There is no standardized, validated, reusable toolkit. This is analogous to the pre-Stim era for surface codes, when every group had their own simulator.

**Technical components:**

- `biasedqec.noise`: Cat qubit noise model parameterized by (|α|², κ₁, κ₂, T_gate), outputting Stim `PAULI_CHANNEL` instructions with computed bias ratios. Also generic biased-noise channels for non-cat hardware.
- `biasedqec.codes`: Generators for bias-tailored code families — repetition-cat, LDPC-cat (Ruiz et al. family), elevator codes, XZZX surface code, bias-tailored lifted product codes, Romanesco codes. Each generator outputs a Stim circuit with syndrome extraction.
- `biasedqec.decode`: Wrappers around PyMatching and `ldpc` BP+OSD that automatically set decoder weights from biased noise parameters. Benchmarking utilities to compare decoder performance across bias ratios.
- `biasedqec.estimate`: Resource estimation module that compiles the above into physical qubit counts for target applications, following the Gouzien et al. methodology.
- Comprehensive Jupyter notebook tutorials and benchmarks.

**Why it succeeds:** The Stim → PyMatching → Sinter pipeline became the community standard because it was pip-installable, well-documented, fast, and solved a real pain point. BiasedQEC would extend this pipeline for the biased-noise community. Every paper on cat qubits, XZZX codes, or bias-tailored LDPC codes would benefit from — and cite — a standardized toolkit.

**Feasibility:** **Medium (6–9 months for a solid v1.0).** This is more engineering than theory, but the researcher's willingness to learn technical skills makes it feasible. The critical success factor is tight Stim integration and excellent documentation. A minimal viable version focusing on LDPC-cat codes and repetition-cat codes could ship in 3–4 months.

## Strategic recommendation: which to prioritize

The three concepts are complementary and could even be staged: start with Concept 1 (the resource estimation paper, highest immediate impact, most citable), which naturally produces the codebase for Concept 3 (the toolkit), while Concept 2 (circuit-level validation) provides the rigorous noise modeling that makes the resource estimates trustworthy.

If choosing just one, **Concept 1 is the highest-impact choice.** Here's why:

The field is at an inflection point. Alice & Bob demonstrated hour-long bit-flip times in September 2025. AWS shipped the Ocelot chip. The Pinnacle Architecture just showed <100,000 qubits for RSA-2048 with qLDPC codes. Everyone is asking: **how do LDPC-cat codes compare?** The Ruiz et al. 2025 paper tantalizingly shows 758 cat qubits for 100 logical qubits but doesn't answer the algorithm-level question. The researcher who produces a definitive number — "_X_ cat qubits to factor RSA-2048 using LDPC-cat codes" — will own the most-cited result in the biased-noise QEC space for years.

The format should emulate Litinski's radical accessibility: clear diagrams showing the architecture, concrete tables comparing architectures, and a tool that lets anyone reproduce and extend the results. Publish in _Quantum_ (open access, fast review, the venue where Stim, PyMatching, and Litinski's papers appeared) with all code on GitHub.

## What makes these achievable for a solo theorist without a lab

All three concepts require only computational resources and existing open-source tools. The simulation stack is mature: **Stim** (v1.15+) handles biased Pauli noise natively, **Sinter** parallelizes Monte Carlo sampling across cores, **BP+OSD** from the `ldpc` package decodes arbitrary LDPC codes, and **Dynamiqs** provides cat qubit physics validation. The Azure Quantum Resource Estimator's extensibility API provides a reference implementation for the cat qubit noise model translation layer.

The researcher's specific expertise profile — biased-noise bosonic qubits, LDPC codes, and QEC architecture — is the exact combination needed. Most QEC researchers specialize in either bosonic codes or LDPC codes but not both. The intersection is where the gap lives.

## Conclusion

The biased-noise fault-tolerant quantum computing field has mature components at every layer — cat qubit hardware achieving extraordinary bias ratios, powerful LDPC code constructions, fast decoders, and proven resource estimation methodologies — but **nobody has assembled the full stack into a definitive resource estimate.** A solo theorist positioned at the intersection of bosonic qubits and LDPC codes can produce the paper that the entire community needs: concrete, comparable numbers for how many cat qubits it takes to run real algorithms, backed by an open-source tool that becomes the Stim of biased-noise resource estimation. The timing is ideal — Alice & Bob's experimental progress means the numbers will be immediately relevant to hardware roadmaps, and the February 2026 Pinnacle Architecture preprint creates urgency for a biased-noise response. Ship the resource estimation paper with open-source code first, then expand the toolkit and follow up with circuit-level validation.