# Bibliography and Concluding Section

---

## BibTeX Entries

```bibtex
@article{Savicky1990,
  author    = {Petr Savick{\'{y}}},
  title     = {Random Boolean Formulas Representing Any Boolean Function 
               with Asymptotically Equal Probability},
  journal   = {Discrete Mathematics},
  volume    = {83},
  number    = {1},
  pages     = {95--103},
  year      = {1990},
  doi       = {10.1016/0012-365X(90)90224-6}
}

@article{LefmannSavicky1997,
  author    = {Hanno Lefmann and Petr Savick{\'{y}}},
  title     = {Some Typical Properties of Large {AND/OR} Boolean Formulas},
  journal   = {Random Structures \& Algorithms},
  volume    = {10},
  number    = {3},
  pages     = {337--351},
  year      = {1997},
  doi       = {10.1002/(SICI)1098-2418(199705)10:3<337::AID-RSA4>3.0.CO;2-X}
}

@article{Khrapchenko1971,
  author    = {V. M. Khrapchenko},
  title     = {A Method of Determining Lower Bounds for the Complexity 
               of {$\Pi$}-Schemes},
  journal   = {Matematicheskie Zametki},
  volume    = {10},
  number    = {1},
  pages     = {83--92},
  year      = {1971},
  note      = {English translation in Mathematical Notes 10(1):474--479}
}

@inproceedings{Tarui2008,
  author    = {Jun Tarui},
  title     = {Smallest Formulas for Parity of $2^k$ Variables Are 
               Essentially Unique},
  booktitle = {Proc.\ 14th Annual International Computing and 
               Combinatorics Conference (COCOON)},
  series    = {LNCS},
  volume    = {5092},
  pages     = {95--105},
  year      = {2008},
  publisher = {Springer},
  doi       = {10.1007/978-3-540-69733-6_10}
}

@article{BrodskyPippenger2005,
  author    = {Alexander Brodsky and Nicholas Pippenger},
  title     = {The Boolean Functions Computed by Random Boolean Formulas 
               or How to Grow the Right Function},
  journal   = {Random Structures \& Algorithms},
  volume    = {27},
  number    = {4},
  pages     = {490--519},
  year      = {2005},
  doi       = {10.1002/rsa.20095}
}

@inproceedings{OliveiraSanthanam2018,
  author    = {Igor Carboni Oliveira and Rahul Santhanam},
  title     = {Hardness Magnification for Natural Problems},
  booktitle = {Proceedings of the 59th IEEE Symposium on Foundations of 
               Computer Science (FOCS)},
  pages     = {65--76},
  year      = {2018},
  doi       = {10.1109/FOCS.2018.00016}
}

@article{CHOPRS2022,
  author    = {Lijie Chen and Shuichi Hirahara and Igor Carboni Oliveira 
               and Jan Pich and Ninad Rajgopal and Rahul Santhanam},
  title     = {Beyond Natural Proofs: Hardness Magnification and 
               Locality},
  journal   = {Journal of the ACM},
  volume    = {69},
  number    = {4},
  pages     = {25:1--25:49},
  year      = {2022},
  doi       = {10.1145/3538391}
}

@article{Hastad1998,
  author    = {Johan H{\aa}stad},
  title     = {The Shrinkage Exponent of {De Morgan} Formulae is 2},
  journal   = {SIAM Journal on Computing},
  volume    = {27},
  number    = {1},
  pages     = {48--64},
  year      = {1998},
  doi       = {10.1137/S0097539794261556}
}

@article{ChauvinFlajoletGardyGittenberger2004,
  author    = {Brigitte Chauvin and Philippe Flajolet and Dani{\`e}le Gardy 
               and Bernhard Gittenberger},
  title     = {{And/Or} Trees Revisited},
  journal   = {Combinatorics, Probability and Computing},
  volume    = {13},
  number    = {4--5},
  pages     = {475--497},
  year      = {2004},
  doi       = {10.1017/S0963548304006273}
}

@inproceedings{GrochowPitassi2014,
  author    = {Joshua A. Grochow and Toniann Pitassi},
  title     = {Circuit Complexity, Proof Complexity, and Polynomial 
               Identity Testing},
  booktitle = {Proceedings of the 55th IEEE Symposium on Foundations of 
               Computer Science (FOCS)},
  pages     = {110--119},
  year      = {2014},
  doi       = {10.1109/FOCS.2014.20}
}

@article{RazborovRudich1997,
  author    = {Alexander A. Razborov and Steven Rudich},
  title     = {Natural Proofs},
  journal   = {Journal of Computer and System Sciences},
  volume    = {55},
  number    = {1},
  pages     = {24--35},
  year      = {1997},
  doi       = {10.1006/jcss.1997.1494}
}
```

---

## §8. Consequences for Meta-Complexity

The structural invariants introduced in this paper — structural fluidity, restriction images, the universe-to-image ratio σ∞(d), and the redundancy budget — provide a new quantitative lens on the circuit-level MCSP presheaf. We conclude by connecting these findings to the broader meta-complexity program.

### 8.1. The Compatibility CSP as a Meta-Complexity Object

The compatibility CSP Γ(T, d, s) — with variables at sub-cubes, domains of formula DAG classes, and STRUCT-MATCH constraints on overlapping pairs — is a well-defined combinatorial object for each truth table T, dimension d, and size bound s. Our results characterize its constraint density: when σ∞(d) is large, STRUCT-MATCH constraints eliminate the vast majority of assignment pairs; empirically the surviving fraction scales on the order of 1/σ∞(d).

The function OD(T) = 1 if and only if this CSP is satisfiable and the satisfying assignment does not extend to a global formula of size ≤ s. The computational complexity of OD is thus the complexity of a specific CSP satisfiability detection problem on an expanding constraint graph. This places OD squarely within the landscape of meta-complexity — alongside MCSP, Gap-MCSP, and circuit minimization — where the difficulty lies not in solving individual instances but in detecting which instances are solvable.

### 8.2. The Sparsity of OD = 1 Instances

Our scaling law provides the first quantitative evidence that OD = 1 instances are rare. The Savický anti-concentration theorem, combined with the empirically verified low canonical compression, implies that formula structures are spread thinly across the function space. Consequently, the compatibility CSP is unsatisfiable for generic truth tables (OD = 0 is the default), and our scaling law strongly suggests that the truth tables where compatible families exist (OD = 1) form a sparse subset.

This sparsity has two implications for the magnification program.

First, it provides quantitative support for the natural proofs evasion argument: if the set {T : OD(T) = 1} has density approaching 2^{−2^{d−1}} or less among truth tables, the OD property is "non-large" in the sense of Razborov–Rudich [RR97], circumventing the natural proofs barrier.

Second, it reframes the hardness question. The difficulty of computing OD is not that "most inputs are hard" (they are easy — output 0). The difficulty, if any, is that the rare OD = 1 instances may be computationally indistinguishable from OD = 0 instances. This is a detection problem: given a truth table T, determine whether the compatibility CSP Γ(T, d, s) has a solution, knowing that solutions are extremely rare. Whether this detection problem is computationally hard is the precise form of the magnification gap for the presheaf approach.

### 8.3. Connections to Proof Complexity

The compatibility CSP naturally connects to algebraic proof complexity. The constraints of Γ(T, d, s) can be encoded as polynomial equations over a suitable field, and the infeasibility of the CSP (when OD(T) = 0) corresponds to the existence of a Nullstellensatz or polynomial calculus refutation. The degree of such a refutation measures "how hard it is to certify that no compatible family exists."

If the Nullstellensatz degree of the infeasibility certificate for Γ(T, d, s) grows super-polynomially with N for high-complexity truth tables T, this would constitute a proof complexity lower bound with implications for the circuit complexity of OD via known connections between proof complexity and circuit complexity (the IPS framework of Grochow–Pitassi [GP14]). Our structural data — particularly the expanding constraint graph with Ω(d/n) edge expansion — suggests that the infeasibility proofs cannot be localized, which is a prerequisite for high degree.

This direction (Path C in the accompanying research roadmap) remains unexplored and represents what we consider the most promising route from structural characterization to computational lower bounds.

### 8.4. What This Work Does Not Do

We emphasize that nothing in this paper constitutes progress toward proving P ≠ NP or even toward proving circuit lower bounds for OD. The structural characterization of the presheaf — however detailed — is a characterization of the *mathematical object*, not of the *computational problem* of deciding its properties. Converting structural properties of combinatorial objects into computational lower bounds for functions that detect those properties is the central challenge of meta-complexity theory, and it remains open.

What this work does provide is a precise, quantitative foundation for future attempts at this conversion. The scaling law σ∞(d) ≈ 2^{d−1}, the three-tier structural taxonomy, the redundancy budget mechanism, and the connection to Savický's distributional theory together form a toolkit that any future argument — whether through magnification, proof complexity, or direct methods — would need to engage with. By establishing these tools and identifying the exact gaps (the compression bound, the restriction entropy scaling, the detection hardness question), we aim to make the next step possible rather than to take it ourselves.

Understanding the structure of Boolean formulas at the level of DAG topology rather than function representation may provide a complementary perspective to classical circuit complexity methods.
