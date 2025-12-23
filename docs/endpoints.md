# Supported ADMET Endpoints

The GAI platform maps hundreds of tool-specific parameters into a set of **Canonical Endpoints**. Each endpoint has a defined "desirability" direction and range used for normalization.

## 1. Absorption Subgroup (Weight: 0.20)

| Canonical Name | Logic | Range/Units | Source Examples |
| :--- | :--- | :--- | :--- |
| **HIA** | Higher Better | 0 – 1 | `hia`, `gi absorption` |
| **Caco2** | Higher Better | -6 – 1 | `caco2`, `caco2 permeability` |
| **logP** | Mid-Ideal | 0 – 5 | `logp`, `xlogp3`, `mlogp` |
| **Pgp_substrate**| Prob Bad | 0 – 1 | `pgp_sub`, `p-glycoprotein substrate` |
| **Bioavailability**| Higher Better | 0 – 1 | `f20`, `bioavailability score` |

## 2. Distribution Subgroup (Weight: 0.15)

| Canonical Name | Logic | Range/Units | Source Examples |
| :--- | :--- | :--- | :--- |
| **BBB** | Prob Penetrate | -2 – 1 | `bbb`, `bbb permeability` |
| **PPB** | Lower Better | 0 – 100% | `ppb`, `plasma protein binding` |
| **Fu** | Higher Better | 0 – 1 | `fu`, `fraction unbound` |
| **VDss** | Mid-Ideal | -1 – 1 | `logvdss`, `vdss (human)` |

## 3. Metabolism Subgroup (Weight: 0.20)

| Canonical Name | Logic | Range/Units | Description |
| :--- | :--- | :--- | :--- |
| **CYP1A2_Inh** | Prob Bad | 0 – 1 | Inhibition risk for CYP1A2 |
| **CYP2C9_Inh** | Prob Bad | 0 – 1 | Inhibition risk for CYP2C9 |
| **CYP2C19_Inh**| Prob Bad | 0 – 1 | Inhibition risk for CYP2C19 |
| **CYP2D6_Inh** | Prob Bad | 0 – 1 | Inhibition risk for CYP2D6 |
| **CYP3A4_Inh** | Prob Bad | 0 – 1 | Inhibition risk for CYP3A4 |

## 4. Excretion Subgroup (Weight: 0.10)

| Canonical Name | Logic | Range/Units | Source Examples |
| :--- | :--- | :--- | :--- |
| **Clearance** | Lower Better | 0 – 50 | `cl-plasma`, `total clearance` |
| **Half_Life** | Higher Better | 0.5 – 24 hrs | `t0.5`, `half-life` |

## 5. Toxicity Subgroup (Weight: 0.25)

| Canonical Name | Logic | Range/Units | Source Examples |
| :--- | :--- | :--- | :--- |
| **hERG** | Prob Bad | 0 – 1 | `herg`, `herg i inhibitor` |
| **Ames** | Prob Bad | 0 – 1 | `ames`, `mutagenesis` |
| **DILI** | Prob Bad | 0 – 1 | `dili`, `hepatotoxicity` |
| **Carcino** | Prob Bad | 0 – 1 | `carcinogenicity` |
| **LD50** | Higher Better | 1 – 5000 | `ld50_oral`, `rat acute toxicity` |
| **SkinSens** | Prob Bad | 0 – 1 | `skinsen`, `skin sensitisation` |

## 6. Physicochemical Subgroup (Weight: 0.10)

| Canonical Name | Logic | Range/Units | Source Examples |
| :--- | :--- | :--- | :--- |
| **MW** | Mid-Ideal | 100 – 600 | `mw`, `molecular weight` |
| **TPSA** | Mid-Ideal | 20 – 140 | `tpsa` |
| **RotBonds** | Lower Better | 0 – 10 | `nrot`, `rotatable bonds` |
| **HBD / HBA** | Lower Better | 0 – 5/10 | `nhd`, `nha`, `acceptors` |
| **QED** | Higher Better | 0 – 1 | `qed` |
| **Fsp3** | Higher Better | 0.4 – 1 | `fsp3`, `fraction csp3` |
| **Solubility** | Higher Better | -6 – 0 | `logs`, `esol log s` |
