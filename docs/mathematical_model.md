# Global ADMET Index (GAI) Mathematical Model

The Global ADMET Index (GAI) is a composite score (0–100) that summarizes the overall ADMET profile of a molecule by aggregating diverse predictive endpoints.

## 1. Structural Formula

The GAI is calculated as a weighted mean of six major sub-indices:

$$GAI = \frac{\sum (W_i \times S_i)}{\sum W_i} \times 100$$

Where:
- **$S_i$**: Subscore for ADMET subgroup $i$ (scaled 0–1).
- **$W_i$**: Weight assigned to subgroup $i$.

### Subgroup Weights ($W_i$)

| Subgroup | Weight | Key Parameters |
| :--- | :--- | :--- |
| **Absorption** | 0.20 | HIA, Caco-2, P-gp, logP |
| **Distribution** | 0.15 | BBB, PPB, Fu, VDss |
| **Metabolism** | 0.20 | CYP Inhibition (1A2, 2C9, 2C19, 2D6, 3A4) |
| **Excretion** | 0.10 | Clearance, Half-life ($t_{1/2}$) |
| **Toxicity** | 0.25 | Ames, DILI, hERG, Carcinogenicity, LD50 |
| **Physicochemical** | 0.10 | MW, TPSA, Fsp3, QED, Rotatable Bonds |

---

## 2. Subgroup Score Computation ($S_i$)

Each subscore $S_i$ is a weighted average of its internal parameters, each normalized between 0 and 1.

### 2.1 Absorption ($S_{abs}$)
$$S_{abs} = 0.4 \times norm(HIA) + 0.3 \times norm(Caco2) + 0.2 \times (1 - norm(Pgp)) + 0.1 \times norm(logP)$$

### 2.2 Distribution ($S_{dist}$)
$$S_{dist} = 0.4 \times norm(BBB) + 0.3 \times (1 - norm(PPB)) + 0.3 \times norm(Fu)$$

### 2.3 Metabolism ($S_{met}$)
$$S_{met} = 1 - mean(CYP\_Flags)$$
*A penalty factor is applied for each major CYP inhibitor detected.*

### 2.4 Excretion ($S_{exc}$)
$$S_{exc} = 0.6 \times (1 - norm(Clearance)) + 0.4 \times norm(t_{1/2})$$

### 2.5 Toxicity ($S_{tox}$)
$$S_{tox} = 1 - mean(Ames + DILI + hERG + Carcino + LD50\_Risk)$$

### 2.6 Physicochemical ($S_{phys}$)
$$S_{phys} = 0.3 \times norm(QED) + 0.2 \times (1 - norm(MW)) + 0.2 \times (1 - norm(TPSA)) + 0.1 \times norm(Fsp3) + 0.2 \times (1 - norm(RotBonds))$$

---

## 3. Normalization Rules

Endpoints are normalized based on their "desirability" direction:

- **Higher is Better** ($norm(x)$): Used for HIA, Caco-2, QED, Fsp3.
- **Lower is Better** ($1 - norm(x)$): Used for MW, TPSA, PPB, Clearance, Toxicity probabilities.
- **Mid-Ideal**: Used for logP (optimal range 0–5).

---

## 4. GAI Interpretation

| GAI Range | Category | Color |
| :--- | :--- | :--- |
| **80 – 100** | Excellent | Green |
| **60 – 79** | Good | Light Green |
| **40 – 59** | Moderate | Yellow |
| **20 – 39** | Poor | Orange |
| **0 – 19** | Toxic / Critical | Red |
