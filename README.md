# Network Meta-Analysis of Parkinson’s Treatments

## 📌 Overview

This project performs a Network Meta-Analysis (NMA) to compare dopamine agonists for Parkinson’s disease.

**Treatments included:**

* Placebo
* Pramipexole
* Ropinirole
* Bromocriptine
* Cabergoline

**Outcome:** Standardised Mean Difference (SMD)
→ Negative values indicate improvement.

---

## 📊 Key Results

* **Pramipexole** showed the strongest effect vs placebo (SMD ≈ -1.81)
* Other treatments showed smaller and non-significant effects
* Pramipexole ranked highest (P-score ≈ 1.0)

👉 **Conclusion:**
Pramipexole is likely the most effective treatment.

---

## 📈 Figures

Add your saved plots in the `outputs/` folder:

```
outputs/
├── arm_plot.png
├── funnel_plot.png
├── ranking_plot.png
├── network_plot.png
```

Then include:

![Arm Plot](outputs/arm_plot.png)
![Funnel Plot](outputs/funnel_plot.png)
![Ranking Plot](outputs/ranking_plot.png)
![Network Plot](outputs/network_plot.png)

---

## 🔬 Methods

* Random-effects Network Meta-Analysis (`netmeta`)
* Pairwise comparisons reconstructed within studies
* Treatment ranking using P-scores

**Diagnostics included:**

* Funnel plot (publication bias)
* Netsplit (consistency)
* Leave-one-out sensitivity analysis

---

## 🧠 Interpretation

* Pramipexole shows a **large and statistically significant effect**
* Other treatments show **uncertainty (wide confidence intervals)**
* The network is **consistent and robust**

---

## ⚠️ Limitations

* Summary-level data used
* Multi-arm correlations approximated
* Small number of studies (n = 7)

---

## ▶️ How to Run

```r
source("run_analysis.R")
```

---

## 📁 Outputs

* Figures → `/outputs/`
* Tables → `/outputs/`
* PDF Report → `NMA_Report.pdf`

---

## 🏁 Final Conclusion

Pramipexole appears to be the most effective dopamine agonist, with a large and significant improvement compared to placebo. Other treatments show smaller and uncertain effects.

---
