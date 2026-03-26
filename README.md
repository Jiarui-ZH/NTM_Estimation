# NTM Estimation — Ad Valorem Equivalent of Non-Tariff Measures

This repository contains the code and supporting data to estimate the **ad valorem equivalent (AVE)** of non-tariff measures (NTMs) for goods trade, using a gravity-model approach with instrumental variables.

---
> For questions, please open an Issue. If you wish to make edits, please submit a PR.
---


## Repository Structure

```
NTM_Estimation/
├── Goods/
│   └── Model (2000 Bootstrap).R      # Main estimation script
├── DATA/
│   ├── BACI/
│   │   ├── BACI.R                    # Cleans raw BACI trade data
│   │   └── BACI_HS12_V202501/
│   ├── MAcMap-HS6/
│   │   ├── MAcMap.R                  # Cleans raw MAcMap tariff data
│   │   ├── HS6 TO GTAP UPDATED.xlsx  # HS6 → GTAP sector crosswalk
│   │   └── Tariffs_2001_2019/
│   ├── NTM/
│   │   ├── NTM Cleaning.R            # Cleans raw Global Trade Alert data
│   │   └── interventions (Download from Global Trade Alert).csv
│   ├── Three_Clostest_Countries/
│   │   └── three_closest_countries.R # Builds IV: neighbour-average NTM & tariff
│   ├── gravity_variables (needs to be updated for year of interest).csv
│   ├── common border cleaned.csv
│   └── landlock.xlsx
└── Sectors and Regions for NTM.xlsx  # Sector & region aggregation crosswalk
```

> **Working directory:** Set your R working directory to the project root (`NTM_Estimation/`) before running any script. In RStudio: *Session → Set Working Directory → To Project Directory*.

---

## Data Sources

### 1. NTM Data — Global Trade Alert
- **Source:** [Global Trade Alert Data Center](https://globaltradealert.org/data-center)
- **File:** `DATA/NTM/interventions (Download from Global Trade Alert).csv`
- Contains bilateral NTM interventions (Red/Amber evaluations) with product-level HS6 codes and implementing/affected jurisdictions.
- **Cleaning script:** `DATA/NTM/NTM Cleaning.R` — filters out Green evaluations and export-related/tariff measures, maps HS6 codes to GTAP sectors, and maps country names to ISO3 codes.

### 2. Trade Data — BACI (CEPII)
- **Source:** [BACI — International Trade Database at the Product Level](https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37)
- **Files:** `DATA/BACI/BACI_HS12_V202501/BACI_HS12_Y[YEAR]_V202501.csv`
- Provides harmonised bilateral trade flows at the HS6 product level for all available years.
- **Cleaning script:** `DATA/BACI/BACI.R` — maps HS6 codes to GTAP sectors and numeric country codes to ISO3.

### 3. Tariff Data — MAcMap-HS6 (CEPII)
- **Source:** [MAcMap-HS6 — Applied Tariffs Database](https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=12)
- **Files:** `DATA/MAcMap-HS6/Tariffs_2001_2019/Tariffs_2001_2019/mmhs6_[YEAR].csv`
- Provides applied bilateral tariff rates (ad valorem equivalents) at the HS6 product level.
- **Cleaning script:** `DATA/MAcMap-HS6/MAcMap.R` — maps HS6 codes to GTAP sectors and computes simple averages within sector-pair cells.

### 4. Gravity Variables
- **File:** `DATA/gravity_variables (needs to be updated for year of interest).csv`
- Contains bilateral gravity covariates: bilateral distance (`dist`), importer and exporter GDP (`gdp_importer`, `gdp_exporter`), and other standard gravity controls. Update this file for the year of interest.

### 5. Additional Controls
- **Landlock status:** `DATA/landlock.xlsx` — binary indicator for landlocked countries.
- **Common border:** `DATA/common border cleaned.csv` — binary indicator for country pairs sharing a land border.
- **Sector/Region crosswalk:** `Sectors and Regions for NTM.xlsx` — maps HS6/GTAP sector codes and country codes to the aggregation used in the final output.

---

## Methodology

The estimation follows a **two-stage IV-PPML gravity model** to recover the ad valorem equivalent (AVE) of NTMs:

### Stage 1 — Instrument Construction
`DATA/Three_Clostest_Countries/three_closest_countries.R` builds bilateral instruments by computing, for each importer–exporter–product triplet, the **average NTM count and tariff rate applied by the three geographically closest neighbours** of the importer. These neighbour averages serve as instruments for the importer's own NTM and tariff policies.

### Stage 2 — Estimation (`Goods/Model (2000 Bootstrap).R`)

1. **Data assembly:** Merges BACI trade flows, MAcMap tariffs, NTM counts, gravity variables, landlock, common border, and the neighbour instruments into a single estimation dataset.

2. **First stage (OLS):** Regresses observed tariffs and NTM counts on their respective neighbour-average instruments plus standard gravity controls (`log distance`, `log GDP`). Predicted values are used in the second stage. Separate first stages are run for the level and two trade-share-weighted versions (importer-share and exporter-share weighted) of each policy variable, following a heterogeneous-coefficient framework.

3. **Second stage (PPML):** Estimates a Poisson pseudo-maximum-likelihood gravity equation for bilateral trade quantities, using the fitted policy values from the first stage:

$$\text{trade}_{ij,k} \sim \text{Poisson}\left(\exp\left[\beta_0^T \hat{\tau}_{ij,k} + \beta_1^T (s_i \hat{\tau}_{ij,k}) + \beta_2^T (s_j \hat{\tau}_{ij,k}) + \beta_0^N \hat{n}_{ij,k} + \beta_1^N (s_i \hat{n}_{ij,k}) + \beta_2^N (s_j \hat{n}_{ij,k}) + \mathbf{X}_{ij}\gamma\right]\right)$$

4. **AVE computation:** The AVE of NTMs for each bilateral–product cell is:

  $$\text{AVE}_{ij,k} = -\frac{\hat{\beta}_{ij,k}^N}{\hat{\beta}_{ij,k}^T} \times n_{ij,k}$$

5. **Bootstrap standard errors:** 2,000 stratified balanced bootstrap replications (stratified by HS6 sector) are used to compute standard errors for the six structural coefficients.

6. **Aggregation:** AVEs are trimmed to the 1st–99th percentile, trade-weighted within each importer–exporter–sector cell, and then mapped to the final sector and region aggregation using the crosswalk in `Sectors and Regions for NTM.xlsx`.

---

## Running the Code

Run scripts in the following order:

1. `DATA/BACI/BACI.R` — produces `DATA/BACI/BACI_CLEANED.csv`
2. `DATA/MAcMap-HS6/MAcMap.R` — produces `DATA/MAcMap-HS6/mmhs6_2019_CLEANED_SimpleAverage.csv`
3. `DATA/NTM/NTM Cleaning.R` — produces `DATA/NTM/NTM_CLEANED.csv`
4. `DATA/Three_Clostest_Countries/three_closest_countries.R` — produces `DATA/Three_Clostest_Countries/Three_closest_countries (for stage 1 reg).csv`
5. `Goods/Model (2000 Bootstrap).R` — produces `Output/AVE_codeNew_collapsed.xlsx`

### Required R packages
`readxl`, `dplyr`, `fixest`, `openxlsx`, `stringr`, `combinat`, `boot`, `writexl`, `readr`, `tidyr`

Install all at once:
```r
install.packages(c("readxl", "dplyr", "fixest", "openxlsx", "stringr",
                   "combinat", "boot", "writexl", "readr", "tidyr"))
```
