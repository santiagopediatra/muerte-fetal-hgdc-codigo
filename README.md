# Stillbirth Recorded in Singleton Pregnancies — HGDC, Quito, 2018–2023

Analysis pipeline and aggregated results for the study: *Factors Associated with Recorded Stillbirth in Singleton Gestations at Hospital General Docente Calderón, Quito, Ecuador, 2018–2023*.

> ### Data Governance and Access Notice
> - **This repository does not contain individual patient data or primary microdata.**
> - The primary database is institutional property of the **Hospital General Docente Calderón (HGDC)** and is not publicly distributed under any format.
> - Access to primary data requires prior institutional authorization and ethics approval from the HGDC Institutional Ethics Committee (CEISH-HGDC). Inquiries should be directed to the HGDC Research and Teaching Unit.
> - Without authorized local data access, users can inspect the complete analysis pipeline, technical documentation, and aggregate statistical outputs in `results/`.

---

## Study Objective and Design

- **Objective:** Evaluate associations between recorded maternal, sociodemographic, and obstetric factors and recorded stillbirth in singleton gestations attended at HGDC.
- **Design:** Cross-sectional, retrospective observational study utilizing anonymized electronic hospital records (2018–2023).
- **Unit of Analysis:** Individual obstetric event.
- **Primary Outcome:** Recorded intrauterine death (`MUERTE_INTRAUTERINA`).
- **Target Population:** Singleton pregnancies with gestational age $\ge$ 22 weeks (or birthweight $\ge$ 500 g if gestational age is missing).

---

## Repository Structure

```text
├── config/                  # Configuration parameters and environment versions
├── docs/                    # Methodological, reproducibility, and disclosure reviews
├── results/                 # Aggregated tabular results, model summaries, and figures
│   ├── figuras/             # Flowcharts and analytical plots (PNG/PDF)
│   ├── informes/            # STROBE compliance and technical audit summaries
│   └── tablas/              # Primary and complementary regression tables (CSV)
├── scripts/                 # Reproducible R analysis pipeline
│   ├── analisis_complementarios/
│   ├── reconstruccion_reanalisis/
│   ├── revision_cientifica_final/
│   └── verificacion/
├── CITATION.cff             # Machine-readable citation file
├── renv.lock                # R package dependency lockfile
└── README.md
