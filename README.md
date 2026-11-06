# Approval-time variability analysis: IVDR PSAs and CTR CTAs

## Overview

This repository contains the R code used for the retrospective analysis of submission-to-approval times for AstraZeneca-sponsored oncology performance study applications (PSAs) under the In Vitro Diagnostic Medical Devices Regulation (IVDR) and clinical trial applications (CTAs) under the Clinical Trial Regulation (CTR).

The analysis covers eligible applications submitted to EU Member States between 2023 and 2025. The code includes cohort preparation, descriptive summaries, statistical analyses, sensitivity analyses, and generation of the manuscript tables and figures.

## Data availability and confidentiality

The underlying application-level data are **not included** in this repository. They are derived from AstraZeneca’s internal regulatory submissions database and are not publicly available because of confidentiality and commercial sensitivity.

Consequently, the analyses and outputs cannot be reproduced from this repository alone. Running the code requires authorized access to the analysis dataset and the necessary permissions to use it. Do not add confidential data, derived records, or identifiable country information to this repository or share them outside approved channels.

For information about the data, refer to the manuscript’s **Data Availability** statement or contact the corresponding author.

## Contents

- `analysis.R` — R script for importing and preparing the data, conducting analyses, and producing manuscript tables and figures.
- `README.md` — this file.
- `IVDR_PSA_CTA_Publication_Analysis.csv` — **not provided**; confidential input data required by the script.

The script may save tables and figures to the current working directory, including a `table_exports/` folder.

## Input data requirements

The script expects a CSV file named:
  
  ```text
IVDR_PSA_CTA_Publication_Analysis.csv