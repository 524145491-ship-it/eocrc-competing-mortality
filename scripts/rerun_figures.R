# ==============================================================================
# Script: rerun_figures.R
# Purpose: 一键重新渲染并落盘全部主图 (Fig 1-5) 与附图 (Fig S1-S4)
# ==============================================================================

cat(">>> 1. Rerunning 02_iptw_balance.R (Fig 1, Fig S1)...\n")
source("scripts/02_iptw_balance.R")

cat(">>> 2. Rerunning 03_metastasis_patterns.R (Fig 2)...\n")
source("scripts/03_metastasis_patterns.R")

cat(">>> 3. Rerunning 04_time_varying_survival.R (Fig 3)...\n")
source("scripts/04_time_varying_survival.R")

cat(">>> 4. Rerunning 05_conditional_survival.R (Fig 4)...\n")
source("scripts/05_conditional_survival.R")

cat(">>> 5. Rerunning 06_competing_risks.R (Fig 5)...\n")
source("scripts/06_competing_risks.R")

cat(">>> 6. Rerunning 07_subgroup_interactions.R (Fig S3, Fig S4)...\n")
source("scripts/07_subgroup_interactions.R")

cat(">>> ALL FIGURES RE-RENDERED PERFECTLY!\n")
