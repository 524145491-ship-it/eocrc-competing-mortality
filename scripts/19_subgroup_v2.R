# ==============================================================================
# Script: 19_subgroup_v2.R
# Purpose: v2 S8 - I-III 期亚组分层与交互作用
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cleaned_cohort_full_v2.rds (ID 衔接)
# Outputs:
#   - results/tableS3_subgroup_interactions_v2.csv
#   - results/figS3_subgroup_forest_v2.csv
# Methods:
#   - 交互检验: coxph(Surv ~ EOCRC * subgroup + 协变量, weights=iptw_w, robust=TRUE)，联合 Wald
#   - 亚组 HR: 各层内 coxph(Surv ~ EOCRC, weights=iptw_w, robust=TRUE)
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
})

source("scripts/mc_trace.R")

set.seed(20261029)

df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df <- readRDS("data/cohort_iii_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")
stopifnot(all(df$Patient_ID %in% df_full$Patient_ID))

# 亚组变量
df[, Stage_Bin := ifelse(Stage %in% c("Stage I", "Stage II"), "Stage I-II", "Stage III")]
df[, Era := ifelse(Year_DX <= 2009, "2000-2009", "2010-2022")]

subgroups <- list(
  list(var = "Subsite", name = "Primary site", levels = c("Right colon", "Left colon", "Rectum")),
  list(var = "Stage_Bin", name = "AJCC stage", levels = c("Stage I-II", "Stage III")),
  list(var = "Sex", name = "Sex", levels = c("Female", "Male")),
  list(var = "Histology", name = "Histology", levels = c("Classic adenocarcinoma", "Mucinous/Signet ring")),
  list(var = "Era", name = "Diagnosis era", levels = c("2000-2009", "2010-2022"))
)

covars <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade", "Stage",
            "Surgery", "Chemo", "Radiation", "LN_Examined")

extract_hr <- function(fit, term = "EOCRC") {
  s <- summary(fit)$coefficients
  est <- s[term, "coef"]
  se <- if ("robust se" %in% colnames(s)) s[term, "robust se"] else s[term, "se(coef)"]
  data.table(HR = exp(est), Lower = exp(est - 1.96 * se), Upper = exp(est + 1.96 * se),
             P = s[term, "Pr(>|z|)"])
}

wald_interaction <- function(fit, var) {
  terms <- grep(paste0("EOCRC:", var), names(coef(fit)), value = TRUE)
  if (length(terms) == 0) return(NA_real_)
  b <- coef(fit)[terms]
  V <- vcov(fit)[terms, terms, drop = FALSE]
  ok <- !is.na(b)
  if (!any(ok)) return(NA_real_)
  b <- b[ok]; V <- V[ok, ok, drop = FALSE]
  W <- as.numeric(t(b) %*% solve(V) %*% b)
  pchisq(W, df = length(b), lower.tail = FALSE)
}

run_subgroups <- function(event_var, outcome_label) {
  out <- list()
  for (sg in subgroups) {
    v <- sg$var; vname <- sg$name; lvls <- sg$levels
    covars_adj <- setdiff(covars, if (v == "Stage_Bin") "Stage" else if (v == "Era") "Year_Group" else v)
    f_int <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ EOCRC * ", v, " + ",
                               paste(covars_adj, collapse = " + ")))
    fit_int <- tryCatch(coxph(f_int, data = df, weights = iptw_w, robust = TRUE), error = function(e) NULL)
    p_int <- if (is.null(fit_int)) NA_real_ else wald_interaction(fit_int, v)

    for (l in lvls) {
      sub <- df[df[[v]] == l]
      n <- nrow(sub)
      ev <- sum(sub[[event_var]] == 1, na.rm = TRUE)
      if (n < 50 || ev < 20) {
        out[[paste(outcome_label, v, l)]] <- data.table(
          Outcome = outcome_label, Subgroup = vname, Level = l, N = n, Events = ev,
          HR = NA_real_, Lower = NA_real_, Upper = NA_real_, P = NA_real_,
          P_interaction = p_int
        )
        next
      }
      f_sub <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ EOCRC"))
      fit_sub <- tryCatch(coxph(f_sub, data = sub, weights = iptw_w, robust = TRUE), error = function(e) NULL)
      if (is.null(fit_sub)) {
        out[[paste(outcome_label, v, l)]] <- data.table(
          Outcome = outcome_label, Subgroup = vname, Level = l, N = n, Events = ev,
          HR = NA_real_, Lower = NA_real_, Upper = NA_real_, P = NA_real_,
          P_interaction = p_int
        )
      } else {
        hr <- extract_hr(fit_sub)
        out[[paste(outcome_label, v, l)]] <- data.table(
          Outcome = outcome_label, Subgroup = vname, Level = l, N = n, Events = ev,
          HR = hr$HR, Lower = hr$Lower, Upper = hr$Upper, P = hr$P,
          P_interaction = p_int
        )
      }
    }
  }
  rbindlist(out)
}

cat("Running subgroup analyses for CSS and OS...\n")
res <- rbind(run_subgroups("CSS_Status", "CSS"), run_subgroups("OS_Status", "OS"))
res[, HR_Str := ifelse(is.na(HR), "-", sprintf("%.2f (%.2f-%.2f)", HR, Lower, Upper))]
res[, P_Str := ifelse(is.na(P), "-", ifelse(P < 0.001, "<0.001", sprintf("%.3f", P)))]
res[, P_int_Str := ifelse(is.na(P_interaction), "-", ifelse(P_interaction < 0.001, "<0.001", sprintf("%.3f", P_interaction)))]
fwrite(res, "results/tableS3_subgroup_interactions_v2.csv")
fwrite(res, "results/figS3_subgroup_forest_v2.csv")

print(res[, .(Outcome, Subgroup, Level, N, Events, HR_Str, P_Str, P_int_Str)])

cat("\nS8 v2 completed successfully.\n")
