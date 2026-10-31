# ==============================================================================
# Figure 3 v2: Survival dynamics in Stage I-III
# Design:
#   A: IPTW-weighted OS Kaplan-Meier (0-120 months).
#   B: IPTW-weighted CSS Kaplan-Meier.
#   C: interval-specific cancer-specific HR (log scale, reference 1).
#   D: RMST absolute difference at 5 and 10 years (months).
#   Canvas 180 x 165 mm; 2 x 2; colors EOCRC #C4615A, LOCRC #384D7A;
#   endpoint colors OS #384D7A, CSS #C4615A.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

df <- readRDS("data/cohort_iii_weighted_v2.rds")
surv_tab <- fread("results/table3_survival_models_v2.csv")
os_row <- surv_tab[Model == "OS: IPTW-weighted Cox"]
css_row <- surv_tab[Model == "CSS: IPTW-weighted Cox"]
os_txt <- sprintf("IPTW HR %.2f
(%.2f-%.2f)", os_row$HR, os_row$Lower, os_row$Upper)
css_txt <- sprintf("IPTW HR %.2f
(%.2f-%.2f)", css_row$HR, css_row$Lower, css_row$Upper)

km_df <- function(event_var) {
  fit <- survfit(as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ Age_Group")),
                 data = df, weights = iptw_w)
  d <- data.frame(
    time = fit$time,
    surv = fit$surv,
    strata = rep(names(fit$strata), fit$strata)
  )
  d$strata <- gsub("Age_Group=", "", d$strata)
  d <- d[d$time <= 120, ]
  rbind(
    data.frame(time = 0, surv = 1, strata = unique(d$strata)),
    d
  )
}

os_df <- km_df("OS_Status")
css_df <- km_df("CSS_Status")

make_km <- function(d, title, hr_text, show_legend = FALSE) {
  d$strata <- factor(d$strata, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))
  ggplot(d, aes(x = time, y = surv, color = strata)) +
    geom_step(linewidth = 0.65) +
    scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = NULL) +
    scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Months since diagnosis", y = "Survival probability",
         title = title, subtitle = "Stage I-III; IPTW-weighted") +
    annotate("text", x = 62, y = 0.12, label = hr_text, hjust = 0, size = 2.7,
             color = "#1E293B", lineheight = 1.05) +
    theme_mc(width_mm = 85) +
    theme(
      legend.position = if (show_legend) c(0.28, 0.18) else "none",
      legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
      legend.key.size = unit(3.5, "mm"),
      plot.title = element_text(size = 8.2, face = "bold"),
      plot.subtitle = element_text(size = 6.9)
    )
}

p3a <- make_km(os_df, "Overall survival", os_txt, TRUE)
p3b <- make_km(css_df, "Cancer-specific survival", css_txt, FALSE)

# Panel C: interval-specific CSS HR
tv <- fread("results/fig3c_timevarying_v2.csv")
tv <- tv[Endpoint == "CSS"]
tv[, Interval := factor(Interval, levels = c("0-12 months", "12-36 months", "36-60 months",
                                             "60-120 months", ">120 months"),
                        labels = c("0-1 y", "1-3 y", "3-5 y", "5-10 y", ">10 y"))]
tv[, X := as.integer(Interval)]

p3c <- ggplot(tv, aes(x = X, y = HR)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = "#C4615A", alpha = 0.18) +
  geom_line(color = "#C4615A", linewidth = 0.65) +
  geom_point(color = "#C4615A", size = 2.4, shape = 16) +
  geom_text(aes(y = 1.18, label = sprintf("%.2f", HR)), size = 2.55, color = "#1E293B", fontface = "bold") +
  scale_x_continuous(breaks = 1:5, labels = levels(tv$Interval), limits = c(0.7, 5.3)) +
  scale_y_log10(breaks = c(0.4, 0.6, 0.8, 1.0), limits = c(0.32, 1.25)) +
  labs(x = "Follow-up interval", y = "Cancer-specific HR (log scale)",
       title = "Time-varying cancer-specific HR", subtitle = "IPTW-weighted interval-specific estimates") +
  theme_mc(width_mm = 85) +
  theme(
    plot.title = element_text(size = 8.2, face = "bold"),
    plot.subtitle = element_text(size = 6.9)
  )

# Panel D: RMST differences
rmst <- fread("results/tableS5_rmst_v2.csv")
rmst[, Label := paste0(Endpoint, ", ", ifelse(Tau_Months == 60, "5-year", "10-year"))]
rmst[, Label := factor(Label, levels = c("OS, 5-year", "CSS, 5-year", "OS, 10-year", "CSS, 10-year"))]

p3d <- ggplot(rmst, aes(x = Difference, y = Label, color = Endpoint)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_errorbar(aes(xmin = Diff_Lower, xmax = Diff_Upper), width = 0.2, linewidth = 0.55) +
  geom_point(size = 2.6, shape = 18) +
  geom_text(aes(x = 20.5, label = sprintf("%.1f (%.1f-%.1f)", Difference, Diff_Lower, Diff_Upper)),
            hjust = 0, size = 2.5, color = "#1E293B") +
  scale_color_manual(values = c("OS" = "#384D7A", "CSS" = "#C4615A"), name = NULL) +
  scale_x_continuous(limits = c(0, 27), breaks = seq(0, 25, by = 5)) +
  coord_cartesian(clip = "off") +
  labs(x = "Restricted mean survival time difference (months)", y = NULL,
       title = "Absolute survival difference (RMST)", subtitle = "Early- vs late-onset; months gained") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.2, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    plot.margin = margin(t = 2, r = 10, b = 2, l = 1, "mm")
  )

fig3 <- mc_compose(p3a, p3b, p3c, p3d,
                   design = "AB\nCD",
                   width_mm = 180, height_mm = 165)
mc_save(fig3, "figures/fig3_survival_dynamics.png", width_mm = 180, height_mm = 165)
