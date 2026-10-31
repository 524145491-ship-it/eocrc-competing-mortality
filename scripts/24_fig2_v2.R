# ==============================================================================
# Figure 2 v2: Organ-specific metastatic tropism and continuous age response
# Design:
#   Panel A: dumbbell plot of crude organ-specific metastasis rates by age group.
#   Panel B: forest plot of adjusted ORs (log scale); main panel.
#   Panel C: continuous-age spline OR curves for any, liver, isolated lung.
#   Canvas 180 x 155 mm; layout A/B on top, C spanning bottom.
#   Colors: EOCRC #C4615A, LOCRC #384D7A; RCS any #384D7A, liver #C4615A,
#           isolated lung #65B9B7.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

met <- fread("results/table2_metastasis_logistic_v2.csv")
ord <- c("Any Distant Metastasis", "Liver Metastasis (Overall)", "Isolated Liver Metastasis",
         "Lung Metastasis (Overall)", "Isolated Lung Metastasis", "Bone Metastasis",
         "Brain Metastasis", "Multiple Organ Metastases (>=2)")
met[, Endpoint := factor(Endpoint, levels = rev(ord))]

# ------------------------------------------------------------------------------
# Panel A: rates (dumbbell, log x)
# ------------------------------------------------------------------------------
rate_long <- melt(
  met[, .(Endpoint, EOCRC_Rate, LOCRC_Rate)],
  id.vars = "Endpoint", variable.name = "Cohort", value.name = "Rate"
)
rate_long[, Cohort := factor(ifelse(Cohort == "EOCRC_Rate", "Early-onset (<50)", "Late-onset (>=50)"),
                             levels = c("Early-onset (<50)", "Late-onset (>=50)"))]

p2a <- ggplot(rate_long, aes(x = Rate, y = Endpoint)) +
  geom_line(aes(group = Endpoint), color = "grey75", linewidth = 0.45) +
  geom_point(aes(color = Cohort, shape = Cohort), size = 2.5, stroke = 0.35) +
  scale_color_manual(values = c("Early-onset (<50)" = "#C4615A", "Late-onset (>=50)" = "#384D7A"), name = NULL) +
  scale_shape_manual(values = c("Early-onset (<50)" = 16, "Late-onset (>=50)" = 17), name = NULL) +
  scale_x_log10(limits = c(0.15, 40), breaks = c(0.2, 1, 5, 20),
                labels = function(x) paste0(x, "%")) +
  labs(x = "Prevalence at diagnosis (log scale)", y = NULL,
       title = "Metastasis by age group",
       subtitle = "2010-2022; n = 256,312") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.0, color = "black"),
    plot.title = element_text(size = 8.3, face = "bold"),
    plot.subtitle = element_text(size = 7.0)
  )

# ------------------------------------------------------------------------------
# Panel B: adjusted OR forest
# ------------------------------------------------------------------------------
p2b <- ggplot(met, aes(y = Endpoint)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_segment(aes(x = Adjusted_OR_Lower, xend = Adjusted_OR_Upper, y = Endpoint, yend = Endpoint),
               linewidth = 0.55, color = "#384D7A") +
  geom_point(aes(x = Adjusted_OR), size = 2.5, shape = 18, color = "#384D7A") +
  geom_text(aes(x = 1.85, label = Adjusted_OR_Str), hjust = 0, size = 2.5,
            color = "#1E293B") +
  scale_x_log10(limits = c(0.65, 4.0), breaks = c(0.8, 1.0, 1.25, 1.5)) +
  coord_cartesian(clip = "off") +
  labs(x = "Adjusted odds ratio (log scale)", y = NULL,
       title = "Adjusted odds ratios",
       subtitle = "Reference: late-onset; adjusted for covariates") +
  theme_mc(width_mm = 88) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 8.3, face = "bold"),
    plot.subtitle = element_text(size = 6.8),
    plot.margin = margin(t = 2, r = 14, b = 2, l = 1, "mm")
  )

# ------------------------------------------------------------------------------
# Panel C: continuous-age RCS
# ------------------------------------------------------------------------------
rcs <- fread("results/fig2c_rcs_curves_v2.csv")
keep <- c("Any Distant Metastasis", "Liver Metastasis (Overall)", "Isolated Lung Metastasis")
rcs <- rcs[Endpoint %in% keep]
rcs[, Endpoint := factor(Endpoint, levels = keep,
                         labels = c("Any distant metastasis", "Liver metastasis", "Isolated lung metastasis"))]

p2c <- ggplot(rcs, aes(x = Age, y = OR, color = Endpoint, fill = Endpoint)) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 50, linetype = "dotted", color = "grey45", linewidth = 0.4) +
  scale_color_manual(values = c("Any distant metastasis" = "#384D7A",
                                "Liver metastasis" = "#C4615A",
                                "Isolated lung metastasis" = "#65B9B7"), name = NULL) +
  scale_fill_manual(values = c("Any distant metastasis" = "#384D7A",
                               "Liver metastasis" = "#C4615A",
                               "Isolated lung metastasis" = "#65B9B7"), name = NULL) +
  scale_x_continuous(breaks = seq(20, 80, by = 10), limits = c(18, 90)) +
  scale_y_log10(breaks = c(0.5, 0.75, 1, 1.5, 2)) +
  coord_cartesian(ylim = c(0.35, 3.0)) +
  labs(x = "Age at diagnosis (years)", y = "Odds ratio vs age 50 (log scale)",
       title = "Continuous age response differs by metastatic site",
       subtitle = "Restricted cubic spline, 5 knots; reference age 50") +
  theme_mc(width_mm = 180) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.5, face = "bold"),
    plot.subtitle = element_text(size = 7.0)
  )

# ------------------------------------------------------------------------------
# Compose
# ------------------------------------------------------------------------------
fig2 <- mc_compose(p2a, p2b, p2c,
                   design = "AB\nCC",
                   heights = c(0.85, 1.15),
                   width_mm = 180, height_mm = 185)
mc_save(fig2, "figures/fig2_metastasis_tropism.png", width_mm = 180, height_mm = 185)
