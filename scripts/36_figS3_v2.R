# ==============================================================================
# Figure S3 v2: Subgroup hazard ratios for cancer-specific survival
# Design: one panel, faceted by subgroup; IPTW-weighted within-subgroup HRs;
# reference line at 1; log x; labels with HR (95% CI).
# Canvas 180 x 120 mm.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

source("scripts/mc_figure.R")

sg <- fread("results/tableS3_subgroup_interactions_v2.csv")
sg <- sg[Outcome == "CSS" & !is.na(HR)]
sg[, Level := factor(Level, levels = unique(Level))]
sg[, Subgroup := factor(Subgroup, levels = c("AJCC stage", "Sex", "Primary site", "Histology", "Diagnosis era"))]

p <- ggplot(sg, aes(x = HR, y = Level)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_segment(aes(x = Lower, xend = Upper, y = Level, yend = Level), linewidth = 0.55, color = "#384D7A") +
  geom_point(size = 2.4, shape = 16, color = "#384D7A") +
  facet_wrap(~ Subgroup, scales = "free_y", ncol = 2) +
  scale_x_log10(limits = c(0.40, 1.25), breaks = c(0.5, 0.7, 1.0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Hazard ratio (log scale)", y = NULL,
       title = "Subgroup hazard ratios for cancer-specific survival",
       subtitle = "IPTW-weighted; reference line at 1; P for interaction in Table S4") +
  theme_mc(width_mm = 180) +
  theme(
    strip.text = element_text(size = 7.2, face = "bold"),
    axis.text.y = element_text(size = 7.0, color = "black"),
    plot.title = element_text(size = 8.5, face = "bold"),
    plot.subtitle = element_text(size = 7.0),
    plot.margin = margin(t = 2, r = 12, b = 2, l = 1, "mm")
  )

mc_save(p, "manuscript/supplementary/figS3_subgroup_forest.png", width_mm = 180, height_mm = 170)
