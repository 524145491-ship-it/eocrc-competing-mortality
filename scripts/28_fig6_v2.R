# ==============================================================================
# Figure 6 v2: Exploratory molecular and genomic triangulation
# Design:
#   A: M1 prevalence in TCGA and ICGC (early vs late), with adjusted ORs.
#   B: TCGA transcriptional score standardized differences (forest).
#   C: ICGC mutation burden and hypermutator proportion (two facets).
#   D: ICGC driver mutation ORs (forest).
#   Canvas 180 x 165 mm; layout AB / CD.
#   Colors early #C4615A, late #384D7A; significant FDR #C4615A.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

# ------------------------------------------------------------------------------
# Panel A: M1 prevalence
# ------------------------------------------------------------------------------
tcga <- fread("results/table6_tcga_m1_v2.csv")
tab7 <- fread("results/table7_icgc_genomic_v2.csv")
icgc_m1 <- tab7[Analysis == "M1 at diagnosis, n (%)"]

m1 <- data.table(
  Cohort = c("TCGA", "TCGA", "ICGC", "ICGC"),
  Group = c("Early-onset", "Late-onset", "Early-onset", "Late-onset"),
  X = c(tcga$Early_M1, tcga$Late_M1, icgc_m1$Early_Value, icgc_m1$Late_Value),
  N = c(tcga$Early_Total, tcga$Late_Total, icgc_m1$N_Early, icgc_m1$N_Late)
)
m1[, Prop := X / N]
p <- m1$Prop; n <- m1$N; z <- 1.96
den <- 1 + z^2 / n
m1[, Lower := (p + z^2/(2*n) - z * sqrt(p*(1-p)/n + z^2/(4*n^2))) / den]
m1[, Upper := (p + z^2/(2*n) + z * sqrt(p*(1-p)/n + z^2/(4*n^2))) / den]
m1[, Group := factor(Group, levels = c("Early-onset", "Late-onset"))]
m1[, Cohort := factor(Cohort, levels = c("TCGA", "ICGC"))]

ann <- data.table(
  Cohort = factor(c("TCGA", "ICGC"), levels = c("TCGA", "ICGC")),
  Label = c(sprintf("aOR %.2f", tcga$OR), sprintf("aOR %.2f", icgc_m1$Effect))
)

p6a <- ggplot(m1, aes(x = Cohort, y = Prop, fill = Group)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), position = position_dodge(width = 0.7),
                width = 0.18, linewidth = 0.45) +
  geom_text(data = ann, aes(x = Cohort, y = 0.30, label = Label), inherit.aes = FALSE,
            size = 2.55, color = "#1E293B", fontface = "bold") +
  scale_fill_manual(values = c("Early-onset" = "#C4615A", "Late-onset" = "#384D7A"), name = NULL) +
  scale_y_continuous(limits = c(0, 0.33), breaks = seq(0, 0.30, by = 0.10),
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = NULL, y = "M1 at diagnosis", title = "M1 prevalence differs across cohorts",
       subtitle = sprintf("Adjusted OR: TCGA %.2f; ICGC %.2f", tcga$OR, icgc_m1$Effect)) +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.1, face = "bold"),
    plot.subtitle = element_text(size = 6.9)
  )

# ------------------------------------------------------------------------------
# Panel B: TCGA transcriptional scores
# ------------------------------------------------------------------------------
sc <- fread("results/table6_tcga_scores_v2.csv")
sc[, SD_Factor := Beta / Std_Difference]
sc[, SD_Lower := Beta_Lower / SD_Factor]
sc[, SD_Upper := Beta_Upper / SD_Factor]
score_order <- c("Mesenchymal", "Immune", "Proliferation", "Epithelial", "Wnt")
sc[, Score := factor(Score, levels = rev(score_order))]

p6b <- ggplot(sc, aes(y = Score, x = Std_Difference)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_segment(aes(x = SD_Lower, xend = SD_Upper, y = Score, yend = Score), linewidth = 0.55, color = "#384D7A") +
  geom_point(size = 2.5, shape = 16, color = "#384D7A") +
  geom_text(aes(x = 0.50, label = sprintf("%.2f", Std_Difference)), hjust = 0,
            size = 2.55, color = "#1E293B") +
  scale_x_continuous(limits = c(-0.55, 0.80), breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  coord_cartesian(clip = "off") +
  labs(x = "Standardized difference (early vs late)", y = NULL,
       title = "TCGA transcriptional programs", subtitle = "Adjusted for stage, site, sex; all FDR > 0.05") +
  theme_mc(width_mm = 85) +
  theme(
    plot.title = element_text(size = 8.1, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    plot.margin = margin(t = 2, r = 8, b = 2, l = 1, "mm")
  )

# ------------------------------------------------------------------------------
# Panel D: ICGC driver mutations
# ------------------------------------------------------------------------------
drv <- tab7[grepl(" mutation, n", Analysis)]
drv[, Gene := sub(" mutation, n.*", "", Analysis)]
drv[, Sig := ifelse(P_FDR < 0.05, "FDR < 0.05", "FDR >= 0.05")]
drv[, Gene := factor(Gene, levels = drv[order(Effect), Gene])]

p6d <- ggplot(drv, aes(y = Gene, x = Effect, color = Sig)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_segment(aes(x = Lower, xend = Upper, y = Gene, yend = Gene), linewidth = 0.55) +
  geom_point(size = 2.6, shape = 18) +
  scale_color_manual(values = c("FDR < 0.05" = "#C4615A", "FDR >= 0.05" = "#384D7A"), name = NULL) +
  scale_x_log10(limits = c(0.2, 16), breaks = c(0.3, 0.5, 1, 2, 4)) +
  coord_cartesian(clip = "off") +
  labs(x = "Odds ratio (log scale)", y = NULL,
       title = "Driver mutations (ICGC)", subtitle = "Exploratory; *FDR < 0.05") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.1, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    plot.margin = margin(t = 2, r = 12, b = 2, l = 1, "mm")
  )

fig6 <- mc_compose(p6a, p6b, p6d,
                   design = "AB\nCC",
                   widths = c(1, 1), heights = c(0.95, 1.15),
                   width_mm = 180, height_mm = 180)
mc_save(fig6, "figures/fig6_molecular_triangulation.png", width_mm = 180, height_mm = 180)
