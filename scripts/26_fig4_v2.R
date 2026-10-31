# ==============================================================================
# Figure 4 v2: Dynamic conditional survival in Stage I-III
# Design:
#   A: 5-year conditional overall survival by accrued survival time.
#   B: 5-year conditional cancer-specific survival.
#   C: stage-stratified heatmap of 5-year conditional overall survival.
#   Canvas 180 x 170 mm; layout AB / CC.
#   Colors EOCRC #C4615A, LOCRC #384D7A; heatmap low #C4615A, mid #E7D894,
#   high #384D7A.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

cs <- fread("results/table_conditional_survival_v2.csv")
cs[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]

make_cs <- function(outcome_label, title, ylim, show_legend = FALSE) {
  d <- cs[Outcome == outcome_label]
  lab <- d[Survived_Years == 5]
  lab[, Label := sprintf("%.1f%%", CS_Rate * 100)]
  lab[, Label_Y := CS_Rate + ifelse(Cohort == "Early-Onset (<50)", 0.018, -0.018)]
  ggplot(d, aes(x = Survived_Years, y = CS_Rate, color = Cohort, shape = Cohort)) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 2.5, stroke = 0.4) +
    geom_text(data = lab, aes(y = Label_Y, label = Label), size = 2.5, fontface = "bold",
              hjust = 0, vjust = 0.5, show.legend = FALSE) +
    scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = NULL) +
    scale_shape_manual(values = c("Early-Onset (<50)" = 16, "Late-Onset (>=50)" = 17), name = NULL) +
    scale_x_continuous(breaks = 0:5, labels = paste0(0:5, " y"), limits = c(-0.2, 6.2)) +
    scale_y_continuous(limits = ylim, breaks = seq(ylim[1], ylim[2], by = 0.1),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Years already survived", y = "5-year conditional survival",
         title = title, subtitle = "Stage I-III; IPTW-weighted") +
    theme_mc(width_mm = 88) +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
      legend.key.size = unit(3.5, "mm"),
      plot.title = element_text(size = 8.3, face = "bold"),
      plot.subtitle = element_text(size = 6.9)
    )
}

p4a <- make_cs("5-Year Overall Survival", "Conditional overall survival", c(0.65, 0.97), TRUE)
p4b <- make_cs("5-Year Cancer-Specific Survival", "Conditional cancer-specific survival", c(0.65, 0.97), FALSE)

# Panel C: stage-stratified heatmap
stage <- fread("results/table_conditional_survival_stage_v2.csv")
order_rows <- c("Stage III LOCRC", "Stage III EOCRC", "Stage II LOCRC", "Stage II EOCRC",
                "Stage I LOCRC", "Stage I EOCRC")
stage[, Row_Label := factor(Row_Label, levels = order_rows)]
stage[, Survived_Col := factor(paste0("+", Survived_Years, " y"),
                               levels = paste0("+", 0:5, " y"))]

p4c <- ggplot(stage, aes(x = Survived_Col, y = Row_Label, fill = CS_Rate)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", CS_Rate * 100)),
            size = 2.6, fontface = "bold",
            color = ifelse(stage$CS_Rate > 0.84, "white", "#1E293B")) +
  scale_fill_gradient2(low = "#C4615A", mid = "#E7D894", high = "#384D7A",
                       midpoint = 0.80, limits = c(0.60, 0.95),
                       breaks = c(0.65, 0.75, 0.85, 0.95),
                       labels = scales::percent_format(accuracy = 1), name = "5-year COS") +
  labs(x = "Years already survived", y = NULL,
       title = "Stage-specific conditional overall survival",
       subtitle = "Rows ordered from Stage I to Stage III; EOCRC vs LOCRC") +
  theme_mc(width_mm = 180) +
  theme(
    axis.text.x = element_text(size = 7.2, color = "black"),
    axis.text.y = element_text(size = 7.2, color = "black"),
    legend.position = "right",
    legend.key.height = unit(12, "mm"),
    legend.key.width = unit(4, "mm"),
    legend.text = element_text(size = 7.0),
    legend.title = element_text(size = 7.2, face = "bold"),
    plot.title = element_text(size = 8.3, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    plot.margin = margin(t = 2, r = 6, b = 5, l = 1, "mm")
  )

fig4 <- mc_compose(p4a, p4b, p4c,
                   design = "AB\nCC",
                   heights = c(0.95, 1.05),
                   width_mm = 180, height_mm = 180)
mc_save(fig4, "figures/fig4_conditional_survival.png", width_mm = 180, height_mm = 180)
