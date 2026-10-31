# ==============================================================================
# Figure S1 v2: Propensity score distributions before and after stabilized IPTW
# Design: two panels side by side, same axes; (a) unweighted distribution by age
#   group, (b) the same cohort reweighted by the stabilized IPTW actually used in
#   the main analysis. Panel letters and legend handled by mc_compose/mc_save.
# Input: data/cohort_iii_weighted_v2.rds (ps, iptw_w, Age_Group) — produced by
#   scripts/12_iptw_balance_v2.R; no re-fitting happens here.
# Canvas: 180 x 80 mm.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

# 单独运行时自己读权重文件；从 12_iptw_balance_v2.R 里 source 时沿用内存中的对象
if (!exists("df_iii_w")) df_iii_w <- readRDS("data/cohort_iii_weighted_v2.rds")

pal <- c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A")

ps_panel <- function(dt, weighted, title, show_y, show_legend = TRUE) {
  g <- ggplot(dt, aes(x = ps, fill = Age_Group, colour = Age_Group))
  g <- if (weighted) g + geom_density(aes(weight = iptw_w), alpha = 0.35, linewidth = 0.4, adjust = 1.2)
       else          g + geom_density(alpha = 0.35, linewidth = 0.4, adjust = 1.2)
  g +
    scale_fill_manual(values = pal, name = NULL) +
    scale_colour_manual(values = pal, name = NULL) +
    labs(x = "Propensity score", y = if (show_y) "Density" else NULL, title = title) +
    theme_mc(width_mm = 88) +
    theme(legend.position = if (show_legend) "bottom" else "none",
          plot.title = element_text(size = 8.0, face = "bold", hjust = 0),
          plot.margin = margin(t = 2, r = 2, b = 1, l = if (show_y) 1 else 6, "mm"))
}

# 图例只放在左侧面板下方：两格各自放一份会重复，收拢到画布右侧又会把两个面板挤窄
p_before <- ps_panel(df_iii_w, FALSE, "Before weighting", TRUE, show_legend = TRUE)
p_after  <- ps_panel(df_iii_w, TRUE,  "After stabilized IPTW", FALSE, show_legend = FALSE)

# 两个面板共用纵轴范围：分别量出各自的密度峰值再取最大，避免同一个分布在两格里的高度不可比
peak <- function(p) max(unlist(lapply(ggplot_build(p)$data, function(d) d$ymax)), na.rm = TRUE)
ymax <- max(peak(p_before), peak(p_after)) * 1.04
p_before <- p_before + coord_cartesian(ylim = c(0, ymax))
p_after  <- p_after  + coord_cartesian(ylim = c(0, ymax))

figS1 <- mc_compose(p_before, p_after, ncol = 2, widths = c(1, 1), legend = "keep",
                    width_mm = 180, height_mm = 80)
mc_save(figS1, "manuscript/supplementary/figS1_ps_density.png", width_mm = 180, height_mm = 80)
