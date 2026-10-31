# ==============================================================================
# Figure 1 v2: Study cohort selection and IPTW covariate balance
# Design:
#   Panel A: CONSORT-style flow with adult restriction and Stage I-III primary set.
#   Panel B: Love plot of absolute SMDs before/after IPTW in the Stage I-III cohort.
#   Main panel: B (the balance result); A provides cohort provenance.
#   Canvas: 180 mm x 115 mm; two panels, A wider (1.35:1).
#   Palette: unadjusted #C4615A, adjusted #384D7A; exclusion boxes light red.
#   No "Figure 1" title inside the image.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

# ------------------------------------------------------------------------------
# Panel A: CONSORT flow
# ------------------------------------------------------------------------------
# 框的尺寸按文字实际占位定（面板数据区 83.8 × 103.9mm，1 个 x 单位 ≈ 0.84mm、1 个 y 单位 ≈ 1.04mm）：
# 最长一行 39mm 宽 → 框宽 53 单位（≈44mm）留出约 2.7mm 左右内边距；三行文字高 8.1mm → 框高 12 单位（≈12.5mm）留出约 2.2mm 上下内边距
main_x <- 28.5; main_w <- 53
main_boxes <- data.frame(
  x = main_x, y = c(92, 78, 64, 50, 36, 22), w = main_w,
  label = c(
    "SEER colorectal cancer\n2000-2022\n654,149 records (631,157 patients)",
    "First primary malignancy\nN = 530,027",
    "Survival > 0 months\nN = 504,397",
    "Colorectal adenocarcinoma\nN = 466,424",
    "Age >= 18 years\nN = 466,322",
    "Stage I-III primary set\nN = 365,382\nEOCRC 43,957 | LOCRC 321,425"
  )
)
# 行数多的框加高：文字上下都要留出与两行框相同的空白
main_boxes$h <- ifelse(lengths(strsplit(main_boxes$label, "\n")) >= 3, 12, 9)

excl_x <- 78.5; excl_w <- 39.5
excl_boxes <- data.frame(
  x = excl_x, y = c(85, 71, 57, 43), w = excl_w, h = 9,
  label = c(
    "Prior cancer\nn = 124,122",
    "Survival <= 0\nn = 25,630",
    "Non-adenocarcinoma\nn = 37,973",
    "Age < 18 years\nn = 102"
  )
)

# 箭头按框的实际边缘算，改框尺寸时不用再手调坐标
main_arrows <- data.frame(
  x = main_x,
  y0 = head(main_boxes$y - main_boxes$h / 2, -1),
  y1 = tail(main_boxes$y + main_boxes$h / 2, -1)
)
excl_arrows <- data.frame(
  y = excl_boxes$y,
  x0 = main_x + main_w / 2,
  x1 = excl_x - excl_w / 2
)

p1a <- ggplot() +
  geom_rect(data = main_boxes,
            aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.5) +
  geom_text(data = main_boxes, aes(x = x, y = y, label = label),
            size = 2.5, lineheight = 1.05, color = "#1E293B") +
  geom_rect(data = excl_boxes,
            aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.45) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label),
            size = 2.5, lineheight = 1.05, color = "#991B1B") +
  # vertical main arrows
  geom_segment(data = main_arrows, aes(x = x, xend = x, y = y0, yend = y1),
               arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # horizontal exclusion arrows
  geom_segment(data = excl_arrows, aes(x = x0, xend = x1, y = y, yend = y),
               arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  annotate("text", x = 50, y = 8,
           label = "Secondary sets: Stage IV n = 87,185; unknown stage n = 13,755\n2010-2022 metastatic subcohort n = 256,312",
           size = 2.5, color = "#475569", hjust = 0.5, lineheight = 1.1) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Study cohort selection") +
  theme_void() +
  theme(
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2.5, unit = "mm")),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, "mm")
  )

# ------------------------------------------------------------------------------
# Panel B: Love plot
# ------------------------------------------------------------------------------
bal <- fread("results/tableS2_iptw_balance_v2.csv")
bal <- bal[!Variable %in% c("distance", "prop.score")]
bal[, Abs_Diff_Un := abs(Diff.Un)]
setorder(bal, -Abs_Diff_Un)
top <- head(bal, 12)

label_map <- c(
  "Sex_Male" = "Male sex",
  "Subsite_Right colon" = "Right colon",
  "Subsite_Left colon" = "Left colon",
  "Subsite_Rectum" = "Rectum",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Race_Non-Hispanic Black" = "Non-Hispanic Black",
  "Race_Hispanic" = "Hispanic",
  "Race_Asian/PI" = "Asian/PI",
  "Chemo_Yes" = "Chemotherapy",
  "Radiation_Yes" = "Radiation",
  "Stage_Stage I" = "Stage I",
  "Stage_Stage II" = "Stage II",
  "Stage_Stage III" = "Stage III",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "LN_Examined_0 nodes" = "0 LNs examined",
  "Year_Group_2000-2005" = "Diagnosed 2000-2005",
  "Year_Group_2006-2010" = "Diagnosed 2006-2010",
  "Year_Group_2011-2015" = "Diagnosed 2011-2015",
  "Year_Group_2016-2022" = "Diagnosed 2016-2022",
  "Histology_Mucinous/Signet ring" = "Mucinous/signet ring",
  "Grade_Poor/Undifferentiated" = "Poorly differentiated",
  "Grade_Unknown" = "Grade unknown",
  "Surgery_Yes" = "Surgery"
)
top[, Clean_Var := label_map[Variable]]
top[is.na(Clean_Var), Clean_Var := Variable]

love_long <- melt(
  top,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Sample",
  value.name = "SMD"
)
love_long[, Sample := factor(ifelse(Sample == "Diff.Un", "Unadjusted", "IPTW-adjusted"),
                             levels = c("Unadjusted", "IPTW-adjusted"))]
love_long[, Abs_SMD := abs(SMD)]
love_long[, Clean_Var := factor(Clean_Var, levels = rev(top$Clean_Var))]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = Clean_Var, color = Sample, shape = Sample)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray60", linewidth = 0.3) +
  geom_point(size = 2.2, stroke = 0.4) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-adjusted" = "#384D7A"), name = NULL) +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-adjusted" = 17), name = NULL) +
  scale_x_continuous(limits = c(0, 0.28), breaks = seq(0, 0.25, by = 0.05),
                     expand = expansion(mult = c(0.035, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.2))) +
  coord_cartesian(clip = "off") +
  # 副标题里的变量数与水平数直接取自平衡性结果表，避免正文/表与图对不上
  labs(x = "Absolute standardized mean difference", y = NULL,
       title = "All covariates balanced below SMD 0.05",
       subtitle = sprintf("Stage I-III cohort; %d covariates, %d levels",
                          length(unique(sub("_.*$", "", bal$Variable))), nrow(bal))) +
  annotate("text", x = 0.055, y = 12.6, label = "Threshold 0.05", size = 2.5,
           color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 75) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 7.0, hjust = 0),
    axis.text.y = element_text(size = 7.1, color = "black"),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 1, "mm")
  )

# ------------------------------------------------------------------------------
# Compose and save
# ------------------------------------------------------------------------------
fig1 <- mc_compose(wrap_elements(full = p1a), p1b,
                   ncol = 2, widths = c(1.35, 1.0),
                   width_mm = 180, height_mm = 115)
mc_save(fig1, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 115)
