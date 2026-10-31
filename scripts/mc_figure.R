# mc_figure.R — MedCortex 科研图辅助：规格 → 字号 → 配色 → 拼图 → 落盘 → 审计
# 用法：source("scripts/mc_figure.R")（开始分析追踪时软件放进项目；也可 source 技能目录里的同名文件）；函数说明见 SKILL.md。
# 依赖：ggplot2、colorspace、png、patchwork（必装）；ragg、systemfonts、jsonlite（有则用）。
# 设计原则（2026-10-12 用户定）：审计只报能算出来的事实；"硬拦"只留会让图误导读者的数据错误（数据行/区间被坐标范围删掉），
# 排版样式问题一律是"提醒"——落盘后打印提醒与自查清单，AI 用 Read 看图自查、自己改。

suppressPackageStartupMessages({ library(ggplot2); library(colorspace) })

# ── 规格：.medcortex/figure_spec.json（Artist 按目标期刊查实落盘）→ 没有就用通用默认 ──
mc_spec <- function(path = ".medcortex/figure_spec.json") {
  def <- list(journal = "generic-default", singleColumnMm = 85, midColumnMm = 120, doubleColumnMm = 180, maxWidthMm = 190, maxHeightMm = 230,
              dpi = 300, format = "png", minFontPt = 7, panelLabel = "a", lineWidthMinPt = 0.25)
  if (!file.exists(path) || !requireNamespace("jsonlite", quietly = TRUE)) return(def)
  j <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  if (is.null(j)) return(def)
  for (k in names(def)) if (!is.null(j[[k]]) && !is.na(j[[k]])) def[[k]] <- j[[k]]
  def
}

# ── 字号随画布宽度定（Nature 系成图尺寸下的紧凑档）：层级逐级差 0.5–1pt，面板字母加粗最大 ──
mc_font <- function(width_mm, spec = mc_spec()) {
  narrow <- width_mm < 120
  f <- if (narrow) list(tick = 7, axis = 7.5, legend = 7, title = 8, tag = 9)
       else        list(tick = 7, axis = 8, legend = 7, title = 8.5, tag = 10)
  m <- spec$minFontPt
  lapply(f, function(v) max(v, m))
}

# ── 字体：Arial → Helvetica → sans（字体不存在时导出会静默换字体，这里先查实） ──
.mc_family <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    for (f in c("Arial", "Helvetica")) {
      hit <- tryCatch(systemfonts::match_fonts(f), error = function(e) NULL)
      if (!is.null(hit) && nrow(hit) && grepl(f, basename(hit$path[1]), ignore.case = TRUE)) return(f)
    }
  }
  "sans"
}

# ── 主题：无灰底、无网格、黑色细轴线、字号按画布、标题左对齐、面板字母在左上角 ──
theme_mc <- function(width_mm, spec = mc_spec(), base_family = .mc_family()) {
  f <- mc_font(width_mm, spec)
  lw <- max(0.25, spec$lineWidthMinPt) / .pt * 2   # pt → ggplot linewidth（mm）
  theme_classic(base_family = base_family) +
    theme(text = element_text(size = f$tick, colour = "black", family = base_family),
          axis.text = element_text(size = f$tick, colour = "black"),
          axis.title = element_text(size = f$axis),
          legend.text = element_text(size = f$legend),
          legend.title = element_text(size = f$legend),
          plot.title = element_text(size = f$title, face = "bold", hjust = 0),
          plot.subtitle = element_text(size = f$legend, colour = "grey30", hjust = 0, margin = margin(b = 1.5, unit = "mm")),
          plot.title.position = "panel",   # 标题、副标题与数据区左边缘对齐（2026-10-26 用户看图定：居中不自然）
          strip.text = element_text(size = f$axis, face = "bold"),
          strip.background = element_blank(),
          plot.tag = element_text(size = f$tag, face = "bold", hjust = 0, vjust = 1),
          plot.tag.position = "topleft",   # 字母单独占左上角的边距，不压坐标轴标签与数据
          axis.line = element_line(linewidth = lw, colour = "black"), axis.ticks = element_line(linewidth = lw, colour = "black"),
          axis.ticks.length = unit(1, "mm"),
          panel.grid = element_blank(), panel.background = element_blank(), plot.background = element_blank(),
          legend.background = element_blank(), legend.key = element_blank(),
          legend.key.size = unit(3, "mm"), plot.margin = margin(2, 2, 2, 2, "mm"))
}

# ── 配色体检：色盲三型 ΔE ≥ 20 且灰度明度差 ≥ 15 才算两两可辨 ──
mc_check_palette <- function(cols) {
  cols <- unique(as.character(cols[!is.na(cols)])); if (length(cols) < 2) return(list(pass = TRUE, n = length(cols)))
  cols <- grDevices::rgb(t(grDevices::col2rgb(cols)), maxColorValue = 255)   # "grey75" 这类颜色名先换成 #RRGGBB
  dE <- function(x) { m <- as.matrix(dist(as(hex2RGB(x), "LAB")@coords)); min(m[upper.tri(m)]) }
  dL <- function(x) { l <- as(hex2RGB(x), "LAB")@coords[, 1]; m <- as.matrix(dist(l)); min(m[upper.tri(m)]) }
  r <- list(n = length(cols), normal = dE(cols), protan = dE(protan(cols)), deutan = dE(deutan(cols)),
            tritan = dE(tritan(cols)), grayscale_L = dL(cols))
  r$pass <- all(c(r$protan, r$deutan, r$tritan) >= 20) && r$grayscale_L >= 15
  r
}

# ── 默认分类色：从 Okabe-Ito（含黑）里贪心挑 n 个，让色盲三型 ΔE 与灰度明度差的最小值最大 ──
.mc_pick_categorical <- function(n) {
  pool <- c("#000000", "#0072B2", "#E69F00", "#009E73", "#D55E00", "#CC79A7", "#56B4E9", "#F0E442", "#999999")
  n <- min(n, length(pool)); if (n <= 1) return(pool[2])
  score <- function(cols) { r <- mc_check_palette(cols); min(r$protan, r$deutan, r$tritan, r$grayscale_L * 1.3) }
  best <- NULL; bestS <- -Inf
  for (first in pool) {
    cols <- first
    while (length(cols) < n) {
      cand <- setdiff(pool, cols); sc <- vapply(cand, function(c) score(c(cols, c)), numeric(1))
      cols <- c(cols, cand[which.max(sc)])
    }
    s <- score(cols); if (s > bestS) { bestS <- s; best <- cols }
  }
  unname(best)
}

# ── 默认分类色：期刊柔和色（深蓝、砖红、青绿、沙金按深→浅排，前 4 色两两过色盲三型与灰度体检）；5 色起必须加形状/直接标注 ──
.MC_SOFT <- c("#384D7A", "#C4615A", "#65B9B7", "#E7D894", "#8A6CAA", "#9B9B9B", "#7E9C5B", "#D99A6C")

# ── 配色按图型分工。n ≥ 5 的分类图不可能同时过色盲与灰度，必须再加形状/图案/直接标注 ──
mc_palette <- function(kind = c("categorical", "sequential", "diverging"), n = 3, name = NULL) {
  kind <- match.arg(kind)
  cols <- switch(kind,
    categorical = {
      if (!is.null(name) && name == "okabe") .mc_pick_categorical(n)
      else if (!is.null(name) && requireNamespace("ggsci", quietly = TRUE)) {
        fn <- get(paste0("pal_", name), asNamespace("ggsci")); fn()(n)
      } else .MC_SOFT[(seq_len(n) - 1) %% length(.MC_SOFT) + 1]
    },
    sequential = viridisLite::viridis(n, option = if (is.null(name)) "D" else name),
    diverging  = RColorBrewer::brewer.pal(max(3, min(n, 11)), if (is.null(name)) "RdBu" else name))
  chk <- mc_check_palette(cols)
  if (!isTRUE(chk$pass) && kind == "categorical")
    message(sprintf("配色体检未过（protan %.0f / deutan %.0f / tritan %.0f / 灰度 %.0f）：%s",
                    chk$protan, chk$deutan, chk$tritan, chk$grayscale_L,
                    if (n >= 5) "分组 ≥ 5 时这是数学必然，请加形状/填充图案或直接标注做冗余编码" else "换一套或加冗余编码"))
  cols
}

# ── 项目配色表 .medcortex/palette.json：同一个分组在所有图里同一个颜色。已登记的照用，新分组按柔和色依次登记 ──
.mc_palette_path <- ".medcortex/palette.json"
.mc_read_palette <- function() {
  if (!file.exists(.mc_palette_path) || !requireNamespace("jsonlite", quietly = TRUE)) return(list())
  tryCatch(as.list(jsonlite::fromJSON(.mc_palette_path)), error = function(e) list())
}
# 配色表读改写的跨进程锁（与 Python 共用 palette.json.lock）：建目录是原子的，并行出图不会分到同色、互相覆盖登记
.mc_palette_lock <- function() {
  lock <- paste0(.mc_palette_path, ".lock")
  dir.create(dirname(.mc_palette_path), showWarnings = FALSE, recursive = TRUE)
  for (i in 1:200) {
    if (dir.create(lock, showWarnings = FALSE)) return(lock)
    age <- as.numeric(difftime(Sys.time(), file.mtime(lock), units = "secs"))
    if (!is.na(age) && age > 30) unlink(lock, recursive = TRUE) else Sys.sleep(0.05)  # 持锁进程崩了留下的
  }
  stop(sprintf("配色表被占用超过 10 秒：确认没有别的出图在跑后删除 %s 再试。", lock), call. = FALSE)
}
mc_colors <- function(levels) {
  levels <- unique(as.character(levels[!is.na(levels)]))
  save <- requireNamespace("jsonlite", quietly = TRUE)
  if (save) { lock <- .mc_palette_lock(); on.exit(unlink(lock, recursive = TRUE), add = TRUE) }
  map <- .mc_read_palette(); used <- unlist(map, use.names = FALSE)
  for (l in levels) if (is.null(map[[l]])) {
    free <- setdiff(.MC_SOFT, used)
    map[[l]] <- if (length(free)) free[1] else .MC_SOFT[length(used) %% length(.MC_SOFT) + 1]
    used <- c(used, map[[l]])
  }
  if (save) {
    tmp <- paste0(.mc_palette_path, ".", Sys.getpid(), ".tmp")
    jsonlite::write_json(map, tmp, auto_unbox = TRUE, pretty = TRUE)
    file.rename(tmp, .mc_palette_path)  # 整体替换：并发读的一方不会读到写了一半的表
  }
  setNames(unlist(map[levels], use.names = FALSE), levels)
}

# ── 拼图：在绘图对象层面用 patchwork 拼（不是把 PNG 贴进网格）。坐标轴对齐、相同图例才合并一份、面板字母统一 ──
# 每个面板可以是 ggplot、patchwork、pheatmap 对象、ComplexHeatmap 的 Heatmap/HeatmapList、grob/gtable、
# 或 mc_image() 包装的真实影像；把已经渲染好的 PNG 读进来贴是被禁止的（字号随缩放乱变、对不齐、标签重复）。
.mc_as_panel <- function(x, w_mm = 80, h_mm = 60) {
  # 嵌套的 patchwork（如 KM 加风险人数表 km / tab）整体算一个面板：包起来只占一个字母，不再把里面每张子图各编一个号
  if (inherits(x, "patchwork")) return(patchwork::wrap_elements(full = x))
  if (inherits(x, "gg")) return(x)
  # 非 ggplot 面板只占绘图区（plot =），面板字母仍有自己的边距，不和列标题叠在一起
  if (inherits(x, "pheatmap")) return(patchwork::wrap_elements(plot = x$gtable))
  if (inherits(x, c("Heatmap", "HeatmapList")) && requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    # 热图的行名、聚类树、图例都是绝对尺寸：按它在拼图里实际占的格子大小来画，不然放进小格子会挤成一团；格子窄时图例放下方，给热图主体留宽度
    side <- if (h_mm >= 45 && w_mm < 110) "bottom" else "right"   # 格子够高就把图例放下方：右侧图例加行名会吃掉大半宽度，主体挤成窄条
    # 行名放不放得下是算得出来的：每行至少要 7pt 字高的 1.15 倍，底部图例约占格子高度的 45%
    rows <- if (inherits(x, "Heatmap") && isTRUE(x@row_names_param$show)) nrow(x@matrix) else 0
    need <- rows * 7 * 0.3528 * 1.15; avail <- h_mm * (if (side == "bottom") 0.55 else 0.8)
    if (need > avail) .mc_note(sprintf("热图 %d 个行名在 %.0fmm 高的格子里放不下（至少要 %.0fmm），一定叠字：只标关键基因（row_names 用 anno_mark）或加高这一排", rows, h_mm, need / (if (side == "bottom") 0.55 else 0.8)))
    # 热图占满整格（full =）：用 plot = 会被对齐到邻格的数据区、主体挤成窄条；面板字母那一行由 mc_compose 统一在顶部边距里留出
    g <- grid::grid.grabExpr(ComplexHeatmap::draw(x, merge_legend = TRUE, heatmap_legend_side = side, annotation_legend_side = side,
                                                  padding = grid::unit(c(2, 2, 2, 2), "mm")),
                             width = w_mm / 25.4, height = h_mm / 25.4)
    return(patchwork::wrap_elements(full = g))
  }
  if (inherits(x, c("grob", "gtable", "gTree"))) return(patchwork::wrap_elements(plot = x))
  # 读进来的位图（png::readPNG、magick::image_read 等，10-12 用户放开位图拼图）：与 mc_image 同样包成面板，审计照常提醒「贴位图」
  if (inherits(x, "magick-image")) x <- as.raster(x)
  if (inherits(x, c("nativeRaster", "raster")) || (is.numeric(x) && length(dim(x)) %in% 2:3 && all(x >= 0 & x <= 1, na.rm = TRUE))) return(.mc_raster_panel(x))
  stop(sprintf("mc_compose 不认识的面板类型：%s", paste(class(x), collapse = "/")), call. = FALSE)
}
.mc_nested <- function(x) inherits(x, "wrapped_patch") && inherits(attr(x, "grobs")$full, "patchwork")
# 每个面板在画布上大约占多大（mm）：design 字符串按字母所占行列求和；否则按 ncol/nrow 均分
.mc_cells <- function(n, design, ncol, nrow, widths, heights, width_mm, height_mm) {
  if (is.character(design)) {
    rows <- strsplit(trimws(strsplit(trimws(design), "\n")[[1]]), "")
    grid <- do.call(rbind, rows)
  } else {
    if (is.null(ncol)) ncol <- if (is.null(nrow)) ceiling(sqrt(n)) else ceiling(n / nrow)
    if (is.null(nrow)) nrow <- ceiling(n / ncol)
    grid <- matrix(c(LETTERS[seq_len(n)], rep("#", nrow * ncol - n)), nrow, ncol, byrow = TRUE)
  }
  cw <- if (is.null(widths)) rep(1, ncol(grid)) else rep_len(widths, ncol(grid))
  rh <- if (is.null(heights)) rep(1, nrow(grid)) else rep_len(heights, nrow(grid))
  cw <- cw / sum(cw) * width_mm; rh <- rh / sum(rh) * height_mm
  lapply(seq_len(n), function(i) {
    at <- which(grid == LETTERS[i], arr.ind = TRUE)
    if (!nrow(at)) return(c(width_mm / 3, height_mm / 2))
    c(sum(cw[unique(at[, 2])]), sum(rh[unique(at[, 1])]))
  })
}
# 图例以实际绘制的内容和映射比对，不能仅凭标题相同就收拢。
.mc_legend_signature <- function(q) {
  if (!inherits(q, "ggplot")) return(NULL)
  g <- .mc_with_device(150, 100, suppressWarnings(ggplotGrob(q)))
  if (is.null(g)) return(NULL)
  guides <- g$grobs[grepl("^guide-box", g$layout$name)]
  guides <- Filter(function(x) !inherits(x, "zeroGrob"), guides)
  if (!length(guides)) return(NULL)
  mapping <- c(as.list(q$mapping), unlist(lapply(q$layers, function(l) as.list(l$mapping)), recursive = FALSE))
  mapping <- mapping[names(mapping) %in% c("colour", "fill", "shape", "linetype", "size", "alpha")]
  list(mapping = lapply(mapping, rlang::as_label), guides = lapply(guides, .mc_unname))
}
# 比较图例前清掉 grid 自动编号的名字（GRID.text.123 这类每次建图都不同），其余内容原样比
.mc_unname <- function(x) {
  if (!inherits(x, "grob")) return(x)   # 只沿 grob 树走（gtable、gTree 也是 grob）
  x$name <- ""
  if (inherits(x$vp, "viewport")) x$vp$name <- ""
  if (!is.null(x$grobs)) x$grobs <- lapply(unname(x$grobs), .mc_unname)
  if (!is.null(x$children)) x$children <- lapply(unname(x$children), .mc_unname)
  if (!is.null(x$childrenOrder)) x$childrenOrder <- rep_len("", length(x$childrenOrder))
  units <- vapply(x, grid::is.unit, logical(1))   # grobwidth 这类单位里还包着带编号的 grob：换成绝对尺寸
  x[units] <- lapply(unclass(x)[units], grid::absolute.size)
  x
}
.mc_same_legend <- function(a, b) isTRUE(all.equal(a, b, check.names = FALSE, check.attributes = FALSE))

# 连续 scale 把有限的原始坐标变成 NA 时，会整行丢掉区间；coord_cartesian 不改 scale 数据。
# text = FALSE 只看数据图层（点、线、误差线、柱等，越界即数据错误），TRUE 只看文字图层（标注越界只提醒）；固定参数（geom_point(y = 10)）同样核。
# 没设 limits 的 scale 只核变换本身（对数轴把负数变成 NaN 同样整行丢掉），不按训练范围核：统计图层的原始值可能超出拟合后的范围，会误拦。
# 只认变成 NA/NaN 的才算删掉：0 取对数得 -Inf，ggplot 会压到轴边照样画出（看图自查判断）；固定参数本就在变换后的空间里，不再变换；geom_blank 不画任何东西
.mc_scale_losses <- function(q, text = FALSE) {
  if (!inherits(q, "ggplot")) return(FALSE)
  b <- suppressWarnings(ggplot_build(q))
  for (layer in q$layers) {
    if (any(grepl("^Geom(Text|Label)", class(layer$geom))) != text || inherits(layer$geom, "GeomBlank")) next
    data <- tryCatch(layer$layer_data(q$data), error = function(e) NULL)   # 图层自己的数据（含 data = 函数时取子集后的）
    if (!is.data.frame(data)) next
    mapping <- if (isTRUE(layer$inherit.aes)) as.list(q$mapping) else list()
    mapping[names(layer$mapping)] <- as.list(layer$mapping)
    fixed <- layer$aes_params
    for (axis in c("x", "y")) {
      scales <- if (axis == "x") b$layout$panel_scales_x else b$layout$panel_scales_y
      for (sc in scales) {
        if (sc$is_discrete()) next
        for (aes in intersect(c(names(mapping), names(fixed)), paste0(axis, c("", "min", "max", "end")))) {
          values <- if (!is.null(fixed[[aes]])) fixed[[aes]] else tryCatch(rlang::eval_tidy(mapping[[aes]], data), error = function(e) NULL)
          if (!is.numeric(values) && !inherits(values, c("Date", "POSIXct"))) next   # 日期时间由 scale 的 transform 换成数值再比
          transformed <- if (!is.null(fixed[[aes]])) values else suppressWarnings(sc$transform(values))
          censored <- if (is.null(sc$limits)) transformed else sc$oob(transformed, sc$get_limits())
          if (any(is.finite(values) & is.na(censored))) return(TRUE)
        }
      }
    }
  }
  FALSE
}
# 只看实际画出来的图例标题：guides(colour = "none")、show.legend = FALSE、legend.position = "none" 隐藏的不算
.mc_raw_legend_titles <- function(q) {
  if (!inherits(q, "ggplot")) return(character())
  g <- .mc_with_device(150, 100, suppressWarnings(ggplotGrob(q)))
  if (is.null(g)) return(character())
  out <- character()
  walk <- function(x, title = FALSE) {
    if (inherits(x, "text")) { if (title) out <<- c(out, as.character(x$label)) }
    else if (inherits(x, "gtable")) for (i in seq_along(x$grobs)) walk(x$grobs[[i]], title || x$layout$name[i] == "title")
    else if (inherits(x, "gTree")) for (ch in x$children) walk(ch, title)
  }
  for (box in g$grobs[grepl("^guide-box", g$layout$name)]) walk(box)
  unique(out[grepl("_", out) | grepl("^[a-z][a-z0-9]*$", out)])
}
# 被拦的图存到所在项目的 .medcortex/figure_qc/rejected/：figures/ 下的按 figures 的上一级（分支即分支目录），
# 只在当前工作目录以内往上找；不在 figures/ 下的退回当前工作目录。子目录写进文件名（panels__fig1a.png），不与根目录同名图互相覆盖
.mc_rejected_path <- function(file) {
  low <- if (.Platform$OS.type == "windows") tolower else identity   # Windows 路径不分大小写
  top <- normalizePath(getwd(), winslash = "/"); prefix <- low(sub("/*$", "/", top))
  dir <- normalizePath(dirname(file), winslash = "/", mustWork = FALSE)
  root <- top; name <- basename(file); sub_dirs <- character()
  while (dir != top && startsWith(low(dir), prefix)) {
    if (basename(dir) == "figures") { root <- dirname(dir); name <- paste(c(sub_dirs, name), collapse = "__"); break }
    sub_dirs <- c(basename(dir), sub_dirs); dir <- dirname(dir)
  }
  rejected <- file.path(root, ".medcortex", "figure_qc", "rejected", name)
  dir.create(dirname(rejected), recursive = TRUE, showWarnings = FALSE)
  rejected
}
# 面板字母的文字：与 patchwork 的 tag_levels 同一套编号（a / A / 1 / i / I）
.mc_tag_labels <- function(tag, n) switch(as.character(tag), a = letters[seq_len(n)], A = paste0("(", LETTERS[seq_len(n)], ")"), "1" = as.character(seq_len(n)),
  i = tolower(as.character(utils::as.roman(seq_len(n)))), I = as.character(utils::as.roman(seq_len(n))), rep("M", n))
# 每个面板数据区（分面图取整个分面区域）的左边与顶边（mm，画布左下为原点）：给每个面板一个独一无二的探针字母放在数据区左上角，渲染一次量出来；
# 量得到数据区的再配上宽高（分面图取左上那一格）。热图、贴进来的图形对象没有 ggplot 数据区，探针落在它的格子上，宽高为 NA。
# 同一排按格子顶边分（探针再放到格子左上角量一次）：固定比例的影像、热图的数据区顶边与邻格不齐，按数据区分会被拆成两排
.mc_patch_areas <- function(p, width_mm, height_mm) {
  n <- length(p); probe <- sprintf("<%d>", seq_len(n))
  corner <- function(where) {
    q <- p + patchwork::plot_annotation(tag_levels = list(probe)) &
      theme(plot.tag.location = where, plot.tag.position = c(0, 1), plot.tag = element_text(size = 7, hjust = 0, vjust = 1, margin = margin()))
    g <- .mc_geometry(q, width_mm, height_mm)
    if (!is.null(g$text)) g$text <- g$text[g$text$tag & g$text$label %in% probe, ]
    g
  }
  g <- corner("panel"); bx <- g$text; pn <- g$panels
  if (is.null(bx) || !nrow(bx)) return(NULL)
  if (!is.null(pn)) { pn <- pn[pn$kind == "plot", ]; for (k in c("x", "y", "w", "h")) pn[[k]] <- as.numeric(pn[[k]]) }
  out <- data.frame(i = seq_len(n), left = NA_real_, top = NA_real_, w = NA_real_, h = NA_real_, cell = NA_real_, cl = NA_real_, ix = NA_real_, it = NA_real_)
  for (k in seq_len(nrow(bx))) {
    i <- match(bx$label[k], probe); out$left[i] <- bx$x0[k] * 25.4; out$top[i] <- bx$y1[k] * 25.4
    hit <- if (is.null(pn)) integer() else which(abs(pn$x * 25.4 - out$left[i]) < 0.3 & abs((pn$y * 25.4 + pn$h) - out$top[i]) < 0.3)
    if (length(hit) && inherits(p[[i]], "ggplot") && !inherits(p[[i]], c("wrapped_patch", "patchwork"))) { out$w[i] <- pn$w[hit[1]]; out$h[i] <- pn$h[hit[1]] }
    # 包起来的嵌套组合没有自己的数据区：ix / it 记格子里左上那个数据区的左边缘与顶边（离探针最近的），面板字母照样锚在它左上角外侧
    ok <- if (is.null(pn) || !.mc_nested(p[[i]])) integer() else which(pn$x * 25.4 >= out$left[i] - 0.3)
    if (length(ok)) { j <- ok[which.min((pn$x[ok] * 25.4 - out$left[i])^2 + (out$top[i] - pn$y[ok] * 25.4 - pn$h[ok])^2)]; out$ix[i] <- pn$x[j] * 25.4; out$it[i] <- pn$y[j] * 25.4 + pn$h[j] }
  }
  cb <- corner("plot")$text
  for (k in seq_len(if (is.null(cb)) 0 else nrow(cb))) { i <- match(cb$label[k], probe); out$cell[i] <- cb$y1[k] * 25.4; out$cl[i] <- cb$x0[k] * 25.4 }
  out$row <- NA_integer_
  ok <- which(!is.na(out$top) & !is.na(out$cell)); if (length(ok)) { o <- ok[order(-out$cell[ok])]; out$row[o] <- cumsum(c(TRUE, abs(diff(out$cell[o])) > 2)) }
  out
}
# 面板字母（通用规则，2026-10-26 用户看图改定）：水平放在各自格子的最左缘（字母左缘对齐格子左边，条目名、坐标轴标签都在它右下方），
# 垂直放在同一排最高的标题之上、同一排共用一条基线。原规则（10-11）锚在数据区左缘，长条目名的面板字母会悬在图中间，看着不自然。
# 全部按毫米量，不手调 hjust/vjust 倍数。热图、贴进来的图形对象、嵌套的 patchwork 组合同样放在格子左上角，基线与同排一致
.mc_place_tags <- function(p, width_mm, height_mm, tag, size, gap_mm = 1.2) {
  n <- length(p); fam <- .mc_family(); room <- size * 0.3528 + 1.5
  plain <- vapply(seq_len(n), function(i) inherits(p[[i]], "ggplot") && !inherits(p[[i]], c("wrapped_patch", "patchwork")), logical(1))
  nest <- vapply(seq_len(n), function(i) .mc_nested(p[[i]]), logical(1))
  measure <- function(expr) { v <- tryCatch(.mc_with_device(width_mm, height_mm, expr), error = function(e) NULL); if (is.null(v)) 0 else v }
  # 数据区顶边以上到画布格子顶边的高度（标题、副标题、顶部分面条、原有上边距）：字母放在它之上；嵌套组合量它左上那张子图
  lift <- vapply(seq_len(n), function(i) if (!plain[i] && !nest[i]) 0 else measure({
    g <- suppressWarnings(ggplotGrob(if (nest[i]) .mc_plots(attr(p[[i]], "grobs")$full)[[1]] else p[[i]])); t <- min(g$layout$t[grepl("^panel", g$layout$name)])
    grid::convertHeight(sum(g$heights[seq_len(t - 1)]), "mm", valueOnly = TRUE) }), numeric(1))
  # 每个面板顶部多留一行字母高度（同排一致），字母放在这行里，不会伸出画布
  pad <- function(i, top = 0, left = 0) {
    m <- measure(grid::convertHeight(calc_element("plot.margin", .mc_theme_of(p[[i]])), "mm", valueOnly = TRUE))
    if (length(m) != 4) m <- c(2, 2, 2, 2)
    p[[i]] <<- p[[i]] + theme(plot.margin = margin(m[1] + top, m[2], m[3], m[4] + left, "mm"))
    if (!nest[i]) return(invisible())
    # 包起来的组合占满整格、不吃外层边距：同样的边距再加在组合自己的边距上
    full <- attr(p[[i]], "grobs")$full
    m <- measure(grid::convertHeight(calc_element("plot.margin", theme_get() + full$patches$annotation$theme), "mm", valueOnly = TRUE))
    if (length(m) != 4) m <- c(2, 2, 2, 2)
    attr(p[[i]], "grobs")$full <<- full + patchwork::plot_annotation(theme = theme(plot.margin = margin(m[1] + top, m[2], m[3], m[4] + left, "mm")))
  }
  for (i in seq_len(n)) pad(i, top = room)
  a <- .mc_patch_areas(p, width_mm, height_mm)
  if (is.null(a)) return(p)
  labs <- .mc_tag_labels(tag, n)
  # 逐个面板取「数据区顶边 + 其上高度」再取最大：横跨多列的面板被 free() 后数据区顶边与同排不齐，两项分开取最大会把字母顶出画布
  # 嵌套组合按它左上那个数据区算，与普通面板同一口径
  top <- ifelse(nest, a$it, a$top)
  base <- vapply(seq_len(n), function(i) { r <- which(a$row == a$row[i] & (plain | (nest & !is.na(top)))); if (is.na(a$row[i]) || !length(r)) NA_real_ else max(top[r] + lift[r]) + 0.6 }, numeric(1))
  el <- function(...) element_text(size = size, face = "bold", family = fam, ...)
  for (i in seq_len(n)) {
    if (is.na(a$top[i])) next
    # 字母左缘离格子左边 0.5mm（探针量的是格子左边，字母放在数据区坐标系里，用负的左边距挪过去）
    dx <- if (is.na(a$cl[i])) gap_mm else a$cl[i] + 0.5 - a$left[i]
    if (plain[i] || (nest[i] && !is.na(a$ix[i]))) p[[i]] <- p[[i]] + theme(plot.tag.location = "panel", plot.tag.position = c(0, 1),
      plot.tag = el(hjust = 0, vjust = 0, margin = margin(l = dx, b = base[i] - a$top[i], unit = "mm")))
    else p[[i]] <- p[[i]] + theme(plot.tag.location = "panel", plot.tag.position = c(0, 1),
      plot.tag = if (is.na(base[i])) el(hjust = 0, vjust = 1, margin = margin(l = gap_mm, unit = "mm")) else el(hjust = 0, vjust = 0, margin = margin(l = gap_mm, b = base[i] - a$top[i], unit = "mm")))
  }
  p
}
.mc_notes <- new.env()
.mc_note <- function(msg) assign("list", c(get0("list", .mc_notes, inherits = FALSE), msg), envir = .mc_notes)
mc_compose <- function(..., design = NULL, ncol = NULL, nrow = NULL, widths = NULL, heights = NULL,
                       width_mm = 180, height_mm = width_mm * 0.7, spec = mc_spec(), tag = spec$panelLabel, legend = c("auto", "keep", "collect")) {
  legend <- match.arg(legend)
  raw <- list(...)
  assign("list", NULL, envir = .mc_notes)
  cells <- .mc_cells(length(raw), design, ncol, nrow, widths, heights, width_mm, height_mm)
  ps <- Map(function(x, wh) .mc_as_panel(x, wh[1], wh[2]), raw, cells)
  # 横跨多列的 ggplot 面板不参与坐标轴对齐：它的长标签会把上下几排的左边距一起撑大，留出大片空白
  if (is.character(design) && "free" %in% getNamespaceExports("patchwork")) {
    grid <- do.call(rbind, strsplit(trimws(strsplit(trimws(design), "\n")[[1]]), ""))
    for (i in seq_along(ps)) if (inherits(ps[[i]], "ggplot") && length(unique(which(grid == LETTERS[i], arr.ind = TRUE)[, 2])) > 1) ps[[i]] <- patchwork::free(ps[[i]])
  }
  signatures <- Filter(Negate(is.null), lapply(raw, .mc_legend_signature))
  if (legend != "keep") legend <- if (length(signatures) > 1 && all(vapply(signatures[-1], function(x) .mc_same_legend(x, signatures[[1]]), logical(1)))) "collect" else "keep"
  # 同列左轴标签相差超过 12mm 时释放左边距，避免一幅长标签拖宽整列。
  if (length(ps) > 1 && "free" %in% getNamespaceExports("patchwork")) {
    cols <- if (!is.null(ncol)) ncol else if (!is.null(nrow)) ceiling(length(ps) / nrow) else ceiling(sqrt(length(ps)))
    left <- vapply(raw, function(q) {
      if (!inherits(q, "ggplot")) return(0)
      measured <- .mc_with_device(width_mm, height_mm, {
        g <- suppressWarnings(ggplotGrob(q)); panel <- which(grepl("^panel", g$layout$name))[1]
        if (is.na(panel)) 0 else grid::convertWidth(sum(g$widths[seq_len(g$layout$l[panel] - 1)]), "mm", valueOnly = TRUE)
      })
      if (is.null(measured)) 0 else measured
    }, numeric(1))
    for (i in seq_along(ps)) {
      peers <- seq_along(ps)[(seq_along(ps) - 1) %% cols == (i - 1) %% cols]
      if (is.character(design)) {
        at <- unique(which(grid == LETTERS[i], arr.ind = TRUE)[, 2])
        peers <- which(vapply(seq_along(ps), function(j) any(which(grid == LETTERS[j], arr.ind = TRUE)[, 2] %in% at), logical(1)))
      }
      if (length(peers) > 1 && max(left[peers]) - min(left[peers]) > 12 && inherits(ps[[i]], "ggplot")) ps[[i]] <- patchwork::free(ps[[i]], type = "panel", side = "l")
    }
  }
  f <- mc_font(width_mm, spec)
  p <- patchwork::wrap_plots(ps, design = design, ncol = ncol, nrow = nrow, widths = widths, heights = heights,
                             guides = legend)
  tag_levels_use <- if (identical(as.character(tag), "A")) list(sprintf("(%s)", LETTERS)) else tag
  p <- p + patchwork::plot_annotation(tag_levels = tag_levels_use)
  # 量不出版面（少见的面板类型）时保留 patchwork 默认字母位置，mc_audit 的字母一致性检查照样会查
  p <- tryCatch(.mc_place_tags(p, width_mm, height_mm, tag, f$tag), error = function(e) p)
  attr(p, "mc_problems") <- get0("list", .mc_notes, inherits = FALSE)   # 拼图时算出的硬伤，mc_audit 一并硬拦
  p
}
# ── 真实影像面板（显微镜、病理、影像照片）：唯一允许的位图面板，裁到必要区域，比例尺另画 ──
mc_image <- function(path) {
  img <- if (grepl("\\.png$", path, ignore.case = TRUE)) png::readPNG(path) else if (requireNamespace("magick", quietly = TRUE)) as.raster(magick::image_read(path)) else stop("读这种图片格式需要 magick 包", call. = FALSE)
  p <- .mc_raster_panel(img)
  attr(p, "mc_photo") <- TRUE
  p
}
# 位图按原始宽高比铺满一个面板（mc_image 的照片、mc_compose 收到的读进来的位图共用）
.mc_raster_panel <- function(img) ggplot() + annotation_raster(img, 0, 1, 0, 1) + coord_fixed(ratio = dim(img)[1] / dim(img)[2], expand = FALSE) + theme_void()

# ── 内部：把 patchwork 拆成子图列表（嵌套的组合也拆开，里面每张子图照样审）；行列数（只数顶层面板） ──
# 外层兄弟面板与包起来的嵌套组合都要拆：嵌套组合排在最后时，它本身就是带着兄弟面板的外层对象
.mc_plots <- function(p) {
  out <- if (inherits(p, "patchwork") && !is.null(p$patches)) do.call(c, lapply(p$patches$plots, .mc_plots)) else list()
  c(out, list(p), if (.mc_nested(p)) .mc_plots(attr(p, "grobs")$full))
}
.mc_grid <- function(p) {
  if (!inherits(p, "patchwork")) return(list(n = 1, cols = 1, rows = 1))
  n <- length(p$patches$plots) + 1
  lay <- p$patches$layout; cols <- NULL; rows <- NULL
  if (!is.null(lay$design)) {
    d <- lay$design
    if (is.character(d)) { ln <- strsplit(trimws(d), "\n")[[1]]; cols <- max(nchar(trimws(ln))); rows <- length(ln) }
    else if (!is.null(d$r) && !is.null(d$b)) { cols <- max(d$r); rows <- max(d$b) }   # patchwork 把版式存成区域表（patch_area，不是 data.frame）
  }
  if (is.null(cols)) cols <- if (!is.null(lay$ncol)) lay$ncol else if (!is.null(lay$nrow)) ceiling(n / lay$nrow) else ceiling(sqrt(n))
  if (is.null(rows)) rows <- ceiling(n / cols)
  list(n = n, cols = cols, rows = rows)
}
.mc_theme_of <- function(p) tryCatch(theme_get() + p$theme, error = function(e) theme_get())
.mc_text_sizes <- function(p) {
  th <- .mc_theme_of(p); out <- c()
  for (el in c("axis.text", "axis.title", "legend.text", "legend.title", "plot.title", "strip.text", "plot.tag")) {
    e <- tryCatch(calc_element(el, th), error = function(e) NULL)
    if (!is.null(e) && !is.null(e$size) && is.numeric(e$size)) out[el] <- e$size
  }
  for (i in seq_along(p$layers)) {
    l <- p$layers[[i]]
    if (inherits(l$geom, c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel"))) {
      s <- l$aes_params$size; if (is.null(s)) s <- 3.88
      out[paste0("geom_text#", i)] <- s * .pt
    }
  }
  out
}
.mc_titles <- function(p) {
  t <- c()
  for (q in .mc_plots(p)) { t <- c(t, q$labels$title, q$labels$subtitle) }
  if (inherits(p, "patchwork")) t <- c(t, p$patches$annotation$title, p$patches$annotation$subtitle)
  as.character(unlist(t))
}
# 离散配色：每个面板的「分组 → 颜色」，以及这个面板有没有用形状/线型/文字标注做冗余编码
.mc_scale_colours <- function(p) {
  out <- list()
  for (q in .mc_plots(p)) {
    b <- tryCatch(ggplot_build(q), error = function(e) NULL); if (is.null(b)) next
    aes_used <- c(names(q$mapping), unlist(lapply(q$layers, function(l) names(l$mapping))))
    redundant <- any(c("shape", "linetype", "label") %in% aes_used)
    for (sc in b$plot$scales$scales) {
      if (!any(sc$aesthetics %in% c("colour", "fill"))) next
      if (!isTRUE(sc$is_discrete())) next
      lim <- sc$get_limits(); cols <- tryCatch(sc$map(lim), error = function(e) NULL)
      if (!is.null(cols) && length(cols) >= 2) out[[length(out) + 1]] <- list(cols = setNames(as.character(cols), as.character(lim)), redundant = redundant)
    }
  }
  out
}
.mc_legend_count <- function(p, width_mm = 150, height_mm = 100) {
  g <- tryCatch(.mc_with_device(width_mm, height_mm, .mc_grob(p)), error = function(e) NULL)
  if (is.null(g)) return(NA_integer_)
  cnt <- 0L
  walk <- function(x) {
    if (inherits(x, "gtable")) {
      nm <- x$layout$name
      cnt <<- cnt + sum(grepl("^guide-box", nm) & vapply(x$grobs, function(gr) !inherits(gr, "zeroGrob"), logical(1)))
      for (gr in x$grobs) walk(gr)
    } else if (inherits(x, "gList")) for (gr in x) walk(gr)
  }
  walk(g); cnt
}
# 每条边着墨的长度（mm）：边缘 2 像素内有墨的列/行数按 dpi 换算。2026-10-26 前按整圈比例 > 1% 判，色条、图例只碰一条边的一小段（实测 0.3%）漏报
.mc_edge <- function(png_path, dpi = 300) {
  if (!requireNamespace("png", quietly = TRUE) || !file.exists(png_path)) return(NULL)
  a <- png::readPNG(png_path); if (length(dim(a)) == 3) { rgb <- a[, , 1:3]; if (dim(a)[3] == 4) rgb[a[, , 4] < 0.05] <- 1 } else rgb <- array(a, c(dim(a), 3))
  # 三通道逐像素取最小值用 pmin 整块算（2026-10-21 实测：300 dpi 通栏图 apply 逐像素 6.7–8.0 秒/张，pmin 0.15–0.33 秒，结果 identical）
  ink <- pmin(rgb[, , 1], rgb[, , 2], rgb[, , 3]) < 0.85
  h <- nrow(ink); w <- ncol(ink); b <- 2; px <- 25.4 / dpi
  c(上 = sum(colSums(ink[1:b, , drop = FALSE]) > 0), 下 = sum(colSums(ink[(h - b + 1):h, , drop = FALSE]) > 0),
    左 = sum(rowSums(ink[, 1:b, drop = FALSE]) > 0), 右 = sum(rowSums(ink[, (w - b + 1):w, drop = FALSE]) > 0)) * px
}

# ── 内部：贴图拼接与 cowplot 拼图（按图层识别；热图的 geom_raster、ComplexHeatmap、mc_image 照片不算） ──
.mc_has_raster <- function(g) {
  if (inherits(g, "rastergrob")) return(TRUE)
  if (inherits(g, "gtable")) return(any(vapply(g$grobs, .mc_has_raster, logical(1))))
  if (inherits(g, "gTree")) return(any(vapply(g$children, .mc_has_raster, logical(1))))
  FALSE
}
.mc_pasted <- function(p) {
  hits <- c()
  for (q in .mc_plots(p)) {
    if (isTRUE(attr(q, "mc_photo"))) next
    if (inherits(q, "wrapped_patch") && any(vapply(attr(q, "grobs"), function(g) inherits(g, "rastergrob"), logical(1))))
      hits <- c(hits, "wrap_elements 贴位图")
    n_draw <- 0
    for (l in q$layers) {
      cl <- class(l$geom)[1]; gp <- l$geom_params
      if (cl == "GeomDrawGrob") { n_draw <- n_draw + 1; if (.mc_has_raster(gp$grob)) hits <- c(hits, "cowplot::draw_image 贴位图") }
      if (cl == "GeomRasterAnn") hits <- c(hits, "annotation_raster 贴位图")
      if (cl == "GeomCustomAnn" && .mc_has_raster(gp$grob)) hits <- c(hits, "annotation_custom 贴位图")
    }
    if (n_draw >= 2) hits <- c(hits, "cowplot::plot_grid 拼图")
  }
  unique(hits)
}

# ── 文字重叠（算出来的，不靠看）：按实际尺寸渲染一遍，每段文字（逐个基因名、刻度、图例）算出在画布上的框，两两比较 ──
# 建图形对象要量字宽，必须先开好与落盘同款的设备：不开时 R 退回 PDF 设备，找不到 Arial 直接报错（拼图上整项检查会静默失效）
.mc_with_device <- function(width_mm, height_mm, expr) {
  if (!requireNamespace("ragg", quietly = TRUE)) return(NULL)
  f <- tempfile(fileext = ".png"); ragg::agg_png(f, width = width_mm, height = height_mm, units = "mm", res = 100)
  on.exit({ grDevices::dev.off(); unlink(f) })
  expr
}
.mc_grob <- function(plot) if (inherits(plot, "patchwork")) patchwork::patchworkGrob(plot) else ggplotGrob(plot)
# 按实际尺寸渲染一遍，量出每段文字的框与每个数据区（ggplot 面板、热图主体）的实际大小
.mc_geometry <- function(plot, width_mm, height_mm) .mc_with_device(width_mm, height_mm, {
  g <- .mc_grob(plot)
  grid::grid.newpage(); grid::grid.draw(g); grid::grid.force()
  tree <- grid::grid.get(g$name)   # 强制展开后的树：ggrepel 等绘制时才算位置的文字已落成具体坐标
  out <- list(); panels <- list()
  size <- function() { a <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE); b <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), valueOnly = TRUE); c(x = a$x, y = a$y, w = (b$x - a$x) * 25.4, h = (b$y - a$y) * 25.4) }
  # 按绘制时的嵌套顺序逐层进视口（grid.ls 在含热图的拼图上会报错，不用它）；单个元素出错只跳过它自己
  # tag：面板字母（ggplot 的 plot.tag、patchwork 改名后的 tag-… / 固定比例面板的 tag.…）里的文字，字母位置检查用
  walk <- function(x, cell = NULL, tag = FALSE) {
    tag <- tag || grepl("^(tag[-.]|plot\\.tag)", if (is.null(x$name)) "" else x$name)
    here <- grid::current.vpPath()
    back <- function() { grid::upViewport(0); if (!is.null(here)) grid::downViewport(here) }
    tryCatch({
      if (!is.null(x$vp)) { if (inherits(x$vp, "vpPath")) grid::downViewport(x$vp) else grid::pushViewport(x$vp) }
      vpn <- grid::current.viewport()$name
      # ggplot 的数据区在 gtable 里叫 panel-1.<行>-<列>…；ComplexHeatmap 的主体视口叫 …heatmap_body_<行>_<列>
      if (grepl("^panel(-[0-9]+)*\\.[0-9]", if (is.null(x$name)) "" else x$name)) panels[[length(panels) + 1]] <<- c(kind = "plot", key = x$name, size(), text = .mc_text_only(x))
      if (grepl("heatmap_body_[0-9]+_[0-9]+$", vpn)) panels[[length(panels) + 1]] <<- c(kind = "heatmap", key = sub("_[0-9]+$", "", vpn), size(), text = FALSE)
      # 开了裁剪的区域（ggplot 数据区、patchwork 格子、热图视口；推入后裁剪状态存成逻辑值）：里面的文字超出它就会被裁掉半截
      if (isTRUE(grid::current.viewport()$clip)) { z <- size(); cell <- c(x0 = z[["x"]], y0 = z[["y"]], x1 = z[["x"]] + z[["w"]] / 25.4, y1 = z[["y"]] + z[["h"]] / 25.4) }
      if (inherits(x, "text")) {
        labs <- as.character(x$label); n <- length(labs)
        for (k in seq_len(min(n, 400))) {
          if (!nzchar(trimws(labs[k]))) next
          tg <- grid::textGrob(labs[k], x = x$x[(k - 1) %% length(x$x) + 1], y = x$y[(k - 1) %% length(x$y) + 1], just = x$just,
            hjust = if (is.null(x$hjust)) NULL else rep_len(x$hjust, n)[k], vjust = if (is.null(x$vjust)) NULL else rep_len(x$vjust, n)[k],
            rot = rep_len(x$rot, n)[k], gp = x$gp)
          a <- grid::deviceLoc(grid::grobX(tg, "west"), grid::grobY(tg, "south"), valueOnly = TRUE)
          b <- grid::deviceLoc(grid::grobX(tg, "east"), grid::grobY(tg, "north"), valueOnly = TRUE)
          out[[length(out) + 1]] <<- data.frame(label = labs[k], x0 = min(a$x, b$x), x1 = max(a$x, b$x), y0 = min(a$y, b$y), y1 = max(a$y, b$y), tag = tag,
            cut = !is.null(cell) && (min(a$x, b$x) < cell[["x0"]] - 0.3 / 25.4 || max(a$x, b$x) > cell[["x1"]] + 0.3 / 25.4 || min(a$y, b$y) < cell[["y0"]] - 0.3 / 25.4 || max(a$y, b$y) > cell[["y1"]] + 0.3 / 25.4))
        }
      } else if (inherits(x, "gTree")) {
        if (!is.null(x$childrenvp)) { at <- grid::current.vpPath(); grid::pushViewport(x$childrenvp); grid::upViewport(0); if (!is.null(at)) grid::downViewport(at) }
        for (nm in x$childrenOrder) walk(x$children[[nm]], cell, tag)
      }
    }, error = function(e) NULL)
    back()
  }
  walk(tree)
  list(text = if (length(out)) do.call(rbind, out) else NULL,
       panels = if (length(panels)) unique(as.data.frame(do.call(rbind, panels), stringsAsFactors = FALSE)) else NULL)
})
.mc_text_boxes <- function(plot, width_mm, height_mm) .mc_geometry(plot, width_mm, height_mm)$text
# 纯文字面板（风险人数表这类）：数据区里除底色、网格、边框外画出来的只有文字
.mc_text_only <- function(x) {
  kinds <- character()
  walk <- function(g) {
    if (grepl("^(grill|panel\\.border)", if (is.null(g$name)) "" else g$name) || inherits(g, c("zeroGrob", "null"))) return()
    if (inherits(g, c("text", "textrepeltree"))) kinds <<- c(kinds, "text")
    else if (inherits(g, "gTree")) for (ch in g$children) walk(ch)
    else kinds <<- c(kinds, "ink")
  }
  walk(x)
  length(kinds) > 0 && all(kinds == "text")
}
# 数据区：ggplot 面板短边 ≥ 40mm（2026-10-26 由 18mm 提高：前几篇图每排塞 4–5 格；30mm 时 TCGA 例的生存曲线 30×50mm 仍漏报）、宽高比 0.3–3（森林图等条目图天然细高）；热图主体（各切片合计）宽 ≥ 25mm、高 ≥ 20mm；纯文字的表格面板（风险人数表）不查
.mc_squashed <- function(plot, width_mm, height_mm, geo = NULL) {
  p <- if (!is.null(geo)) geo$panels else tryCatch(.mc_geometry(plot, width_mm, height_mm)$panels, error = function(e) NULL)
  if (is.null(p) || !nrow(p)) return(character())
  for (k in c("x", "y", "w", "h")) p[[k]] <- as.numeric(p[[k]])
  out <- character()
  pl <- p[p$kind == "plot" & p$text != "TRUE", ]; pl <- pl[!duplicated(round(pl[, c("x", "y", "w", "h")], 2)), ]
  for (i in seq_len(nrow(pl))) {
    w <- pl$w[i]; h <- pl$h[i]
    if (min(w, h) < 40 || w / h < 0.3 || w / h > 3) out <- c(out, sprintf("面板数据区 %.0f×%.0fmm（宽高比 %.2f）", w, h, w / h))
  }
  hm <- p[p$kind == "heatmap", ]
  for (k in unique(hm$key)) {
    s <- hm[hm$key == k, ]; w <- sum(s$w[!duplicated(round(s$x, 3))]); h <- max(s$h)
    if (w < 25 || h < 20) out <- c(out, sprintf("热图主体 %.0f×%.0fmm", w, h))
  }
  out
}
# 相邻面板的标题、副标题在同一行上挨得太近（< 3mm）：读起来连成一句（2026-10-26 实例：两格副标题相距 1.5mm，看着是一行字）
.mc_title_crowd <- function(plot, geo) {
  bx <- geo$text; tt <- unique(.mc_titles(plot))
  if (is.null(bx) || !nrow(bx) || length(tt) < 2) return(character())
  bx <- bx[bx$label %in% tt, ]; out <- character()
  for (i in seq_len(nrow(bx) - 1)) for (j in (i + 1):nrow(bx)) {
    h <- (min(bx$y1[i], bx$y1[j]) - max(bx$y0[i], bx$y0[j])) * 25.4
    gap <- (max(bx$x0[i], bx$x0[j]) - min(bx$x1[i], bx$x1[j])) * 25.4
    if (h > 0.3 && gap < 3) out <- c(out, sprintf("「%s」与「%s」相距 %.1fmm", bx$label[i], bx$label[j], max(gap, 0)))
  }
  out
}
.mc_text_overlaps <- function(plot, width_mm, height_mm, geo = NULL) {
  bx <- if (!is.null(geo)) geo$text else tryCatch(.mc_text_boxes(plot, width_mm, height_mm), error = function(e) NULL)
  if (is.null(bx) || !nrow(bx)) return(character())
  # 超出画布的文字会被裁掉半截（轴标题、图例最常见）
  out <- bx$label[bx$x0 < -0.2 / 25.4 | bx$y0 < -0.2 / 25.4 | bx$x1 > (width_mm + 0.2) / 25.4 | bx$y1 > (height_mm + 0.2) / 25.4]
  hits <- c(if (length(out)) sprintf("「%s」超出画布被裁", unique(out)), if (any(bx$cut)) sprintf("「%s」超出所在区域被裁", unique(bx$label[bx$cut])))
  for (i in seq_len(nrow(bx) - 1)) for (j in (i + 1):nrow(bx)) {
    # 上下方向要真交叠 0.3mm 以上；左右方向间距不足 0.3mm 也算（刻度标签首尾相接读起来就粘成一串）
    w <- (min(bx$x1[i], bx$x1[j]) - max(bx$x0[i], bx$x0[j])) * 25.4; h <- (min(bx$y1[i], bx$y1[j]) - max(bx$y0[i], bx$y0[j])) * 25.4
    # 同一位置重复画的相同标签（显著性括号这类按组重复绘制）看上去就是一段字，不算重叠
    if (bx$label[i] == bx$label[j] && max(abs(c(bx$x0[i] - bx$x0[j], bx$x1[i] - bx$x1[j], bx$y0[i] - bx$y0[j], bx$y1[i] - bx$y1[j]))) * 25.4 < 0.1) next
    if (w > -0.3 && h > 0.3) hits <- c(hits, sprintf("「%s」×「%s」", bx$label[i], bx$label[j]))
  }
  hits
}

# ── 文字压数据：只看数据区内的文字框，数框里有多少几何图形的墨迹（有可见墨迹且不足 80% 算压线/出界；几乎整段落在填充色块里的柱内标签不算）──
# 只含数据的底图：图层一个不删（坐标范围照原样训练），主题文字设透明、轴线与图例底框不画（版面不变）；嵌套的组合逐层同样处理。
.mc_data_only <- function(q) {
  if (.mc_nested(q)) attr(q, "grobs")$full <- .mc_data_only(attr(q, "grobs")$full)
  if (inherits(q, "patchwork") && !is.null(q$patches)) q$patches$plots <- lapply(q$patches$plots, .mc_data_only)
  if (!inherits(q, "ggplot")) return(q)
  q + theme(text = element_text(colour = NA), axis.text = element_text(colour = NA), axis.line = element_blank(), axis.ticks = element_blank(),
            legend.text = element_text(colour = NA), legend.title = element_text(colour = NA), legend.background = element_blank(), legend.box.background = element_blank(),
            legend.key = element_blank(), plot.tag = element_text(colour = NA), plot.title = element_text(colour = NA), plot.subtitle = element_text(colour = NA))
}
# 底图里凡是文字的 grob（文字、标签底框、ggrepel 绘制时才排位的文字树）一律换成空 grob：不按图层类名判断，
# ggvenn、显著性括号（geom_signif、stat_pvalue_manual）、嵌套子图等任何图层自带的字都不算数据
.mc_blank_text <- function(x) {
  if (inherits(x, c("text", "roundrect", "textrepeltree", "labelrepeltree"))) return(grid::nullGrob(name = x$name))
  if (inherits(x, "gtable")) x$grobs <- lapply(x$grobs, .mc_blank_text)
  else if (inherits(x, "gTree")) for (k in seq_along(x$children)) x$children[[k]] <- .mc_blank_text(x$children[[k]])
  x
}
.mc_text_on_data <- function(plot, w, h, geo = NULL, dpi = 150) {
  g <- if (is.null(geo)) .mc_geometry(plot, w, h) else geo; bx <- g$text; pn <- g$panels
  if (is.null(bx) || is.null(pn) || !requireNamespace("ragg", quietly = TRUE)) return(character())
  pn <- pn[pn$kind == "plot", ]; for (k in c("x", "y", "w", "h")) pn[[k]] <- as.numeric(pn[[k]])
  inpanel <- function(i) any(bx$x0[i] >= pn$x - 0.02 & bx$x1[i] <= pn$x + pn$w / 25.4 + 0.02 & bx$y0[i] >= pn$y - 0.02 & bx$y1[i] <= pn$y + pn$h / 25.4 + 0.02)
  f <- tempfile(fileext = ".png"); on.exit(unlink(f))
  ragg::agg_png(f, width = w, height = h, units = "mm", res = dpi, background = "white")
  tryCatch(grid::grid.draw(.mc_blank_text(suppressMessages(.mc_grob(.mc_data_only(plot))))), finally = grDevices::dev.off())
  a <- png::readPNG(f); ink <- pmin(a[, , 1], a[, , 2], a[, , 3]) < 0.80; H <- nrow(ink); Wd <- ncol(ink); out <- character()   # 同 .mc_edge：不逐像素 apply（2026-10-21 实测慢 40 倍、结果相同）
  for (i in seq_len(nrow(bx))) {
    x0 <- max(1, floor(bx$x0[i] * dpi)); x1 <- min(Wd, ceiling(bx$x1[i] * dpi)); y0 <- max(1, floor(H - bx$y1[i] * dpi)); y1 <- min(H, ceiling(H - bx$y0[i] * dpi))
    if (x1 <= x0 || y1 <= y0 || !inpanel(i)) next
    box <- ink[y0:y1, x0:x1]; cov <- mean(box)
    # 细线穿过文字只占很小比例：按墨迹像素数算（≥ 15 像素即一段可见的线）；≥ 80% 视为整段落在填充色块里（柱内标签）
    if (sum(box) >= 15 && cov < 0.8) out <- c(out, sprintf("「%s」（覆盖 %.0f%%）", bx$label[i], cov * 100))
  }
  unique(out)
}
# 数据区里的文字（森林图数值列、热图格子、柱上数值、图上标注）：回合末其中的小数都要能在结果表里找到；坐标刻度、色条、图例在数据区外，不在此列
.mc_data_texts <- function(geo) {
  bx <- geo$text; pn <- geo$panels
  if (is.null(bx) || is.null(pn)) return(character())
  for (k in c("x", "y", "w", "h")) pn[[k]] <- as.numeric(pn[[k]])
  inside <- vapply(seq_len(nrow(bx)), function(i) any(bx$x0[i] >= pn$x - 0.02 & bx$x1[i] <= pn$x + pn$w / 25.4 + 0.02 & bx$y0[i] >= pn$y - 0.02 & bx$y1[i] <= pn$y + pn$h / 25.4 + 0.02), logical(1))
  unique(as.character(bx$label[inside]))
}
# ── 曲线直接标注：读出已画好的线（按绘图坐标），在每组末端写组名（与线同色）代替图例；末端太近时上下错开 ──
mc_label_ends <- function(p, panel_mm = 70, layer = NULL, size_pt = 7) {
  b <- ggplot_build(p)
  if (is.null(layer)) layer <- which(vapply(p$layers, function(l) inherits(l$geom, c("GeomLine", "GeomStep", "GeomPath")), logical(1)))[1]
  d <- b$data[[layer]]; sc <- b$plot$scales$get_scales("colour")
  lev <- sc$get_limits(); col <- sc$map(lev)
  e <- do.call(rbind, lapply(split(d, d$group), function(s) s[which.max(s$x), c("x", "y", "colour")]))
  e$label <- lev[match(e$colour, col)]; e <- e[!is.na(e$label), ]; e <- e[order(e$y), ]
  gap <- diff(range(b$layout$panel_params[[1]]$y.range)) * 0.055
  for (i in seq_len(nrow(e))[-1]) if (e$y[i] - e$y[i - 1] < gap) e$y[i] <- e$y[i - 1] + gap
  xr <- b$layout$panel_params[[1]]$x.range; span <- diff(xr)
  # 标签放在数据区里：横轴右端按最长标签加长（不靠 plot.margin，拼图对齐时边距会被吃掉、标签伸进邻格）
  lab_mm <- max(nchar(e$label)) * size_pt * 0.2 + 2; data_mm <- max(panel_mm - 14, lab_mm + 15)
  extra <- span * lab_mm / (data_mm - lab_mm)
  # 在原坐标上只换横轴范围：原来的 ylim（KM 的 0–1）、expand 等设置都保留
  suppressMessages(p + annotate("text", x = e$x + span * 0.015, y = e$y, label = e$label, colour = e$colour, hjust = 0, size = size_pt / .pt) +
    ggproto(NULL, p$coordinates, limits = list(x = c(min(d$x), max(d$x) + extra), y = p$coordinates$limits$y)) + guides(x = guide_axis(cap = "upper")) +
    theme(legend.position = "none"))
}
# 连续轴的显示范围、刻度与变换名（各版本 ggplot2 接口不同，取不到返回 NULL）
.mc_axes <- function(q) {
  b <- tryCatch(suppressWarnings(ggplot_build(q)), error = function(e) NULL); if (is.null(b)) return(NULL)
  pp <- b$layout$panel_params[[1]]
  lapply(c(x = "x", y = "y"), function(ax) {
    v <- pp[[ax]]; sc <- (if (ax == "x") b$layout$panel_scales_x else b$layout$panel_scales_y)[[1]]
    if (is.null(v) || is.null(sc) || isTRUE(sc$is_discrete())) return(NULL)
    tr <- tryCatch(sc$get_transformation()$name, error = function(e) tryCatch(sc$trans$name, error = function(e) "identity"))
    br <- tryCatch(v$get_breaks(), error = function(e) NULL); lb <- tryCatch(v$get_labels(), error = function(e) NULL)
    list(range = tryCatch(v$continuous_range, error = function(e) NULL), breaks = br, labels = lb, trans = tr,
         title = tryCatch(as.character(get_labs(q)[[ax]]), error = function(e) as.character(q$labels[[ax]])))
  })
}
# 刻度标签四舍五入后与刻度不符：等间距刻度上，标签解析出的数（去掉 %、千分位、空格，认 Unicode 负号）按同一倍数换算回刻度，偏差超过一格的 1/10 即不符
.mc_tick_mismatch <- function(q) {
  out <- character()
  for (ax in Filter(Negate(is.null), .mc_axes(q))) {
    if (!identical(ax$trans, "identity") || length(ax$breaks) != length(ax$labels) || !is.character(ax$labels)) next
    keep <- is.finite(ax$breaks) & !is.na(ax$labels) & nzchar(trimws(ax$labels)); br <- ax$breaks[keep]; lb <- ax$labels[keep]
    if (length(br) < 3) next
    step <- diff(br); if (max(abs(step - mean(step))) > 1e-9 * max(1, abs(mean(step)))) next
    v <- suppressWarnings(as.numeric(gsub("[\\s,%   ]", "", gsub("−", "-", lb), perl = TRUE)))
    if (anyNA(v)) next
    # 按绝对值拟合（四舍五入不改正负号）：镜像柱、人口金字塔两侧都写正数（labels = abs）也成立
    k <- sum(abs(v * br)) / sum(br^2); if (!is.finite(k) || k == 0) next
    bad <- which(abs(abs(v) - k * abs(br)) > 0.1 * k * mean(step))
    if (length(bad)) out <- c(out, sprintf("刻度 %s 显示成「%s」", format(br[bad[1]], digits = 6), lb[bad[1]]))
  }
  out
}
# 同一位置的组间点/误差线重叠：离散轴同一取值上有两组以上的点或误差线、没有错开（position_dodge），区间重叠过半或点几乎重合
.mc_dodge_overlap <- function(q) {
  b <- tryCatch(suppressWarnings(ggplot_build(q)), error = function(e) NULL); if (is.null(b)) return(character())
  out <- character()
  for (k in seq_along(q$layers)) {
    l <- q$layers[[k]]
    if (!inherits(l$geom, c("GeomPoint", "GeomErrorbar", "GeomErrorbarh", "GeomPointrange", "GeomLinerange", "GeomCrossbar"))) next
    if (inherits(l$position, c("PositionDodge", "PositionDodge2", "PositionJitterdodge", "PositionJitter"))) next
    d <- b$data[[k]]
    for (ax in c("x", "y")) {
      sc <- (if (ax == "x") b$layout$panel_scales_x else b$layout$panel_scales_y)[[1]]
      if (is.null(sc) || !isTRUE(sc$is_discrete()) || is.null(d[[ax]])) next
      o <- if (ax == "x") "y" else "x"; lo <- if (is.null(d[[paste0(o, "min")]])) d[[o]] else d[[paste0(o, "min")]]; hi <- if (is.null(d[[paste0(o, "max")]])) d[[o]] else d[[paste0(o, "max")]]
      if (is.null(lo) || is.null(hi)) next
      span <- diff(range(c(lo, hi), na.rm = TRUE)); if (!is.finite(span) || span == 0) span <- 1
      for (key in split(seq_len(nrow(d)), paste(d$PANEL, round(d[[ax]], 6)))) {
        if (length(unique(d$group[key])) < 2) next
        for (i in key) for (j in key) if (i < j && d$group[i] != d$group[j]) {
          ov <- min(hi[i], hi[j]) - max(lo[i], lo[j]); short <- min(hi[i] - lo[i], hi[j] - lo[j])
          if ((short > 0 && ov > 0.5 * short) || (short <= 0 && abs(lo[i] - lo[j]) < 0.02 * span)) { out <- c(out, as.character(round(d[[ax]][i], 3))); break }
        }
      }
    }
  }
  unique(out)
}
# 并排同类面板：同一排、坐标轴标题相同（同一指标）的面板，数据区宽（或高）相差超过 10%、或该轴范围/刻度不同；比值没用对数轴。都只提醒（排版问题不硬拦）：
# 森林图要逐行横向比效应量必须一致，量级不同的多基因箱线图、随访时长不同的 KM、有意加宽的主图可保留，看图判断
.mc_side_by_side <- function(plot, width_mm, height_mm, tag) {
  res <- character()
  if (!inherits(plot, "patchwork") || length(plot) < 2) return(res)
  a <- tryCatch(.mc_patch_areas(plot, width_mm, height_mm), error = function(e) NULL); if (is.null(a)) return(res)
  labs <- .mc_tag_labels(tag, length(plot))
  axes <- lapply(seq_len(length(plot)), function(i) if (is.na(a$w[i])) NULL else .mc_axes(plot[[i]]))
  same <- function(u, v) length(u) == length(v) && all(abs(u - v) <= 1e-3 * max(1, abs(c(u, v))))
  for (r in unique(na.omit(a$row))) {
    idx <- which(a$row == r & !vapply(axes, is.null, logical(1)))
    for (ax in c("x", "y")) {
      titles <- vapply(idx, function(i) { t <- axes[[i]][[ax]]$title; if (length(t) && nzchar(t[1])) t[1] else NA_character_ }, character(1))
      for (t in unique(na.omit(titles))) {
        g <- idx[which(titles == t)]; if (length(g) < 2) next
        size <- if (ax == "x") a$w[g] else a$h[g]; msg <- character()
        if (max(size) > 1.1 * min(size)) msg <- c(msg, sprintf("「%s」%s数据区 %s 不等宽：widths 用等宽（如 c(1, 1, 1)；patchwork 的 widths 管的是数据区，条目名加在外面）",
          t, paste(labs[g], collapse = "/"), paste(sprintf("%s %.0fmm", labs[g], size), collapse = "、")))
        rng <- lapply(g, function(i) axes[[i]][[ax]]$range); brk <- lapply(g, function(i) sort(axes[[i]][[ax]]$breaks[is.finite(axes[[i]][[ax]]$breaks)]))
        if (!all(vapply(rng[-1], same, logical(1), rng[[1]])) || !all(vapply(brk[-1], same, logical(1), brk[[1]])))
          msg <- c(msg, sprintf("「%s」%s的坐标范围或刻度不同：设成相同范围与刻度（coord_cartesian 同一组 limits、同一组 breaks）", t, paste(labs[g], collapse = "/")))
        if (!length(msg)) next
        res <- c(res, paste0("并排同类面板不一致：", msg, "。森林图这类逐行横向比效应量的必须一致；量级不同、随访时长不同、主图有意加宽可保留，看图确认"))
      }
    }
  }
  ratio <- "\\b(s|cs)?(HR|OR|RR|IRR)\\b|(?i:hazard ratio|odds ratio|risk ratio)"
  for (i in seq_along(axes)) for (ax in c("x", "y")) {
    v <- axes[[i]][[ax]]
    if (!is.null(v) && length(v$title) && grepl(ratio, v$title[1], perl = TRUE) && !grepl("log", v$trans))
      res <- c(res, sprintf("%s「%s」是比值却用了线性轴：改 scale_%s_log10()，1 两侧的倍数才对称", labs[i], v$title[1], ax))
  }
  res
}
# 面板字母位置：每个字母配它右下方最近的数据区（每个数据区只认离它最近的字母），同一排字母基线高度相差 > 1.5mm 即不一致
# （字母锚在格子最左缘，到数据区的距离随条目名宽度变，不再比）
.mc_tag_drift <- function(geo) {
  tg <- geo$text; pn <- geo$panels
  if (is.null(tg) || is.null(pn) || !"tag" %in% names(tg)) return(character())
  tg <- tg[tg$tag, ]; pn <- pn[pn$kind == "plot", ]; if (nrow(tg) < 2 || !nrow(pn)) return(character())
  for (k in c("x", "y", "w", "h")) pn[[k]] <- as.numeric(pn[[k]])
  px <- pn$x * 25.4; pt <- pn$y * 25.4 + pn$h; tx <- tg$x1 * 25.4; ty <- tg$y0 * 25.4
  own <- vapply(seq_along(px), function(j) { ok <- which(tx <= px[j] + 0.5 & ty >= pt[j] - 0.5); if (!length(ok)) NA_integer_ else ok[which.min((px[j] - tx[ok])^2 + (ty[ok] - pt[j])^2)] }, integer(1))
  m <- do.call(rbind, lapply(seq_len(nrow(tg)), function(i) { j <- which(own == i); if (!length(j)) return(NULL); j <- j[which.min((px[j] - tx[i])^2 + (ty[i] - pt[j])^2)]
    data.frame(label = tg$label[i], dx = px[j] - tx[i], base = ty[i], top = pt[j]) }))
  if (is.null(m) || nrow(m) < 2) return(character())
  m <- m[order(-m$top), ]; m$row <- cumsum(c(TRUE, abs(diff(m$top)) > 2)); out <- character()
  for (r in unique(m$row)) { s <- m[m$row == r, ]
    if (nrow(s) > 1 && diff(range(s$base)) > 1.5)
      out <- c(out, paste(sprintf("%s 基线 %.1fmm", s$label, s$base), collapse = "，")) }
  out
}

# ── 审计：算出来的事实。返回 list(hard, warn, info)，并打印中文报告。hard 只有数据错误（会误导读者），其余都是 warn（提醒，看图自查） ──
mc_audit <- function(plot, png_path = NULL, width_mm, height_mm, dpi = NULL, spec = mc_spec(), quiet = FALSE) {
  hard <- c(); warn <- c(); info <- c()
  dpi <- if (is.null(dpi)) spec$dpi else dpi
  for (q in .mc_plots(plot)) {
    if (.mc_scale_losses(q)) hard <- c(hard, "坐标范围导致数据行/置信区间被删除：改用 coord_cartesian() 保留区间，或明确绘制越界截断箭头。")
    else if (.mc_scale_losses(q, text = TRUE)) warn <- c(warn, "标注文字超出坐标范围被删掉：把标注放进坐标范围内，或用 coord_cartesian() 设视窗（不删图层数据）")
    titles <- .mc_raw_legend_titles(q)
    if (length(titles)) warn <- c(warn, paste0("图例标题是原始变量名：", paste(titles, collapse = "、"), "；改成读者能理解的显示名称。"))
    ticks <- tryCatch(.mc_tick_mismatch(q), error = function(e) character())
    if (length(ticks)) warn <- c(warn, sprintf("刻度标签四舍五入后与刻度不符（%s）：调高标签精度（如 percent_format(accuracy = 0.1)）或改用整齐的 breaks（如每 2%%、5%% 一格）", paste(ticks, collapse = "；")))
    dodge <- tryCatch(.mc_dodge_overlap(q), error = function(e) character())
    if (length(dodge)) warn <- c(warn, sprintf("同一位置的组间点/误差线重叠（%s 处）：用 position_dodge(width = 0.4) 把各组错开，点与误差线用同一个 dodge", paste(dodge, collapse = "、")))
  }
  # 1 字号
  sizes <- unlist(lapply(.mc_plots(plot), .mc_text_sizes))
  small <- sizes[sizes < spec$minFontPt - 1e-6]
  # 显示向下取到 0.01：一位小数会把 6.96 显示成「7.0pt 低于 7pt」，自相矛盾
  if (length(small)) warn <- c(warn, sprintf("字号低于规格下限 %spt：%s，改成 ≥%spt", spec$minFontPt,
    paste(unique(sprintf("%s=%.2fpt", names(small), floor(small * 100 + 1e-6) / 100)), collapse = "，"), spec$minFontPt))
  # 2 图上编号标题
  bad <- grep("^\\s*(fig(ure)?\\.?|图)\\s*\\d", .mc_titles(plot), ignore.case = TRUE, value = TRUE)
  if (length(bad)) warn <- c(warn, sprintf("图上写了编号大标题「%s」：图号与图注属正文，去掉", bad[1]))
  lett <- grep("^\\s*(\\([A-Za-z]\\)|[A-Za-z][.:)]|[A-Za-z]\\s{2,})", .mc_titles(plot), value = TRUE)
  if (length(lett)) warn <- c(warn, sprintf("面板字母写进了标题「%s」：标题只写内容，字母交给 mc_compose（plot_annotation(tag_levels)）统一放左上角", trimws(lett[1])))
  if (length(attr(plot, "mc_problems"))) warn <- c(warn, attr(plot, "mc_problems"))
  geo <- tryCatch(.mc_geometry(plot, width_mm, height_mm), error = function(e) NULL)   # 一次渲染，下面几项共用
  overlaps <- .mc_text_overlaps(plot, width_mm, height_mm, geo)
  ondata <- if (is.null(geo)) character() else tryCatch(.mc_text_on_data(plot, width_mm, height_mm, geo), error = function(e) character())
  if (length(ondata)) warn <- c(warn, sprintf("文字或图例压在数据上 %d 处：%s。曲线用 mc_label_ends() 在线尾直接标注、图例放到本面板下方（legend.position = \"bottom\"）、风险人数等说明放进图下方的表或图注，不要写在数据区里；柱内数值标签放不下就去掉或改到柱外", length(ondata), paste(head(ondata, 4), collapse = "、")))
  crowd <- if (is.null(geo)) character() else tryCatch(.mc_title_crowd(plot, geo), error = function(e) character())
  if (length(crowd)) warn <- c(warn, sprintf("相邻面板的标题或副标题挤在一起 %d 处：%s。缩短副标题（设定细节放图注）或调 widths，两格之间至少留 3mm", length(crowd), paste(head(crowd, 3), collapse = "、")))
  squashed <- .mc_squashed(plot, width_mm, height_mm, geo)
  if (length(squashed)) warn <- c(warn, sprintf("数据区被压扁或太小 %d 处：%s。加宽这一列（widths）、加高这一排（heights）、缩短长标签，或换通栏画布、减少一行的面板数", length(squashed), paste(head(squashed, 4), collapse = "、")))
  if (length(overlaps)) warn <- c(warn, sprintf("文字重叠或被裁 %d 处：%s%s。标注用 ggrepel::geom_text_repel 避让（拥挤时上调往右、下调往左引出），或只标关键项、加大面板、缩短标签", length(overlaps), paste(head(overlaps, 4), collapse = "、"), if (length(overlaps) > 4) " 等" else ""))
  cjk <- unique(grep("[\u4e00-\u9fff]", geo$text$label, value = TRUE))
  if (length(cjk)) warn <- c(warn, sprintf("图内有中文 %d 处：%s。投 SCI 的图内文字（标题、坐标轴、图例、分组标签）一律英文；用户明确要求中文时忽略本条", length(cjk), paste(head(cjk, 4), collapse = "、")))
  pasted <- .mc_pasted(plot)
  if (length(pasted)) warn <- c(warn, sprintf("用 %s：把渲染好的 PNG 当图片拼，字号随缩放乱变、对不齐。在绘图对象层面用 mc_compose() 拼；真实影像照片用 mc_image()", paste(pasted, collapse = "、")))
  # 3 面板字母、并排同类面板与宽高比（一行放几个面板不设上限，挤不挤看上面数据区与文字的几何审计）
  g <- .mc_grid(plot)
  if (g$n > 1 && inherits(plot, "patchwork") && is.null(plot$patches$annotation$tag_levels))
    warn <- c(warn, sprintf("%d 个面板没有面板字母：用 mc_compose() 拼，或加 plot_annotation(tag_levels = \"%s\")", g$n, spec$panelLabel))
  drift <- if (is.null(geo)) character() else tryCatch(.mc_tag_drift(geo), error = function(e) character())
  if (length(drift)) warn <- c(warn, sprintf("同一排面板字母位置不一致（%s）：用 mc_compose() 拼，字母自动放在各自格子左上角、同排同一基线；拼好后不要再用 & 套整套主题（会冲掉字母设置）", paste(drift, collapse = "；")))
  warn <- c(warn, tryCatch(.mc_side_by_side(plot, width_mm, height_mm, spec$panelLabel), error = function(e) character()))
  ar <- (width_mm / g$cols) / (height_mm / g$rows)
  if (ar > 2.5 || ar < 0.6) warn <- c(warn, sprintf("单面板宽高比 %.2f 超出 1:1–2:1 的舒适区（%.0f×%.0fmm，%d 列 %d 行）", ar, width_mm, height_mm, g$cols, g$rows))
  info <- c(info, sprintf("面板 %d 个，%d 列 %d 行，单面板约 %.0f×%.0fmm", g$n, g$cols, g$rows, width_mm / g$cols, height_mm / g$rows))
  # 4 配色
  # 配色体检不过、同一分组与项目配色表不一致都提醒（纯数字的分组不比，各图含义不同）；有形状/线型/直接标注兜底的只需看图确认
  hex <- function(x) toupper(grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue = 255))
  pal <- .mc_read_palette()
  for (sc in unique(.mc_scale_colours(plot))) {
    chk <- mc_check_palette(sc$cols)
    if (!isTRUE(chk$pass)) {
      msg <- sprintf("离散配色 %d 色未过色盲/灰度体检（protan %.0f / deutan %.0f / tritan %.0f / 灰度 %.0f，判据 ≥20 与 ≥15）", chk$n, chk$protan, chk$deutan, chk$tritan, chk$grayscale_L)
      warn <- c(warn, paste0(msg, if (sc$redundant) "：已有形状/线型/标注区分，看图确认" else "：改用 mc_colors() / mc_palette() 的期刊柔和色，或加形状/线型/直接标注"))
    }
    for (lv in names(sc$cols)) {
      if (grepl("^[0-9.]+$", lv) || is.null(pal[[lv]]) || is.na(sc$cols[[lv]])) next
      if (hex(pal[[lv]]) != hex(sc$cols[[lv]])) warn <- c(warn, sprintf("分组「%s」在本项目配色表里是 %s，这张图用了 %s：用 mc_colors() 取色，同一分组全文同一个颜色", lv, pal[[lv]], hex(sc$cols[[lv]])))
    }
  }
  # 5 图例份数
  lc <- .mc_legend_count(plot, width_mm, height_mm)
  signatures <- Filter(Negate(is.null), lapply(.mc_plots(plot), .mc_legend_signature))
  duplicate <- length(signatures) > 1 && any(vapply(seq_along(signatures)[-1], function(i) any(vapply(signatures[seq_len(i - 1)], function(x) .mc_same_legend(x, signatures[[i]]), logical(1))), logical(1)))
  if (!is.na(lc) && lc > 1 && duplicate) warn <- c(warn, "相同图例重复出现：可用 mc_compose 自动合并；不同语义的图例保留。")
  # 6 规格：宽度、高度、dpi（figure_spec.json 的 maxWidthMm 按目标期刊覆盖），超规格与不在单栏 / 1.5 栏 / 通栏三档上都提醒
  maxw <- if (is.null(spec$maxWidthMm)) spec$doubleColumnMm else spec$maxWidthMm
  if (width_mm > maxw + 0.5) warn <- c(warn, sprintf("宽度 %.0fmm 超过期刊版面最大宽度 %smm（规格来源：%s）", width_mm, maxw, spec$journal))
  else if (all(abs(width_mm - unlist(spec[c("singleColumnMm", "midColumnMm", "doubleColumnMm")])) > 3))
    warn <- c(warn, sprintf("宽度 %.0fmm 不是单栏 %s / 1.5 栏 %s / 通栏 %smm（规格来源：%s）", width_mm, spec$singleColumnMm, spec$midColumnMm, spec$doubleColumnMm, spec$journal))
  if (!is.null(spec$maxHeightMm) && height_mm > spec$maxHeightMm) warn <- c(warn, sprintf("高度 %.0fmm 超过规格上限 %smm", height_mm, spec$maxHeightMm))
  if (dpi < spec$dpi) warn <- c(warn, sprintf("导出 %d dpi 低于规格 %s dpi", dpi, spec$dpi))
  # 7 渲染后贴边/裁切
  if (!is.null(png_path)) {
    e <- .mc_edge(png_path, dpi); e <- e[!is.null(e) & e >= 0.8]
    if (length(e)) warn <- c(warn, sprintf("渲染图有内容伸到画布边缘被切（%s）：多半是图例、色条或长标签放不下，图例改放本面板下方、缩短标签或加 plot.margin，不要伸出画布", paste(sprintf("%s边 %.1fmm", names(e), e), collapse = "、")))
  }
  rep <- c(if (length(hard)) c("【硬拦：数据错误，必须改】", paste0("- ", hard)) else "【硬拦项】无",
           if (length(warn)) c("【审计提醒，看图核对】", paste0("- ", warn)) else "【审计提醒】无",
           paste0("· ", info))
  if (!quiet) cat(paste(rep, collapse = "\n"), "\n")
  invisible(list(ok = length(hard) == 0, hard = hard, warn = warn, info = info, report = paste(rep, collapse = "\n"),
                 texts = unique(as.character(geo$text$label)),   # 图内文字记进回执，回合末核对其中统计量的来源
                 data_texts = if (is.null(geo)) character() else tryCatch(.mc_data_texts(geo), error = function(e) character())))
}

# ── 出图自查清单：mc_save 落盘后打印，AI 用 Read 看图逐条核对（与 SKILL.md「出图自查清单」、mc_figure.py 的 SELFCHECK 逐字一致，测试比对） ──
.MC_SELFCHECK <- c(
  "文字和图例没有压住曲线、点、柱子或误差线，柱内数字放得下。",
  "文字之间不重叠、不首尾粘连，基因名、刻度、图例各自分得清。",
  "没有文字被裁掉半截或贴到画布边缘，轴标题和图例完整。",
  "白底、无网格、无灰底和多余边框，只留左下细轴线。",
  "整张图只用一套语义配色，同一分组和其他图同色，黑白打印也分得清（分不清就加形状、线型或直接标注）。",
  "图例标题是读者看得懂的名称（不是 strata、group 这类变量名），相同图例只留一份，图例不挤占数据区。",
  "每个数据区大小合适，没有被压成窄条或扁条，主面板一眼能找到。",
  "缩到印刷尺寸所有文字仍读得清，最小字号不低于规格下限（默认 7pt）。",
  "每个面板左上角有统一大小的粗体字母，同一排字母高度和到数据区的距离一致，字母没写进标题。",
  "图上没有 Figure N / 图 N 这类编号标题，面板标题只写内容。",
  "并排比较同一指标的面板等宽、坐标范围和刻度相同，比值（HR/OR/RR）用对数轴并画出无效线。",
  "刻度标签和刻度位置对得上，没有四舍五入后显示错的值（如 0.025 显示成 2%）。",
  "同一位置的多组点和误差线已错开，没有叠在一起。",
  "数据完整：没有点或置信区间被坐标范围删掉，超出范围的画了截断箭头。",
  "n、统计量、误差棒类型（SD/SEM/CI）和单位在图上或图注里写清。",
  "每个面板标题说出这一格的发现，关键统计量（效应量与 95% CI、P、n）标在图上且不压数据。",
  "没有原始变量代码或软件包名，没有空白或只写一行字的占位面板，每种颜色都有图例或直接标注说明。",
  "版面整齐：同列各面板数据区左缘、同排数据区上下沿与横轴各在一条直线上，每排铺满、留白均匀，格子大小与内容相称，图例放法和各面板风格一致（逐项见「版面整齐」）。"
)

# ── 落盘：先渲到临时文件审计，只有数据错误类硬拦才不落盘（被拦的存到项目 .medcortex/figure_qc/rejected/ 供查看）；
#    排版样式问题照常落盘，打印本图提醒 + 自查清单，AI 看图自查 ──
mc_save <- function(plot, file, width_mm, height_mm, dpi = NULL, spec = mc_spec(), bg = "white") {
  # 只渲染与审计 PNG：别的后缀写出去仍是 PNG 字节，被拦时还会覆盖同名原文件，所以写任何东西之前就拒绝
  if (!grepl("\\.png$", file, ignore.case = TRUE)) stop(sprintf("mc_save 只输出 PNG：%s 请改成 .png 后缀（期刊要 TIFF/PDF 时，过审后再由 PNG 转换）。", file), call. = FALSE)
  dpi <- if (is.null(dpi)) spec$dpi else dpi
  # 先把图算出来再开设备：mc_save(mc_compose(...)) 时拼图在 ggsave 开的设备里才被求值，拼图量尺寸开关的设备会把它关掉，图没写出来
  force(plot)
  tmp <- tempfile(fileext = ".png")
  if (requireNamespace("ragg", quietly = TRUE)) ggsave(tmp, plot, device = ragg::agg_png, width = width_mm, height = height_mm, units = "mm", dpi = dpi, bg = bg)
  else ggsave(tmp, plot, width = width_mm, height = height_mm, units = "mm", dpi = dpi, bg = bg)
  a <- mc_audit(plot, tmp, width_mm, height_mm, dpi, spec, quiet = TRUE)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  if (!a$ok) {
    cat(a$report, "\n")
    rej <- .mc_rejected_path(file); file.copy(tmp, rej, overwrite = TRUE); unlink(tmp)
    stop(sprintf("未落盘 %s：有数据错误类硬拦项，见上方报告；被拦的图存在 %s 供查看。", file, rej), call. = FALSE)
  }
  if (!file.copy(tmp, file, overwrite = TRUE)) stop(sprintf("未落盘 %s：渲染没有生成图片文件。", file), call. = FALSE)
  unlink(tmp)
  .mc_receipt(file, width_mm, height_mm, dpi, a)
  cat(paste(c(sprintf("已落盘 %s（%.0f×%.0fmm，%d dpi）。", file, width_mm, height_mm, dpi),
              if (length(a$warn)) c("【审计提醒】（程序算出的排版样式问题，不拦，可能误报，看图判断改不改）", paste0("- ", a$warn)) else "【审计提醒】无",
              "【出图自查清单】", sprintf("%d. %s", seq_along(.MC_SELFCHECK), .MC_SELFCHECK),
              sprintf("用 Read 打开 %s 亲眼看图，逐条对照清单与提醒；不合格就改代码重画、重新 mc_save；全部合格后在回复里逐条写明核对结果。完成门会核对每张图落盘后是否看过这一版，过关再画下一张。", normalizePath(file))),
            collapse = "\n"), "\n")
  invisible(a)
}

# ── 体检回执：figures/ 根目录的投稿大图过了审计才留，阶段完成门按它核对（文件指纹对不上 = 落盘后被别的代码改写过） ──
.mc_receipt <- function(file, width_mm, height_mm, dpi, a) {
  f <- normalizePath(file)
  if (basename(dirname(f)) != "figures") return(invisible(NULL))
  dir <- file.path(dirname(dirname(f)), ".medcortex", "figure_qc"); dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  esc <- function(x) gsub("([\"\\\\])", "\\\\\\1", gsub("[\r\n\t]", " ", x))
  warn <- if (length(a$warn)) paste0("\"", esc(a$warn), "\"", collapse = ",") else ""
  quote <- function(x) if (length(x)) paste0("\"", esc(x), "\"", collapse = ",") else ""
  writeLines(sprintf('{"file":"%s","md5":"%s","width_mm":%s,"height_mm":%s,"dpi":%d,"warn":[%s],"texts":[%s],"data_texts":[%s],"at":"%s"}',
                     esc(basename(f)), unname(tools::md5sum(f)), width_mm, height_mm, as.integer(dpi), warn, quote(a$texts), quote(a$data_texts),
                     format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), file.path(dir, paste0(basename(f), ".json")), useBytes = TRUE)
  invisible(NULL)
}
