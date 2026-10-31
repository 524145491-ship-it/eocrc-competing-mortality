# MedCortex 运行追踪的检查点（由 AnalysisTrace 生成，请勿修改）。
#
# 用法：source("scripts/mc_trace.R")
#       df <- mc_checkpoint(df, "排除年龄 < 18 岁", id = "SEQN")
#       rx <- mc_checkpoint(rx, "处方药表", id = "SEQN", side = TRUE)   # 单独读入、要并回主表的另一张表
# 读入数据后打一个，每次筛选 / 合并后再打一个。人数由程序在真实数据上数，写进运行记录；
# 不在 AnalysisTrace 里运行时什么都不做。只用基础 R，不依赖任何包。

.mc_json_str <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\r", "\\\\r", x)
  x <- gsub("\t", "\\\\t", x)
  x <- gsub("[[:cntrl:]]", " ", x)
  paste0("\"", x, "\"")
}

# ID / 取值的规范写法，与 Python 版同一套：整数值写成整数（-0 也是 0），其余小数逐个按 17 位有效数字；
# integer64 按原样的十进制；因子按标签；缺失与空白字符串不算
.mc_keys <- function(v) {
  v <- v[!is.na(v)]
  if (is.factor(v)) v <- as.character(v)
  if (is.character(v)) v <- v[nzchar(trimws(v))]
  keys <- if (inherits(v, "integer64")) as.character(v)
    else if (is.logical(v)) ifelse(v, "TRUE", "FALSE")
    else if (is.numeric(v)) { v[v == 0] <- 0; ifelse(v == round(v), sprintf("%.0f", v), sprintf("%.17g", v)) }
    else as.character(v)
  enc2utf8(as.character(keys))
}

# 去重后的 ID 个数与指纹（md5）；数据仓落表时也用它，和 Python 版算法一致
.mc_id_hash <- function(v) {
  keys <- sort(unique(.mc_keys(v)), method = "radix")
  if (!length(keys)) return(list(n = 0L, hash = NULL))
  tmp <- tempfile()
  con <- file(tmp, open = "wb")
  writeLines(keys, con, sep = "\n", useBytes = TRUE)
  close(con)
  hash <- unname(tools::md5sum(tmp))
  unlink(tmp)
  list(n = length(keys), hash = hash)
}

# 主脚本里写这句调用的那一行：Rscript 不留行号，这里解析一次主脚本（--file=），按语法树找和最外层调用一样的那句——
# 不受引号、数字写法（1e+05）、空格、管道写法影响；找不到（如被 source 进来运行）记 NA，追踪再按调用原文与标签找
.mc_src <- new.env()
.mc_entry_line <- function(top) {
  if (is.null(.mc_src$exprs)) {
    f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
    .mc_src$exprs <- if (length(f) == 1 && file.exists(f)) tryCatch(parse(f, keep.source = TRUE), error = function(e) expression()) else expression()
  }
  ex <- .mc_src$exprs
  has <- function(e) {
    if (identical(e, top)) return(TRUE)
    if (!is.call(e)) return(FALSE)
    p <- as.list(e)
    for (k in seq_along(p)) if (is.call(p[[k]]) && has(p[[k]])) return(TRUE)
    FALSE
  }
  refs <- attr(ex, "srcref")
  for (i in seq_along(ex)) if (!is.null(refs) && has(ex[[i]])) return(refs[[i]][1])
  NA
}

# 保留的编号（只存编号）写成压缩文件：核验时可从被排除的人里抽查
.mc_write_ids <- function(v, hash, path) {
  folder <- file.path(dirname(path), "ids")
  dir.create(folder, showWarnings = FALSE)
  target <- file.path(folder, paste0(hash, ".txt.gz"))
  if (file.exists(target)) return(invisible(NULL))
  con <- gzfile(target, "wb")  # 二进制写 UTF-8：与指纹算法一致，Windows 上不转 CRLF、不转本机编码
  writeLines(sort(unique(.mc_keys(v)), method = "radix"), con, useBytes = TRUE)
  close(con)
}

# 按 UTF-8 原始字节追加写：R 4.2 以前的 Windows 版 cat 会把中文标签转成 GBK，Python 按 UTF-8 读出来是乱码
.mc_append <- function(path, text, append = TRUE) {
  con <- file(path, if (append) "ab" else "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
}

# keep：纳排只打标记、不删行时传入组标记，只数标记为真的人，数据原样返回
mc_checkpoint <- function(df, label, id = NULL, keep = NULL, side = FALSE) {
  path <- Sys.getenv("MC_TRACE_CHECKPOINTS")
  if (!nzchar(path)) return(invisible(df))
  view <- if (is.null(keep)) df else df[which(keep %in% TRUE), , drop = FALSE]
  parts <- c(paste0("\"label\":", .mc_json_str(label)),
             paste0("\"rows\":", nrow(view)),
             paste0("\"columns\":", ncol(view)))
  if (isTRUE(side)) parts <- c(parts, "\"side\":true")
  if (!is.null(id)) {
    if (!id %in% names(view)) stop("mc_checkpoint：数据里没有 id 列 ", id, call. = FALSE)
    h <- .mc_id_hash(view[[id]])
    if (!is.null(h$hash)) .mc_write_ids(view[[id]], h$hash, path)
    parts <- c(parts, paste0("\"id\":", .mc_json_str(id)), paste0("\"n_id\":", h$n),
               paste0("\"id_hash\":", if (is.null(h$hash)) "null" else .mc_json_str(h$hash)))
  }
  miss <- head(vapply(view, function(x) sum(is.na(x)), numeric(1)), 500)
  parts <- c(parts, paste0("\"missing\":{", if (length(miss)) paste0(.mc_json_str(names(miss)), ":", miss, collapse = ",") else "", "}"))
  # 分类列（不超过 10 种取值）各取值的个数：交付说明里写的「男性 N 人」「死亡 N 例」由它核对；表太大时不数
  if (nrow(view) <= 2e6) {
    lv <- character(0)
    for (col in setdiff(head(names(view), 500), id)) {
      keys <- .mc_keys(view[[col]])
      if (!length(keys) || length(unique(keys)) > 10) next
      tab <- table(keys)
      lv <- c(lv, paste0(.mc_json_str(col), ":{", paste0(.mc_json_str(names(tab)), ":", as.integer(tab), collapse = ","), "}"))
    }
    parts <- c(parts, paste0("\"levels\":{", paste(lv, collapse = ","), "}"))
    # 数值列（超过 10 种取值）的最小、5% 分位、95% 分位、最大：收尾「取值体检」据此点名 9999 这类编码值、单位错
    rg <- character(0)
    for (col in setdiff(head(names(view), 500), id)) {
      x <- view[[col]]
      if (!is.numeric(x) || inherits(x, "integer64")) next
      x <- as.numeric(x[is.finite(x)])
      if (length(unique(x)) <= 10) next
      q <- c(min(x), quantile(x, c(0.05, 0.95), names = FALSE), max(x))
      rg <- c(rg, paste0(.mc_json_str(col), ":[", paste(sprintf("%.10g", q), collapse = ","), "]"))
    }
    parts <- c(parts, paste0("\"ranges\":{", paste(rg, collapse = ","), "}"))
  }
  # 最外层的调用：R 记不到文件和行号，追踪据此确认检查点确实由脚本里写的调用触发（直接调用或经技能函数）
  top <- sys.calls()[[1]]
  parts <- c(parts, paste0("\"call\":", .mc_json_str(paste(deparse(sys.call()), collapse = " "))),
             paste0("\"caller\":", .mc_json_str(paste(deparse(top[[1]]), collapse = " "))),
             paste0("\"top\":", .mc_json_str(paste(deparse(top), collapse = " "))))
  el <- tryCatch(.mc_entry_line(top), error = function(e) NA)
  if (!is.na(el)) parts <- c(parts, paste0("\"entry_line\":", el))
  .mc_append(path, paste0("{", paste(parts, collapse = ","), "}\n"))
  # 实际加载的包与版本：复跑结果不同时据此判断是不是环境变了
  envp <- Sys.getenv("MC_TRACE_ENV")
  if (nzchar(envp)) {
    ns <- sort(loadedNamespaces())
    v <- vapply(ns, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) ""), character(1))
    .mc_append(envp, paste0("{", paste0(.mc_json_str(ns), ":", .mc_json_str(v), collapse = ","), "}"), append = FALSE)
  }
  invisible(df)
}
