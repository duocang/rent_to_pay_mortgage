# ==============================================================================
# R/report.R — PDF 报告生成 (可扩展架构)
#
# 添加新报告章节步骤:
#   1. 在 R/translations.R 添加翻译键 (report_sec_xxx)
#   2. 在 build_report_params() 中传入新数据
#   3. 在 report/template.Rmd 添加新 section (用 null check 保护)
# ==============================================================================

#' 构建报告参数列表
#' @return 适用于 rmarkdown::render(params = ...) 的列表
build_report_params <- function(t, p, roi, tax, sens_oc, sens_rt, hold) {
  list(
    t            = t,
    p            = p,
    roi          = roi,
    tax          = tax,
    sens_oc      = sens_oc,
    sens_rt      = sens_rt,
    hold         = hold,
    generated_at = Sys.time(),
    app_dir      = getwd()
  )
}

#' 渲染 PDF 报告 (失败时回退到 HTML)
#'
#' @param report_params  build_report_params() 返回的列表
#' @param output_file    输出文件的绝对路径 (含 .pdf 后缀)
#' @param template_path  Rmd 模板路径 (默认 report/template.Rmd)
#' @return TRUE (PDF 成功) 或 FALSE (HTML 回退)
render_report <- function(report_params, output_file,
                          template_path = file.path("report", "template.Rmd")) {

  if (!requireNamespace("rmarkdown", quietly = TRUE))
    stop("Package 'rmarkdown' required.\n  install.packages('rmarkdown')")

  # ---- 确保 pandoc 可用 (RStudio 内运行通常已有) ----
  if (!rmarkdown::pandoc_available()) {
    rstudio_pandoc <- file.path("/Applications/RStudio.app",
      "Contents/Resources/app/quarto/bin/tools/aarch64")
    if (dir.exists(rstudio_pandoc)) {
      Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
    }
  }

  # ---- 检测 CJK 字体 ----
  cjk <- switch(Sys.info()[["sysname"]],
    "Darwin"  = "PingFang SC",
    "Windows" = "Microsoft YaHei",
                "Noto Sans CJK SC")

  # ---- 动态 LaTeX header ----
  htex <- file.path(tempdir(), "rpt_header.tex")
  writeLines(c(
    "\\usepackage{xeCJK}",
    paste0("\\setCJKmainfont{", cjk, "}"),
    "\\usepackage{booktabs}",
    "\\usepackage{longtable}",
    "\\usepackage{float}",
    "\\usepackage{xcolor}",
    "\\definecolor{posgreen}{HTML}{27AE60}",
    "\\definecolor{negred}{HTML}{E74C3C}",
    "\\usepackage{fancyhdr}",
    "\\pagestyle{fancy}",
    "\\fancyhf{}",
    "\\fancyfoot[C]{\\thepage}",
    "\\renewcommand{\\headrulewidth}{0pt}"
  ), htex)

  # ---- 复制模板到临时目录 ----
  tmp <- file.path(tempdir(), "mortgage_report.Rmd")
  file.copy(template_path, tmp, overwrite = TRUE)

  out_dir  <- dirname(output_file)
  out_name <- basename(output_file)

  # ---- 渲染 PDF → 失败回退 HTML ----
  tryCatch({
    rmarkdown::render(
      input         = tmp,
      output_format = rmarkdown::pdf_document(
        latex_engine    = "xelatex",
        toc             = TRUE,
        toc_depth       = 2,
        number_sections = TRUE,
        fig_caption     = TRUE,
        includes        = rmarkdown::includes(in_header = htex)
      ),
      output_file = out_name,
      output_dir  = out_dir,
      params      = report_params,
      envir       = new.env(parent = globalenv()),
      quiet       = TRUE
    )
    invisible(TRUE)
  }, error = function(e) {
    message("[report] PDF failed: ", e$message, "\n  → falling back to HTML")
    html_name <- sub("\\.pdf$", ".html", out_name)
    rmarkdown::render(
      input         = tmp,
      output_format = rmarkdown::html_document(
        toc = TRUE, toc_depth = 2, number_sections = TRUE,
        self_contained = TRUE, theme = "flatly"
      ),
      output_file = html_name,
      output_dir  = out_dir,
      params      = report_params,
      envir       = new.env(parent = globalenv()),
      quiet       = TRUE
    )
    file.copy(file.path(out_dir, html_name), output_file, overwrite = TRUE)
    invisible(FALSE)
  })
}
