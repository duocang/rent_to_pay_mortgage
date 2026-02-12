# ==============================================================================
# app.R — Shiny 主程序 (R/ 目录下的文件由 Shiny 自动加载)
# ==============================================================================
library(shiny)
library(plotly)
library(DT)
library(bslib)

source("R/translations.R")  # 多语言支持
source("R/compute.R")       # 计算引擎 (IRR, 基础ROI, 德国税务分析, 敏感度分析)
source("R/report.R")        # PDF 报告生成 (可扩展架构)

# ==============================================================================
# Helper: format €
# ==============================================================================
fmt_eur <- function(x, digits = 0) {
  paste0(formatC(round(x, digits), format = "f", digits = digits, big.mark = ","), " €")
}

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),

  tags$head(tags$style(HTML("
    .well { padding: 10px 15px; }
    .well .form-group {
      display: flex; flex-direction: row; align-items: center; margin-bottom: 5px;
    }
    .well .form-group > label {
      flex: 0 0 60%; max-width: 60%; margin-bottom: 0;
      font-weight: normal; font-size: 12.5px;
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding-right: 4px;
    }
    .well .form-group input { flex: 1; height: 28px; padding: 3px 6px; font-size: 12.5px; }
    .well hr { margin: 6px 0; }
    .well .checkbox { margin: 4px 0; font-size: 12.5px; }
    #calc_btn { margin-top: 8px; width: 100%; padding: 6px; }
    .sec-hdr { font-size: 13px; font-weight: 700; margin: 4px 0 2px 0; }
    .info-card { border-radius: 6px; padding: 14px 18px; margin-bottom: 12px; font-size: 13.5px; line-height: 1.7; }
    .info-card .card-title { font-size: 15px; font-weight: 700; margin-bottom: 8px; }
    .info-card .item { display: flex; justify-content: space-between; }
    .info-card .item span:last-child { font-weight: 600; }
    .note-box { border-left: 4px solid; padding: 10px 14px; margin: 8px 0;
                border-radius: 0 6px 6px 0; font-size: 13px; }
    .note-green { border-color: #27ae60; background: #eafaf1; }
    .note-blue  { border-color: #2980b9; background: #eaf2f8; }
    .metric-card { text-align: center; padding: 10px; border-radius: 6px; margin-bottom: 10px; position: relative; }
    .metric-card h5 { font-size: 13px; margin-bottom: 2px; }
    .metric-card h3 { margin: 0; font-size: 22px; }
    .help-btn { position:absolute; top:5px; right:7px; width:20px; height:20px;
      border-radius:50%; border:1.5px solid rgba(0,0,0,.2); background:rgba(255,255,255,.7);
      font-size:12px; line-height:17px; text-align:center; cursor:pointer;
      color:#555; font-weight:700; transition:all .2s; padding:0; }
    .help-btn:hover { background:#2c3e50; color:#fff; border-color:#2c3e50; }
    .calc-step { display:flex; justify-content:space-between; padding:4px 0;
      border-bottom:1px solid #f0f0f0; font-size:13px; }
    .calc-step:last-child { border-bottom:none; }
    .calc-step .lbl { color:#555; }
    .calc-step .val { font-weight:600; font-family:'SF Mono','Consolas',monospace; }
    .calc-formula { background:#f8f9fa; border-radius:6px; padding:10px 14px;
      font-family:'SF Mono','Consolas',monospace; font-size:12px;
      margin:8px 0; border-left:3px solid #3498db; line-height:1.7; }
    .calc-result { font-size:18px; font-weight:700; text-align:center;
      padding:8px; margin-top:6px; border-radius:6px; }
    .calc-section { font-weight:700; font-size:13px; margin:12px 0 4px 0;
      padding-bottom:3px; border-bottom:2px solid #eee; color:#2c3e50; }
    #lang-container {
      position: absolute; top: 15px; right: 30px; z-index: 1000;
      display: flex; gap: 10px;
    }
    .lang-btn {
      cursor: pointer; font-weight: bold; color: #95a5a6; font-size: 14px;
      transition: all .3s; text-decoration: none !important;
      border: none; background: none; padding: 0 2px;
    }
    .lang-btn:hover { color: #2c3e50; }
    .lang-btn.active { color: #2c3e50; font-size: 18px; border-bottom: 2px solid #2c3e50; }
    .summary-tbl td:first-child { font-weight: 600; }
    .summary-tbl td:last-child  { text-align: right; }
    .summary-tbl { width: 100%; font-size: 14px; }
    .summary-tbl td { padding: 5px 10px; border-bottom: 1px solid #eee; }
  ")),
  tags$script(HTML("
    $(document).on('click', '.lang-btn', function() {
      var lang = $(this).attr('data-value');
      Shiny.setInputValue('lang', lang);
      $('.lang-btn').removeClass('active');
      $(this).addClass('active');
    });
  "))),

  # ---- Language switcher ----
  tags$div(id = "lang-container",
    actionLink("lang_cn", "汉", class = "lang-btn active", `data-value` = "CN"),
    span("|", style = "color:#ccc"),
    actionLink("lang_en", "EN", class = "lang-btn", `data-value` = "EN"),
    span("|", style = "color:#ccc"),
    actionLink("lang_de", "DE", class = "lang-btn", `data-value` = "DE")
  ),

  uiOutput("ui_title"),

  sidebarLayout(
    sidebarPanel(width = 3,
      tags$div(class = "sec-hdr", uiOutput("sec1", inline = TRUE)),
      numericInput("purchase_price", "购买价格 (万元)", 25, step = 1),
      numericInput("extra_costs", "额外杂费 (万元)", 3, step = 0.5),
      numericInput("loan_amount", "贷款金额 (万元)", 20, step = 1),
      numericInput("annual_rate", "贷款年利率 (%)", 3.84, step = 0.01),
      numericInput("loan_term_years", "贷款年限 (年)", 20, step = 1),
      hr(),
      tags$div(class = "sec-hdr", uiOutput("sec2", inline = TRUE)),
      numericInput("start_year", "起始年份", as.integer(format(Sys.Date(), "%Y")), step = 1),
      numericInput("initial_rent", "月租金·净 (元)", 1200, step = 50),
      numericInput("monthly_utilities", "物业水暖网费 (元/月)", 0, step = 50),
      numericInput("annual_rent_increase", "租金年增幅 (元/月)", 50, step = 10),
      numericInput("hold_years", "持有年数", 10, step = 1),
      uiOutput("hold_years_alert"),
      numericInput("sale_price", "预期卖出价 (万元)", 28, step = 1),
      checkboxInput("prepay_with_excess", "多余租金自动提前还贷", value = FALSE),
      hr(),
      tags$div(class = "sec-hdr", uiOutput("sec3", inline = TRUE)),
      numericInput("monthly_salary", "月薪·税前 (元)", 5500, step = 100),
      numericInput("combined_tax_rate", "综合边际税率 (%)", 47.5, step = 0.5),
      numericInput("building_ratio", "建筑物占比 (%)", 70, step = 5),
      numericInput("annual_opcost", "年运营成本 (元)", 5000, step = 500),
      numericInput("afa_rate", "折旧率 AfA (%)", 2, step = 0.5),
      hr(),
      actionButton("calc_btn", "开始计算", class = "btn-primary")
    ),

    mainPanel(width = 9,
      uiOutput("ui_metrics"),
      uiOutput("ui_download_btn"),
      uiOutput("ui_tabs")
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # ---- Translation reactive ----
  tr <- reactive({
    lang <- input$lang; if (is.null(lang)) lang <- "CN"
    TR[[lang]]
  })

  # ---- Update labels on language change ----
  observeEvent(input$lang, {
    t <- tr(); lang <- input$lang; if (is.null(lang)) lang <- "CN"
    if (is.null(t)) return()
    m <- if (lang == "CN") list(pp = 25, ec = 3, la = 20, sp = 28, s = 1, se = 0.5) else
         list(pp = 250, ec = 30, la = 200, sp = 280, s = 10, se = 5)
    updateNumericInput(session, "purchase_price", label = t$purchase_price, value = m$pp, step = m$s)
    updateNumericInput(session, "extra_costs",    label = t$extra_costs,    value = m$ec, step = m$se)
    updateNumericInput(session, "loan_amount",    label = t$loan_amount,    value = m$la, step = m$s)
    updateNumericInput(session, "sale_price",     label = t$sale_price,     value = m$sp, step = m$s)
    for (nm in c("annual_rate","loan_term_years","start_year","initial_rent",
                 "monthly_utilities","annual_rent_increase","hold_years"))
      updateNumericInput(session, nm, label = t[[nm]])
    updateNumericInput(session, "monthly_salary",     label = t$monthly_salary)
    updateNumericInput(session, "combined_tax_rate",   label = t$combined_tax_rate)
    updateNumericInput(session, "building_ratio",      label = t$building_ratio)
    updateNumericInput(session, "annual_opcost",       label = t$annual_opcost)
    updateNumericInput(session, "afa_rate",            label = t$afa_rate)
    updateCheckboxInput(session, "prepay_with_excess", label = t$prepay_with_excess)
    updateActionButton(session,  "calc_btn",           label = t$calc_btn)
  })

  # ---- Section headers ----
  output$sec1 <- renderUI(tr()$sec_property)
  output$sec2 <- renderUI(tr()$sec_rental)
  output$sec3 <- renderUI(tr()$sec_tax)
  output$ui_title <- renderUI(titlePanel(tr()$title))

  # ---- Hold period alert (§23 EStG) ----
  output$hold_years_alert <- renderUI({
    t <- tr(); hy <- input$hold_years
    if (is.null(hy)) return(NULL)
    # ≥ 10 years → exempt
    if (hy >= 10) {
      return(div(style = "background:#eafaf1; border-left:3px solid #27ae60; padding:5px 8px; margin:-2px 0 4px 0; font-size:11.5px; border-radius:0 4px 4px 0; color:#1e8449;",
          t$cg_exempt))
    }
    # < 10 years → detailed tax warning
    m <- unit_m()
    sp <- input$sale_price; pp <- input$purchase_price
    if (is.null(sp) || is.null(pp)) return(NULL)
    gain <- (sp - pp) * m
    if (gain <= 0) {
      return(div(style = "background:#fdecea; border-left:3px solid #e74c3c; padding:5px 8px; margin:-2px 0 4px 0; font-size:11.5px; border-radius:0 4px 4px 0; color:#c0392b;",
          t$cg_warn_no_gain))
    }
    rate <- input$combined_tax_rate
    if (is.null(rate)) rate <- 47.5
    tax_loss <- gain * rate / 100
    div(style = "background:linear-gradient(135deg,#fdecea,#fef5f3); border-left:3px solid #e74c3c; padding:7px 9px; margin:-2px 0 6px 0; border-radius:0 5px 5px 0; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
      tags$div(style = "font-weight:700; font-size:11.5px; color:#c0392b; margin-bottom:4px;",
        t$cg_warn_title,
        tags$span(style = "font-weight:400; font-size:9.5px; color:#b0b0b0;", " §23 EStG")),
      tags$div(style = "font-size:10.5px; color:#5a3a3a; line-height:1.6;",
        tags$div(style = "display:flex; justify-content:space-between; margin-bottom:1px;",
          tags$span(t$cg_warn_gain),
          tags$span(style = "font-weight:600;", fmt_eur(gain))),
        tags$div(style = "font-size:9px; color:#aaa; text-align:right; margin-bottom:2px;",
          fmt_eur(sp * m), " \u2212 ", fmt_eur(pp * m)),
        tags$div(style = "display:flex; justify-content:space-between;",
          tags$span(t$cg_warn_tax_loss,
            tags$span(style = "color:#aaa;", paste0(" ", rate, "%"))),
          tags$b(style = "color:#c0392b; font-size:12px;",
            paste0("\u2248 ", fmt_eur(tax_loss))))
      ),
      tags$hr(style = "margin:5px 0 4px 0; border-color:#f5c6cb; opacity:0.5;"),
      tags$div(style = "font-size:10px; color:#27ae60; font-weight:500;", t$cg_warn_tip)
    )
  })

  # ---- Unit multiplier ----
  unit_m <- reactive({ lang <- input$lang; if (is.null(lang)) lang <- "CN"; if (lang == "CN") 10000 else 1000 })

  # ---- Common parameters (reactive) ----
  params <- reactive({
    m <- unit_m()
    list(
      purchase_price = input$purchase_price * m,
      extra_costs    = input$extra_costs * m,
      loan_amount    = input$loan_amount * m,
      sale_price     = input$sale_price * m,
      annual_rate    = input$annual_rate / 100,
      loan_term_years = input$loan_term_years,
      hold_years     = input$hold_years,
      initial_rent   = input$initial_rent,
      monthly_utilities = input$monthly_utilities,
      annual_rent_increase = input$annual_rent_increase,
      prepay_with_excess = input$prepay_with_excess,
      monthly_salary = input$monthly_salary,
      combined_tax_rate = input$combined_tax_rate / 100,
      building_ratio = input$building_ratio / 100,
      operating_costs = input$annual_opcost,
      afa_rate = input$afa_rate / 100,
      start_year = input$start_year,
      down_payment = input$purchase_price * m - input$loan_amount * m,
      total_investment = (input$purchase_price * m - input$loan_amount * m) + input$extra_costs * m
    )
  })

  # ---- Basic ROI ----
  roi <- eventReactive(input$calc_btn, {
    p <- params()
    res <- compute_basic_roi(
      purchase_price = p$purchase_price, down_payment = p$down_payment,
      extra_costs = p$extra_costs, loan_amount = p$loan_amount,
      annual_rate = p$annual_rate, loan_term_years = p$loan_term_years,
      hold_years = p$hold_years, initial_rent = p$initial_rent,
      monthly_utilities = p$monthly_utilities, annual_rent_increase = p$annual_rent_increase,
      sale_price = p$sale_price, prepay_with_excess = p$prepay_with_excess)
    res$yearly_details$Year <- res$yearly_details$Year + p$start_year - 1
    res
  }, ignoreNULL = FALSE)

  # ---- Tax Analysis ----
  tax <- eventReactive(input$calc_btn, {
    p <- params()
    res <- compute_tax_analysis(
      purchase_price = p$purchase_price, loan_amount = p$loan_amount,
      annual_rate = p$annual_rate, loan_term_years = p$loan_term_years,
      hold_years = p$hold_years, initial_rent = p$initial_rent,
      annual_rent_increase = p$annual_rent_increase, sale_price = p$sale_price,
      total_investment = p$total_investment, building_ratio = p$building_ratio,
      afa_rate = p$afa_rate, operating_costs = p$operating_costs,
      combined_tax_rate = p$combined_tax_rate)
    res$tax_details$Year <- res$tax_details$Year + p$start_year - 1
    res
  }, ignoreNULL = FALSE)

  # ---- Sensitivity ----
  sens_oc <- eventReactive(input$calc_btn, {
    p <- params()
    compute_sens_opcost(p$purchase_price, p$loan_amount, p$annual_rate,
      p$loan_term_years, p$hold_years, p$initial_rent, p$annual_rent_increase,
      p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
      p$combined_tax_rate)
  }, ignoreNULL = FALSE)

  sens_rt <- eventReactive(input$calc_btn, {
    p <- params()
    compute_sens_rate(p$purchase_price, p$loan_amount, p$annual_rate,
      p$loan_term_years, p$hold_years, p$initial_rent, p$annual_rent_increase,
      p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
      p$operating_costs, p$combined_tax_rate)
  }, ignoreNULL = FALSE)

  hold_an <- eventReactive(input$calc_btn, {
    p <- params()
    compute_hold_analysis(p$purchase_price, p$loan_amount, p$annual_rate,
      p$loan_term_years, p$initial_rent, p$annual_rent_increase,
      p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
      p$operating_costs, p$combined_tax_rate, base_hold = p$hold_years)
  }, ignoreNULL = FALSE)

  # ============================================================================
  # METRIC CARDS
  # ============================================================================
  output$ui_metrics <- renderUI({
    t <- tr()
    help <- function(id) actionButton(id, "?", class = "help-btn")
    fluidRow(
      column(3, div(class = "metric-card", style = "background:#d4edda;",
        help("help_irr"), h5(t$irr_title), h3(textOutput("v_irr")))),
      column(3, div(class = "metric-card", style = "background:#d1ecf1;",
        help("help_irr_at"), h5(t$irr_aftertax_title), h3(textOutput("v_irr_at")))),
      column(3, div(class = "metric-card", style = "background:#fff3cd;",
        help("help_profit"), h5(t$total_profit_title), h3(textOutput("v_profit")))),
      column(3, div(class = "metric-card", style = "background:#f8d7da;",
        help("help_mp"), h5(t$monthly_payment_title), h3(textOutput("v_mp"))))
    )
  })
  output$v_irr    <- renderText(paste0(roi()$irr, "%"))
  output$v_irr_at <- renderText(paste0(tax()$irr_after_tax, "%"))
  output$v_profit <- renderText(fmt_eur(tax()$after_tax_profit))
  output$v_mp     <- renderText(fmt_eur(roi()$monthly_payment))

  # ---- Helper: build step row ----
  stp <- function(lbl, val) tags$div(class = "calc-step",
    tags$span(class = "lbl", lbl), tags$span(class = "val", val))

  # ---- Modal 1: Pre-tax IRR ----
  observeEvent(input$help_irr, {
    t <- tr(); r <- roi(); p <- params()
    cfs <- r$cashflows; n <- length(cfs) - 1
    cf_items <- lapply(seq_along(cfs), function(i) {
      yr <- i - 1
      lbl <- if (yr == 0) t$mdl_year0 else {
        ystr <- gsub("\\{n\\}", yr, t$mdl_year_n)
        if (yr == n) paste0(ystr, "  ", t$mdl_final_year_add) else ystr
      }
      stp(lbl, fmt_eur(cfs[i]))
    })
    showModal(modalDialog(title = t$modal_irr_title, size = "l", easyClose = TRUE,
      footer = modalButton(t$mdl_close),
      tags$div(class = "calc-section", t$mdl_formula),
      div(class = "calc-formula", t$mdl_irr_npv, br(), t$mdl_irr_def),
      tags$div(class = "calc-section", t$mdl_inputs),
      stp(t$mdl_initial_invest, fmt_eur(p$total_investment)),
      stp(t$mdl_sale_price, fmt_eur(p$sale_price)),
      stp(t$hold_years, paste0(p$hold_years, " yr")),
      tags$div(class = "calc-section", t$mdl_cashflows),
      tagList(cf_items),
      tags$div(class = "calc-section", t$mdl_result),
      div(class = "calc-result", style = "background:#d4edda; color:#155724;",
          paste0(t$irr_title, " = ", r$irr, "%"))
    ))
  })

  # ---- Modal 2: After-tax IRR ----
  observeEvent(input$help_irr_at, {
    t <- tr(); tx <- tax(); p <- params()
    cfs <- tx$cashflows; n <- length(cfs) - 1
    cf_items <- lapply(seq_along(cfs), function(i) {
      yr <- i - 1
      lbl <- if (yr == 0) t$mdl_year0 else {
        ystr <- gsub("\\{n\\}", yr, t$mdl_year_n)
        if (yr == n) paste0(ystr, "  ", t$mdl_final_year_add) else ystr
      }
      stp(lbl, fmt_eur(cfs[i]))
    })
    showModal(modalDialog(title = t$modal_irr_at_title, size = "l", easyClose = TRUE,
      footer = modalButton(t$mdl_close),
      tags$div(class = "calc-section", t$mdl_formula),
      div(class = "calc-formula", t$mdl_irr_npv, br(), t$mdl_irr_def),
      div(style = "font-size:12px; color:#777; margin:4px 0 8px 0;", t$mdl_irr_at_note),
      tags$div(class = "calc-section", t$mdl_inputs),
      stp(t$mdl_initial_invest, fmt_eur(p$total_investment)),
      stp(t$mdl_sale_price, fmt_eur(p$sale_price)),
      stp(t$hold_years, paste0(p$hold_years, " yr")),
      stp(paste0(t$lbl_marginal, " (", t$lbl_soli, ")"), paste0(p$combined_tax_rate * 100, "%")),
      stp(t$mdl_cg_tax, fmt_eur(tx$capital_gains_tax)),
      tags$div(class = "calc-section", t$mdl_cashflows),
      tagList(cf_items),
      tags$div(class = "calc-section", t$mdl_result),
      div(class = "calc-result", style = "background:#d1ecf1; color:#0c5460;",
          paste0(t$irr_aftertax_title, " = ", tx$irr_after_tax, "%"))
    ))
  })

  # ---- Modal 3: After-tax Profit ----
  observeEvent(input$help_profit, {
    t <- tr(); tx <- tax(); p <- params()
    ns <- tx$net_sale; tatcf <- tx$total_after_tax_cf
    rem <- tx$remaining_loan; cgt <- tx$capital_gains_tax
    showModal(modalDialog(title = t$modal_profit_title, size = "l", easyClose = TRUE,
      footer = modalButton(t$mdl_close),
      tags$div(class = "calc-section", t$mdl_formula),
      div(class = "calc-formula", t$mdl_profit_formula),
      tags$div(class = "calc-section", paste0(t$mdl_steps, " \u2460 ", t$mdl_initial_invest)),
      stp(t$mdl_down_payment, fmt_eur(p$down_payment)),
      stp(t$mdl_extra_costs, fmt_eur(p$extra_costs)),
      stp(paste0(t$mdl_initial_invest, " = ", t$mdl_down_payment, " + ", t$mdl_extra_costs),
          fmt_eur(p$total_investment)),
      tags$div(class = "calc-section", paste0(t$mdl_steps, " \u2461 ", t$mdl_total_atcf)),
      stp(t$mdl_total_atcf, fmt_eur(tatcf)),
      tags$div(class = "calc-section", paste0(t$mdl_steps, " \u2462 ", t$mdl_net_sale)),
      stp(t$mdl_sale_price, fmt_eur(p$sale_price)),
      stp(paste0("\u2212 ", t$mdl_remaining_loan), fmt_eur(rem)),
      stp(paste0("\u2212 ", t$mdl_cg_tax), fmt_eur(cgt)),
      stp(t$mdl_net_sale,
          paste0(fmt_eur(p$sale_price), " \u2212 ", fmt_eur(rem), " \u2212 ", fmt_eur(cgt), " = ", fmt_eur(ns))),
      tags$div(class = "calc-section", paste0(t$mdl_steps, " \u2463 ", t$mdl_result)),
      div(class = "calc-formula",
        paste0(fmt_eur(tatcf), " + ", fmt_eur(ns), " \u2212 ", fmt_eur(p$total_investment),
               " = ", fmt_eur(tx$after_tax_profit))),
      div(class = "calc-result", style = "background:#fff3cd; color:#856404;",
          paste0(t$total_profit_title, " = ", fmt_eur(tx$after_tax_profit)))
    ))
  })

  # ---- Modal 4: Monthly Payment ----
  observeEvent(input$help_mp, {
    t <- tr(); p <- params(); r <- roi()
    mr <- p$annual_rate / 12
    n_months <- p$loan_term_years * 12
    factor <- (1 + mr)^n_months
    numer <- p$loan_amount * mr * factor
    denom <- factor - 1
    showModal(modalDialog(title = t$modal_mp_title, size = "l", easyClose = TRUE,
      footer = modalButton(t$mdl_close),
      tags$div(class = "calc-section", t$mdl_formula),
      div(class = "calc-formula", t$mdl_mp_formula),
      tags$div(class = "calc-section", t$mdl_inputs),
      stp(t$mdl_loan_amount, fmt_eur(p$loan_amount)),
      stp(t$mdl_annual_rate, paste0(p$annual_rate * 100, "%")),
      stp(t$mdl_monthly_rate, paste0(round(mr * 100, 4), "%  (", p$annual_rate * 100, "% \u00F7 12)")),
      stp(t$mdl_total_months, paste0(n_months, "  (", p$loan_term_years, " \u00D7 12)")),
      tags$div(class = "calc-section", t$mdl_steps),
      stp(paste0("(1+r)\u207F"), round(factor, 4)),
      stp(t$mdl_numerator, fmt_eur(numer, 2)),
      stp(t$mdl_denominator, round(denom, 4)),
      stp("M = num / denom", paste0(fmt_eur(numer, 2), " \u00F7 ", round(denom, 4))),
      tags$div(class = "calc-section", t$mdl_result),
      div(class = "calc-result", style = "background:#f8d7da; color:#721c24;",
          paste0(t$monthly_payment_title, " = ", fmt_eur(r$monthly_payment, 2)))
    ))
  })

  # ============================================================================
  # DOWNLOAD REPORT
  # ============================================================================
  output$ui_download_btn <- renderUI({
    div(style = "text-align: right; margin: -6px 0 8px 0;",
      downloadButton("download_report", label = tr()$download_report,
                     class = "btn btn-outline-secondary btn-sm"))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("mortgage_report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = tr()$report_generating, value = 0.3, {
          rp <- build_report_params(
            t = tr(), p = params(),
            roi = roi(), tax = tax(),
            sens_oc = sens_oc(), sens_rt = sens_rt(),
            hold = hold_an())
          setProgress(0.5)
          temp_pdf <- tempfile(fileext = ".pdf")
          render_report(rp, temp_pdf)
          file.copy(temp_pdf, file, overwrite = TRUE)
          setProgress(1)
        })
      }, error = function(e) {
        showNotification(
          paste0(tr()$report_error, "\n", conditionMessage(e)),
          type = "error", duration = 15)
      })
    }
  )

  # ============================================================================
  # TABS
  # ============================================================================
  output$ui_tabs <- renderUI({
    t <- tr()
    tabsetPanel(id = "main_tabs",
      # ---- Tab 1: Investment Analysis ----
      tabPanel(t$tab_investment, br(),
        fluidRow(
          column(6, plotlyOutput("pl_cashflow", height = "360px")),
          column(6, plotlyOutput("pl_equity",   height = "360px"))
        ), br(),
        fluidRow(column(12, plotlyOutput("pl_rent", height = "320px")))
      ),
      # ---- Tab 2: Tax Analysis ----
      tabPanel(t$tab_tax, br(),
        fluidRow(
          column(6, uiOutput("ui_tax_profile")),
          column(6, uiOutput("ui_prop_profile"))
        ),
        fluidRow(
          column(6, div(class = "note-box note-green",
            tags$b(t$note_refund_title), br(), t$note_refund_body)),
          column(6, div(class = "note-box note-blue",
            tags$b(t$note_afa_title), br(), t$note_afa_body))
        ), br(),
        h5(t$tax_table_title), DTOutput("tbl_tax"), br(),
        fluidRow(
          column(6, plotlyOutput("pl_income_cost", height = "360px")),
          column(6, plotlyOutput("pl_tax_effect",  height = "360px"))
        ), br(),
        fluidRow(column(12, plotlyOutput("pl_atcf", height = "340px")))
      ),
      # ---- Tab 3: Overview ----
      tabPanel(t$tab_overview, br(),
        h5(t$overview_compare),
        fluidRow(
          column(4, div(class = "metric-card", style = "background:#d4edda;",
            h5(t$lbl_pretax), h3(textOutput("ov_irr_pre")))),
          column(4, div(class = "metric-card", style = "background:#d1ecf1;",
            h5(t$lbl_aftertax), h3(textOutput("ov_irr_post")))),
          column(4, uiOutput("ov_cgtax_card"))
        ),
        uiOutput("ui_cg_note"),
        h5(t$overview_summary), uiOutput("ui_summary_table"), br(),
        fluidRow(
          column(6, plotlyOutput("pl_sens_oc",   height = "340px")),
          column(6, plotlyOutput("pl_sens_rate", height = "340px"))
        ), br(),
        fluidRow(column(12, plotlyOutput("pl_hold", height = "360px"))), br(),
        uiOutput("ui_insights")
      ),
      # ---- Tab 4: Data Tables ----
      tabPanel(t$tab_data, br(),
        h5(t$data_basic_title), DTOutput("tbl_basic"), br(),
        h5(t$data_tax_title),   DTOutput("tbl_tax_full")
      )
    )
  })

  # ============================================================================
  # TAX TAB – Info Cards
  # ============================================================================
  output$ui_tax_profile <- renderUI({
    t <- tr(); p <- params()
    annual <- p$monthly_salary * 12
    ci <- function(lab, val) tags$div(class = "item", tags$span(lab), tags$span(val))
    div(class = "info-card", style = "background:#eaf2f8; border:1px solid #aed6f1;",
      div(class = "card-title", t$tax_profile_title),
      ci(t$monthly_salary, fmt_eur(p$monthly_salary)),
      ci(t$lbl_annual_salary, fmt_eur(annual)),
      ci(t$lbl_tax_class, t$lbl_tax_class_val),
      ci(t$lbl_marginal, paste0(input$combined_tax_rate, "% (", t$lbl_soli, ")")),
      hr(style = "margin:6px 0"),
      tags$div(style = "font-size:12.5px; color:#555;", icon("user"), " ", t$lbl_status_val)
    )
  })

  output$ui_prop_profile <- renderUI({
    t <- tr(); tx <- tax()
    ci <- function(lab, val) tags$div(class = "item", tags$span(lab), tags$span(val))
    div(class = "info-card", style = "background:#eafaf1; border:1px solid #a9dfbf;",
      div(class = "card-title", t$prop_profile_title),
      ci(t$lbl_building_val, paste0(fmt_eur(tx$building_value), " (", input$building_ratio, "%)")),
      ci(t$lbl_annual_afa, fmt_eur(tx$annual_afa)),
      ci(t$lbl_annual_opcost, fmt_eur(input$annual_opcost)),
      ci(t$lbl_yr1_interest, fmt_eur(tx$tax_details$Loan_Interest[1])),
      ci(t$lbl_afa_period, t$lbl_afa_period_val),
      hr(style = "margin:6px 0"),
      tags$div(style = "font-size:12.5px; color:#555;", icon("check-circle"), " ", t$lbl_hold_note)
    )
  })

  # ============================================================================
  # TAX TAB – Table & Charts
  # ============================================================================
  output$tbl_tax <- renderDT({
    df <- tax()$tax_details; t <- tr()
    if (length(t$tbl_tax_cols) == ncol(df)) colnames(df) <- t$tbl_tax_cols
    datatable(df, rownames = FALSE,
              options = list(pageLength = 15, dom = "t", scrollX = TRUE)) %>%
      formatCurrency(2:ncol(df), currency = "", digits = 0) %>%
      formatStyle(columns = which(colnames(df) == t$tbl_tax_cols[8]),
                  color = styleInterval(0, c("#e74c3c", "#27ae60")),
                  fontWeight = "bold")
  })

  output$pl_income_cost <- renderPlotly({
    df <- tax()$tax_details; t <- tr()
    plot_ly(df, x = ~Year) %>%
      add_bars(y = ~Loan_Interest,   name = t$lbl_interest,     marker = list(color = "#3498db")) %>%
      add_bars(y = ~Depreciation,    name = t$lbl_depreciation, marker = list(color = "#9b59b6")) %>%
      add_bars(y = ~Operating_Costs, name = t$lbl_opcost,       marker = list(color = "#e67e22")) %>%
      add_trace(y = ~Annual_Rent, name = t$lbl_rent_income,
                type = "scatter", mode = "lines+markers",
                line = list(color = "#27ae60", width = 3)) %>%
      layout(title = list(text = t$chart_income_cost, y = .95), barmode = "stack",
             margin = list(t = 50), xaxis = list(title = t$axis_year, tickformat = "d"),
             yaxis = list(title = t$axis_amount),
             legend = list(orientation = "h", y = -0.18))
  })

  output$pl_tax_effect <- renderPlotly({
    df <- tax()$tax_details; t <- tr()
    colors <- ifelse(df$Tax_Effect >= 0, "#27ae60", "#e74c3c")
    plot_ly(df, x = ~Year, y = ~Tax_Effect, type = "bar",
            marker = list(color = colors),
            text = ~paste0(ifelse(Tax_Effect >= 0, "+", ""), round(Tax_Effect), " €"),
            textposition = "outside", hoverinfo = "text") %>%
      layout(title = list(text = t$chart_tax_effect, y = .95),
             margin = list(t = 50), xaxis = list(title = t$axis_year, tickformat = "d"),
             yaxis = list(title = t$axis_amount), showlegend = FALSE)
  })

  output$pl_atcf <- renderPlotly({
    df <- tax()$tax_details; t <- tr()
    plot_ly(df, x = ~Year) %>%
      add_bars(y = ~After_Tax_CF, name = t$lbl_after_tax,
               marker = list(color = "#1abc9c")) %>%
      add_trace(y = ~Cumulative_CF, name = t$lbl_cumulative,
                type = "scatter", mode = "lines+markers",
                line = list(color = "#2c3e50", width = 2, dash = "dash")) %>%
      layout(title = list(text = t$chart_aftertax_cf, y = .95),
             margin = list(t = 50), xaxis = list(title = t$axis_year, tickformat = "d"),
             yaxis = list(title = t$axis_amount),
             legend = list(orientation = "h", y = -0.18))
  })

  # ============================================================================
  # INVESTMENT TAB – Charts
  # ============================================================================
  output$pl_cashflow <- renderPlotly({
    df <- roi()$yearly_details; t <- tr()
    # Merge tax effect from tax analysis
    tx_df <- tax()$tax_details
    df$Tax_Effect <- tx_df$Tax_Effect[seq_len(nrow(df))]
    plot_ly(df, x = ~Year) %>%
      add_bars(y = ~Excess_Cash,   name = t$plot_cf_excess, marker = list(color = "#27ae60")) %>%
      add_bars(y = ~Out_of_Pocket, name = t$plot_cf_oop,    marker = list(color = "#e74c3c")) %>%
      add_trace(y = ~Tax_Effect, name = t$plot_cf_tax,
                type = "scatter", mode = "lines+markers",
                line = list(color = "#8e44ad", width = 2.5),
                marker = list(color = ~ifelse(Tax_Effect >= 0, "#27ae60", "#e74c3c"),
                              size = 7, line = list(color = "#8e44ad", width = 1.5)),
                hovertemplate = paste0("%{y:,.0f} €<extra>", t$plot_cf_tax, "</extra>")) %>%
      layout(title = list(text = t$plot_cf_title, y = .95), barmode = "group",
             margin = list(t = 50), yaxis = list(title = t$axis_amount),
             xaxis = list(title = t$axis_year, tickformat = "d"),
             legend = list(orientation = "h", y = -0.18))
  })

  output$pl_equity <- renderPlotly({
    df <- roi()$yearly_details; r <- roi(); t <- tr()
    pv <- seq(r$purchase_price, r$sale_price, length.out = r$hold_years + 1)[-1]
    df$Equity <- pv - df$Remaining_Balance
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~Remaining_Balance, name = t$plot_equity_loan,
                type = "scatter", mode = "lines+markers", fill = "tozeroy",
                fillcolor = "rgba(231,76,60,0.15)", line = list(color = "#e74c3c")) %>%
      add_trace(y = ~Equity, name = t$plot_equity_net,
                type = "scatter", mode = "lines+markers", fill = "tozeroy",
                fillcolor = "rgba(39,174,96,0.15)", line = list(color = "#27ae60")) %>%
      layout(title = list(text = t$plot_equity_title, y = .95),
             margin = list(t = 50), yaxis = list(title = t$axis_amount),
             xaxis = list(title = t$axis_year, tickformat = "d"))
  })

  output$pl_rent <- renderPlotly({
    df <- roi()$yearly_details; t <- tr()
    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~Monthly_Rent, name = t$plot_rent_monthly,
                type = "scatter", mode = "lines+markers",
                line = list(color = "#17a2b8", width = 3),
                fill = "tozeroy", fillcolor = "rgba(23,162,184,0.1)") %>%
      layout(title = list(text = t$plot_rent_title, y = .95),
             margin = list(t = 50), yaxis = list(title = paste(t$plot_rent_monthly, "(€)")),
             xaxis = list(title = t$axis_year, tickformat = "d"))
  })

  # ============================================================================
  # OVERVIEW TAB
  # ============================================================================
  output$ov_irr_pre  <- renderText(paste0(roi()$irr, "%"))
  output$ov_irr_post <- renderText(paste0(tax()$irr_after_tax, "%"))

  # ---- Dynamic CG tax metric card (changes color) ----
  output$ov_cgtax_card <- renderUI({
    t <- tr(); tx <- tax(); p <- params()
    cg <- tx$capital_gains_tax
    exempt <- (p$hold_years >= 10)
    if (exempt) {
      bg <- "#eafaf1"; clr <- "#1e8449"
      lbl <- paste0("\u2705 ", t$lbl_cg_tax)
    } else if (cg > 0) {
      bg <- "#fdecea"; clr <- "#c0392b"
      lbl <- paste0("\u26A0\uFE0F ", t$lbl_cg_tax)
    } else {
      bg <- "#fce4ec"; clr <- "#666"
      lbl <- t$lbl_cg_tax
    }
    div(class = "metric-card", style = paste0("background:", bg, ";"),
      h5(style = paste0("color:", clr, ";"), lbl),
      h3(style = paste0("color:", clr, ";"),
         if (exempt) "0 \u20AC" else fmt_eur(cg)))
  })

  # ---- CG status banner (overview) ----
  output$ui_cg_note <- renderUI({
    t <- tr(); tx <- tax(); p <- params()
    cg   <- tx$capital_gains_tax
    gain <- max(0, p$sale_price - p$purchase_price)
    rate <- p$combined_tax_rate * 100
    fmt_t <- function(tmpl, ...) {
      args <- list(...)
      for (nm in names(args)) tmpl <- gsub(paste0("{", nm, "}"), args[[nm]], tmpl, fixed = TRUE)
      tmpl
    }
    if (p$hold_years >= 10) {
      # Exempt — show how much saved
      would_pay <- gain * p$combined_tax_rate
      detail <- if (would_pay > 0) fmt_t(t$cg_note_exempt_detail, saved = fmt_eur(would_pay)) else ""
      div(style = "background:linear-gradient(135deg,#eafaf1,#d5f5e3); border-left:4px solid #27ae60; padding:10px 14px; margin:8px 0 12px 0; border-radius:0 6px 6px 0;",
        tags$div(style = "font-weight:700; font-size:13px; color:#1e8449;", t$cg_note_exempt_title),
        if (nchar(detail) > 0) tags$div(style = "font-size:12px; color:#2e7d32; margin-top:2px;", detail)
      )
    } else if (gain > 0 && cg > 0) {
      # Taxed — show breakdown
      detail <- fmt_t(t$cg_note_detail, gain = fmt_eur(gain), rate = rate, tax = fmt_eur(cg))
      div(style = "background:linear-gradient(135deg,#fdecea,#fef5f3); border-left:4px solid #e74c3c; padding:10px 14px; margin:8px 0 12px 0; border-radius:0 6px 6px 0; box-shadow:0 1px 3px rgba(0,0,0,0.06);",
        tags$div(style = "font-weight:700; font-size:13px; color:#c0392b;", t$cg_note_deducted),
        tags$div(style = "font-size:12px; color:#5a3a3a; margin-top:3px;", detail),
        tags$div(style = "font-size:11px; color:#27ae60; margin-top:5px; font-weight:500;", t$cg_warn_tip)
      )
    } else {
      div(style = "background:#f5f5f5; border-left:4px solid #90a4ae; padding:8px 14px; margin:8px 0 12px 0; border-radius:0 6px 6px 0;",
        tags$div(style = "font-size:12.5px; color:#666;", t$cg_note_no_gain))
    }
  })

  output$ui_summary_table <- renderUI({
    t <- tr(); tx <- tax(); p <- params()
    mr <- function(lab, val) tags$tr(tags$td(lab), tags$td(style = "text-align:right; font-weight:600;", val))
    total_ret <- tx$total_after_tax_cf + tx$net_sale
    tags$table(class = "summary-tbl", style = "max-width:600px;",
      mr(t$lbl_initial_invest,  fmt_eur(-p$total_investment)),
      mr(t$lbl_total_atcf,      fmt_eur(tx$total_after_tax_cf)),
      mr(t$lbl_net_sale,        fmt_eur(tx$net_sale)),
      mr(t$lbl_cg_tax,          fmt_eur(tx$capital_gains_tax)),
      tags$tr(tags$td(colspan = "2", tags$hr(style = "margin:4px 0"))),
      mr(t$lbl_total_return, tags$span(style = "color:#27ae60;", fmt_eur(total_ret))),
      mr(t$lbl_net_profit, tags$span(style = "color:#27ae60; font-size:16px;", fmt_eur(tx$after_tax_profit)))
    )
  })

  # ---- Sensitivity charts ----
  output$pl_sens_oc <- renderPlotly({
    df <- sens_oc(); t <- tr(); p <- params()
    plot_ly(df, x = ~Operating_Costs, y = ~IRR, type = "scatter", mode = "lines+markers",
            line = list(color = "#e67e22", width = 2.5), name = "IRR") %>%
      add_trace(x = rep(p$operating_costs, 2), y = range(df$IRR),
                type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", dash = "dash", width = 1.5),
                name = t$lbl_current, showlegend = TRUE) %>%
      layout(title = list(text = t$overview_sens_oc, y = .95),
             margin = list(t = 50), xaxis = list(title = t$annual_opcost),
             yaxis = list(title = "IRR (%)"))
  })

  output$pl_sens_rate <- renderPlotly({
    df <- sens_rt(); t <- tr(); p <- params()
    plot_ly(df, x = ~Rate, y = ~IRR, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 2.5), name = "IRR") %>%
      add_trace(x = rep(p$annual_rate * 100, 2), y = range(df$IRR),
                type = "scatter", mode = "lines",
                line = list(color = "#e74c3c", dash = "dash", width = 1.5),
                name = t$lbl_current, showlegend = TRUE) %>%
      layout(title = list(text = t$overview_sens_rate, y = .95),
             margin = list(t = 50), xaxis = list(title = t$annual_rate),
             yaxis = list(title = "IRR (%)"))
  })

  output$pl_hold <- renderPlotly({
    df <- hold_an(); t <- tr()
    colors <- ifelse(df$Hold_Years >= 10, "#27ae60", "#e74c3c")
    plot_ly(df, x = ~Hold_Years) %>%
      add_bars(y = ~IRR, marker = list(color = colors),
               text = ~paste0(IRR, "%"), textposition = "outside",
               name = paste("IRR", t$lbl_aftertax)) %>%
      add_trace(y = ~CG_Tax / 1000, name = t$lbl_cg_tax,
                type = "scatter", mode = "lines+markers", yaxis = "y2",
                line = list(color = "#e67e22", width = 2)) %>%
      layout(title = list(text = t$overview_hold, y = .95),
             margin = list(t = 50, r = 60),
             xaxis = list(title = t$hold_years, dtick = 1),
             yaxis  = list(title = "IRR (%)", side = "left"),
             yaxis2 = list(title = paste(t$lbl_cg_tax, "(k€)"),
                           overlaying = "y", side = "right"),
             shapes = list(
               list(type = "line", x0 = 9.5, x1 = 9.5, y0 = 0, y1 = 1,
                    yref = "paper", line = list(color = "#e74c3c", width = 2, dash = "dot"))),
             annotations = list(
               list(x = 10, y = 1.05, yref = "paper", text = t$lbl_year10_line,
                    showarrow = FALSE, font = list(color = "#e74c3c", size = 11))),
             legend = list(orientation = "h", y = -0.18))
  })

  output$ui_insights <- renderUI({
    t <- tr()
    div(class = "info-card", style = "background:#fef9e7; border:1px solid #f9e79f;",
      div(class = "card-title", t$overview_insights),
      tags$ul(style = "padding-left:18px; margin:0;",
        lapply(t$insights, function(txt) tags$li(style = "margin-bottom:4px;", txt)))
    )
  })

  # ============================================================================
  # DATA TAB – Tables
  # ============================================================================
  output$tbl_basic <- renderDT({
    df <- roi()$yearly_details; t <- tr()
    if (length(t$tbl_basic_cols) == ncol(df)) colnames(df) <- t$tbl_basic_cols
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatCurrency(2:ncol(df), currency = "", digits = 2)
  })

  output$tbl_tax_full <- renderDT({
    df <- tax()$tax_details; t <- tr()
    if (length(t$tbl_tax_cols) == ncol(df)) colnames(df) <- t$tbl_tax_cols
    datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) %>%
      formatCurrency(2:ncol(df), currency = "", digits = 0) %>%
      formatStyle(columns = which(colnames(df) == t$tbl_tax_cols[8]),
                  color = styleInterval(0, c("#e74c3c", "#27ae60")),
                  fontWeight = "bold")
  })
}

shinyApp(ui, server)
