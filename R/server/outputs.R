# ==============================================================================
# Server Outputs (Render Logic)
# ==============================================================================

# ---- Section headers ----
output$sec1 <- renderUI(tr()$sec_property)
output$sec2 <- renderUI(tr()$sec_rental)
output$sec3 <- renderUI(tr()$sec_tax)
output$ui_title <- renderUI(titlePanel(tr()$title))

# ---- Advanced Parameters Badges (Dynamic) ----
output$adv_badges <- renderUI({
  t <- tr()
  # Define list of advanced parameters to show
  # Format: list(input_id, label, unit)
  adv_items <- list(
    list("monthly_salary", t$monthly_salary, ""),
    list("combined_tax_rate", t$combined_tax_rate, "%"),
    list("building_ratio", t$building_ratio, "%"),
    list("annual_opcost", t$annual_opcost, ""),
    list("afa_rate", t$afa_rate, "%"),
    list("selling_cost_rate", t$selling_cost_rate, "%"),
    list("vacancy_rate", t$vacancy_rate, "%"),
    list("sondertilgung_rate", t$sondertilgung_rate, "%"),
    list("opcost_inflation", t$opcost_inflation, "%")
  )

  badges <- lapply(adv_items, function(item) {
    val <- input[[item[[1]]]]
    if (is.null(val)) return(NULL)

    # Extract short label (remove text in parens)
    lbl <- gsub("\\s*\\(.*\\)", "", item[[2]])

    # Format value
    val_str <- paste0(val, item[[3]])

    tags$span(style="display:inline-block; font-size:10px; padding:2px 6px; margin:2px; background:#e9ecef; border-radius:10px; color:#495057; border:1px solid #ced4da;",
      tags$b(lbl), ": ", val_str
    )
  })

  tagList(badges)
})

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

# ============================================================================
# METRIC CARDS
# ============================================================================
output$ui_metrics <- renderUI({
  t <- tr()
  help <- function(id) actionButton(id, "?", class = "help-btn")

  # 使用 Flexbox 布局实现等宽
  # flex: 1 确保每个子元素平分空间
  # gap: 10px 设置卡片间距
  div(style = "display: flex; gap: 10px; margin-bottom: 10px;",
    div(style = "flex: 1; min-width: 0;",
      div(class = "metric-card", style = "background:#d4edda; height: 100%;",
        help("help_irr"), h5(t$irr_title), h3(textOutput("v_irr")))),

    div(style = "flex: 1; min-width: 0;",
      div(class = "metric-card", style = "background:#d1ecf1; height: 100%;",
        help("help_irr_at"), h5(t$irr_aftertax_title), h3(textOutput("v_irr_at")))),

    div(style = "flex: 1; min-width: 0;",
      div(class = "metric-card", style = "background:#e8daef; height: 100%;",
        help("help_coc"), h5(t$coc_title), h3(textOutput("v_coc")))),

    div(style = "flex: 1; min-width: 0;",
      div(class = "metric-card", style = "background:#fff3cd; height: 100%;",
        help("help_profit"), h5(t$total_profit_title), h3(textOutput("v_profit")))),

    div(style = "flex: 1; min-width: 0;",
      div(class = "metric-card", style = "background:#f8d7da; height: 100%;",
        help("help_mp"), h5(t$monthly_payment_title), h3(textOutput("v_mp"))))
  )
})
output$v_irr    <- renderText({ r <- roi(); req(r); paste0(r$irr, "%") })
output$v_irr_at <- renderText({ tx <- tax(); req(tx); paste0(tx$irr_after_tax, "%") })
output$v_coc    <- renderText({ tx <- tax(); req(tx); paste0(tx$coc_avg_return, "%") })
output$v_profit <- renderText({ tx <- tax(); req(tx); fmt_eur(tx$after_tax_profit) })
output$v_mp     <- renderText({ r <- roi(); req(r); fmt_eur(r$monthly_payment) })

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
    # ---- Tab 4: Summary Report ----
    tabPanel(t$tab_summary, br(),
      div(class = "rpt-container",
        tags$button(class = "rpt-copy", id = "copy_rpt",
          onclick = "var t=document.getElementById('rpt_text');navigator.clipboard.writeText(t.textContent).then(function(){var b=document.getElementById('copy_rpt');b.textContent='\u2705';setTimeout(function(){b.textContent='\ud83d\udccb';},1500);})",
          "\ud83d\udccb"),
        tags$pre(id = "rpt_text", class = "rpt-pre",
          textOutput("summary_text", inline = TRUE))
      )
    ),
    # ---- Tab 5: Data Tables ----
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
           legend = list(orientation = "h", y = -0.18)) %>%
    config(displayModeBar = FALSE)
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
           yaxis = list(title = t$axis_amount), showlegend = FALSE) %>%
    config(displayModeBar = FALSE)
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
           legend = list(orientation = "h", y = -0.18)) %>%
    config(displayModeBar = FALSE)
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
           legend = list(orientation = "h", y = -0.18)) %>%
    config(displayModeBar = FALSE)
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
           xaxis = list(title = t$axis_year, tickformat = "d")) %>%
    config(displayModeBar = FALSE)
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
           xaxis = list(title = t$axis_year, tickformat = "d")) %>%
    config(displayModeBar = FALSE)
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
    mr(t$lbl_net_profit, tags$span(style = "color:#27ae60; font-size:16px;", fmt_eur(tx$after_tax_profit))),
    tags$tr(tags$td(colspan = "2", tags$hr(style = "margin:4px 0"))),
    mr(t$coc_title, tags$span(style = "color:#6c3483; font-weight:700;", paste0(tx$coc_return, "%")))
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
           yaxis = list(title = "IRR (%)")) %>%
    config(displayModeBar = FALSE)
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
           yaxis = list(title = "IRR (%)")) %>%
    config(displayModeBar = FALSE)
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
           legend = list(orientation = "h", y = -0.18)) %>%
    config(displayModeBar = FALSE)
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
# ============================================================================
# SUMMARY REPORT TAB  (plain text, copy-friendly)
# ============================================================================
output$summary_text <- renderText({
  t <- tr(); p <- params(); r <- roi(); tx <- tax()
  lang <- input$lang; if (is.null(lang)) lang <- "CN"
  yr_s <- if (lang == "DE") " J." else if (lang == "EN") " yrs" else " \u5e74"
  W <- 48  # total line width

  # --- helpers: right-aligned label : value ---
  eur  <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ",")
  eur2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
  ln <- function(lbl, val) {
    gap <- max(1, W - nchar(lbl, type = "bytes") - nchar(val, type = "bytes"))
    paste0(lbl, strrep(" ", gap), val)
  }
  bar  <- function(ch = "-") strrep(ch, W)
  blank <- ""

  gain <- p$sale_price - p$purchase_price
  end_rent <- p$initial_rent + p$annual_rent_increase * max(0, p$hold_years - 1)
  mode_txt <- if (p$prepay_with_excess) t$stab_mode_prepay else t$stab_mode_hold
  net_tax  <- tx$total_refunds - tx$total_payments
  net_sign <- if (net_tax >= 0) "+" else "-"
  total_cash <- r$excess_cash + r$net_sale_proceed
  cg_txt <- if (p$hold_years >= 10) paste0("0  (exempt, s.23 EStG)") else
            paste0(eur(tx$capital_gains_tax), " EUR")

  lines <- c(
    bar("="),
    ln("", t$stab_title),
    ln("", mode_txt),
    bar("="),
    blank,
    paste0("[ ", t$stab_sec_property, " ]"),
    bar(),
    ln(t$stab_purchase,     paste0(eur(p$purchase_price), " EUR")),
    ln(t$stab_sale,         paste0(eur(p$sale_price), " EUR")),
    ln(t$stab_appreciation, paste0(eur(gain), " EUR  (",
       round(gain / p$purchase_price * 100, 1), "%)")),
    ln(t$stab_hold,         paste0(p$hold_years, yr_s)),
    ln(t$stab_selling_cost,  paste0(p$selling_cost_rate * 100, "%")),
    ln(t$stab_vacancy,       paste0(p$vacancy_rate * 100, "%")),
    blank,
    paste0("[ ", t$stab_sec_loan, " ]"),
    bar(),
    ln(t$stab_loan_amount,  paste0(eur(p$loan_amount), " EUR")),
    ln(t$stab_rate,         paste0(p$annual_rate * 100, "%")),
    ln(t$stab_term,         paste0(p$loan_term_years, yr_s)),
    if (p$zinsbindung_years < p$loan_term_years)
      ln(t$stab_zinsbindung, paste0(p$zinsbindung_years, yr_s)) else NULL,
    if (p$zinsbindung_years < p$loan_term_years)
      ln(t$stab_refi_rate,   paste0(p$refi_rate * 100, "%")) else NULL,
    ln(t$stab_monthly_pmt,  paste0(eur2(r$monthly_payment), " EUR")),
    ln(t$stab_remaining,    paste0(eur(r$remaining_loan), " EUR")),
    blank,
    paste0("[ ", t$stab_sec_rental, " ]"),
    bar(),
    ln(t$stab_start_rent,    paste0(eur(p$initial_rent), " EUR")),
    ln(t$stab_rent_increase, paste0("+", eur(p$annual_rent_increase), " EUR/yr")),
    ln(t$stab_end_rent,      paste0(eur(end_rent), " EUR")),
    ln(t$stab_total_rent,    paste0(eur(r$total_rent_collected), " EUR")),
    ln(t$stab_total_mortgage, paste0(eur(r$total_mortgage_paid), " EUR")),
    blank,
    paste0("[ ", t$stab_sec_invest, " ]"),
    bar(),
    ln(t$stab_downpayment,   paste0(eur(p$total_investment), " EUR")),
    ln(t$stab_oop,           paste0(eur(r$out_of_pocket_extra), " EUR")),
    ln(t$stab_total_in,      paste0(eur(r$actual_total_in), " EUR")),
    bar(". "),
    ln(t$stab_excess_cash,   paste0(eur(r$excess_cash), " EUR")),
    ln(t$stab_net_sale,      paste0(eur(r$net_sale_proceed), " EUR")),
    if (p$selling_cost_rate > 0) ln(t$lbl_selling_costs, paste0("-", eur(r$selling_costs), " EUR")) else NULL,
    ln(t$stab_total_cash,    paste0(eur(total_cash), " EUR")),
    bar(". "),
    ln(t$stab_total_profit,  paste0(eur(r$total_profit), " EUR  ***")),
    blank,
    paste0("[ ", t$stab_sec_tax, " ]"),
    bar(),
    ln(t$stab_building_val,   paste0(eur(tx$building_value), " EUR")),
    ln(t$stab_annual_afa,     paste0(eur(tx$annual_afa), " EUR")),
    ln(t$stab_annual_opcost,  paste0(eur(p$operating_costs), " EUR")),
    ln(t$stab_opcost_infl,    paste0(p$opcost_inflation * 100, "%")),
    ln(t$stab_sondertilgung,  paste0(p$sondertilgung_rate * 100, "%")),
    ln(t$stab_marginal_rate,  paste0(p$combined_tax_rate * 100, "%")),
    bar(". "),
    ln(t$stab_total_refund,   paste0("+", eur(tx$total_refunds), " EUR")),
    ln(t$stab_total_payment,  paste0("-", eur(tx$total_payments), " EUR")),
    ln(t$stab_net_tax,        paste0(net_sign, eur(abs(net_tax)), " EUR")),
    ln(t$stab_cg_tax,         cg_txt),
    bar(". "),
    ln(t$stab_profit_at,      paste0(eur(tx$after_tax_profit), " EUR  ***")),
    blank,
    paste0("[ ", t$stab_sec_return, " ]"),
    bar(),
    ln(t$stab_irr_pre,        paste0(r$irr, "%")),
    ln(t$stab_irr_post,       paste0(tx$irr_after_tax, "%")),
    ln(t$stab_coc,            paste0(tx$coc_return, "%")),
    ln(t$stab_simple_return,  paste0(r$simple_annualized, "%")),
    blank,
    bar("="),
    paste0("[ ", t$stab_sec_note, " ]"),
    paste0("1. ", t$stab_note_1),
    paste0("2. ", t$stab_note_2),
    paste0("3. ", t$stab_note_3),
    paste0("4. ", t$stab_note_4),
    paste0("5. ", t$stab_note_5),
    bar("=")
  )
  paste(lines, collapse = "\n")
})
