# ==============================================================================
# Server Observers (Interaction Logic)
# ==============================================================================

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
  for (nm in c("annual_rate","loan_term_years","zinsbindung_years","refi_rate",
               "start_year","initial_rent",
               "monthly_utilities","annual_rent_increase","hold_years"))
    updateNumericInput(session, nm, label = t[[nm]])
  updateNumericInput(session, "monthly_salary",     label = t$monthly_salary)
  updateNumericInput(session, "combined_tax_rate",   label = t$combined_tax_rate)
  updateNumericInput(session, "building_ratio",      label = t$building_ratio)
  updateNumericInput(session, "annual_opcost",       label = t$annual_opcost)
  updateNumericInput(session, "afa_rate",            label = t$afa_rate)
  updateNumericInput(session, "selling_cost_rate",   label = t$selling_cost_rate)
  updateNumericInput(session, "vacancy_rate",        label = t$vacancy_rate)
  updateNumericInput(session, "sondertilgung_rate",  label = t$sondertilgung_rate)
  updateNumericInput(session, "opcost_inflation",    label = t$opcost_inflation)
  updateCheckboxInput(session, "prepay_with_excess", label = t$prepay_with_excess)
  updateActionButton(session,  "calc_btn",           label = t$calc_btn)
  updateActionButton(session,  "btn_tax_guide",      label = t$btn_tax_guide)
})

# ---- Modal: Tax Guide (New) ----
observeEvent(input$btn_tax_guide, {
  t <- tr()
  # Use current inputs if possible, otherwise defaults
  p_salary <- isolate(input$monthly_salary)
  p_rent_net <- isolate(input$initial_rent) * 12
  # Simple estimation of deduction for example purpose:
  # Interest ~ 3.5%, AfA ~ 1.5% of price, OpCost ~ 20% rent
  # We just use static example logic or simple heuristic here to be illustrative
  
  # Heuristic example values
  ex_salary <- p_salary * 12
  # Calculate Year-1 Tax Effect based on current inputs (if valid)
  # We try to use the reactive 'tax()' but it might not be ready if Calc not clicked.
  # So we'll try-catch or just use current inputs to do a quick calc.
  
  # Quick calc for example
  lang <- isolate(input$lang)
  if (is.null(lang)) lang <- "CN"
  
  m_factor <- if(lang == "CN") 10000 else 1000
  
  pp <- isolate(input$purchase_price) * m_factor
  la <- isolate(input$loan_amount) * m_factor
  ec <- isolate(input$extra_costs) * m_factor
  rt <- isolate(input$annual_rate) / 100
  br <- isolate(input$building_ratio) / 100
  ar <- isolate(input$afa_rate) / 100
  oc <- isolate(input$annual_opcost)
  trate <- isolate(input$combined_tax_rate) / 100
  
  # Year 1 Estimate
  inc <- p_rent_net
  ded_int <- la * rt
  ded_afa <- (pp + ec) * br * ar
  ded_op  <- oc
  res_rent <- inc - (ded_int + ded_afa + ded_op)
  
  is_loss <- res_rent < 0
  tax_effect <- abs(res_rent) * trate
  
  col_res <- if(is_loss) "#27ae60" else "#e74c3c" # Green for refund, Red for pay
  txt_res <- if(is_loss) paste0("+ ", fmt_eur(tax_effect)) else paste0("- ", fmt_eur(tax_effect))
  lbl_res <- if(is_loss) t$mdl_tax_refund else t$mdl_tax_pay
  
  showModal(modalDialog(
    title = t$mdl_tax_title, size = "l", easyClose = TRUE,
    footer = modalButton(t$mdl_close),
    
    tags$div(style="font-size:15px; margin-bottom:15px;", t$mdl_tax_intro),
    
    tags$div(class = "calc-section", t$mdl_tax_step1),
    tags$div(style="margin-left:10px;", t$mdl_tax_step1_d),
    
    tags$div(class = "calc-section", t$mdl_tax_step2),
    div(class = "calc-formula", t$mdl_tax_step2_f),
    div(style="font-size:13px; color:#666;", t$mdl_tax_step2_n),
    
    tags$div(class = "calc-section", t$mdl_tax_step3),
    div(style="margin-bottom:6px;", markdown(t$mdl_tax_case_loss)),
    div(markdown(t$mdl_tax_case_gain)),
    
    tags$div(class = "calc-section", t$mdl_tax_example),
    div(class = "well", style="background:#f8f9fa; border:1px solid #ddd;",
      stp(t$mdl_tax_ex_sal, fmt_eur(ex_salary)),
      stp(t$mdl_tax_ex_rent, fmt_eur(res_rent), 
          val_style = if(is_loss) "color:#c0392b" else "color:#27ae60"), # Red for loss, Green for profit (Accounting view)
      hr(),
      stp(t$mdl_tax_ex_total, fmt_eur(ex_salary + res_rent), val_style="font-weight:700"),
      hr(),
      stp(lbl_res, txt_res, val_style = paste0("font-weight:700; font-size:16px; color:", col_res))
    )
  ))
})

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

# ---- Modal 5: Cash-on-Cash Return ----
observeEvent(input$help_coc, {
  t <- tr(); tx <- tax(); p <- params()
  
  # Year 1 CoC
  yr1cf <- tx$tax_details$After_Tax_CF[1]
  
  # Average CoC (New)
  # Calculate average annual after-tax cash flow over holding period
  # Note: tax_details has 'After_Tax_CF' for each year.
  # We should NOT include the final sale proceeds in CoC usually, just operating CF.
  # compute_tax_analysis returns 'tax_details' where 'After_Tax_CF' includes sale proceeds in final year?
  # Let's check compute.R.
  # In compute.R: 
  #   ycf <- atcf
  #   if (yr == hold_years) { ... ycf <- ycf + sale... }
  #   cfs <- c(cfs, ycf)
  #   rows[[yr]] <- data.frame(..., After_Tax_CF = round(atcf), ...)
  # 
  # Wait, in rows[[yr]], 'After_Tax_CF' is 'atcf' which is 'surplus + te'.
  # It does NOT include the sale proceeds (Net Sale is separate).
  # So taking mean(tx$tax_details$After_Tax_CF) is correct for Operating CoC.
  
  avg_cf <- mean(tx$tax_details$After_Tax_CF)
  coc_avg <- if (p$total_investment > 0) avg_cf / p$total_investment * 100 else 0
  
  showModal(modalDialog(title = t$modal_coc_title, size = "l", easyClose = TRUE,
    footer = modalButton(t$mdl_close),
    
    # Formula Section
    tags$div(class = "calc-section", t$mdl_formula),
    div(class = "calc-formula", t$mdl_coc_formula),
    div(class = "calc-formula", t$mdl_coc_avg_formula), # Added avg formula
    div(style = "font-size:12px; color:#777; margin:4px 0 8px 0;", t$mdl_coc_note),
    
    # Inputs
    tags$div(class = "calc-section", t$mdl_inputs),
    stp(t$mdl_coc_equity, fmt_eur(p$total_investment)),
    stp(t$mdl_coc_yr1cf,  fmt_eur(yr1cf)),
    stp(t$mdl_coc_avgcf,  fmt_eur(avg_cf)), # Added avg CF input
    
    # Steps / Result
    tags$div(class = "calc-section", t$mdl_result),
    
    # Result 1: Year 1 CoC
    div(style="margin-bottom: 5px;",
      stp("CoC (Year 1)", paste0(fmt_eur(yr1cf), " \u00F7 ", fmt_eur(p$total_investment),
        " = ", tx$coc_return, "%"))
    ),
    div(class = "calc-result", style = "background:#e8daef; color:#6c3483; margin-bottom:10px;",
        paste0("CoC (Year 1) = ", tx$coc_return, "%")),
        
    # Result 2: Average CoC
    div(style="margin-bottom: 5px;",
      stp("CoC (Avg)", paste0(fmt_eur(avg_cf), " \u00F7 ", fmt_eur(p$total_investment),
        " = ", round(coc_avg, 2), "%"))
    ),
    div(class = "calc-result", style = "background:#d6eaf8; color:#2e86c1;",
        paste0("CoC (Avg) = ", round(coc_avg, 2), "%"))
  ))
})
