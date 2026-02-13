# ==============================================================================
# Server Reactives (Core Logic)
# ==============================================================================

# ---- Translation reactive ----
tr <- reactive({
  lang <- input$lang; if (is.null(lang)) lang <- "CN"
  TR[[lang]]
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
    selling_cost_rate  = input$selling_cost_rate / 100,
    vacancy_rate       = input$vacancy_rate / 100,
    sondertilgung_rate = input$sondertilgung_rate / 100,
    opcost_inflation   = input$opcost_inflation / 100,
    zinsbindung_years  = input$zinsbindung_years,
    refi_rate          = input$refi_rate / 100,
    start_year = input$start_year,
    down_payment = input$purchase_price * m - input$loan_amount * m,
    total_investment = (input$purchase_price * m - input$loan_amount * m) + input$extra_costs * m
  )
})

# ---- Validated params (fires on calc_btn, shows modal on error) ----
vp <- eventReactive(input$calc_btn, {
  t <- tr()
  errs <- validate_inputs(input, t)
  if (length(errs) > 0) {
    showModal(modalDialog(
      title = t$val_title, size = "m", easyClose = TRUE,
      footer = modalButton(t$mdl_close),
      tags$div(style = "max-height:400px; overflow-y:auto;",
        tagList(lapply(errs, function(e)
          div(style = "padding:5px 0; font-size:13px; border-bottom:1px solid #f0f0f0;",
            icon("exclamation-circle", style = "color:#e74c3c; margin-right:6px;"), e))))
    ))
    return(NULL)
  }
  params()
}, ignoreNULL = FALSE)

# ---- Basic ROI ----
roi <- reactive({
  p <- vp(); req(p)
  res <- compute_basic_roi(
    purchase_price = p$purchase_price, down_payment = p$down_payment,
    extra_costs = p$extra_costs, loan_amount = p$loan_amount,
    annual_rate = p$annual_rate, loan_term_years = p$loan_term_years,
    hold_years = p$hold_years, initial_rent = p$initial_rent,
    monthly_utilities = p$monthly_utilities, annual_rent_increase = p$annual_rent_increase,
    sale_price = p$sale_price, prepay_with_excess = p$prepay_with_excess,
    selling_cost_rate = p$selling_cost_rate, vacancy_rate = p$vacancy_rate,
    sondertilgung_rate = p$sondertilgung_rate, opcost_inflation = p$opcost_inflation,
    zinsbindung_years = p$zinsbindung_years, refi_rate = p$refi_rate)
  res$yearly_details$Year <- res$yearly_details$Year + p$start_year - 1
  res
})

# ---- Tax Analysis ----
tax <- reactive({
  p <- vp(); req(p)
  res <- compute_tax_analysis(
    purchase_price = p$purchase_price, loan_amount = p$loan_amount,
    annual_rate = p$annual_rate, loan_term_years = p$loan_term_years,
    hold_years = p$hold_years, initial_rent = p$initial_rent,
    annual_rent_increase = p$annual_rent_increase, sale_price = p$sale_price,
    total_investment = p$total_investment, building_ratio = p$building_ratio,
    afa_rate = p$afa_rate, operating_costs = p$operating_costs,
    combined_tax_rate = p$combined_tax_rate,
    prepay_with_excess = p$prepay_with_excess,
    monthly_utilities = p$monthly_utilities,
    selling_cost_rate = p$selling_cost_rate, vacancy_rate = p$vacancy_rate,
    sondertilgung_rate = p$sondertilgung_rate, opcost_inflation = p$opcost_inflation,
    zinsbindung_years = p$zinsbindung_years, refi_rate = p$refi_rate)
  res$tax_details$Year <- res$tax_details$Year + p$start_year - 1
  res
})

# ---- Sensitivity ----
sens_oc <- reactive({
  p <- vp(); req(p)
  compute_sens_opcost(p$purchase_price, p$loan_amount, p$annual_rate,
    p$loan_term_years, p$hold_years, p$initial_rent, p$annual_rent_increase,
    p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
    p$combined_tax_rate, p$prepay_with_excess, p$monthly_utilities,
    p$selling_cost_rate, p$vacancy_rate, p$sondertilgung_rate, p$opcost_inflation,
    p$zinsbindung_years, p$refi_rate)
})

sens_rt <- reactive({
  p <- vp(); req(p)
  compute_sens_rate(p$purchase_price, p$loan_amount, p$annual_rate,
    p$loan_term_years, p$hold_years, p$initial_rent, p$annual_rent_increase,
    p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
    p$operating_costs, p$combined_tax_rate,
    p$prepay_with_excess, p$monthly_utilities,
    p$selling_cost_rate, p$vacancy_rate, p$sondertilgung_rate, p$opcost_inflation,
    p$zinsbindung_years, p$refi_rate)
})

hold_an <- reactive({
  p <- vp(); req(p)
  compute_hold_analysis(p$purchase_price, p$loan_amount, p$annual_rate,
    p$loan_term_years, p$initial_rent, p$annual_rent_increase,
    p$sale_price, p$total_investment, p$building_ratio, p$afa_rate,
    p$operating_costs, p$combined_tax_rate,
    p$prepay_with_excess, p$monthly_utilities,
    p$selling_cost_rate, p$vacancy_rate, p$sondertilgung_rate, p$opcost_inflation,
    p$zinsbindung_years, p$refi_rate,
    base_hold = p$hold_years)
})
