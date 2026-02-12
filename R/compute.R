# ==============================================================================
# R/compute.R — 计算引擎 (IRR, 基础ROI, 德国税务分析, 敏感度分析)
# ==============================================================================

# ---- IRR (Newton's method) ---------------------------------------------------
compute_irr <- function(cf, guess = 0.1, tol = 1e-9, max_iter = 1000) {
  r <- guess
  for (i in seq_len(max_iter)) {
    npv <- 0; dnpv <- 0
    for (t in seq_along(cf)) {
      n <- t - 1
      npv  <- npv  + cf[t] / (1 + r)^n
      dnpv <- dnpv - n * cf[t] / (1 + r)^(n + 1)
    }
    if (abs(dnpv) < 1e-15) break
    r_new <- r - npv / dnpv
    if (abs(r_new - r) < tol) return(r_new)
    r <- r_new
  }
  r
}

# ---- Monthly annuity payment -------------------------------------------------
annuity_payment <- function(loan, rate, term_years) {
  mr <- rate / 12; n <- term_years * 12
  loan * mr * (1 + mr)^n / ((1 + mr)^n - 1)
}

# ---- Basic (pre-tax) ROI ----------------------------------------------------
compute_basic_roi <- function(purchase_price, down_payment, extra_costs,
                              loan_amount, annual_rate, loan_term_years,
                              hold_years, initial_rent, monthly_utilities = 0,
                              annual_rent_increase = 0, sale_price,
                              prepay_with_excess = FALSE) {

  total_inv <- down_payment + extra_costs
  mr        <- annual_rate / 12
  n_total   <- loan_term_years * 12
  mp        <- annuity_payment(loan_amount, annual_rate, loan_term_years)

  bal <- loan_amount
  tot_rent <- 0; tot_mort <- 0; oop <- 0; exc <- 0; tot_pre <- 0
  rent <- initial_rent
  cfs  <- c(-total_inv)

  rows <- vector("list", hold_years)

  for (yr in seq_len(hold_years)) {
    if (yr > 1) rent <- rent + annual_rent_increase
    y_rent <- 0; y_mort <- 0; y_oop <- 0; y_exc <- 0; y_pre <- 0

    for (mo in 1:12) {
      if (bal <= 0) { y_rent <- y_rent + rent; y_exc <- y_exc + rent; next }
      int  <- bal * mr
      prin <- min(mp - int, bal)
      pay  <- int + prin
      bal  <- bal - prin
      net  <- rent - monthly_utilities
      d    <- net - pay
      if (d < 0) {
        y_oop <- y_oop + abs(d)
      } else if (prepay_with_excess && bal > 0) {
        pp <- min(d, bal); bal <- bal - pp
        y_pre <- y_pre + pp; y_mort <- y_mort + pp
        left <- d - pp; if (left > 0) y_exc <- y_exc + left
      } else {
        y_exc <- y_exc + d
      }
      y_rent <- y_rent + rent; y_mort <- y_mort + pay
    }

    tot_rent <- tot_rent + y_rent; tot_mort <- tot_mort + y_mort
    oop <- oop + y_oop; exc <- exc + y_exc; tot_pre <- tot_pre + y_pre

    ycf <- y_exc - y_oop
    if (yr == hold_years) ycf <- ycf + (sale_price - max(bal, 0))
    cfs <- c(cfs, ycf)

    rows[[yr]] <- data.frame(
      Year = yr, Monthly_Rent = round(rent, 2), Monthly_Payment = round(mp, 2),
      Yearly_Rent = round(y_rent, 2), Yearly_Mortgage = round(y_mort, 2),
      Out_of_Pocket = round(y_oop, 2), Excess_Cash = round(y_exc, 2),
      Prepaid = round(y_pre, 2), Remaining_Balance = round(max(bal, 0), 2))
  }

  df  <- do.call(rbind, rows)
  irr <- compute_irr(cfs)
  rem <- round(max(bal, 0), 2)
  ns  <- sale_price - rem
  ain <- total_inv + oop
  aou <- ns + exc
  sr  <- (aou / ain)^(1 / hold_years) - 1

  list(purchase_price = purchase_price, sale_price = sale_price,
       hold_years = hold_years, prepay_with_excess = prepay_with_excess,
       loan_amount = loan_amount, monthly_payment = round(mp, 2),
       remaining_loan = rem, total_investment = total_inv,
       out_of_pocket_extra = round(oop, 2), actual_total_in = round(ain, 2),
       total_rent_collected = round(tot_rent, 2),
       total_mortgage_paid = round(tot_mort, 2),
       excess_cash = round(exc, 2), total_prepaid = round(tot_pre, 2),
       net_sale_proceed = round(ns, 2), total_profit = round(aou - ain, 2),
       simple_annualized = round(sr * 100, 2), irr = round(irr * 100, 2),
       cashflows = cfs, yearly_details = df)
}

# ---- German Tax Analysis (§21 EStG) -----------------------------------------
# When prepay_with_excess = TRUE the monthly surplus (rent − mortgage) is used
# to reduce the loan principal.  This lowers interest in later years and means
# the actual cash-in-hand is ~0 (only the tax refund/payment remains).
compute_tax_analysis <- function(purchase_price, loan_amount, annual_rate,
                                 loan_term_years, hold_years, initial_rent,
                                 annual_rent_increase, sale_price,
                                 total_investment,
                                 building_ratio  = 0.70,
                                 afa_rate        = 0.02,
                                 operating_costs = 5000,
                                 combined_tax_rate = 0.475,
                                 prepay_with_excess = FALSE,
                                 monthly_utilities  = 0) {

  mr  <- annual_rate / 12
  mp  <- annuity_payment(loan_amount, annual_rate, loan_term_years)
  bv  <- purchase_price * building_ratio
  afa <- bv * afa_rate

  bal  <- loan_amount
  rent <- initial_rent
  cfs  <- c(-total_investment)
  cum  <- 0
  rows <- vector("list", hold_years)

  for (yr in seq_len(hold_years)) {
    if (yr > 1) rent <- rent + annual_rent_increase

    y_int <- 0; y_exc <- 0; y_oop <- 0; y_paid <- 0

    for (mo in 1:12) {
      if (bal <= 0) {
        # Loan already cleared – all net rent is surplus
        y_exc <- y_exc + (rent - monthly_utilities)
        next
      }
      int  <- bal * mr
      prin <- min(mp - int, bal)
      pay  <- int + prin
      y_int  <- y_int + int
      y_paid <- y_paid + pay
      bal    <- bal - prin

      net <- rent - monthly_utilities
      d   <- net - pay
      if (d < 0) {
        y_oop <- y_oop + abs(d)
      } else if (prepay_with_excess && bal > 0) {
        pp   <- min(d, bal); bal <- bal - pp
        left <- d - pp
        if (left > 0) y_exc <- y_exc + left
      } else {
        y_exc <- y_exc + d
      }
    }

    # --- Tax calculation (unchanged logic: income vs deductions) ---
    ar   <- rent * 12
    ded  <- y_int + afa + operating_costs
    ti   <- ar - ded
    te   <- if (ti < 0) abs(ti) * combined_tax_rate else -ti * combined_tax_rate

    # After-tax CF = actual cash surplus (after prepayment) + tax effect
    surplus <- y_exc - y_oop
    atcf    <- surplus + te
    cum     <- cum + atcf

    ycf <- atcf
    if (yr == hold_years) {
      rem <- max(bal, 0)
      cgt <- if (hold_years >= 10) 0 else max(0, sale_price - purchase_price) * combined_tax_rate
      ycf <- ycf + sale_price - rem - cgt
    }
    cfs <- c(cfs, ycf)

    rows[[yr]] <- data.frame(
      Year = yr, Annual_Rent = round(ar), Loan_Interest = round(y_int),
      Depreciation = round(afa), Operating_Costs = round(operating_costs),
      Total_Deductible = round(ded), Taxable_Income = round(ti),
      Tax_Effect = round(te), After_Tax_CF = round(atcf),
      Cumulative_CF = round(cum), Remaining_Loan = round(max(bal, 0)))
  }

  df   <- do.call(rbind, rows)
  irr  <- compute_irr(cfs)
  rem  <- max(bal, 0)
  cgt  <- if (hold_years >= 10) 0 else max(0, sale_price - purchase_price) * combined_tax_rate
  ns   <- sale_price - rem - cgt
  tref <- sum(df$Tax_Effect[df$Tax_Effect > 0])
  tpay <- sum(abs(df$Tax_Effect[df$Tax_Effect < 0]))
  tatcf <- sum(df$After_Tax_CF)

  list(tax_details = df, cashflows = cfs,
       irr_after_tax = round(irr * 100, 2),
       monthly_payment = round(mp, 2), annual_mortgage = round(mp * 12, 2),
       annual_afa = round(afa), building_value = round(bv),
       total_refunds = round(tref), total_payments = round(tpay),
       net_tax_benefit = round(tref - tpay),
       total_after_tax_cf = round(tatcf),
       net_sale = round(ns), capital_gains_tax = round(cgt),
       remaining_loan = round(rem),
       after_tax_profit = round(tatcf + ns - total_investment))
}

# ---- Sensitivity: Operating Costs --------------------------------------------
compute_sens_opcost <- function(purchase_price, loan_amount, annual_rate,
                                loan_term_years, hold_years, initial_rent,
                                annual_rent_increase, sale_price, total_investment,
                                building_ratio, afa_rate, combined_tax_rate,
                                prepay_with_excess = FALSE,
                                monthly_utilities = 0,
                                oc_range = seq(2000, 10000, 500)) {
  do.call(rbind, lapply(oc_range, function(oc) {
    r <- compute_tax_analysis(purchase_price, loan_amount, annual_rate,
           loan_term_years, hold_years, initial_rent, annual_rent_increase,
           sale_price, total_investment, building_ratio, afa_rate, oc,
           combined_tax_rate, prepay_with_excess, monthly_utilities)
    data.frame(Operating_Costs = oc, IRR = r$irr_after_tax)
  }))
}

# ---- Sensitivity: Interest Rate ----------------------------------------------
compute_sens_rate <- function(purchase_price, loan_amount, annual_rate,
                              loan_term_years, hold_years, initial_rent,
                              annual_rent_increase, sale_price, total_investment,
                              building_ratio, afa_rate, operating_costs,
                              combined_tax_rate,
                              prepay_with_excess = FALSE,
                              monthly_utilities = 0,
                              rate_range = seq(0.02, 0.06, 0.005)) {
  do.call(rbind, lapply(rate_range, function(rt) {
    r <- compute_tax_analysis(purchase_price, loan_amount, rt,
           loan_term_years, hold_years, initial_rent, annual_rent_increase,
           sale_price, total_investment, building_ratio, afa_rate, operating_costs,
           combined_tax_rate, prepay_with_excess, monthly_utilities)
    data.frame(Rate = rt * 100, IRR = r$irr_after_tax)
  }))
}

# ---- Hold Period Analysis ----------------------------------------------------
compute_hold_analysis <- function(purchase_price, loan_amount, annual_rate,
                                  loan_term_years, initial_rent,
                                  annual_rent_increase, sale_price,
                                  total_investment, building_ratio, afa_rate,
                                  operating_costs, combined_tax_rate,
                                  prepay_with_excess = FALSE,
                                  monthly_utilities = 0,
                                  base_hold = 10, hold_range = 5:15) {
  apr <- (sale_price / purchase_price)^(1 / base_hold) - 1

  do.call(rbind, lapply(hold_range, function(hy) {
    sp <- purchase_price * (1 + apr)^hy
    r  <- compute_tax_analysis(purchase_price, loan_amount, annual_rate,
            loan_term_years, hy, initial_rent, annual_rent_increase, sp,
            total_investment, building_ratio, afa_rate, operating_costs,
            combined_tax_rate, prepay_with_excess, monthly_utilities)
    data.frame(Hold_Years = hy, IRR = r$irr_after_tax,
               CG_Tax = r$capital_gains_tax, Net_Sale = r$net_sale)
  }))
}
