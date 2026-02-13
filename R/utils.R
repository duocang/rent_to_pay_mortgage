# ==============================================================================
# Helper functions
# ==============================================================================

# Format currency
fmt_eur <- function(x, digits = 0) {
  paste0(formatC(round(x, digits), format = "f", digits = digits, big.mark = ","), " €")
}

# Unit multiplier helper (used in validation logic if needed, but here it's reactive in server)
# validate_inputs takes 'inp' which is a list-like object (input) and 't' (translations)

validate_inputs <- function(inp, t) {
  errs <- character(0)
  # Helper: validate one numeric input
  chk <- function(id, label, min_val = NULL, max_val = NULL, gt0 = FALSE) {
    v <- inp[[id]]
    if (is.null(v) || length(v) == 0 || is.na(v)) {
      errs <<- c(errs, paste0(label, " \u2014 ", t$val_required)); return()
    }
    if (gt0 && v <= 0) { errs <<- c(errs, paste0(label, " \u2014 ", t$val_positive)); return() }
    if (!is.null(min_val) && v < min_val)
      errs <<- c(errs, paste0(label, " \u2014 ", gsub("{min}", min_val, t$val_min, fixed = TRUE)))
    if (!is.null(max_val) && v > max_val)
      errs <<- c(errs, paste0(label, " \u2014 ", gsub("{max}", max_val, t$val_max, fixed = TRUE)))
  }
  # Property
  chk("purchase_price",    t$purchase_price,    gt0 = TRUE)
  chk("extra_costs",       t$extra_costs,       min_val = 0)
  chk("loan_amount",       t$loan_amount,       min_val = 0)
  chk("annual_rate",       t$annual_rate,       min_val = 0.01, max_val = 99)
  chk("loan_term_years",   t$loan_term_years,   min_val = 1,    max_val = 50)
  chk("zinsbindung_years", t$zinsbindung_years, min_val = 1,    max_val = 50)
  chk("refi_rate",         t$refi_rate,         min_val = 0.01, max_val = 99)
  # Rental
  chk("start_year",           t$start_year,           min_val = 1900)
  chk("initial_rent",         t$initial_rent,         min_val = 0)
  chk("monthly_utilities",    t$monthly_utilities,    min_val = 0)
  chk("annual_rent_increase", t$annual_rent_increase, min_val = 0)
  chk("hold_years",           t$hold_years,           min_val = 1, max_val = 50)
  chk("sale_price",           t$sale_price,           gt0 = TRUE)
  # Tax
  chk("monthly_salary",      t$monthly_salary,      min_val = 0)
  chk("combined_tax_rate",   t$combined_tax_rate,   min_val = 0.1, max_val = 99)
  chk("building_ratio",      t$building_ratio,      min_val = 1,   max_val = 100)
  chk("annual_opcost",       t$annual_opcost,       min_val = 0)
  chk("afa_rate",            t$afa_rate,            min_val = 0,   max_val = 99)
  chk("selling_cost_rate",   t$selling_cost_rate,   min_val = 0,   max_val = 99)
  chk("vacancy_rate",        t$vacancy_rate,        min_val = 0,   max_val = 99)
  chk("sondertilgung_rate",  t$sondertilgung_rate,  min_val = 0,   max_val = 100)
  chk("opcost_inflation",    t$opcost_inflation,    min_val = 0,   max_val = 99)
  # Cross-field business-logic checks
  if (length(errs) == 0) {
    pp <- inp$purchase_price; la <- inp$loan_amount
    zb <- inp$zinsbindung_years; lt <- inp$loan_term_years
    if (!is.null(la) && !is.null(pp) && la > pp)
      errs <- c(errs, t$val_loan_exceeds)
    if (!is.null(zb) && !is.null(lt) && zb > lt)
      errs <- c(errs, t$val_zins_exceeds)
  }
  errs
}

# Helper: build step row (for Modals)
stp <- function(lbl, val) tags$div(class = "calc-step",
  tags$span(class = "lbl", lbl), tags$span(class = "val", val))
