#' 房产投资收益分析（增强版）
#'
#' @param purchase_price      房屋购买价格（元）
#' @param down_payment        首付（元）
#' @param extra_costs         额外税费杂费（元）
#' @param loan_amount         贷款金额（元）
#' @param annual_rate         贷款年利率（小数，如 0.03 表示 3%）
#' @param loan_term_years     贷款总期限（年）
#' @param hold_years          持有年数
#' @param initial_rent        初始月租金（元）
#' @param annual_rent_increase 每年月租金增加额（元，默认0）
#' @param sale_price          卖出价格（元）
#' @param prepay_with_excess  是否用多余租金提前还贷（默认FALSE）
#'
#' @return 一个包含详细收益信息的列表
real_estate_roi <- function(purchase_price,
                            down_payment,
                            extra_costs,
                            loan_amount,
                            annual_rate,
                            loan_term_years,
                            hold_years,
                            initial_rent,
                            annual_rent_increase = 0,
                            sale_price,
                            prepay_with_excess = FALSE) {

  # ---------- 初始投入 ----------
  total_investment <- down_payment + extra_costs

  # ---------- 月利率 & 月供（等额本息） ----------
  monthly_rate <- annual_rate / 12
  n_total      <- loan_term_years * 12

  monthly_payment <- loan_amount * monthly_rate * (1 + monthly_rate)^n_total /
    ((1 + monthly_rate)^n_total - 1)

  # ---------- 逐月模拟 ----------
  balance        <- loan_amount
  total_rent     <- 0
  total_mortgage <- 0
  out_of_pocket  <- 0
  excess_cash    <- 0
  total_prepaid  <- 0
  current_rent   <- initial_rent

  # 逐年现金流向量（从投资者角度：负=支出，正=收入）
  cashflows <- c(-total_investment)

  yearly_details <- data.frame(
    Year              = integer(),
    Monthly_Rent      = numeric(),
    Monthly_Payment   = numeric(),
    Yearly_Rent       = numeric(),
    Yearly_Mortgage   = numeric(),
    Out_of_Pocket     = numeric(),
    Excess_Cash       = numeric(),
    Prepaid           = numeric(),
    Remaining_Balance = numeric(),
    stringsAsFactors  = FALSE
  )

  for (year in 1:hold_years) {
    if (year > 1) {
      current_rent <- current_rent + annual_rent_increase
    }

    yearly_rent     <- 0
    yearly_mortgage <- 0
    yearly_oop      <- 0
    yearly_excess   <- 0
    yearly_prepaid  <- 0

    for (month in 1:12) {
      if (balance <= 0) {
        # 贷款已还清，租金全部是多余现金
        yearly_rent    <- yearly_rent + current_rent
        yearly_excess  <- yearly_excess + current_rent
        next
      }

      # 正常还贷：利息 + 本金
      interest       <- balance * monthly_rate
      principal      <- min(monthly_payment - interest, balance)
      actual_payment <- interest + principal
      balance        <- balance - principal

      # 租金 vs 月供
      diff <- current_rent - actual_payment
      if (diff < 0) {
        yearly_oop <- yearly_oop + abs(diff)
      } else {
        if (prepay_with_excess && balance > 0) {
          # 多余部分提前还贷
          prepay_amount  <- min(diff, balance)
          balance        <- balance - prepay_amount
          yearly_prepaid <- yearly_prepaid + prepay_amount
          yearly_mortgage <- yearly_mortgage + prepay_amount  # ★ 关键修正
          # 提前还贷后如果还有剩余（贷款已还完），算多余现金
          leftover <- diff - prepay_amount
          if (leftover > 0) {
            yearly_excess <- yearly_excess + leftover
          }
        } else {
          yearly_excess <- yearly_excess + diff
        }
      }

      yearly_rent     <- yearly_rent + current_rent
      yearly_mortgage <- yearly_mortgage + actual_payment  # 正常月供部分
    }

    total_rent     <- total_rent + yearly_rent
    total_mortgage <- total_mortgage + yearly_mortgage
    out_of_pocket  <- out_of_pocket + yearly_oop
    excess_cash    <- excess_cash + yearly_excess
    total_prepaid  <- total_prepaid + yearly_prepaid

    # 该年净现金流
    year_cf <- yearly_excess - yearly_oop
    if (year == hold_years) {
      year_cf <- year_cf + (sale_price - max(balance, 0))
    }
    cashflows <- c(cashflows, year_cf)

    yearly_details <- rbind(yearly_details, data.frame(
      Year              = year,
      Monthly_Rent      = round(current_rent, 2),
      Monthly_Payment   = round(monthly_payment, 2),
      Yearly_Rent       = round(yearly_rent, 2),
      Yearly_Mortgage   = round(yearly_mortgage, 2),
      Out_of_Pocket     = round(yearly_oop, 2),
      Excess_Cash       = round(yearly_excess, 2),
      Prepaid           = round(yearly_prepaid, 2),
      Remaining_Balance = round(max(balance, 0), 2)
    ))
  }

  # ---------- IRR 计算（牛顿法） ----------
  irr <- compute_irr(cashflows)

  # ---------- 简单收益计算 ----------
  remaining_loan   <- round(max(balance, 0), 2)
  net_sale_proceed <- sale_price - remaining_loan
  actual_total_in  <- total_investment + out_of_pocket
  actual_total_out <- net_sale_proceed + excess_cash
  simple_return    <- (actual_total_out / actual_total_in)^(1 / hold_years) - 1
  total_profit     <- actual_total_out - actual_total_in

  results <- list(
    purchase_price       = purchase_price,
    sale_price           = sale_price,
    hold_years           = hold_years,
    prepay_with_excess   = prepay_with_excess,
    loan_amount          = loan_amount,
    monthly_payment      = round(monthly_payment, 2),
    remaining_loan       = remaining_loan,
    total_investment     = total_investment,
    out_of_pocket_extra  = round(out_of_pocket, 2),
    actual_total_in      = round(actual_total_in, 2),
    total_rent_collected = round(total_rent, 2),
    total_mortgage_paid  = round(total_mortgage, 2),
    excess_cash          = round(excess_cash, 2),
    total_prepaid        = round(total_prepaid, 2),
    net_sale_proceed     = round(net_sale_proceed, 2),
    total_profit         = round(total_profit, 2),
    simple_annualized    = round(simple_return * 100, 2),
    irr                  = round(irr * 100, 2),
    cashflows            = cashflows,
    yearly_details       = yearly_details
  )

  # ---------- 打印摘要 ----------
  prt_money <- function(label, val) {
    if (abs(val) >= 10000) {
      cat(sprintf("%s%12.2f 万\n", label, val / 10000))
    } else {
      cat(sprintf("%s%12.2f 元\n", label, val))
    }
  }

  cat("============================================\n")
  cat("         房产投资收益分析\n")
  if (prepay_with_excess) {
    cat("       【多余租金 → 提前还贷】\n")
  } else {
    cat("       【多余租金 → 存为现金】\n")
  }
  cat("============================================\n")
  prt_money("购买价格:          ", purchase_price)
  cat("--------------------------------------------\n")
  prt_money("首付 + 杂费:       ", total_investment)
  prt_money("额外自掏腰包:       ", out_of_pocket)
  prt_money("实际总投入:        ", actual_total_in)
  cat("--------------------------------------------\n")
  prt_money("贷款:            ", loan_amount)
  cat(sprintf("贷款年利率:        %12.2f%%\n",    annual_rate * 100))
  cat(sprintf("贷款年限:        %12d 年\n",     loan_term_years))
  prt_money("等额本息月供:      ", monthly_payment)
  prt_money("提前还贷总额:      ", total_prepaid)

  cat("--------------------------------------------\n")
  prt_money("起始月租金（净）:   ", initial_rent)
  prt_money("月租金年增:        ", annual_rent_increase)

  cat("--------------------------------------------\n")
  cat(sprintf("持有年数:          %12d 年\n",     hold_years))
  prt_money("卖出价格:          ", sale_price)
  cat("--------------------------------------------\n")
  prt_money("剩余贷款:          ", remaining_loan)
  prt_money("卖房净得:          ", net_sale_proceed)
  prt_money("多余租金现金:      ", excess_cash)
  prt_money("实际总现金结余:    ", excess_cash + net_sale_proceed)
  cat("--------------------------------------------\n")
  prt_money("总利润:            ", total_profit)
  cat(sprintf("简单年化收益率:    %11.2f%%\n",    simple_return * 100))
  cat(sprintf("IRR（内部收益率）: %11.2f%%\n",    irr * 100))
  cat("============================================\n")





  # ---------- 打印逐年明细（中文对齐） ----------
  cat("\n逐年明细：\n")

  # 列名和对应宽度（宽度要考虑中文占2格）
  headers <- c("年份", "    月租金", "          月供", "        年租金",
               "      年房贷", "      自掏腰包", "      多余现金", " 提前还贷", "      剩余贷款")
  widths  <- c(6, 12, 12, 12, 12, 12, 12, 12, 14)

  # 计算中文字符的显示宽度
  display_width <- function(s) {
    chars <- unlist(strsplit(s, ""))
    sum(ifelse(nchar(chars, type = "bytes") > 1, 2, 1))
  }

  # 右对齐填充（考虑中文宽度）
  pad_right_align <- function(s, width) {
    dw <- display_width(s)
    pad <- max(0, width - dw)
    paste0(paste(rep(" ", pad), collapse = ""), s)
  }

  # 打印表头
  header_line <- paste(mapply(pad_right_align, headers, widths), collapse = "")
  cat(header_line, "\n")
  cat(paste(rep("-", sum(widths)), collapse = ""), "\n")

  # 打印每行数据
  for (i in 1:nrow(yearly_details)) {
    vals <- c(
      sprintf("%d",    yearly_details$Year[i]),
      sprintf("%.2f",  yearly_details$Monthly_Rent[i]),
      sprintf("%.2f",  yearly_details$Monthly_Payment[i]),
      sprintf("%.2f",  yearly_details$Yearly_Rent[i]),
      sprintf("%.2f",  yearly_details$Yearly_Mortgage[i]),
      sprintf("%.2f",  yearly_details$Out_of_Pocket[i]),
      sprintf("%.2f",  yearly_details$Excess_Cash[i]),
      sprintf("%.2f",  yearly_details$Prepaid[i]),
      sprintf("%.2f",  yearly_details$Remaining_Balance[i])
    )
    row_line <- paste(mapply(pad_right_align, vals, widths), collapse = "")
    cat(row_line, "\n")
  }





  cat("\n逐年现金流（IRR输入）：\n")
  cat(sprintf("  第0年: %12.2f\n", cashflows[1]))
  for (i in 2:length(cashflows)) {
    cat(sprintf("  第%d年: %12.2f\n", i - 1, cashflows[i]))
  }

  invisible(results)
}


#' 用牛顿法计算 IRR
compute_irr <- function(cashflows, guess = 0.1, tol = 1e-9, max_iter = 1000) {
  r <- guess
  for (i in 1:max_iter) {
    npv  <- 0
    dnpv <- 0
    for (t in seq_along(cashflows)) {
      n <- t - 1
      npv  <- npv  + cashflows[t] / (1 + r)^n
      dnpv <- dnpv - n * cashflows[t] / (1 + r)^(n + 1)
    }
    if (abs(dnpv) < 1e-15) break
    r_new <- r - npv / dnpv
    if (abs(r_new - r) < tol) return(r_new)
    r <- r_new
  }
  warning("IRR 未能收敛，返回最后一次迭代结果")
  return(r)
}


# 场景A：多余租金存着，不提前还贷
cat("========== 场景A：不提前还贷 ==========\n\n")
result_a <- real_estate_roi(
  purchase_price       = 250000,
  down_payment         = 50000,
  extra_costs          = 30000,
  loan_amount          = 200000,
  annual_rate          = 0.0384,
  loan_term_years      = 20,
  hold_years           = 10,
  initial_rent         = 1200,
  annual_rent_increase = 50,
  sale_price           = 280000,
  prepay_with_excess   = FALSE
)

cat("\n\n========== 场景B：多余租金提前还贷 ==========\n\n")
result_b <- real_estate_roi(
  purchase_price       = 250000,
  down_payment         = 50000,
  extra_costs          = 30000,
  loan_amount          = 200000,
  annual_rate          = 0.0384,
  loan_term_years      = 20,
  hold_years           = 10,
  initial_rent         = 1200,
  annual_rent_increase = 50,
  sale_price           = 280000,
  prepay_with_excess   = TRUE
)