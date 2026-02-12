library(shiny)
library(plotly)
library(DT)
library(bslib)

# ==============================================================================
# Translation Dictionary
# ==============================================================================
TR <- list(
  CN = list(
    title = "房产投资收益计算器",
    params = "参数设置",
    # Inputs
    purchase_price = "购买价格 (万元)",
    down_payment = "首付 (万元)",
    extra_costs = "额外杂费 (万元)",
    loan_amount = "贷款金额 (万元)",
    annual_rate = "贷款年利率 (%)",
    loan_term_years = "贷款年限 (年)",
    start_year = "起始年份",
    initial_rent = "初始月租 (€)",
    monthly_utilities = "物业水暖网费 (€/月)",
    annual_rent_increase = "租金年增幅 (€/月)",
    hold_years = "持有年数",
    sale_price = "预期卖出价格 (万元)",
    prepay_with_excess = "多余租金自动提前还贷",
    calc_btn = "开始计算",
    # Outputs
    irr_title = "IRR (内部收益率)",
    total_profit_title = "总利润",
    simple_roi_title = "简单年化收益",
    monthly_payment_title = "月供 (银行还款)",
    # Tabs
    tab_plots = "图表分析",
    tab_table = "详细数据表",
    tab_summary = "投资摘要",
    # Plots
    plot_cf_title = "年度运营现金流",
    plot_cf_excess = "多余现金/盈余",
    plot_cf_oop = "自掏腰包/亏损",
    plot_equity_title = "资产权益积累",
    plot_equity_loan = "剩余贷款",
    plot_equity_net = "净资产",
    axis_amount = "金额 (€)",
    axis_year = "年份",
    # Table Headers
    tbl_cols = c("年份", "月租", "月供", "年租金", "年还贷", "自掏腰包", "多余现金", "提前还贷", "剩余贷款"),
    # Summary
    sum_title = "房产投资收益分析摘要",
    sum_invest = "实际总投入",
    sum_net_sale = "卖房净得",
    sum_loan = "剩余贷款",
    sum_rent = "总租金收入",
    sum_mortgage = "总支付月供",
    sum_profit = "总利润",
    # Modals
    modal_irr_title = "什么是内部收益率 (IRR)?",
    modal_simple_title = "什么是简单年化收益?",
    plot_rent_title = "租金增长趋势",
    plot_rent_annual = "年租金收入",
    plot_rent_monthly = "月租金"
  ),
  EN = list(
    title = "Real Estate ROI Calculator",
    params = "Parameters",
    purchase_price = "Purchase Price (k€)",
    down_payment = "Down Payment (k€)",
    extra_costs = "Extra Costs (k€)",
    loan_amount = "Loan Amount (k€)",
    annual_rate = "Annual Interest Rate (%)",
    loan_term_years = "Loan Term (Years)",
    start_year = "Start Year",
    initial_rent = "Initial Monthly Rent (€)",
    monthly_utilities = "Monthly Utilities (€)",
    annual_rent_increase = "Annual Rent Increase (€/mo)",
    hold_years = "Holding Years",
    sale_price = "Expected Sale Price (k€)",
    prepay_with_excess = "Auto Prepay with Excess Rent",
    calc_btn = "Calculate",
    irr_title = "IRR (Internal Rate of Return)",
    total_profit_title = "Total Profit",
    simple_roi_title = "Simple Annualized Return",
    monthly_payment_title = "Monthly Payment",
    tab_plots = "Charts",
    tab_table = "Detailed Table",
    tab_summary = "Summary",
    plot_cf_title = "Operating Cash Flow",
    plot_cf_excess = "Excess Cash/Surplus",
    plot_cf_oop = "Out of Pocket/Deficit",
    plot_equity_title = "Equity Buildup",
    plot_equity_loan = "Remaining Loan",
    plot_equity_net = "Net Equity",
    axis_amount = "Amount (€)",
    axis_year = "Year",
    tbl_cols = c("Year", "Monthly Rent", "Monthly Payment", "Yearly Rent", "Yearly Mortgage", "Out of Pocket", "Excess Cash", "Prepaid", "Remaining Balance"),
    sum_title = "Real Estate Investment Analysis Summary",
    sum_invest = "Total Investment",
    sum_net_sale = "Net Sale Proceeds",
    sum_loan = "Remaining Loan",
    sum_rent = "Total Rent Collected",
    sum_mortgage = "Total Mortgage Paid",
    sum_profit = "Total Profit",
    modal_irr_title = "What is IRR (Internal Rate of Return)?",
    modal_simple_title = "What is Simple Annualized Return?",
    plot_rent_title = "Rent Growth Trend",
    plot_rent_annual = "Annual Rent Income",
    plot_rent_monthly = "Monthly Rent"
  ),
  DE = list(
    title = "Immobilien-Renditerechner",
    params = "Parameter",
    purchase_price = "Kaufpreis (T€)",
    down_payment = "Anzahlung (T€)",
    extra_costs = "Nebenkosten (T€)",
    loan_amount = "Darlehensbetrag (T€)",
    annual_rate = "Zinssatz (%)",
    loan_term_years = "Laufzeit (Jahre)",
    start_year = "Startjahr",
    initial_rent = "Anfangsmiete (€)",
    monthly_utilities = "Monatliche Nebenkosten (€)",
    annual_rent_increase = "Mietsteigerung (€/Monat)",
    hold_years = "Haltedauer (Jahre)",
    sale_price = "Verkaufspreis (T€)",
    prepay_with_excess = "Sondertilgung mit Überschuss",
    calc_btn = "Berechnen",
    irr_title = "Interner Zinsfuß (IRR)",
    total_profit_title = "Gesamtgewinn",
    simple_roi_title = "Einfache jährliche Rendite",
    monthly_payment_title = "Monatliche Rate",
    tab_plots = "Diagramme",
    tab_table = "Detaillierte Tabelle",
    tab_summary = "Zusammenfassung",
    plot_cf_title = "Operativer Cashflow",
    plot_cf_excess = "Überschuss",
    plot_cf_oop = "Zuzahlung",
    plot_equity_title = "Eigenkapitalaufbau",
    plot_equity_loan = "Restschuld",
    plot_equity_net = "Eigenkapital",
    axis_amount = "Betrag (€)",
    axis_year = "Jahr",
    tbl_cols = c("Jahr", "Monatsmiete", "Monatsrate", "Jahresmiete", "Jahresrate", "Zuzahlung", "Überschuss", "Sondertilgung", "Restschuld"),
    sum_title = "Zusammenfassung der Immobilieninvestition",
    sum_invest = "Gesamtinvestition",
    sum_net_sale = "Nettoverkaufserlös",
    sum_loan = "Restschuld",
    sum_rent = "Gesamtmieteinnahmen",
    sum_mortgage = "Gezahlte Hypothek",
    sum_profit = "Gesamtgewinn",
    modal_irr_title = "Was ist der Interne Zinsfuß (IRR)?",
    modal_simple_title = "Was ist die einfache jährliche Rendite?",
    plot_rent_title = "Mietentwicklung",
    plot_rent_annual = "Jahresmieteinnahmen",
    plot_rent_monthly = "Monatsmiete"
  )
)

# ==============================================================================
# 核心逻辑函数
# ==============================================================================

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
  return(r)
}

real_estate_roi <- function(purchase_price,
                            down_payment,
                            extra_costs,
                            loan_amount,
                            annual_rate,
                            loan_term_years,
                            hold_years,
                            initial_rent,
                            monthly_utilities = 0,
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
        yearly_rent    <- yearly_rent + current_rent
        yearly_excess  <- yearly_excess + current_rent
        next
      }

      interest  <- balance * monthly_rate
      principal <- min(monthly_payment - interest, balance)
      actual_payment <- interest + principal
      balance   <- balance - principal

      # 净租金 = 租金 - 物业水暖网费
      net_rent <- current_rent - monthly_utilities

      # 用净租金去还贷
      diff <- net_rent - actual_payment
      if (diff < 0) {
        yearly_oop <- yearly_oop + abs(diff)
      } else {
        if (prepay_with_excess && balance > 0) {
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
      yearly_mortgage <- yearly_mortgage + actual_payment
    }

    total_rent     <- total_rent + yearly_rent
    total_mortgage <- total_mortgage + yearly_mortgage
    out_of_pocket  <- out_of_pocket + yearly_oop
    excess_cash    <- excess_cash + yearly_excess
    total_prepaid  <- total_prepaid + yearly_prepaid

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

  irr <- compute_irr(cashflows)

  remaining_loan   <- round(max(balance, 0), 2)
  net_sale_proceed <- sale_price - remaining_loan
  actual_total_in  <- total_investment + out_of_pocket
  actual_total_out <- net_sale_proceed + excess_cash
  simple_return    <- (actual_total_out / actual_total_in)^(1 / hold_years) - 1
  total_profit     <- actual_total_out - actual_total_in

  list(
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
}

# ==============================================================================
# Shiny App UI
# ==============================================================================

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),

  tags$head(
    tags$style(HTML("
      /* 侧边栏整体紧凑化 */
      .well {
        padding: 10px 15px;
      }

      /* 表单组 Flex 布局：左 Label 右 Input */
      .well .form-group {
        display: flex;
        flex-direction: row;
        align-items: center;
        margin-bottom: 6px;
      }

      /* 左侧 Label 样式 */
      .well .form-group > label {
        flex: 0 0 65%;
        max-width: 65%;
        margin-bottom: 0;
        font-weight: normal;
        font-size: 13px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        padding-right: 5px;
      }

      /* 右侧 Input 样式 */
      .well .form-group input {
        flex: 1;
        height: 30px;
        padding: 4px 8px;
        font-size: 13px;
      }

      /* 分割线紧凑化 */
      .well hr {
        margin-top: 8px;
        margin-bottom: 8px;
      }

      /* 按钮样式微调 */
      #calc_btn {
        margin-top: 10px;
        width: 100%;
        padding: 6px;
      }

      /* Checkbox 特殊处理 */
      .well .checkbox {
        margin-top: 5px;
        margin-bottom: 5px;
        font-size: 13px;
      }

      /* 语言切换器样式 */
      #lang-container {
        position: absolute;
        top: 15px;
        right: 30px;
        z-index: 1000;
        display: flex;
        gap: 10px;
      }

      .lang-btn {
        cursor: pointer;
        font-weight: bold;
        color: #95a5a6;
        font-size: 14px;
        transition: all 0.3s ease;
        text-decoration: none !important;
        border: none;
        background: none;
        padding: 0 2px;
      }

      .lang-btn:hover {
        color: #2c3e50;
        transform: scale(1.1);
      }

      .lang-btn.active {
        color: #2c3e50;
        font-size: 18px; /* 选中变大 */
        border-bottom: 2px solid #2c3e50;
      }
    ")),

    # 自定义 JS 处理点击事件
    tags$script(HTML("
      $(document).on('click', '.lang-btn', function() {
        var lang = $(this).attr('data-value');
        Shiny.setInputValue('lang', lang);

        $('.lang-btn').removeClass('active');
        $(this).addClass('active');
      });
    "))
  ),

  # 顶部语言切换器 (绝对定位)
  tags$div(id = "lang-container",
    actionLink("lang_cn", "汉", class = "lang-btn active", `data-value`="CN", title="汉语"),
    span("|", style="color: #ccc;"),
    actionLink("lang_en", "EN", class = "lang-btn", `data-value`="EN", title="English"),
    span("|", style="color: #ccc;"),
    actionLink("lang_de", "DE", class = "lang-btn", `data-value`="DE", title="Deutsch")
  ),

  # 标题区域动态输出
  uiOutput("ui_title"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      # 移除原有的 radioButtons
      # radioButtons("lang", ...),

      numericInput("purchase_price", "购买价格 (万元)", 25, step = 1),
      numericInput("extra_costs", "额外杂费 (万元)", 3, step = 0.5),
      hr(),
      numericInput("loan_amount", "贷款金额 (万元)", 20, step = 1),
      numericInput("annual_rate", "贷款年利率 (%)", 3.84, step = 0.01),
      numericInput("loan_term_years", "贷款年限 (年)", 20, step = 1),
      hr(),
      numericInput("start_year", "起始年份", as.integer(format(Sys.Date(), "%Y")), step = 1),
      numericInput("initial_rent", "初始月租 (€)", 1800, step = 50),
      numericInput("monthly_utilities", "物业水暖网费 (元/月)", 350, step = 50),
      numericInput("annual_rent_increase", "租金年增幅 (元/月)", 50, step = 10),
      hr(),
      numericInput("hold_years", "持有年数", 10, step = 1),
      numericInput("sale_price", "预期卖出价格 (万元)", 28, step = 1),

      checkboxInput("prepay_with_excess", "多余租金自动提前还贷", value = TRUE),

      actionButton("calc_btn", "开始计算", class = "btn-primary")
    ),

    mainPanel(
      width = 9,

      # 顶部关键指标行
      uiOutput("ui_metrics"),

      # 主要标签页
      uiOutput("ui_tabs")
    )
  )
)

# ==============================================================================
# Shiny App Server
# ==============================================================================

server <- function(input, output, session) {

  # 获取当前语言的翻译
  tr <- reactive({
    lang <- input$lang
    if (is.null(lang)) lang <- "CN" # 默认中文
    TR[[lang]]
  })

  # 动态更新 Inputs Labels 和 Values
  observeEvent(input$lang, {
    t <- tr()
    lang <- input$lang
    if (is.null(lang)) lang <- "CN"

    if (!is.null(t)) {
      # 根据语言设置默认值和步长
      # CN: 单位万元 (例如 25)
      # EN/DE: 单位千元 (例如 250)
      if (lang == "CN") {
        updateNumericInput(session, "purchase_price", label = t$purchase_price, value = 25, step = 1)
        updateNumericInput(session, "extra_costs", label = t$extra_costs, value = 3, step = 0.5)
        updateNumericInput(session, "loan_amount", label = t$loan_amount, value = 20, step = 1)
        updateNumericInput(session, "sale_price", label = t$sale_price, value = 28, step = 1)
      } else {
        updateNumericInput(session, "purchase_price", label = t$purchase_price, value = 250, step = 10)
        updateNumericInput(session, "extra_costs", label = t$extra_costs, value = 30, step = 5)
        updateNumericInput(session, "loan_amount", label = t$loan_amount, value = 200, step = 10)
        updateNumericInput(session, "sale_price", label = t$sale_price, value = 280, step = 10)
      }

      updateNumericInput(session, "annual_rate", label = t$annual_rate)
      updateNumericInput(session, "loan_term_years", label = t$loan_term_years)
      updateNumericInput(session, "start_year", label = t$start_year)
      updateNumericInput(session, "initial_rent", label = t$initial_rent)
      updateNumericInput(session, "monthly_utilities", label = t$monthly_utilities)
      updateNumericInput(session, "annual_rent_increase", label = t$annual_rent_increase)
      updateNumericInput(session, "hold_years", label = t$hold_years)
      updateCheckboxInput(session, "prepay_with_excess", label = t$prepay_with_excess)
      updateActionButton(session, "calc_btn", label = t$calc_btn)
    }
  })

  # 渲染标题
  output$ui_title <- renderUI({
    titlePanel(tr()$title)
  })

  # 渲染指标区域
  output$ui_metrics <- renderUI({
    t <- tr()
    fluidRow(
      column(3,
            div(class = "alert alert-success", style = "text-align: center; position: relative;",
            tags$div(
              style = "position: absolute; top: 0px; right: 10px; cursor: pointer;",
              actionLink("irr_help", label = icon("question-circle"), style = "color: #155724; font-size: 18px;")),
                h4(t$irr_title),
                h2(textOutput("irr_val"))
            )
      ),
      column(3,
             div(class = "alert alert-info", style = "text-align: center;",
               h4(t$total_profit_title),
               h2(textOutput("total_profit_val")))
      ),
      column(3,
             div(class = "alert alert-warning", style = "text-align: center; position: relative;",
                 tags$div(
                   style = "position: absolute; top: 0px; right: 10px; cursor: pointer;",
                   actionLink("simple_roi_help", label = icon("question-circle"), style = "color: #856404; font-size: 18px;")
                 ),
                 h4(t$simple_roi_title),
                 h2(textOutput("simple_roi_val"))
             )
      ),
      column(3,
             div(class = "alert alert-danger", style = "text-align: center;",
                 h4(t$monthly_payment_title),
                 h2(textOutput("monthly_payment_val"))
             )
      )
    )
  })

  # 渲染标签页
  output$ui_tabs <- renderUI({
    t <- tr()
    tabsetPanel(
      tabPanel(t$tab_plots,
               br(),
               fluidRow(
                 column(6, plotlyOutput("plot_cashflow", height = "400px")),
                 column(6, plotlyOutput("plot_equity", height = "400px"))
               ),
               br(),
               fluidRow(
                 column(12, plotlyOutput("plot_rent", height = "400px"))
               )
      ),
      tabPanel(t$tab_table,
               br(),
               DTOutput("detail_table")
      ),
      tabPanel(t$tab_summary,
               br(),
               verbatimTextOutput("summary_text")
      )
    )
  })

  # 反应式计算结果
  roi_data <- eventReactive(input$calc_btn, {
    # 单位转换：中文=万元(x10000)，外文=千元(x1000)
    lang <- input$lang
    if (is.null(lang)) lang <- "CN"
    m <- if (lang == "CN") 10000 else 1000

    p_price  <- input$purchase_price * m
    e_costs  <- input$extra_costs * m
    l_amount <- input$loan_amount * m
    s_price  <- input$sale_price * m

    res <- real_estate_roi(
      purchase_price       = p_price,
      down_payment         = p_price - l_amount,
      extra_costs          = e_costs,
      loan_amount          = l_amount,
      annual_rate          = input$annual_rate / 100,
      loan_term_years      = input$loan_term_years,
      hold_years           = input$hold_years,
      initial_rent         = input$initial_rent,
      monthly_utilities    = input$monthly_utilities,
      annual_rent_increase = input$annual_rent_increase,
      sale_price           = s_price,
      prepay_with_excess   = input$prepay_with_excess
    )

    start_y <- input$start_year
    res$yearly_details$Year <- res$yearly_details$Year + start_y - 1

    res
  }, ignoreNULL = FALSE)

  # 输出数值
  output$irr_val <- renderText({ paste0(roi_data()$irr, "%") })
  output$total_profit_val <- renderText({ paste0("€", format(roi_data()$total_profit, big.mark = ",")) })
  output$simple_roi_val <- renderText({ paste0(roi_data()$simple_annualized, "%") })
  output$monthly_payment_val <- renderText({ paste0("€", format(roi_data()$monthly_payment, big.mark = ",")) })

  # 摘要文本
  output$summary_text <- renderPrint({
    res <- roi_data()
    t <- tr()

    cat("============================================\n")
    cat(paste("        ", t$sum_title, "\n"))
    cat("============================================\n")
    cat(sprintf("%-20s: %12.2f \n", t$sum_invest, res$actual_total_in))
    cat(sprintf("%-20s: %12.2f \n", t$sum_net_sale, res$net_sale_proceed))
    cat(sprintf("%-20s: %12.2f \n", t$sum_loan, res$remaining_loan))
    cat(sprintf("%-20s: %12.2f \n", t$sum_rent, res$total_rent_collected))
    cat(sprintf("%-20s: %12.2f \n", t$sum_mortgage, res$total_mortgage_paid))
    cat("--------------------------------------------\n")
    cat(sprintf("%-20s: %12.2f \n", t$sum_profit, res$total_profit))
  })

  # 详细表格
  output$detail_table <- renderDT({
    df <- roi_data()$yearly_details
    t <- tr()

    # 重命名列
    if(length(t$tbl_cols) == ncol(df)) {
      colnames(df) <- t$tbl_cols
    }

    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  # 图表1
  output$plot_cashflow <- renderPlotly({
    df <- roi_data()$yearly_details
    t <- tr()

    plot_ly(df, x = ~Year) %>%
      add_bars(y = ~Excess_Cash, name = t$plot_cf_excess, marker = list(color = 'green')) %>%
      add_bars(y = ~Out_of_Pocket, name = t$plot_cf_oop, marker = list(color = 'red')) %>%
      layout(title = list(text = t$plot_cf_title, y = 0.95),
             margin = list(t = 60),
             yaxis = list(title = t$axis_amount),
             xaxis = list(title = t$axis_year, tickformat = "d"),
             barmode = 'group')
  })

  # 图表2
  output$plot_equity <- renderPlotly({
    df <- roi_data()$yearly_details
    res <- roi_data()
    t <- tr()

    prop_values <- seq(res$purchase_price, res$sale_price, length.out = res$hold_years + 1)[-1]
    df$Property_Value <- prop_values
    df$Equity <- df$Property_Value - df$Remaining_Balance

    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~Remaining_Balance, name = t$plot_equity_loan, type = 'scatter', mode = 'lines+markers', fill = 'tozeroy') %>%
      add_trace(y = ~Equity, name = t$plot_equity_net, type = 'scatter', mode = 'lines+markers', fill = 'tonexty') %>%
      layout(title = list(text = t$plot_equity_title, y = 0.95),
             margin = list(t = 60),
             yaxis = list(title = t$axis_amount),
             xaxis = list(title = t$axis_year, tickformat = "d"))
  })

  # 图表3: 租金趋势
  output$plot_rent <- renderPlotly({
    df <- roi_data()$yearly_details
    t <- tr()

    plot_ly(df, x = ~Year) %>%
      add_trace(y = ~Monthly_Rent, name = t$plot_rent_monthly, type = 'scatter', mode = 'lines+markers', line = list(color = '#17a2b8', width = 3)) %>%
      layout(title = list(text = t$plot_rent_title, y = 0.95),
             margin = list(t = 60),
             yaxis = list(title = paste(t$plot_rent_monthly, "(€)")),
             xaxis = list(title = t$axis_year, tickformat = "d"),
             legend = list(x = 0.1, y = 0.9))
  })

  # 弹窗内容 (保持中文或根据需要扩展，这里做简化处理以适应多语言结构)
  # 为简化代码，这里仅保留中文逻辑，实际应根据 tr() 内容渲染不同HTML
  # 但由于HTML太长，这里用简化的判断演示

  observeEvent(input$irr_help, {
    t <- tr()
    lang <- input$lang
    if (is.null(lang)) lang <- "CN"

    # 简单的多语言内容示例
    content <- div("Explanation not available in this language yet.")

    if (lang == "CN") {
      content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("一句话解释："), "IRR 就是这个投资项目的", strong("实际年化复利回报率"), "。"),
          hr(),
          h5(icon("subscript"), " 数学公式"),
          div(style = "background-color: #f8f9fa; padding: 10px; text-align: center;", HTML("∑ [ C<sub>t</sub> / (1 + r)<sup>t</sup> ] = 0")),
          p("通俗地说：就是找到一个利率 r，让未来的所有收入打折（折现）到现在，刚好抵消掉所有的投入成本。"),
          hr(),
          h5(icon("calculator"), " 结合公式看例子"),
          tags$div(style = "background-color: #fcfcfc; border-left: 4px solid #28a745; padding: 10px; margin-bottom: 10px;",
            p(strong("情况 A (1年)：投入100，1年后拿回110")),
            p("公式：", code("-100 + 110 / (1+r)¹ = 0")),
            p("计算：", code("110 / (1+r) = 100"), " → ", strong("r = 10%"))
          ),
          tags$div(style = "background-color: #fcfcfc; border-left: 4px solid #17a2b8; padding: 10px; margin-bottom: 10px;",
            p(strong("情况 B (2年)：投入100，2年后拿回121")),
            p("公式：", code("-100 + 121 / (1+r)² = 0")),
            p("计算：", code("(1+r)² = 1.21"), " → ", code("1+r = 1.1"), " → ", strong("r = 10%")),
            p(tags$small("注：这就是复利。虽然总收益21%，但年化IRR仍是10%。"))
          ),
          tags$div(style = "background-color: #fcfcfc; border-left: 4px solid #ffc107; padding: 10px; margin-bottom: 10px;",
            p(strong("情况 C (10年)：投入100，10年后拿回200")),
            p("公式：", code("-100 + 200 / (1+r)¹⁰ = 0")),
            p("计算：", code("(1+r)¹⁰ = 2"), " → ", code("1+r = 2^(1/10) ≈ 1.0718"), " → ", strong("r ≈ 7.18%")),
            p(tags$small("注：10年翻倍听起来很多，但其实年化只有约 7.18%。时间越长，对IRR的稀释作用越明显。"))
          ),
          hr(),
          h5(icon("lightbulb"), " IRR 对投资的指导意义"),
          tags$ul(
            tags$li(strong("与贷款利率对比："), "这是判断是否利用了“正杠杆”的关键。如果 ", span("IRR > 贷款利率", style="color:green; font-weight:bold;"), "，说明你借银行的钱赚了差价（用别人的钱生钱）。反之如果 IRR < 贷款利率，说明投资回报覆盖不了资金成本，实际上在亏钱。"),
            tags$li(strong("与国债/理财对比："), "IRR 应该显著高于无风险收益率（如国债 3%）。如果房产投资的 IRR 只有 3-4%，考虑到房产的流动性差、交易成本高、维护麻烦，还不如直接买国债躺平。"),
            tags$li(strong("资金效率指标："), "IRR 是衡量资金使用效率的最佳指标。它比“租售比”或“简单回报率”更科学，因为它考虑了", strong("资金的时间价值"), "——早收到的钱比晚收到的钱更值钱。")
          )
      )
    } else if (lang == "EN") {
      content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("Summary:"), "IRR is the ", strong("actual annualized compound return rate"), " of this investment."),
          hr(),
          h5(icon("subscript"), " Formula"),
          div(style = "background-color: #f8f9fa; padding: 10px; text-align: center;", HTML("∑ [ C<sub>t</sub> / (1 + r)<sup>t</sup> ] = 0")),
          p("Simply put: It is the rate r that discounts all future cash flows to the present so that they exactly equal the initial investment."),
          hr(),
          h5(icon("calculator"), " Example"),
          tags$div(style = "background-color: #fcfcfc; border-left: 4px solid #28a745; padding: 10px; margin-bottom: 10px;",
            p(strong("Case A: Get 110 back after 1 year (Invest 100)")),
            p("Formula: ", code("-100 + 110 / (1+r)¹ = 0")),
            p("Result: ", strong("r = 10%"))
          )
      )
    } else if (lang == "DE") {
      content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("Zusammenfassung:"), "Der IRR ist die ", strong("tatsächliche jährliche Rendite"), " dieser Investition."),
          hr(),
          h5(icon("subscript"), " Formel"),
          div(style = "background-color: #f8f9fa; padding: 10px; text-align: center;", HTML("∑ [ C<sub>t</sub> / (1 + r)<sup>t</sup> ] = 0")),
          p("Einfach ausgedrückt: Es ist der Zinssatz r, der alle zukünftigen Zahlungsströme so diskontiert, dass sie genau der Anfangsinvestition entsprechen.")
      )
    }

    showModal(modalDialog(
      title = t$modal_irr_title,
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      content
    ))
  })

  observeEvent(input$simple_roi_help, {
    t <- tr()
    lang <- input$lang
    if (is.null(lang)) lang <- "CN"

    content <- div("Explanation not available.")
    if (lang == "CN") {
       content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("定义："), "忽略资金进出时间，仅对比", strong("总投入"), "和", strong("总回报"), "计算出的平均年化增长率。"),
          hr(),
          p("它假设所有投入都在第0年，所有回报都在最后一年。是一个粗略估算。")
       )
    } else if (lang == "EN") {
       content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("Definition:"), "Average annualized growth rate comparing only ", strong("Total Investment"), " and ", strong("Total Return"), ", ignoring timing."),
          hr(),
          p("It assumes all investments occur at Year 0 and all returns at the final year. It is a rough estimate.")
       )
    } else if (lang == "DE") {
       content <- div(style = "font-size: 15px; line-height: 1.6;",
          p(strong("Definition:"), "Durchschnittliche jährliche Wachstumsrate, die nur ", strong("Gesamtinvestition"), " und ", strong("Gesamtertrag"), " vergleicht."),
          hr(),
          p("Sie geht davon aus, dass alle Investitionen im Jahr 0 und alle Erträge im letzten Jahr anfallen. Es ist eine grobe Schätzung.")
       )
    }

    showModal(modalDialog(
      title = t$modal_simple_title,
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      content
    ))
  })
}

shinyApp(ui, server)
