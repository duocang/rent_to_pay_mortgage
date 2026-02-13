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
    .rpt-container { position:relative; max-width:720px; margin:0 auto; }
    .rpt-pre { font-family:'SF Mono','Consolas','Courier New',monospace;
      font-size:13px; line-height:1.6; background:#fafbfc;
      border:1px solid #e1e4e8; border-radius:8px;
      padding:20px 24px; white-space:pre; overflow-x:auto; margin:0; }
    .rpt-copy { position:absolute; top:8px; right:8px; padding:4px 12px;
      font-size:12px; border:1px solid #ccc; border-radius:4px;
      background:#fff; cursor:pointer; z-index:10; }
    .rpt-copy:hover { background:#2c3e50; color:#fff; border-color:#2c3e50; }
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
      numericInput("zinsbindung_years", "固定利率期 (年)", 10, step = 1),
      numericInput("refi_rate", "再融资利率 (%)", 4.5, step = 0.1),
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
      numericInput("selling_cost_rate", "卖房成本 (%)", 3, step = 0.5),
      numericInput("vacancy_rate", "空置率 (%)", 0, step = 1),
      numericInput("sondertilgung_rate", "年提前还贷上限 (%)", 5, step = 1),
      numericInput("opcost_inflation", "运营成本年通胀 (%)", 2, step = 0.5),
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
