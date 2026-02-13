# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {

  # 1. 加载核心 Reactives (数据计算)
  source("R/server/reactives.R", local = TRUE)

  # 2. 加载 Observers (事件处理)
  source("R/server/observers.R", local = TRUE)

  # 3. 加载 Outputs (UI渲染)
  source("R/server/outputs.R", local = TRUE)

}
