# ------------------------
# Version: 1.0.0
# Author: Shuichi Sugiura
# Date: 2025-08-07
# Code Name: Cerisier
# Description:
# This Shiny app randomly assigns stimulus durations (e.g., 30,45,55) in a non-repeating order
# to each participant and outputs the result to a UTF-8 BOM encoded CSV.
# Fully compatible with Excel on Windows/macOS (no garbled characters).
# ------------------------

library(shiny)

# ==== UI ====
ui <- fluidPage(
  titlePanel("実験試行ランダマイザー"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_sub", "被験者数（N）", value = 100, min = 1),
      textInput("durations", "提示時間（カンマ区切り）", value = "30, 45, 55"),
      numericInput("seed", "Seed（任意，空欄可）", value = NA),
      textInput("filename", "CSVファイル名（拡張子不要）", value = "trial_order"),
      actionButton("generate", "ランダム順を生成"),
      downloadButton("download", "CSVダウンロード"),
      hr(),
      helpText(strong("App Info")),
      helpText("Version: 1.0.0"),
      helpText("Author: Shuichi Sugiura"),
      helpText("Code Name: Cerisier"),
      helpText("Date: 2025-08-07")
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

# ==== Server ====
server <- function(input, output) {
  results <- reactiveVal(data.frame())
  
  observeEvent(input$generate, {
    # 入力を数値ベクトルに変換
    dur_vec <- as.numeric(unlist(strsplit(input$durations, ",")))
    
    # エラーチェック：重複や無効な値
    if (any(is.na(dur_vec)) || length(unique(dur_vec)) != length(dur_vec)) {
      showNotification("提示時間はカンマ区切りの異なる数値で入力してください（例: 30,45,55）", type = "error")
      return()
    }
    
    # Seed設定（再現性のため）
    if (!is.na(input$seed)) {
      set.seed(input$seed)
    }
    
    # ランダム順生成
    orders <- replicate(input$n_sub, sample(dur_vec), simplify = FALSE)
    
    df <- data.frame(ID = 1:input$n_sub)
    for (i in seq_along(dur_vec)) {
      df[[paste0("Trial", i)]] <- sapply(orders, `[`, i)
    }
    
    results(df)
  })
  
  # 表示出力
  output$table <- renderTable({
    results()
  })
  
  # CSVダウンロード処理（UTF-8 BOM）
  output$download <- downloadHandler(
    filename = function() {
      fname <- trimws(input$filename)
      if (nchar(fname) == 0) fname <- paste0("output_", Sys.Date())
      paste0(fname, ".csv")
    },
    content = function(file) {
      # BOMを書き込み
      writeBin(charToRaw('\ufeff'), file)
      # CSV追記（BOMを保持）
      write.table(results(), file, sep = ",", row.names = FALSE, fileEncoding = "UTF-8", append = TRUE, col.names = TRUE)
    }
  )
}

# ==== Launch App ====
shinyApp(ui = ui, server = server)
