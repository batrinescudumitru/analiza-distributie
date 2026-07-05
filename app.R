library(shiny)
library(ggplot2)
library(moments)

# ========== UI ==========
ui <- fluidPage(
  titlePanel("🔬 Analiza repartiției pentru date numerice"),

  tags$head(tags$style(HTML("
    .btn-ai { background-color: #10a37f; color: white; border: none; }
    .btn-ai:hover { background-color: #0d8a6b; color: white; }
    #ai_result { background: #f0f9f5; border-left: 4px solid #10a37f;
                 padding: 12px; border-radius: 4px; margin-top: 10px; }
  "))),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      fileInput("file", "📂 Încarcă fișier CSV",
                accept = c(".csv", "text/csv")),

      tags$small(tags$em("Fișierul trebuie să fie CSV (virgulă sau punct-virgulă ca separator)")),

      hr(),

      uiOutput("column_selector"),

      hr(),
      numericInput("bins", "Număr bin-uri histogramă:",
                   value = 30, min = 5, max = 100),

      actionButton("analyze", "🚀 Analizează distribuția",
                   class = "btn-primary", width = "100%"),

      hr(),

      uiOutput("ai_button_ui")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("📊 Vizualizări",
                 plotOutput("histogram_plot", height = "500px"),
                 plotOutput("qq_plot", height = "400px")
        ),
        tabPanel("📈 Teste statistice",
                 verbatimTextOutput("test_results")
        ),
        tabPanel("📋 Distribuții sugerate",
                 verbatimTextOutput("distribution_suggestions")
        ),
        tabPanel("🤖 Interpretare AI",
                 uiOutput("ai_section")
        )
      )
    )
  )
)

# ========== SERVER ==========
server <- function(input, output, session) {

  analysis_done <- reactiveVal(FALSE)
  stats_text    <- reactiveVal("")

  # Citire date CSV
  data <- reactive({
    req(input$file)
    tryCatch({
      # încearcă mai întâi cu virgulă, apoi cu punct-virgulă
      df <- tryCatch(
        read.csv(input$file$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      if (is.null(df) || ncol(df) <= 1) {
        df <- read.csv2(input$file$datapath, stringsAsFactors = FALSE)
      }
      df
    }, error = function(e) {
      showNotification(paste("Eroare la citirea fișierului:", e$message), type = "error")
      NULL
    })
  })

  # Selector coloană
  output$column_selector <- renderUI({
    req(data())
    df <- data()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(numeric_cols) == 0) {
      return(p("⚠️ Nu există coloane numerice în acest fișier."))
    }
    selectInput("column", "📌 Selectează coloana numerică:",
                choices = numeric_cols)
  })

  # Obține valorile
  get_values <- reactive({
    req(data(), input$column)
    df <- data()
    values <- df[[input$column]]
    values[!is.na(values)]
  })

  # Buton AI (apare doar după analiză)
  output$ai_button_ui <- renderUI({
    req(analysis_done())
    actionButton("open_chatgpt", "🤖 Interpretează cu ChatGPT",
                 class = "btn-ai", width = "100%",
                 onclick = sprintf(
                   "window.open('https://chatgpt.com/?q=%s', '_blank')",
                   URLencode(stats_text(), reserved = TRUE)
                 ))
  })

  # Secțiunea AI din tab
  output$ai_section <- renderUI({
    if (!analysis_done()) {
      return(p("⚠️ Rulează mai întâi analiza (butonul 🚀), apoi apasă butonul de interpretare AI."))
    }
    tagList(
      p("Apasă butonul din bara laterală pentru a deschide ChatGPT cu rezultatele tale pre-completate."),
      p("ChatGPT va primi automat statisticile și îți va oferi o interpretare detaliată."),
      tags$hr(),
      p(tags$strong("Prompt care va fi trimis:")),
      tags$pre(style = "white-space: pre-wrap; font-size: 12px;",
               stats_text())
    )
  })

  # Analiză la click
  observeEvent(input$analyze, {
    values <- get_values()
    req(length(values) >= 3)

    n    <- length(values)
    m    <- mean(values)
    med  <- median(values)
    s    <- sd(values)
    skew <- skewness(values)
    kurt <- kurtosis(values)
    mn   <- min(values)
    mx   <- max(values)

    # Histogramă
    output$histogram_plot <- renderPlot({
      df_plot <- data.frame(x = values)
      ggplot(df_plot, aes(x = x)) +
        geom_histogram(aes(y = after_stat(density)),
                       bins = input$bins,
                       fill = "steelblue", color = "white", alpha = 0.7) +
        geom_density(color = "red", linewidth = 1.2) +
        labs(title = paste("Histogramă -", input$column),
             x = input$column, y = "Densitate") +
        theme_minimal()
    })

    # Q-Q Plot
    output$qq_plot <- renderPlot({
      df_plot <- data.frame(sample = values)
      ggplot(df_plot, aes(sample = sample)) +
        stat_qq(color = "steelblue", size = 2) +
        stat_qq_line(color = "red", linewidth = 1.2) +
        labs(title = paste("Q-Q Plot -", input$column),
             x = "Cuantile teoretice", y = "Cuantile observate") +
        theme_minimal()
    })

    # Test Shapiro-Wilk
    sw_text <- ""
    if (n >= 3 && n <= 5000) {
      sw <- shapiro.test(values)
      sw_text <- sprintf(
        "\nTestul Shapiro-Wilk:\n  W = %.4f\n  p-value = %.4f\n  Concluzie: %s",
        sw$statistic, sw$p.value,
        ifelse(sw$p.value > 0.05, "Normal (p > 0.05)", "Non-normal (p <= 0.05)")
      )
    }

    # Test KS
    ks <- ks.test(values, "pnorm", mean = m, sd = s)
    ks_text <- sprintf(
      "\nTestul Kolmogorov-Smirnov:\n  D = %.4f\n  p-value = %.4f\n  Concluzie: %s",
      ks$statistic, ks$p.value,
      ifelse(ks$p.value > 0.05, "Normal (p > 0.05)", "Non-normal (p <= 0.05)")
    )

    # Distribuții sugerate
    dist_list <- c()
    if (abs(skew) < 0.5 && abs(kurt - 3) < 0.5) dist_list <- c(dist_list, "NORMALĂ")
    if (skew > 0.5 && all(values > 0))           dist_list <- c(dist_list, "LOG-NORMALĂ")
    if (abs(skew - 2) < 0.8 && all(values > 0))  dist_list <- c(dist_list, "EXPONENȚIALĂ")
    if (skew > 0.3 && skew < 2 && all(values > 0)) dist_list <- c(dist_list, "GAMMA")
    if (all(values >= 0 & values <= 1))           dist_list <- c(dist_list, "BETA")
    if (abs(skew) < 0.1 && abs(kurt - 1.8) < 0.2) dist_list <- c(dist_list, "UNIFORMĂ")
    if (length(dist_list) == 0) dist_list <- "Niciuna potrivită exact — analiză suplimentară necesară"

    # Text teste statistice
    output$test_results <- renderPrint({
      cat("========== STATISTICI DESCRIPTIVE ==========\n")
      cat("  Număr observații:", n, "\n")
      cat("  Medie:", round(m, 4), "\n")
      cat("  Mediană:", round(med, 4), "\n")
      cat("  Abatere standard:", round(s, 4), "\n")
      cat("  Skewness:", round(skew, 4), "\n")
      cat("  Kurtosis:", round(kurt, 4), "\n")
      cat("  Min:", round(mn, 4), "\n")
      cat("  Max:", round(mx, 4), "\n\n")
      cat("========== TESTE DE NORMALITATE ==========")
      cat(sw_text)
      cat(ks_text, "\n")
    })

    output$distribution_suggestions <- renderPrint({
      cat("========== DISTRIBUȚII SUGERATE ==========\n")
      for (d in dist_list) cat("✅", d, "\n")
      cat("\nNOTĂ: Verificați și tabelul de teste statistice.\n")
    })

    # Prompt pentru ChatGPT
    prompt <- paste0(
      "Sunt student și am rulat o analiză statistică a distribuției unui set de date ",
      "în R. Te rog să interpretezi rezultatele de mai jos în termeni simpli și să îmi ",
      "explici ce distribuție au datele și ce înseamnă asta practic.\n\n",
      "COLOANA ANALIZATĂ: ", input$column, "\n\n",
      "STATISTICI DESCRIPTIVE:\n",
      "  N = ", n, "\n",
      "  Medie = ", round(m, 4), "\n",
      "  Mediană = ", round(med, 4), "\n",
      "  Abatere standard = ", round(s, 4), "\n",
      "  Skewness = ", round(skew, 4), "\n",
      "  Kurtosis = ", round(kurt, 4), "\n",
      "  Min = ", round(mn, 4), ", Max = ", round(mx, 4), "\n\n",
      "TESTE DE NORMALITATE:\n",
      sw_text, "\n",
      ks_text, "\n\n",
      "DISTRIBUȚII SUGERATE DE PROGRAM: ", paste(dist_list, collapse = ", "), "\n\n",
      "Te rog:\n",
      "1. Explică ce înseamnă valorile skewness și kurtosis obținute\n",
      "2. Interpretează rezultatele testelor de normalitate\n",
      "3. Confirmă sau infirmă distribuțiile sugerate\n",
      "4. Recomandă ce metode statistice sunt potrivite pentru aceste date"
    )

    stats_text(prompt)
    analysis_done(TRUE)

    showNotification("✅ Analiză completă! Poți acum să interpretezi cu AI.", type = "message")
  })
}

# ========== RULEAZĂ ==========
if (interactive()) {
  shinyApp(ui, server)
}
