# app.R

# Check and load required packages
pkgs <- c("shiny", "plotly", "readxl", "dplyr", "stringr")
missing_pkgs <- pkgs[!pkgs %in% installed.packages()]
if (length(missing_pkgs)) install.packages(missing_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Load data ---
data_url <- "https://raw.githubusercontent.com/GCMusto/eba-risk-dashboard/main/data/Data_Annex_InteractiveRiskDashboard_Q2_2024.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
download.file(data_url, temp_file, mode = "wb")
df <- read_excel(temp_file, sheet = "KRIs by country and EU")

# Clean column names
names(df) <- str_remove_all(names(df), "\\[|\\]")
names(df) <- str_trim(names(df))

# Parse Period as Year and Quarter
df <- df %>%
  mutate(
    Period = as.character(Period),
    Year = substr(Period, 1, 4),
    Quarter = paste0("Q", as.integer(substr(Period, 5, 6))/3),
    PeriodLabel = paste0(Year, "-", Quarter),
    Ratio = as.numeric(Ratio)
  ) %>%
  arrange(Year, Quarter)

# --- UI ---
ui <- fluidPage(
  titlePanel("📊 EBA Risk Dashboard – Key risk indicators by country"),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicator", "Select indicator:",
                  choices = sort(unique(df$Name))),
      selectInput("period", "Select period:",
                  choices = unique(df$PeriodLabel)),
      width = 3
    ),
    mainPanel(
      plotlyOutput("barPlot", height = "600px"),
      br(),
      textOutput("caption")
    )
  )
)

# --- SERVER ---
server <- function(input, output) {
  filtered <- reactive({
    df %>%
      filter(Name == input$indicator, PeriodLabel == input$period)
  })

  output$barPlot <- renderPlotly({
    plot_df <- filtered()
    validate(need(nrow(plot_df) > 0, "No data found for this combination."))

    p <- ggplot(plot_df, aes(x = reorder(Country, Ratio), y = Ratio,
                             text = paste0(Country, ": ", scales::percent(Ratio, 0.1)))) +
      geom_col(fill = "#2E86AB") +
      coord_flip() +
      labs(x = NULL, y = "Ratio", title = input$indicator) +
      theme_minimal(base_size = 14)
    ggplotly(p, tooltip = "text")
  })

  output$caption <- renderText({
    paste0("Data source: EBA Risk Dashboard (", input$period,
           "). Indicator: ", input$indicator, ".")
  })
}

# --- RUN APP ---
shinyApp(ui, server)

