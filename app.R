# app.R

# Check and load required packages
pkgs <- c("shiny", "plotly", "readxl", "dplyr", "stringr", "bslib", "countrycode")
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
    Ratio = as.numeric(Ratio),
    CountryName = countrycode(Country, origin="iso2c", destination = "country.name", custom_match = c(EU = "European Union"), warn = TRUE)
  ) %>%
  arrange(desc(Year), desc(Quarter))

# --- UI ---
ui <- fluidPage(
  title = "EBA Risk Dashboard",
  theme = bs_theme(bootswatch = "flatly", primary = "#2E86AB", bg = "white", fg = "#222"),
  tags$style(HTML("
    .well {
      background-color: #FDFDFD !important;
      border: 1px solid #E1E1E1 !important;
      box-shadow: none !important;
    }
  ")),
  titlePanel(div(
    h3("📊 EBA Risk Dashboard", style = "margin-bottom:0"),
    h5("Interactive key risk indicators by country", style = "color:gray")
  )),

  div(
    style = "margin-top: 25px",
    tabsetPanel(
      tabPanel("Overview by country",
	sidebarLayout(
	  sidebarPanel(
	    selectInput("indicator", "Select indicator:",
			choices = sort(unique(df$Name))),
	    selectInput("period", "Select period:",
			choices = unique(df$PeriodLabel)),
	    width = 3
	  ),
	  mainPanel(
	    uiOutput("summaryBox"),
	    plotlyOutput("barPlot", height = "600px"),
	    br(),
	    textOutput("caption")
	  )
	)
      ),

      tabPanel("Trends over time",
	sidebarLayout(
	  sidebarPanel(
	    selectInput("indicatorTrend", "Select indicator:",
			choices = sort(unique(df$Name))),
	    selectInput("country", "Select country (one or more):",
			choices = sort(unique(df$CountryName)),
			multiple = TRUE,
			selected = "Italy"),
	    width = 3
	  ),
	  mainPanel(
	    plotlyOutput("trendPlot", height = "600px"),
	    br(),
	    textOutput("trendCaption")
	  )
	)
      )
    )
  )
)

# --- SERVER ---
server <- function(input, output, session) {

  # --- Overview tab ---
  filtered_overview <- reactive({
    df %>%
      filter(Name == input$indicator, PeriodLabel == input$period)
  })

  output$summaryBox <- renderUI({
    plot_df <- filtered_overview()
    div(
      style = "background:#f8f9fa; padding:10px; border-radius:8px; margin-bottom:10px;",
      strong("Summary: "),
      paste0("Mean = ", scales::percent(mean(plot_df$Ratio, na.rm = TRUE), 0.1),
             " | Max = ", scales::percent(max(plot_df$Ratio, na.rm = TRUE), 0.1),
             " (", plot_df$Country[which.max(plot_df$Ratio)], ")",
             " | Min = ", scales::percent(min(plot_df$Ratio, na.rm = TRUE), 0.1),
             " (", plot_df$CountryName[which.min(plot_df$Ratio)], ")")
    )
  })

  output$barPlot <- renderPlotly({
    plot_df <- filtered_overview()
    validate(need(nrow(plot_df) > 0, "No data found for this combination."))

    p <- ggplot(plot_df, aes(x = reorder(CountryName, Ratio), y = Ratio,
                             fill = ifelse(CountryName == "European Union", "EU", "Other"),
                             text = paste0(CountryName, ": ", scales::percent(Ratio, 0.1)))) +
      geom_col() +
      scale_fill_manual(values = c("EU" = "#F39C12", "Other" = "#2E86AB"), guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "Ratio", title = input$indicator) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  output$caption <- renderText({
    paste0("Data source: EBA Risk Dashboard (", input$period, 
           "). Indicator: ", input$indicator, ".")
  })

  # --- Trends tab ---
  filtered_trend <- reactive({
    df %>%
      filter(Name == input$indicatorTrend, CountryName %in% input$country)
  })

  output$trendPlot <- renderPlotly({
    plot_df <- filtered_trend()
    validate(need(nrow(plot_df) > 0, "No data found for this combination."))

    # Ensure chronological order of quarters
    plot_df <- plot_df %>%
      mutate(PeriodLabel = factor(PeriodLabel, levels = unique(df$PeriodLabel)))

    p <- ggplot(plot_df, aes(x = PeriodLabel, y = Ratio, color = CountryName, group = CountryName,
                             text = paste0("Country: ", CountryName,
					   "<br>Period: ", PeriodLabel,
					   "<br>Ratio: ", scales::percent(Ratio, 0.1)))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.5) +
      labs(x = "Period", y = "Ratio",
           title = input$indicatorTrend) +
      theme_minimal(base_size = 10) +
      scale_color_brewer(palette = "Pastel2") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  output$trendCaption <- renderText({
    country_list <- paste0(input$country, collapse = ", ")
    paste0("Trend of ", input$indicatorTrend, " for ", country_list, 
           " across available quarters in the EBA Risk Dashboard.")
  })
}

# --- RUN APP ---
shinyApp(ui, server)
