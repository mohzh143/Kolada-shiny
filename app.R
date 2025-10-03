library(shiny)
library(liuKoladaMini)

ui <- fluidPage(
  # Dynamic title displayed at the top of the app
  titlePanel(textOutput("dynamic_title")),

  sidebarLayout(
    sidebarPanel(
      # User input for municipality ID (default: Linköping "0580")
      textInput("muni", "Municipality ID", value = "0580"),

      # User input for KPI ID (default: unemployment rate for foreign-born 20–64 "N02282")
      textInput("kpi",  "KPI ID", value = "N02282"),

      # Slider to select the year range
      sliderInput("yrs", "Years", min = 2000, max = 2025,
                  value = c(2019, 2024), step = 1),

      # Button to trigger data fetch (renamed from "Fetch" to "Search")
      actionButton("go", "Search")
    ),

    mainPanel(
      # Print raw data summary
      verbatimTextOutput("summary"),

      # Show plot of the results
      plotOutput("p", height = 300)
    )
  )
)

server <- function(input, output) {

  # Event: when user clicks "Search", fetch KPI values for selected municipality & years
  vals <- eventReactive(input$go, {
    yrs <- seq(input$yrs[1], input$yrs[2])
    kld_values(input$muni, input$kpi, yrs)
  })

  # Event: when user clicks "Search", also fetch readable names for the title
  name_info <- eventReactive(input$go, {
    # Municipality name (fallback to ID if not found)
    muni_name <- tryCatch({
      m <- kld_municipalities()
      nm <- m$title[match(input$muni, m$id)]
      ifelse(is.na(nm), input$muni, nm)
    }, error = function(e) input$muni)

    # KPI name (fallback to ID if not found)
    kpi_name <- tryCatch({
      k <- kld_indicators()
      nm <- k$title[match(input$kpi, k$id)]
      ifelse(is.na(nm), input$kpi, nm)
    }, error = function(e) input$kpi)

    list(muni = muni_name, kpi = kpi_name)
  }, ignoreInit = TRUE)

  # Dynamic title: e.g. "Kolada mini: Unemployment rate (foreign-born, 20–64) — Linköping"
  output$dynamic_title <- renderText({
    n <- name_info()
    if (is.null(n)) {
      "Kolada mini"   # Default placeholder before any search
    } else {
      paste0("Kolada mini: ", n$kpi, " — ", n$muni)
    }
  })

  # Show raw values table in console-like view
  output$summary <- renderPrint({
    req(vals())
    vals()
  })

  # Show plot of the selected KPI
  output$p <- renderPlot({
    req(vals())
    kld_plot(vals())
  })
}

# Run the Shiny app
shinyApp(ui, server)
