library(shiny)
library(liuKoladaMini)

ui <- fluidPage(
  titlePanel("Kolada mini: unemployment rate"),
  sidebarLayout(
    sidebarPanel(
      textInput("muni", "Municipality ID", value = "0580"),   # Linköping
      textInput("kpi",  "KPI ID",         value = "N02282"),  # 外出生失业率 20-64
      sliderInput("yrs", "Years", min = 2000, max = 2025, value = c(2019, 2024), step = 1),
      actionButton("go", "Fetch")
    ),
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("p", height = 300)
    )
  )
)

server <- function(input, output) {
  vals <- eventReactive(input$go, {
    yrs <- seq(input$yrs[1], input$yrs[2])
    kld_values(input$muni, input$kpi, yrs)
  })

  output$summary <- renderPrint({
    req(vals())
    vals()
  })

  output$p <- renderPlot({
    req(vals())
    print(kld_plot(vals()))
  })
}

shinyApp(ui, server)
