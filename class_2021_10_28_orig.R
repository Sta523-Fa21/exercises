library(shiny)
library(tidyverse)

shinyApp(
  ui = fluidPage(
    titlePanel("Beta-Binomial Visualizer"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        sliderInput("n", "# of flips", min=0, max=100, value=20),
        h4("Prior:"),
        numericInput("alpha", "Prior # of heads", min=0, value=5),
        numericInput("beta", "Prior # of tails", min=0, value=5)
      ),
      mainPanel = mainPanel(
        plotOutput("plot")
      )
    )
  ),
  server = function(input, output, session) {
    
    observe({
      updateSliderInput(session, "x", max = input$n)
    }) %>%
      bindEvent(input$n)
    
    d = reactive({
      tibble(
        p = seq(0, 1, length.out = 1000)
      ) %>%
        mutate(
          prior = dbeta(p, input$alpha, input$beta),
          likelihood = dbinom(input$x, size = input$n, prob = p),
          posterior = dbeta(p, input$alpha + input$x, input$beta + input$n - input$x)
        ) %>%
        pivot_longer(
          cols = -p,
          names_to = "distribution",
          values_to = "density"
        ) %>%
        group_by(distribution) %>%
        mutate(
          density = density / sum(density / n())
        )
    })
    
    output$plot = renderPlot({
      ggplot(d(), aes(x=p, y=density, color=forcats::as_factor(distribution))) +
        geom_line(size=2) + 
        scale_color_manual(values = c("#7fc97f", "#beaed4", "#dfc086", "#e78ac3")) +
        labs(color = "Distribution")
    })
  }
)