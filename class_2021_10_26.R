library(tidyverse)
library(shiny)

pal = c("#7fc97f", "#beaed4", "#dfc086")
pal_names = c("Green", "Purple", "Orange")


shinyApp(
  ui = fluidPage(
    titlePanel("Beta-Binomial conjugate example"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Data:"),
        sliderInput("x", "# of heads", min = 0, max = 100, value = 5),
        sliderInput("n", "# of flips", min = 1, max = 100, value = 10),
        h4("Prior:"),
        numericInput("alpha", "Prior # of heads", min=0, value = 5),
        numericInput("beta", "Prior # of tails", min=0, value = 5),
        h4("Options:"),
        checkboxInput("options", "Show options", value = FALSE),
        conditionalPanel(
          "input.options",
          checkboxInput("bw", "Use theme_bw for plot", value = FALSE),
          checkboxInput("facet", "Use facets", value = FALSE),
          selectInput("prior", "Color for prior", choices = pal_names, selected = pal_names[1]),
          selectInput("likelihood", "Color for likelihood", choices = pal_names, selected = pal_names[2]),
          selectInput("posterior", "Color for posterior", choices = pal_names, selected = pal_names[3])
        )
      ),
      mainPanel = mainPanel(
        plotOutput("plot")
      )  
    )
  ),
  server = function(input, output, session) {
    
    observe({
      choices = c(input$prior, input$likelihood, input$posterior)
      
      if (input$prior == input$likelihood) 
        updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
      
      if (input$prior == input$posterior) 
        updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
      
    }) %>%
      bindEvent(input$prior)
    
    observe({
      choices = c(input$prior, input$likelihood, input$posterior)
      
      if (input$likelihood == input$prior) 
        updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
      
      if (input$likelihood == input$posterior) 
        updateSelectInput(session, "posterior", selected = setdiff(pal_names, choices))
      
    }) %>%
      bindEvent(input$likelihood)
    
    observe({
      choices = c(input$prior, input$likelihood, input$posterior)
      
      if (input$posterior == input$likelihood) 
        updateSelectInput(session, "likelihood", selected = setdiff(pal_names, choices))
      
      if (input$posterior == input$prior) 
        updateSelectInput(session, "prior", selected = setdiff(pal_names, choices))
      
    }) %>%
      bindEvent(input$posterior)
    
    
    observe({
      updateSliderInput(session, inputId = "x", max = input$n)
    }) %>%
      bindEvent(input$n)
    
    d = reactive({
      tibble(
        p = seq(from = 0, to = 1, length.out = 1001)
      ) %>%
        mutate(
          prior = dbeta(p, input$alpha, input$beta),
          likelihood = dbinom(input$x, size = input$n, prob = p),
          posterior = dbeta(p, input$alpha + input$x, input$beta + input$n - input$x)
        ) %>%
        pivot_longer(-p, names_to = "distribution", values_to = "density") %>%
        mutate(
          distribution = as_factor(distribution)
        )
    })
    
    output$plot = renderPlot({
      
      color_choices = c(
        which(pal_names == input$prior),
        which(pal_names == input$likelihood),
        which(pal_names == input$posterior)
      )
      
      color_pal = pal[color_choices]
      
      g = ggplot(d(), aes(x = p, y = density, color = distribution)) +
        geom_line(size=2) +
        scale_color_manual(values = color_pal)
      
      if (input$bw) 
        g = g + theme_bw()
      
      if (input$facet)
        g = g + facet_wrap(~distribution)
      
      g
    })
    
    
  }
)

