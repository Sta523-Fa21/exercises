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
        numericInput("beta", "Prior # of tails", min=0, value=5),
        h4("ABC:"),
        numericInput("nsim", "# of prior draws", min=1, value = 10000),
        numericInput("nmin", "Minimum # of posterior draws", min=1, value = 500)
      ),
      mainPanel = mainPanel(
        plotOutput("plot"),
        textOutput("summary")
      )
    )
  ),
  server = function(input, output, session) {
    
    observe({
      updateSliderInput(session, "x", max = input$n)
    }) %>%
      bindEvent(input$n)
    
    d = reactive({
      req(input$alpha, input$beta)
      
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
    
    abc_post = reactive({
      
      prior = rbeta(n = input$nsim, input$alpha, input$beta)
      
      gen_model_sims = rbinom(n = input$nsim, size = input$n, prob = prior)
      
      posterior = prior[gen_model_sims == input$x]
      
      posterior
    })
    
    abc_dens = reactive({
      d_post = density(abc_post())
      
      tibble(
        distribution = "posterior (ABC)",
        p = d_post$x,
        density = d_post$y
      )
    })
    
    output$summary = renderText({
      glue::glue(
        "Ran {input$nsim} generative model simulations and obtained ",
        "{length(abc_post())} posterior samples. ",
        "Efficiency was {100*length(abc_post()) / input$nsim}%."
      )
    })
    
    
    output$plot = renderPlot({
      
      validate(
        need(length(abc_post()) >= input$nmin, "Insufficient posterior draws generated, try increasing # of simulations!")
      )
      
      
      bind_rows(
        d(),
        abc_dens()
      ) %>%
        ggplot(aes(x=p, y=density, color=forcats::as_factor(distribution))) +
          geom_line(size=2) + 
          scale_color_manual(values = c("#7fc97f", "#beaed4", "#dfc086", "#e78ac3")) +
          labs(color = "Distribution")
    })
  }
)