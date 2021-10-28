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
        selectInput("prior", "Prior Model", choices = c("Trunc Normal"="tnorm", "Beta" = "beta"), selected = "beta"),
        uiOutput("prior_params"),
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
    
    output$prior_params = renderUI({
      print("Render parameter UI")
      
      if (input$prior == "beta") {
        list(
          numericInput("alpha", "Prior # of heads", min=0, value=5),
          numericInput("beta", "Prior # of tails", min=0, value=5)
        )
      } else if (input$prior == "tnorm") {
        list(
          numericInput("mean", "Prior mean", min=0, value=0.5),
          numericInput("sd", "Prior std dev", min=0, value=0.1)
        )
      } else {
        
      }
    })
    
    
    abc = reactive({
      
      print("Calculate ABC")
      
      
      if (input$prior == "beta") {
        req(input$alpha, input$beta)
        prior = rbeta(n = input$nsim, input$alpha, input$beta)
      } else if (input$prior == "tnorm") {
        req(input$mean, input$sd)
        prior = truncnorm::rtruncnorm(n = input$nsim, a=0, b=1, mean = input$mean, sd = input$sd)
      } else {
        stop()
      }
      
      gen_model_sims = rbinom(n = input$nsim, size = input$n, prob = prior)
      
      posterior = prior[gen_model_sims == input$x]
      
      list(
        prior = prior,
        posterior = posterior
      )
    })
    
    abc_dens = reactive({
      
      print("Calculate densities")
      
      d_prior = density(abc()$prior)
      d_post = density(abc()$posterior)
      
      bind_rows(
        tibble(
          distribution = "prior",
          p = d_prior$x,
          density = d_prior$y
        ),
        tibble(
          distribution = "likelihood",
          p = seq(0, 1, length.out = 1000)
        ) %>%
          mutate(
            density = dbinom(input$x, size = input$n, prob = p),
            density = density / sum(density / n())
          ) 
        ,
        tibble(
          distribution = "posterior (ABC)",
          p = d_post$x,
          density = d_post$y
        )
      )
    })

    output$summary = renderText({
      print("Create summary")
      
      glue::glue(
        "Ran {input$nsim} generative model simulations and obtained ",
        "{length(abc()$posterior)} posterior samples. ",
        "Efficiency was {100*length(abc()$posterior) / input$nsim}%."
      )
    })

    output$plot = renderPlot({

      print("Create plot")
      
      validate(
        need(length(abc()$posterior) >= input$nmin, "Insufficient posterior draws generated, try increasing # of simulations!")
      )
      
      abc_dens() %>%
        ggplot(aes(x=p, y=density, color=forcats::as_factor(distribution))) +
          geom_line(size=2) +
          scale_color_manual(values = c("#7fc97f", "#beaed4", "#dfc086", "#e78ac3")) +
          labs(color = "Distribution")
    })
  }
)