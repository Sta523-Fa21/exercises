## profvis Demo

n = 1e6
d = tibble(
  x1 = rt(n, df = 3),
  x2 = rt(n, df = 3),
  x3 = rt(n, df = 3),
  x4 = rt(n, df = 3),
  x5 = rt(n, df = 3),
) %>%
  mutate(y = -2*x1 - 1*x2 + 0*x3 + 1*x4 + 2*x5 + rnorm(n))

profvis::profvis(
  lm(y~., data=d)
)

## profvis shiny demo

profvis::profvis({
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
  ) %>%
    print()
})



system.time(sqrt(1:1e7))


system.time(unlist(mclapply(1:10, function(x) rnorm(1e5), mc.cores = 4)))

system.time(unlist(mclapply(1:10, function(x) rnorm(1e5), mc.cores = 5)))

system.time(unlist(mclapply(1:10, function(x) rnorm(1e5), mc.cores = 10)))
            


## furrr


# ?future::plan
future::plan(future::multisession, workers=5)

res = furrr::future_map_dbl(
  1:10,
  function(i) {
    mean(rnorm(1e8))
  },
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
)


## Bootstrap example


set.seed(3212016)
d = data.frame(x = 1:120) %>%
  mutate(y = sin(2*pi*x/120) + runif(length(x),-1,1))
l = loess(y ~ x, data=d)
p = predict(l, se=TRUE)
d = d %>% mutate(
  pred_y = p$fit,
  pred_y_se = p$se.fit
)


n_iter = 5000
res = map_dfr(
  seq_len(n_iter),
  function(i) {
    d %>%
      select(x, y) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      mutate(
        ., 
        i = i,
        bs_pred_y = loess(y~x, data=.) %>% predict()
      )
  }
) %>%
  group_by(x,y) %>%
  summarize(
    bs_low = quantile(bs_pred_y, probs = 0.025),
    bs_upp = quantile(bs_pred_y, probs = 0.975)
  )

ggplot(d, aes(x,y)) +
  geom_point(color="gray50") +
  geom_ribbon(
    aes(ymin = pred_y - 1.96 * pred_y_se, 
        ymax = pred_y + 1.96 * pred_y_se), 
    fill="red", alpha=0.25
  ) +
  geom_line(aes(y=pred_y)) +
  theme_bw() +
  geom_ribbon(
    data = res,
    aes(ymin=bs_low, ymax = bs_upp), color="blue", alpha=0.25
  )



future::plan(future::multisession, workers=8)

n_iter = 10000
res = furrr::future_map_dfr(
  .options = furrr::furrr_options(seed=TRUE),
  .progress = TRUE,
  seq_len(n_iter),
  function(i) {
    d %>%
      select(x, y) %>%
      slice_sample(prop = 1, replace = TRUE) %>%
      mutate(
        ., 
        i = i,
        bs_pred_y = loess(y~x, data=.) %>% predict()
      )
  }
) %>%
  group_by(x,y) %>%
  summarize(
    bs_low = quantile(bs_pred_y, probs = 0.025),
    bs_upp = quantile(bs_pred_y, probs = 0.975)
  )

ggplot(d, aes(x,y)) +
  geom_point(color="gray50") +
  geom_ribbon(
    aes(ymin = pred_y - 1.96 * pred_y_se, 
        ymax = pred_y + 1.96 * pred_y_se), 
    fill="red", alpha=0.25
  ) +
  geom_line(aes(y=pred_y)) +
  theme_bw() +
  geom_ribbon(
    data = res,
    aes(ymin=bs_low, ymax = bs_upp), color="blue", alpha=0.25
  )







