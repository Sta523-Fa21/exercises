library(tidyverse)
library(tidymodels)


## Data 

#  Original data from Antonio, Almeida, and Nunes (2019) 
#  - https://doi.org/10.1016/j.dib.2018.11.126
#  - Data dictionary - https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-02-11#data-dictionary

hotels = read_csv(
  'https://tidymodels.org/start/case-study/hotels.csv'
) %>%
  mutate(
    across(where(is.character), as.factor)
  )







































## Random Forest

rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% 
  set_engine("ranger", num.threads = 4) %>% 
  set_mode("classification")

rf_recipe = recipe(children ~ ., data = hotel_train) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date)

rf_workflow = workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(rf_recipe)

rf_model %>%
  parameters()

rf_res = rf_workflow %>% 
  tune_grid(
    val_set,
    grid = 25,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )
