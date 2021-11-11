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

## EDA

hotels %>%
  count(children) %>%
  mutate(prop = n / sum(n))


### Test train split

set.seed(123)
splits = initial_split(hotels, strata = children)


hotel_train = training(splits)
hotel_test = testing(splits)


hotel_train %>%
  count(children) %>%
  mutate(prop = n / sum(n))

hotel_test %>%
  count(children) %>%
  mutate(prop = n / sum(n))

### Validation set

val_set = validation_split(hotel_train, strata = children, prop = 0.8)
val_set



## Logistic Regression

show_engines("logistic_reg")

lr_model = logistic_reg() %>%
  set_engine("glm")

lr_model %>%
  translate()

### Recipe 

lr_recipe = recipe(children ~ ., data = hotel_train) %>%
  step_date(arrival_date) %>%
  step_rm(arrival_date) %>%
  step_rm(country) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

lr_recipe %>%
  prep() %>%
  bake(new_data = hotel_train) %>%
  #select(starts_with("arrival")) %>%
  dim()

### Workflow

lr_workflow = workflow() %>%
  add_recipe(lr_recipe) %>%
  add_model(lr_model)

### Fit model

lr_fit = lr_workflow %>%
  fit(data = hotel_train)


lr_fit %>% 
  tidy() %>% 
  View()


lr_fit_val = lr_workflow %>%
  fit_resamples(resamples = val_set)

### Model performance

lr_fit %>%
  augment(new_data = hotel_train) %>%
  #select(starts_with(".pred")) %>%
  #roc_auc(children, .pred_children)
  roc_curve(children, .pred_children) %>%
  autoplot()

lr_fit %>%
  augment(new_data = hotel_train) %>%
  accuracy(children, .pred_class)


lr_fit_val %>%
  collect_metrics()



## Lasso model

lasso_model = logistic_reg(mixture = 1, penalty = tune()) %>%
  set_engine("glmnet")


#lasso_model %>% translate()

lasso_recipe = recipe(children ~ ., data = hotel_train) %>%
  step_date(arrival_date) %>%
  step_rm(arrival_date) %>%
  step_rm(country) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

lasso_recipe %>%
  prep() %>%
  bake(new_data = hotel_train) %>%
  select(starts_with("arrival")) %>%
  View()

lasso_workflow = workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_model)



#lasso_workflow %>% # bad idea
#  fit(hotel_train)

lasso_res = lasso_workflow %>%
  tune_grid(
    val_set,
    grid = tibble(
      penalty = 10^seq(-4, -1, length.out = 30)
    ),
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc)
  )

lasso_res


lasso_res %>% 
  collect_metrics() %>%
  ggplot(aes(x=penalty, y=mean)) + 
    geom_point() +
    geom_path() +
    scale_x_log10()


lasso_res %>% 
  collect_metrics() %>%
  arrange(desc(mean))

lasso_res %>%
  show_best()

lasso_best = lasso_res %>% select_best()


lasso_res %>%
  collect_predictions(parameters = lasso_best) %>%
  roc_curve(children, .pred_children) %>%
  autoplot()


## Combining Results

bind_rows(
  lasso_res %>%
    collect_predictions(parameters = lasso_best) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = "Lasso"),
  
  lr_fit %>%
    augment(new_data = hotel_train) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = "glm")
) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
    geom_path()




## Random Forest

rf_model = rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% 
  set_engine("ranger", num.threads = 4) %>% 
  set_mode("classification")

rf_recipe = recipe(children ~ ., data = hotel_train) %>% 
  step_date(arrival_date) %>% 
  step_rm(arrival_date) %>%
  step_rm(country)

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


autoplot(rf_res)

collect_metrics(rf_res)
show_best(rf_res)


### compare all models

bind_rows(
  lasso_res %>%
    collect_predictions(parameters = lasso_best) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = "Lasso"),
  
  lr_fit %>%
    augment(new_data = hotel_train) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = "glm"),
  
  rf_res %>%
    collect_predictions(parameters = select_best(rf_res)) %>%
    roc_curve(children, .pred_children) %>%
    mutate(model = "random forest")
) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_path()

