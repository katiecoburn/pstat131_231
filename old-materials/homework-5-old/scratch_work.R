library(tidyverse)
library(tidymodels)
library(janitor)

pokemon <- read_csv("homework-5/data/Pokemon.csv") %>% 
  clean_names()

pokemon %>% 
  ggplot(aes(x = type_1)) +
  geom_bar()

pokemon <- pokemon %>% 
  filter(type_1 %in% c('Bug', 'Fire', 'Grass', 'Normal', 'Water', 'Psychic'))

pokemon <- pokemon %>% 
  mutate(type_1 = factor(type_1), legendary = factor(legendary))

pokemon_split <- initial_split(pokemon, prop = 0.7, strata = type_1)
pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)

pokemon_folds <- vfold_cv(data = pokemon_train, v = 5, strata = type_1)

pokemon_recipe <- recipe(type_1 ~ legendary + generation + 
                           sp_atk + attack + speed + defense + 
                           hp + sp_def, data = pokemon_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

elastic_net_spec <- multinom_reg(penalty = tune(), 
                                 mixture = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("glmnet")

en_workflow <- workflow() %>% 
  add_recipe(pokemon_recipe) %>% 
  add_model(elastic_net_spec)

en_grid <- grid_regular(penalty(range = c(-5, 5)), 
                        mixture(range = c(0, 1)), levels = 10)
en_grid

tune_res <- tune_grid(
  en_workflow,
  resamples = pokemon_folds, 
  grid = en_grid
)

tune_res

collect_metrics(tune_res)
autoplot(tune_res)

best_model <- select_best(tune_res, metric = "roc_auc")

en_final <- finalize_workflow(en_workflow, best_model)

en_final_fit <- fit(en_final, data = pokemon_train)

predicted_data <- augment(en_final_fit, new_data = pokemon_test) %>% 
  select(type_1, starts_with(".pred"))

predicted_data %>% roc_auc(type_1, .pred_Bug:.pred_Water)

predicted_data %>% roc_curve(type_1, .pred_Bug:.pred_Water) %>% 
  autoplot()

predicted_data %>% 
  conf_mat(truth = type_1, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
