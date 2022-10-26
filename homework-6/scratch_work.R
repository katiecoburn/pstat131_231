library(tidyverse)
library(tidymodels)
library(janitor)
library(vip)
library(rpart)
library(rpart.plot)
library(ranger)

pokemon <- read_csv("homework-6/data/Pokemon.csv") %>% 
  clean_names() %>% 
  filter(type_1 %in% c('Bug', 'Fire', 'Grass', 'Normal', 'Water', 'Psychic')) %>% 
  mutate(legendary = factor(legendary),
         type_1 = factor(type_1))



set.seed(3435)
pokemon_split <- initial_split(pokemon, strata = "type_1")

pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)

pokemon_fold <- vfold_cv(pokemon_train, v = 5, strata = "type_1")

pokemon_recipe <- recipe(type_1 ~ legendary + generation + hp + attack +
                           defense + sp_atk + sp_def + speed,
                         data = pokemon_train) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

tree_spec <- decision_tree() %>%
  set_engine("rpart")
class_tree_spec <- tree_spec %>%
  set_mode("classification") %>% 
  set_args(cost_complexity = tune())
class_tree_wf <- workflow() %>% 
  add_model(class_tree_spec) %>% 
  add_recipe(pokemon_recipe)

param_grid <- grid_regular(cost_complexity(range = c(-3, -1)), levels = 10)

tune_res <- tune_grid(
  class_tree_wf, 
  resamples = pokemon_fold, 
  grid = param_grid, 
  metrics = metric_set(roc_auc)
)

write_rds(tune_res, "homework-6/model-results/decision_tree_tuning.rds")

autoplot(tune_res)

collect_metrics(tune_res) %>% 
  arrange(-mean)
best_complexity <- select_best(tune_res)

class_tree_final <- finalize_workflow(class_tree_wf, best_complexity)

class_tree_final_fit <- fit(class_tree_final, data = pokemon_train)
class_tree_final_fit %>%
  extract_fit_engine() %>%
  rpart.plot(roundint = F)

predicted_data <- augment(class_tree_final_fit, new_data = pokemon_test) %>% 
  select(type_1, starts_with(".pred"))

predicted_data %>% roc_auc(type_1, .pred_Bug:.pred_Water)

predicted_data %>% roc_curve(type_1, .pred_Bug:.pred_Water) %>% 
  autoplot()

predicted_data %>% 
  conf_mat(truth = type_1, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

rf_spec <- rand_forest(mtry = tune(), 
                       trees = tune(),
                       min_n = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

class_forest_rf <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(pokemon_recipe)

param_grid <- grid_regular(mtry(c(1, 8)), 
                           trees(c(10,100)),
                           min_n(c(10, 100)),
                           levels = 5)

tune_res <- tune_grid(
  class_forest_rf, 
  resamples = pokemon_fold, 
  grid = param_grid, 
  metrics = metric_set(roc_auc)
)

save(tune_res, 'homework-6/rf.RData')

autoplot(tune_res)

best_forest <- select_best(tune_res)

class_tree_final <- finalize_workflow(class_forest_rf, best_forest)

class_tree_final_fit <- fit(class_tree_final, data = pokemon_train)
class_tree_final_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)


predicted_data <- augment(class_tree_final_fit, new_data = pokemon_test) %>% 
  select(type_1, starts_with(".pred"))

predicted_data %>% roc_auc(type_1, .pred_Bug:.pred_Water)

predicted_data %>% roc_curve(type_1, .pred_Bug:.pred_Water) %>% 
  autoplot()

predicted_data %>% 
  conf_mat(truth = type_1, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

pokemon_train %>% 
  ggplot(aes(x = generation, fill = type_1)) +
  geom_bar(position = "dodge")
