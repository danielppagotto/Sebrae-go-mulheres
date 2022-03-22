library(tidyverse); library(tidymodels); library(vip)

setwd("~/GitHub/Sebrae-go-mulheres/determinantes empreendedorismo")

# lendo a base e ja transformando valores 

base_mulheres_b <- readxl::read_excel("base_mulheres.xlsx", 
                                    col_types = c("text", "numeric", "text", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric")) %>% janitor::clean_names()


base_mulheres[is.na(base_mulheres)] <- 0

base_mulheres <- base_mulheres_b %>% 
  mutate(log_emp_mulher = log(tx_emp_mulher),
         log_pibpcapta = log(pib_per_capta),
         log_empregos = log(empregos_formais))

base_mulheres <- base_mulheres %>% 
  filter(log_pibpcapta != -Inf & log_empregos != -Inf)

base_mulheres <- base_mulheres %>% 
  select(mesorregiao,log_emp_mulher, crimes_patrim,crimes_contra_pessoa,
         escolarid_trab, pib_per_capta, empregos_formais,infra_agua,infra_internet, 
         infra_energia, infra_esgoto, st_agro, st_ind, st_serv, ideb_quinto_ano, 
         ideb_nono_ano, crimes_sex, escolarid_trab, remun_mediana)


# base_mulheres %>% 
#   GGally::ggpairs()


# split -------------------------------------------------------------------

splits <- initial_split(base_mulheres, prop = 0.7, strata = mesorregiao)

split_treino <- training(splits)
split_teste <- testing(splits)

validacao_cruzada <- vfold_cv(split_treino, v = 10, strata = mesorregiao)

# recipes -----------------------------------------------------------------

recipe_model <- recipe(log_emp_mulher ~ ., data = split_treino) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.80) %>% 
  step_normalize(all_numeric(), -all_predictors())


preparado <- prep(recipe_model, training = split_treino)

base_pos_recipe <- juice(preparado)

modelo1 <- lm(log_emp_mulher ~ ., data = base_pos_recipe)
summary(modelo1)

# modelos ------------------------------------------------------------------

reg_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

svm_model <- svm_linear(cost = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("regression")

dt_model <- decision_tree(
  cost_complexity = tune(), min_n = tune()) %>%
  set_engine("rpart") %>%
  set_mode("regression")
 
rf_model <- rand_forest(
  mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

nn_model <- mlp(
  hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet") %>% 
  set_mode("regression")

xgb_model <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

wf <- workflow_set(
  preproc = list(rp <- recipe_model),
  models = list(xgboost = xgb_model, rand_forest = rf_model, neural_network = nn_model, 
                decision_tree = dt_model, svm = svm_model, reg_linear = reg_model))

# treinando ---------------------------------------------------------------

grid_ctrl <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)


grid_results <- wf %>% 
  workflow_map(resamples = validacao_cruzada,
               grid = 15, 
               control = grid_ctrl)


metricas_treino <- treinando_modelo %>% 
  collect_metrics()

treinando_modelo %>% 
  show_best(metric = "rmse")

autoplot(grid_results)

best_grid_modelo <- select_best(treinando_modelo, "rmse")

# Eu queria inspecionar as colunas predict e a coluna real. Como eu consigo fazer isso?
# sei que o best esta dentro do objeto treinando_modelo, mas gostaria de descobrir qual e

# avaliando no teste -------------------------------------------------------------

best_workflow <- finalize_workflow(empreend_wf, best_grid_modelo)

final_reg_model <- empreend_model %>% 
  finalize_model(best_grid_modelo)

best_workflow %>% 
  last_fit(best_workflow, split = splits) %>% 
  unnest(.predictions)  %>% 
  yardstick::rmse(.pred, log_emp_mulher)

best_workflow %>% 
  last_fit(best_workflow, split = splits) %>% 
  unnest(.predictions) %>% 
  ggplot(aes(x = .pred, y = log_emp_mulher)) + geom_point()


# como eu consigo inspecionar .predict e coluna real no modelo final?

final_modelo <- fit(best_workflow, data = base_mulheres)

best_workflow %>% 
  fit(data = base_mulheres) %>% 
  extract_fit_parsnip() %>% 
  vip(geom = 'col') 

best_workflow %>% 
  fit(data = base_mulheres) %>% 
  extract_fit_parsnip() %>% 
  vi %>% 
  ggplot(aes(x = fct_reorder(Variable, Importance), y = Importance, fill = Sign)) + geom_col() + coord_flip() +
  theme_minimal() + xlab("Importance")

previsoes <- workflow() %>% 
  add_recipe(recipe_model) %>% 
  add_model(final_reg_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() 

# nao teria que fazer com a base completa?

previsoes %>% 
  select(.row, log_emp_mulher, .pred) %>% 
  ggplot(aes(x = log_emp_mulher, y = .pred)) +
  geom_point() + geom_smooth(method="lm", se = FALSE)

