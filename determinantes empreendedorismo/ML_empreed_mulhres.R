library(tidyverse); library(tidymodels); library(vip)

setwd("~/GitHub/Sebrae-go-mulheres/determinantes empreendedorismo")

# lendo a base e ja transformando valores 

base_mulheres <- readxl::read_excel("base_mulheres.xlsx", 
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

base_mulheres <- base_mulheres %>% 
  mutate(log_emp_mulher = log(tx_emp_mulher),
         log_pibpcapta = log(pib_per_capta),
         log_empregos = log(empregos_formais))

base_mulheres <- base_mulheres %>% 
  filter(log_pibpcapta != -Inf & log_empregos != -Inf)

base_mulheres <- base_mulheres %>% 
  select(mesorregiao,log_emp_mulher, crimes_patrim,crimes_contra_pessoa,escolarid_trab,
         log_pibpcapta, log_empregos,infra_agua,infra_internet, infra_energia,
         infra_esgoto, st_agro, st_ind, st_serv,
         ideb_quinto_ano, ideb_nono_ano, crimes_sex, 
         escolarid_trab, remun_mediana)


base_mulheres %>% 
  GGally::ggpairs()


# split -------------------------------------------------------------------

splits <- initial_split(base_mulheres, prop = 0.7, strata = mesorregiao)

split_treino <- training(splits)
split_teste <- testing(splits)

validacao_cruzada <- vfold_cv(split_treino, v = 5, strata = mesorregiao)

# recipes -----------------------------------------------------------------

recipe_model <- recipe(log_emp_mulher ~ ., data = split_treino) %>%
                step_dummy(all_nominal_predictors()) %>% 
                step_corr(all_numeric(), -all_outcomes(), threshold = 0.80)


preparado <- prep(recipe_model, training = split_treino)

base_pos_recipe <- juice(preparado)

# modelo ------------------------------------------------------------------

dt_model <- decision_tree(
  cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
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

dt_workflow <- workflow() %>% 
  add_model(xgb_model) %>% 
  add_recipe(recipe_model)
  
dt_workflow

# treinando ---------------------------------------------------------------

treinando_modelo <- dt_workflow %>% 
  tune_grid(resamples = validacao_cruzada,
            grid = 10, 
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse,rsq,mae,mape))

metricas_treino <- treinando_modelo %>% 
                      collect_metrics()

treinando_modelo %>% 
  show_best(metric = "rmse")

autoplot(treinando_modelo)

best_grid_modelo <- select_best(treinando_modelo, "rmse")


# finalizando -------------------------------------------------------------

best_workflow <- finalize_workflow(dt_workflow, best_grid_modelo)

best_workflow %>% 
  last_fit(best_workflow, split = splits) %>% 
  unnest(.predictions)
  
final_modelo_xgb <- fit(best_workflow, data = base_mulheres)

best_workflow %>% 
  fit(data = base_mulheres) %>% 
  extract_fit_parsnip() %>% 
  vi() 

vi()



previsoes <- workflow() %>% 
  add_recipe(recipe_model) %>% 
  add_model(final_modelo) %>% 
  last_fit(splits) %>% 
  collect_predictions() 


previsoes %>% 
  select(.row, log_emp_mulher, .pred) %>% 
  ggplot(aes(x = log_emp_mulher, y = .pred)) +
  geom_point() + geom_smooth(method="lm", se = FALSE)
