library(tidyverse); library(tidymodels); library(vip); library(SHAPforxgboost); 
library(modelStudio)

setwd("~/GitHub/Sebrae-go-mulheres/determinantes empreendedorismo")

# lendo a base e ja transformando valores 

base_mulheres <- readxl::read_excel("base_mulheres.xlsx", 
                                    col_types = c("text", "text", "text", 
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
                                                  "numeric", "text")) %>% janitor::clean_names()


munic_2018 <- read_delim("munic_2018.csv", 
                         ";", escape_double = FALSE, col_types = cols(cod_ibge = col_character()), 
                         locale = locale(encoding = "ISO-8859-1", 
                                         asciify = TRUE), trim_ws = TRUE) 

base_mulheres[is.na(base_mulheres)] <- 0

base_mulheres <- base_mulheres %>% 
  mutate(log_emp_mulher = log(tx_emp_mulher),
         log_pibpcapta = log(pib_per_capta),
         log_empregos = log(empregos_formais)) %>% 
  left_join(munic_2018, by = "cod_ibge")

base_mulheres <- base_mulheres %>% 
  filter(log_pibpcapta != -Inf & log_empregos != -Inf)

base_mulheres <- base_mulheres %>% 
  dplyr::select(mesorregiao,log_emp_mulher, crimes_patrim,crimes_contra_pessoa,escolarid_trab,
         pib_per_capta, empregos_formais,infra_agua, infra_esgoto, infra_internet, infra_energia,
         infra_esgoto, st_agro, st_ind, st_serv, trafico, contrav_penais, eq_orc_mun, 
         rec_proprios, ideb_quinto_ano, ideb_nono_ano, crimes_sex, 
         escolarid_trab, remun_mediana, sebrae,  msau39, starts_with("mppm"),
         idm_geral, idm_educ, idm_eco, evol_pib, pib_per_capta, infra_escolas) 

writexl::write_xlsx(base_mulheres, "base_mulheres.xlsx")

base_mulheres %>% 
  group_by(mesorregiao) %>% 
  summarise(taxa = mean(log_emp_mulher))


base_mulheres %>% 
  dplyr::select(log_emp_mulher, crimes_patrim, crimes_contra_pessoa, st_serv) %>%  
  GGally::ggpairs()


# split -------------------------------------------------------------------
set.seed(123)

splits <- initial_split(base_mulheres, prop = 0.7, strata = mesorregiao)

split_treino <- training(splits)
split_teste <- testing(splits)

validacao_cruzada <- vfold_cv(split_treino, v = 10, strata = mesorregiao)

# recipes -----------------------------------------------------------------

recipe_model <- recipe(log_emp_mulher ~ ., data = base_mulheres) %>%
                step_dummy(all_nominal_predictors()) %>% 
                step_corr(all_numeric(), -all_outcomes(), threshold = 0.80) %>% 
                step_scale(all_numeric(), -all_outcomes()) 

preparado <- prep(recipe_model, training = split_treino)

base_pos_recipe <- juice(preparado)

modelo1 <- lm(log_emp_mulher ~ ., data = base_pos_recipe)
summary(modelo1)

# modelo ------------------------------------------------------------------

linear_model <- linear_reg(penalty = tune(), mixture = tune()) %>% 
                      set_engine("glmnet") %>% 
                      set_mode("regression")
 
dt_model <- decision_tree(   
  cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")

boost_model <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

rf_model <- rand_forest(
   mtry = tune(), min_n = tune(), trees = 1000) %>% 
   set_engine("ranger", importance = "permutation") %>% 
   set_mode("regression") 

conjunto <- workflow_set(
  preproc = list(recipe = recipe_model),
  models = list(linear_model, dt_model, boost_model, rf_model)
)

# tutorial treinando v?rios modelos ---------------------------------------------------------------

grid_ctrl <- control_grid(
  save_pred = TRUE, 
  parallel_over = "everything",
  save_workflow = TRUE
)

set.seed(123)
grid_results <- conjunto %>% 
  workflow_map(
    resamples = validacao_cruzada, 
    grid = 15, 
    control = grid_ctrl
  )

autoplot(grid_results) + theme_minimal()

empreend_wf <- workflow(recipe_model, rf_model) 
  
# treinando ---------------------------------------------------------------

doParallel::registerDoParallel()

treinando_modelo <- empreend_wf %>% 
  tune_grid(resamples = validacao_cruzada,
            grid = 20, 
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(rmse,rsq,mae,mape))

metricas_treino <- treinando_modelo %>% 
                      collect_metrics()

treinando_modelo %>% 
  show_best(metric = "rsq")

treinando_modelo %>% 
  show_best(metric = "rmse")

treinando_modelo %>% 
  show_best(metric = "mape")

autoplot(treinando_modelo)

best_grid_modelo <- select_best(treinando_modelo, "rmse")

# Eu queria inspecionar as colunas predict e a coluna real. Como eu consigo fazer isso?
# sei que o best esta dentro do objeto treinando_modelo, mas gostaria de descobrir qual e

# avaliando no teste -------------------------------------------------------------

best_workflow <- 
  empreend_wf %>% 
  finalize_workflow(best_grid_modelo) %>% 
  last_fit(splits)

final_model <- empreend_model %>% 
                       finalize_model(best_grid_modelo)

best_workflow %>% 
  unnest(.predictions)  %>% 
  yardstick::rmse(.pred, log_emp_mulher)

best_workflow %>% 
  unnest(.predictions) %>% 
  ggplot(aes(x = .pred, y = log_emp_mulher)) + geom_point() + 
  geom_smooth(method = "lm") + theme_minimal()

# como eu consigo inspecionar .predict e coluna real no modelo final?

variaveis <- final_model %>% 
   extract_fit_parsnip() %>%
   tidy()

ajuste_final <- best_workflow %>% 
  fit(data = base_mulheres) 

best_workflow %>% 
  extract_fit_parsnip() %>% 
  vip(geom = 'col') 

best_workflow %>% 
  extract_fit_parsnip() %>% 
  vi %>% 
  ggplot(aes(x = fct_reorder(Variable, Importance), y = Importance)) + geom_col() + coord_flip() +
  theme_minimal() + xlab("Importance") + 
  theme(text = element_text(size=20))

previsoes <- workflow() %>% 
  add_recipe(recipe_model) %>% 
  add_model(final_reg_model) %>% 
  last_fit(splits) %>% 
  collect_predictions() 

# nao teria que fazer com a base completa?

previsoes_teste <- previsoes %>% 
  select(.row, log_emp_mulher, .pred) %>% 
  mutate(diferenca = log_emp_mulher - .pred)

previsoes %>% 
  select(.row, log_emp_mulher, .pred) %>% 
  ggplot(aes(x = log_emp_mulher, y = .pred)) +
  geom_point() + geom_smooth(method="lm", se = FALSE)


# shap for xgboost --------------------------------------------------------
  
xgb_fit <- extract_fit_parsnip(best_workflow) 

shap_empreend <- shap.prep(xgb_model = extract_fit_engine(xgb_fit),
                           X_train = bake(preparado, 
                                         has_role("predictor"),
                                         new_data = NULL, 
                                         composition = "matrix"))

shap.plot.summary(shap_empreend)

shap.importance(shap_empreend)

shap.plot.dependence(shap_empreend,
                     x = "infra_energia",
                     y = "trafico",
                     color_feature = "infra_esgoto")


# random forest 


fit_rf <- rand_forest(
  mtry = 6, min_n = 19, trees = 1000) %>% 
  set_engine("ranger", importance = "permutation") %>% 
  set_mode("regression") %>% 
  fit(log_emp_mulher ~ ., data = base_pos_recipe)

# explainer 

explainer <- DALEX::explain(
  model = fit_rf, 
  data = base_pos_recipe, 
  y = base_pos_recipe$log_emp_mulher,
  label = "Random Forest"
)

modelStudio::modelStudio(explainer)
