library(tidyverse)
library(Hmisc)
library(GGally)
library(psych)
library(stats)
library(StatMatch)
library(lmtest)
library(MASS)
library(car)
library(gvlma)

setwd("~/GitHub/Sebrae-go-mulheres/determinantes empreendedorismo")

# lendo a base e ja transformando valores 

base_mulheres <- readxl::read_excel("base_mulheres.xlsx", 
                            col_types = c("text", "numeric", "text", "numeric", "numeric", "numeric","numeric", "numeric", 
                                          "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric",  "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))

base_mulheres <- base_mulheres %>% 
  janitor::clean_names()

base_mulheres[is.na(base_mulheres)] <- 0

base_mulheres <- base_mulheres %>% 
  mutate(log_emp_mulher = log(tx_emp_mulher),
         log_pibpcapta = log(pib_per_capta),
         log_empregos = log(empregos_formais))

base_mulheres <- base_mulheres %>% 
  filter(log_pibpcapta != -Inf & log_empregos != -Inf)

base_mulheres <- base_mulheres %>% 
  select(localidade,tx_emp_mulher,log_emp_mulher,
         crimes_patrim,crimes_contra_pessoa,escolarid_trab,
         pib_per_capta, log_pibpcapta, empregos_formais, 
         log_empregos,infra_agua,infra_internet)

base_mulheres <- base_mulheres %>% 
  filter(crimes_patrim != 0 & crimes_contra_pessoa != 0 &
           escolarid_trab != 0 & infra_agua != 0 &
           infra_internet != 0)

## Gerar estatistica descritiva 

estatistica_desc <- base_mulheres %>% 
  select(-localidade,-log_emp_mulher,-log_empregos,-log_pibpcapta) %>% 
  psych::describe() %>% 
  round(digits = 2)

estatistica_desc<-estatistica_desc %>% 
  select(-vars,-trimmed,-mad,-se,-n) 

DT::datatable(estatistica_desc)

## Gerar gráficos 

base_mulheres %>% 
  select(tx_emp_mulher,crimes_patrim,crimes_contra_pessoa,contrav_penais,
         escolarid_trab,pib_per_capta,empregos_formais,remun_mediana,
         infra_agua, infra_internet,) %>% 
  GGally::ggpairs()

out_m <- base_mulheres %>% 
  select(log_emp_mulher,crimes_patrim,crimes_contra_pessoa,
         escolarid_trab,log_pibpcapta,log_empregos,
         infra_agua, infra_internet) %>%
  psych::outlier() 

limiar_m <- mean(out_m) + (2 * sd(out_m))

base_m_sem_outliers <- base_mulheres %>% 
         select(localidade,tx_emp_mulher,log_emp_mulher,
                crimes_patrim,crimes_contra_pessoa,escolarid_trab,
                log_pibpcapta,log_empregos,infra_agua, infra_internet) %>% 
         cbind(out_m) %>% 
         filter(out_m < limiar_m) 

## rodar modelo 

modelo_mulher <- lm(formula = log_emp_mulher ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos + log_pibpcapta +
                infra_agua + infra_internet , data = base_m_sem_outliers)

summary(modelo_mulher)

## pressupostos 
## Multicolinearidade 

car::vif(modelo_mulher)

modelo_mulher <- lm(formula = log_emp_mulher ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos +
                infra_agua + infra_internet , data = base_sem_outliers)

summary(modelo_mulher)

## retirando casos extremos

cutoff <- 4/((nrow(base_m_sem_outliers)-length(modelo1$coefficients)-2)) 
plot(modelo1, which=4, cook.levels=cutoff)

car::influencePlot(modelo1, 
                   id.method="identify",
                   main="Influence Plot", 
                   sub="Circle size is proportial to Cook's Distance" )

base_sem_outliers2 <- base_m_sem_outliers %>% 
  filter(localidade != "Sitio dAbadia" & localidade != "Damolandia" &
           localidade != "Santa Barbara de Goias")

modelo_mulher <- lm(formula = log_emp_mulher ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos +
                infra_agua + infra_internet , data = base_sem_outliers2)

summary(modelo_mulher)

#model_diag <- augment(modelo1)

# Homocedasticidade 

lmtest::bptest(modelo_mulher)

# normalidade 

car::qqPlot(modelo_mulher, main="QQ Plot") #qq plot for studentized resid 
ggplot2::ggplot(modelo_mulher,aes(x=resid(modelo_mulher)))+geom_density()
stats::shapiro.test(resid(modelo_mulher))

# sintese

gvmodel <- gvlma::gvlma(modelo_mulher)
summary(gvmodel)


######################################################
#                                                    #
#              Taxas empreendedorismo homens         #                                      #
#                                                    #
######################################################


base_homens <- read_excel("determinantes.xlsx", 
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
                                            "numeric", "numeric", "numeric")) %>%
                janitor::clean_names()

base_homens <- base_homens %>% 
  mutate(log_emp_homens = log(perc_homens),
         log_pibpcapta = log(pib_per_capta),
         log_empregos = log(empregos_formais))

base_homens <- base_homens %>% 
  filter(log_pibpcapta != -Inf & log_empregos != -Inf)

base_homens_t <- base_homens %>% 
  select(localidade,perc_homens,log_emp_homens,
         crimes_patrim,crimes_contra_pessoa,escolarid_trab,
         pib_per_capta, log_pibpcapta, empregos_formais, 
         log_empregos,infra_agua,infra_internet)

base_homens_t <- base_homens_t %>% 
  filter(crimes_patrim != 0 & crimes_contra_pessoa != 0 &
           escolarid_trab != 0 & infra_agua != 0 &
           infra_internet != 0)

## Gerar estatistica descritiva 

estatistica_desc_h <- base_homens_t %>% 
  select(-localidade,-log_emp_homens,-log_empregos,-log_pibpcapta) %>% 
  psych::describe() %>% 
  round(digits = 2)


base_homens %>% 
  select(perc_homens,crimes_patrim,crimes_contra_pessoa,escolarid_trab,pib_per_capta,
         empregos_formais,infra_agua, infra_internet,) %>% 
  GGally::ggpairs()

out_h <- base_homens_t %>% 
  select(log_emp_homens,crimes_patrim,crimes_contra_pessoa,
         escolarid_trab,log_pibpcapta,log_empregos,
         infra_agua, infra_internet) %>%
  psych::outlier() 

limiar_h <- mean(out_h) + (2 * sd(out_h))

base_h_sem_outliers <- base_homens_t %>% 
  select(localidade,perc_homens,log_emp_homens,
         crimes_patrim,crimes_contra_pessoa,escolarid_trab,
         log_pibpcapta,log_empregos,infra_agua, infra_internet) %>% 
  cbind(out_h) %>% 
  filter(out_h < limiar_h) 

# Rodando modelo

modelo_h <- lm(formula = log_emp_homens ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos + log_pibpcapta +
                infra_agua + infra_internet , data = base_h_sem_outliers)

summary(modelo_h)

## pressupostos 
## Multicolinearidade 

car::vif(modelo_h)

modelo_h <- lm(formula = log_emp_homens ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos +
                infra_agua + infra_internet , data = base_h_sem_outliers)

summary(modelo_h)

## retirando casos extremos

cutoff <- 4/((nrow(base_h_sem_outliers)-length(modelo_h$coefficients)-2)) 
plot(modelo_h, which=4, cook.levels=cutoff)

car::influencePlot(modelo1, 
                   id.method="identify",
                   main="Influence Plot", 
                   sub="Circle size is proportial to Cook's Distance" )

base_h_sem_outliers2 <- base_h_sem_outliers %>% 
  filter(localidade != "Sitio dAbadia" & localidade != "Morro Agudo de Goias")

modelo_h <- lm(formula = log_emp_homens ~  crimes_contra_pessoa + crimes_patrim +
                escolarid_trab + log_empregos +
                infra_agua + infra_internet , data = base_h_sem_outliers2)

summary(modelo_h)

#model_diag <- augment(modelo1)

# Homocedasticidade 

lmtest::bptest(modelo_h)

# normalidade 

car::qqPlot(modelo_h, main="QQ Plot") #qq plot for studentized resid 
ggplot2::ggplot(modelo_h,aes(x=resid(modelo_h)))+geom_density()
stats::shapiro.test(resid(modelo_h))

# sintese

gvmodel_h <- gvlma::gvlma(modelo_h)
summary(gvmodel_h)


plot(modelo_h)


