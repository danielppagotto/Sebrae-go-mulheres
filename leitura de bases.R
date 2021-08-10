library(tidyverse)

# Arquivo que contem as leituras inicias da base

estab0 <- vroom("estab0.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab1 <- vroom("estab1.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab2 <- vroom("estab2.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab3 <- vroom("estab3.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab4 <- vroom("estab4.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab5 <- vroom("estab5.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab6 <- vroom("estab6.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")


estab7 <- vroom("estab7.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab8 <- vroom("estab8.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estab9 <- vroom("estab9.ESTABELE", col_names = FALSE) %>% 
  select(X1, X2, X3, X4, X5, 
         X6, X7, X11, X12, X20, X21) %>% 
  filter(X20 == "GO" & X6 == "02")

estabelecimentos <- rbind(estab0, estab1, estab2, 
                          estab3, estab4, estab5, 
                          estab6, estab7, estab8,
                          estab9) %>% 
  rename(cnpj_basico = X1, cnpj_ordem = X2, 
         cnpj_dv = X3, matriz = X4, nome_fantasia = X5, 
         situacao = X6, data_situacao_atual = X7, 
         data_inicio_atividade = X11, cnae = X12, uf = X20, 
         municipio = X21)


# Sócios ------------------------------------------------------------------



# Empresas leitura --------------------------------------------------------

setwd("~/LAPEI/Projeto SEBRAE/Atualização pesquisas/empreendedorismo RFB/empresas")

emp1 <- vroom("empresa1.EMPRECSV", col_names = FALSE) 
emp2 <- vroom("empresa2.EMPRECSV", col_names = FALSE) 
emp3 <- vroom("empresa3.EMPRECSV", col_names = FALSE) 
emp4 <- vroom("empresa4.EMPRECSV", col_names = FALSE) 
emp5 <- vroom("empresa5.EMPRECSV", col_names = FALSE) 
emp6 <- vroom("empresa6.EMPRECSV", col_names = FALSE) 
emp7 <- vroom("empresa7.EMPRECSV", col_names = FALSE) 
emp8 <- vroom("empresa8.EMPRECSV", col_names = FALSE) 
emp9 <- vroom("empresa9.EMPRECSV", col_names = FALSE) 
emp10 <- vroom("empresa10.EMPRECSV", col_names = FALSE) 



# Socios ------------------------------------------------------------------

setwd("~/LAPEI/Projeto SEBRAE/Atualização pesquisas/empreendedorismo RFB/socios")

socio1 <- vroom("socio1.SOCIOCSV", col_names = FALSE) 
socio2 <- vroom("socio2.SOCIOCSV", col_names = FALSE) 
socio3 <- vroom("socio3.SOCIOCSV", col_names = FALSE) 
socio4 <- vroom("socio4.SOCIOCSV", col_names = FALSE) 
socio5 <- vroom("socio5.SOCIOCSV", col_names = FALSE) 
socio6 <- vroom("socio6.SOCIOCSV", col_names = FALSE) 
socio7 <- vroom("socio7.SOCIOCSV", col_names = FALSE) 
socio8 <- vroom("socio8.SOCIOCSV", col_names = FALSE) 
socio9 <- vroom("socio9.SOCIOCSV", col_names = FALSE) 
socio10 <- vroom("socio10.SOCIOCSV", col_names = FALSE) 

socios <- rbind(socio1, socio2, socio3, socio4, socio5, 
                socio6, socio7, socio8, socio9, socio10) %>% 
  select(-X4, -X8, -X9, -X10) %>% 
  rename(cnpj_basico = X1, id_socio = X2, 
         nome_socio = X3, qualificacao_socio = X5, 
         entrada_sociedade = X6, pais = X7, faixa_etaria = X11)

write.csv(socios, "socios.csv")


# Tratamentos -------------------------------------------------------------


empresa <- rbind(emp1, emp2, emp3, emp4, emp5, emp6, emp7, emp8, emp9, emp10)

empresa <- empresa %>% 
  rename(razao_social = X2, natureza_juridica = X3, 
         qualifica_responsavel = X4, capital_social = X5,
         porte_empresa = X6, ente_federativo = X7)

ativas <- estabelecimentos %>% 
  left_join(empresa, by = c("cnpj_basico" = "X1"))  

write.csv(ativas,"empresas_ativas.csv")
