#tratamento Munic
library(readxl); library(tidyverse)

saude_munic_2018 <- read_excel("base_munic_2018.xlsx", 
                              sheet = "saude") %>% 
                    janitor::clean_names() %>% 
                    select(cod_ibge, starts_with("MSAU39")) %>% 
                    select(-msau3919)

pmulher_munic_2018 <- read_excel("base_munic_2018.xlsx", 
                                 sheet = "politica_mulheres") %>% 
                      janitor::clean_names() %>% 
                      select(cod_ibge, mppm01, mppm11, mppm13, mppm20, mppm23)

munic_2018 <- saude_munic_2018 %>% 
  left_join(pmulher_munic_2018, by = "cod_ibge")

write.csv(munic_2018, "munic_2018.csv")
