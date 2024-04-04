
library(tidyverse)
library(data.table)

isp <- fread("0. Docs/BaseDPEvolucaoMensalCisp.csv", encoding = "Latin-1")
pop <- fread("0. Docs/PopulacaoEvolucaoMensalCisp.csv", encoding = "Latin-1")

glimpse(isp)
glimpse(pop)

pop$cisp <- pop$circ

isp_pop <- left_join(isp, pop) %>% select(-circ)
