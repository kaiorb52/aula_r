
# setup -------------------------------------------------------------------

library(tidyverse)#readr, dply, tidyr, ggplot2, etc.
library(janitor)# funções marotas para ajudas na limpeza
library(here)#ajuda a construir caminhos para os arquivos
#obs.: se o pacote não estiver instaldo, rodar 
#install.packages("nome_do_pacote")

# importação dos dados ----------------------------------------------------
url <- here("0. Docs/BaseDPEvolucaoMensalCisp.csv")
guess_encoding(url) #verificar o encoding. Mas se o arquivo foi feito
                  # em windows e no brasil, windows-1252

  # dois jeitos possíveis
# isp <- read_delim(url, delim = ";", quote = '"', col_names = T,
#            na = "", locale = locale(decimal_mark = ".",
#                                     encoding = "windows-1252"))
isp <- read_csv2(url, locale = locale(encoding = "windows-1252"))



# manejo de dados: dplyr --------------------------------------------------
names(isp)# verifica o nome das variáveis
View(isp)# abre uma janela para visualizar os dados
head(isp)# 5 primeiras linhas de todas as colunas
str(isp)#estrutura do banco de dados
glimpse(isp)# lampejo sobre o banco de dados. É um str com head, adpatado ao
            #tamanho da janela e monitor
isp$regiao |> unique() #lista casos únicos de uma variável 
isp$munic |> unique() |> sort() #lista casos únicos de uma variável e ordena

## select ------------------------------------------------------------------


isp |> select(cisp, aisp, risp) 


### select_helper -----------------------------------------------------------
#1) var1:var2
isp |> select(cisp, mes, ano, 21:33) |>glimpse()

#2) starts_with

isp |> select(cisp, mes, ano, starts_with("roubo")) |> glimpse()

#3) ends_with

isp |> select(cisp, mes, ano, ends_with("transeunte")) |> glimpse()

#4) contains

glimpse(isp)
isp |> select(cisp, mes, ano, contains("roubo")) |> 
  glimpse()

#5) matches
isp <- isp |> 
  rename("roupo_apos_saque" = roubo_apos_saque)
glimpse(isp)
isp |> select(cisp, mes, ano, matches("rou[bp]o")) |> 
  glimpse()


#6) num_range
isp2 <- isp

names(isp2) <- paste0("V", 1:ncol(isp2))
glimpse(isp2)
isp2 |> select(num_range("V", range = 10:15))

#7) all_of
vars <- c("cisp","mes","ano", "hom_doloso", "latrocinio")




isp |> select(all_of(vars))



#8) any_of

vars2 <- c(vars, "troleoelo") |> unlist()
 isp |> select(any_of(vars2))


#9) everything
isp |> select(everything())
#10) last_col
isp |> select(last_col())
#11) where
isp |> select(where(is.character)) |> glimpse()


#renomear colunas com o select
isp |> select(cisp, mes, ano, "miae" = hom_por_interv_policial) |> 
  glimpse()
# isp <- isp |> rename("miae" = hom_por_interv_policial)
glimpse(isp)

##filter--------

# casos só da capital
isp |> filter(regiao == "Capital") -> isp2




isp$regiao |> unique()
isp2$regiao |> unique()

#casos da região metropolitana
isp_rmrj <- isp |> 
  filter(regiao == "Capital"| regiao == "Baixada Fluminense" |
           regiao == "Grande Niterói")
isp_rmrj <- isp |> 
  filter(regiao %in% c("Capital", "Baixada Fluminense",
                       "Grande Niterói"))

#casos da rmrj na pandemia (2020, 2021, 2022)
isp |> 
  filter(regiao %in% c("Capital", "Baixada Fluminense",
                       "Grande Niterói") &
           ano %in% c(2020, 2021, 2022)) -> isp_rmrj_pand
dim(isp_rmrj_pand)
unique(isp_rmrj_pand$ano)
unique(isp_rmrj_pand$regiao)

isp |> 
  filter(regiao %in% c("Capital", "Baixada Fluminense",
                       "Grande Niterói") &
           between(ano, 2020, 2023)) -> isp_rmrj_pand
dim(isp_rmrj_pand)
unique(isp_rmrj_pand$ano)
unique(isp_rmrj_pand$regiao)

#delegacia com mais homicídio doloso em um mês/ano
isp |> 
  filter(hom_doloso == max(hom_doloso, na.rm =T)) |> 
  select(cisp, mes, ano, hom_doloso, munic, regiao)

#Todas as delegacias por mes/ano em Campos dos Goytacazes onde
#houve mais de 5 homicídios dolosos
isp |> 
  select(cisp, mes, ano, munic, hom_doloso) |> 
  filter(munic == "Campos dos Goytacazes" &
           hom_doloso >= 5)

  
##mutate------
isp <- isp |> 
  mutate(peso_miae = hom_por_interv_policial/letalidade_violenta)
glimpse(isp)
summary(isp)
 
# delegacia/mes onde a polícia teve mais peso na
#letalidade violenta

isp |> 
  filter(peso_miae == max(peso_miae, na.rm = T)) |> 
  select(cisp, mes, ano, munic, regiao, letalidade_violenta,
         hom_por_interv_policial, peso_miae) |> View()


## summarise------
rj <- isp |> 
  summarise(
    total = sum(hom_doloso, na.rm = T),
    media = mean(hom_doloso, na.rm = T),
    mediana = median(hom_doloso, na.rm = T),
    desvio_padrao = sd(hom_doloso, na.rm =T),
    max = max(hom_doloso, na.rm = T),
    min = min(hom_doloso, na.rm = T)
    ) 

## group_by-------
## com summarise
isp |>
  group_by(regiao) |> 
  summarise(
    total = sum(hom_doloso, na.rm = T),
    media = mean(hom_doloso, na.rm = T),
    mediana = median(hom_doloso, na.rm = T),
    desvio_padrao = sd(hom_doloso, na.rm =T),
    max = max(hom_doloso, na.rm = T),
    min = min(hom_doloso, na.rm = T)
  ) 


## com filter
isp |> 
  group_by(regiao) |> 
  filter(hom_doloso > 2 * 3.04) |> 
  select(cisp, mes, ano, munic,regiao, hom_doloso) -> barra_pesada
  

## mutate

isp |> 
  group_by(regiao) |> 
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = T),
    let_viol = sum(letalidade_violenta, na.rm = T)
  ) |> 
  ungroup() |> 
  mutate(peso_miae = miae/let_viol)


## arrange----
isp |> 
  group_by(regiao) |> 
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = T),
    let_viol = sum(letalidade_violenta, na.rm = T)
  ) |> 
  ungroup() |> 
  mutate(peso_miae = miae/let_viol) |> 
  arrange(let_viol)

isp |> 
  group_by(regiao) |> 
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = T),
    let_viol = sum(letalidade_violenta, na.rm = T)
  ) |> 
  ungroup() |> 
  mutate(peso_miae = miae/let_viol) |> 
  arrange(-let_viol)

isp |> 
  group_by(regiao, ano) |> 
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = T),
    let_viol = sum(letalidade_violenta, na.rm = T)
  ) |> 
  ungroup() |> 
  mutate(peso_miae = miae/let_viol) |> 
  arrange(desc(regiao))




isp <- isp |> arrange(-hom_doloso)

# total de roubos, furtos e sequestros por aisp e por ano
glimpse(isp)

isp |> group_by(aisp, ano) |> 
  summarise(sum_sequestro = sum(sequestro, na.rm = T),
            sum_roubo = sum(total_roubos, na.rm = T),
            sum_furtos = sum(total_furtos, na.rm = T)) |> 
  View()
  


## joins------
#leitura dos dados de população
pop <- read_csv2("0. Docs/PopulacaoEvolucaoMensalCisp.csv",
                 locale = locale(encoding = "windows-1252"))

View(isp)
View(pop)
glimpse(isp)
glimpse(pop)

#metodo a
isp2 <- left_join(isp, pop,
                  by = c("cisp" = "circ", "mes", "ano"))
any(is.na(isp2$pop_circ))

isp3 <- inner_join(isp, pop, 
                   by = c("cisp" = "circ", "mes", "ano"))
#opa, deu ruim. Tem delegacias sem informação de população
isp_na <- isp2 |> filter(is.na(pop_circ))#só entre 2005 e 2021
                                  #tem todas as dps com população

#filtrando para 2005 e 2021
isp_3_21 <- isp2 |> 
  filter(between(ano, 2005, 2021))

#nomes das delegacias
nomes <- read_csv2("0. Docs/Relacao_RISPxAISPxCISP.csv",
                   locale = locale(encoding = "windows-1252")) |> 
  janitor::clean_names()

#sumarizando os nomes e tirando os casos duplicados
nomes2 <- nomes |> 
  group_by(cisp) |> 
  summarise(
    mun = paste(municipio, collapse = "-"),
    ter = paste(unidade_territorial, collapse = "-")
  )

isp_nomes <- left_join(isp_3_21, nomes2, by = "cisp")

#taxa de homicídio por ano

#passo 1: pegar a população de cada ano = último mes de cada ano
#população
pop_ano <- isp_nomes |>
  select(munic, ano,  mes, pop_circ) |> 
  filter(mes == 12) |> 
  group_by(ano) |> 
  summarise(pop = sum(pop_circ))

#passo2: crimes: sumarisado por ano
crimes <- isp_nomes |> 
  select(ano, hom_doloso, hom_por_interv_policial,
         letalidade_violenta) |> 
  group_by(ano) |> 
  summarise(hom_doloso = sum(hom_doloso, na.rm = T),
            miae = sum(hom_por_interv_policial,na.rm = T),
            let = sum(letalidade_violenta, na.rm = T))
glimpse(crimes)

#passo3: join entre a população/ano e o total de crimes por ano
taxas <- inner_join(pop_ano, crimes, by = "ano")

# pass4:mutate
calc_taxa <- function(x, y){
  x/y*100000
}

taxa <- taxas %>%
  mutate(
    perct_hom_doloso = calc_taxa(hom_doloso, pop),
    perct_interv_pm = calc_taxa(miae, pop),
    perct_letalidade_violenta = calc_taxa(let, pop)
  )

