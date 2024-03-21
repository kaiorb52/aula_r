
# setup -------------------------------------------------------------------


instalados <- installed.packages() 

instalados <- instalados[ ,1]
if(!"pacman" %in% instalados){
  install.packages("pacman")
  library(pacman)
} else {
  library(pacman)
}

pacman::p_load(
  readr, #ler dados
  dplyr  #manejo e limpeza
)


# importação dos dados ----------------------------------------------------
url <- "https://www.ispdados.rj.gov.br/Arquivos/BaseDPEvolucaoMensalCisp.csv"
  
isp <- read_delim(url, delim = ";", quote = '"', col_names = T,
           na = "", locale = locale(decimal_mark = ".",
                                    encoding = "windows-1252"))
isp <- read_csv2("0. Docs/BaseDPEvolucaoMensalCisp.csv", locale = locale(encoding = "windows-1252"))

guess_encoding(url)

# select ------------------------------------------------------------------

names(isp)
glimpse(isp)
isp |> select(cisp, aisp, risp) 


# select_helper -----------------------------------------------------------
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

#7) all_of
vars <- c("cisp","mes","ano", "hom_doloso", "latrocinio")

vars2 <- c(vars, "troleoelo") |> unlist()
isp <- isp |> select(all_of(vars))
isp <- isp |> select(all_of(vars2))

#8) any_of
isp <- isp |> select(any_of(vars2))

#9) everything
isp |> select(everything())

#10) last_col
isp |> select(last_col())

#11) where

#filter ----

isp %>% filter(regiao == "Capital") -> isp2

isp$regiao %>% unique() # lista casos unicos
isp$munic %>% unique() %>% sort() # lista casos unicos

isp$regiao %>% unique() 
isp2$regiao %>% unique() 

isp %>% filter(regiao == "Capital" |
  regiao == "Grande Niterói" |
    regiao == "Baixada Fluminense" ) -> isp2
    
isp %>% filter(regiao %in% c("Capital", "Grande Niterói", "Baixada Fluminense")) -> isp_rmrj

dim(isp_rmrj)

#casos do rmrj na pandemia (2020, 2021, 2022)

isp_rmrj %>% filter(regiao %in% c("Capital", "Grande Niterói", "Baixada Fluminense") & 
                 ano %in% c(2020, 2021, 2022)) -> isp_rmrj_pandemia

dim(isp_rmrj)
unique(isp_rmrj$ano)
unique(isp_rmrj$regiao)

#delegacias com mais homicidios doloso em um mês/ano
isp %>%
  filter(hom_doloso == max(hom_doloso, na.rm = TRUE)) %>%
  select(cisp, mes, ano, hom_doloso, munic, regiao)

#Todas as delegacias por mes/ano em Campos dos Goytacazes onde
#houve mais de 5 homicídios dolosos

isp %>%
  filter(munic ==  "Campos dos Goytacazes") %>%
  filter(hom_doloso > 5) %>%
  select(cisp, mes, ano, hom_doloso, munic, regiao)

#mutate
isp <- isp %>%
  mutate(peso_miae = hom_por_interv_policial/letalidade_violenta)

glimpse(isp)
summary(isp)

summary(isp$peso_miae)
isp$peso_miae

#delegacia/mes onde a polícia teve mais peso na
#letalidade violtenta

isp %>%
  filter(peso_miae == max(peso_miae, na.rm = TRUE)) %>%
  select(cisp, mes, ano, munic, regiao, letalidade_violenta, hom_por_interv_policial, peso_miae) %>% View()

# summarise ----

rj <- isp %>%
  summarise(
    total = sum(hom_doloso, na.rm = TRUE),
    media = mean(hom_doloso, na.rm = TRUE),
    media = median(hom_doloso, na.rm = TRUE),
    desvio_padrao = sd(hom_doloso, na.rm = TRUE),
    max = max(hom_doloso, na.rm = TRUE),
    min = min(hom_doloso, na.rm = TRUE)
  ) %>% mutate(regiao = "Estado - total")
#%>% View()
  
# group_by ----

isp %>%
  group_by(regiao) %>%
  summarise(
    total = sum(hom_doloso, na.rm = TRUE),
    media = mean(hom_doloso, na.rm = TRUE),
    media = median(hom_doloso, na.rm = TRUE),
    desvio_padrao = sd(hom_doloso, na.rm = TRUE),
    max = max(hom_doloso, na.rm = TRUE),
    min = min(hom_doloso, na.rm = TRUE)
  )  %>% bind_rows(rj)

## com filter

isp %>%
  group_by(regiao) %>%
  filter(hom_doloso > 2 * 3.04) %>%
  select(cisp, mes, ano, munic, hom_doloso) -> barra_pesada
  
glimpse(barra_pesada)
View(barra_pesada)

# mutate 

isp %>%
  group_by(regiao) %>%
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = TRUE),
    let_viol = sum(letalidade_violenta, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(peso_miae = miae/let_viol)


## arrange ----

isp %>%
  group_by(regiao) %>%
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = TRUE),
    let_viol = sum(letalidade_violenta, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(peso_miae = miae/let_viol) %>%
  arrange(-let_viol)

isp %>%
  group_by(regiao) %>%
  summarise(
    miae = sum(hom_por_interv_policial, na.rm = TRUE),
    let_viol = sum(letalidade_violenta, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(peso_miae = miae/let_viol) %>%
  arrange(desc(regiao))

#total de roubos, furtos e sequestros por aisp e por ano
isp %>%
  group_by(aisp, ano) %>%
  summarise(
    sum_sequestro = sum(sequestro, na.rm = TRUE),
    sum_roubos = sum(total_roubos, na.rm = TRUE),
    sum_furtos = sum(total_furtos, na.rm = TRUE)
  ) %>% summary()
  
