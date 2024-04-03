
# 0. LIBRARIES ----

pacman::p_load(
  tidyverse,
  data.table
)

# 1. IMPORTAÇÃO DO DADOS ISP ----

url <- "https://www.ispdados.rj.gov.br/Arquivos/BaseDPEvolucaoMensalCisp.csv"

isp_rj <- fread(url, encoding = "Latin-1")

rm(url)

# 2. ANALISE ----
## 2.1 Questão I, Qual é o ano com mais homicídios dolosos? ----

q_1 <- isp_rj |>
  group_by(ano) |>
  summarise(sum_hom_doloso = sum(hom_doloso, na.rm = TRUE)) |>
  arrange(desc(sum_hom_doloso)) |>
  head(5) 

print(q_1) 

# Resposta:
# Segundo os dados do isp, o ano com mais mortes por homisidio doloso é 2002.

## 2.2 Questão II, Em qual município, considerando todo o intervalo entre 2012 e 2018, há maior proporção de autos de resistência em relação ao total de casos de letalidade violenta (obs.: é o período todo entre 2012 e 2018, não ano-a-ano)? ----

q_2 <- isp_rj |>
  filter(ano >= 2012 & ano <= 2018) |>
  group_by(munic) |>
  summarise(
    sum_hom_interv_pm = sum(hom_por_interv_policial),
    sum_letd_violenta = sum(letalidade_violenta)
  ) |>
  mutate(ind_interv_pm = sum_hom_interv_pm/sum_letd_violenta) |>
  arrange(desc(ind_interv_pm)) |> 
  head(5) 

print(q_2) 

# Resposta:
# considerando o intervalo entre os anos de 2012 e 2018, 
# o municipio de Niterói se demostra como o de maior homidio de intervenção policial propocional a quantidade de letalidade violenta.

## 2.3 Questão III, Qual é a delegacia/ano com mais casos de letalidade violenta por 100.000 habitantes? ----

q_3 <- isp_rj |>
  group_by(cisp, ano) |>
  summarise(sum_ltd_violenta = sum(letalidade_violenta)) |>
  mutate(ind_ltd_violenta = 100000/sum_ltd_violenta) |>
  arrange((ind_ltd_violenta)) |> view()

print(q_3)

isp_rj %>%
  filter(cisp == 39) %>%
  select(munic) # 39 = Rio de Janeiro

# Resposta:
# Como demostrado, o 39º CISP da zona Oeste do Rio de Janeiro,
# no ano de 2003 contem o maior numero de letalidade violenta. 
# Além disso, No universo de 100.000 habitantes ocorreriam 1 morte por letalidade violenta, para cada 250 habitantes

