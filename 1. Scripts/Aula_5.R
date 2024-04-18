
pacman::p_load(
  tidyverse,
  data.table,
  here, 
  janitor
)

isp <- read_csv2("0. Docs/BaseDPEvolucaoMensalCisp.csv", locale = locale(encoding = "windows-1252"))

## joins------
#leitura dos dados de população
pop <- read_csv2("0. Docs/PopulacaoEvolucaoMensalCisp.csv",
                 locale = locale(encoding = "windows-1252"))

# View(isp)
# View(pop)
# glimpse(isp)
# glimpse(pop)

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

##metodo I
taxas2 <- taxas %>%
  mutate(
    perct_hom_doloso = (hom_doloso/pop) *100000,
    perct_interv_pm = (miae/pop) *100000,
    perct_letalidade_violenta = (let/pop) *1e5
  )

#nova função
calc_taxa <- function(x, y){
  (x/y)*100000
}

calc_taxa <- function(crim, pop){
  t <- (crim/pop)
  t*100000
}

##metodo II
taxas2 <- taxas %>%
  mutate(
    perct_hom_doloso = calc_taxa(hom_doloso, pop),
    perct_interv_pm = calc_taxa(miae, pop),
    perct_letalidade_violenta = calc_taxa(let, pop)
  )

##metodo III
taxas2 <- taxas %>%
  mutate(across(
    hom_doloso:let, ~ calc_taxa(.x, pop=pop),
    .names="taxa_{col}")
  )

#exercicio:

pop_ano <- isp_nomes |>
  select(munic, ano,  mes, pop_circ) |> 
  filter(mes == 12) |> 
  group_by(ano) |> 
  summarise(pop = sum(pop_circ))

sum_crimes <- function(x){
  sum(x, na.rm = T)
}

crimes_todos <- isp_nomes |> 
  # select(ano, hom_doloso, hom_por_interv_policial,
  #        letalidade_violenta) |> 
  group_by(ano) |> 
  summarise(
    across(
      hom_doloso:registro_ocorrencias, ~ sum_crimes(.x),
      .names="{col}")
  )

#passo3: join entre a população/ano e o total de crimes por ano
taxas_todos <- inner_join(pop_ano, crimes_todos, by = "ano")

calc_taxa <- function(crim, pop){
  t <- (crim/pop)
  t*100000
}

taxas_todos_2 <- taxas_todos %>%
  mutate(across(
    hom_doloso:registro_ocorrencias, ~ calc_taxa(.x, pop=pop),
    
    .names="taxa_{col}")
  )


