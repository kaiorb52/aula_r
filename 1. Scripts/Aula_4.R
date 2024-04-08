
pacman::p_load(
  tidyverse,
  data.table,
  here  
)

isp <- fread("0. Docs/BaseDPEvolucaoMensalCisp.csv", encoding = "Latin-1")
pop <- read_csv2("0. Docs/PopulacaoEvolucaoMensalCisp.csv", 
                 locale = locale(encoding = "windows-1252"))

#glimpse(isp)
#glimpse(pop)

pop$cisp <- pop$circ

isp_pop <- left_join(isp, pop) |> select(-circ)

#any(is.na(isp_pop$pop_circ))

isp_pop <- left_join(isp, pop) |> select(-circ)

isp_na <- isp_pop |> filter(is.na(pop_circ))

#table(isp_na$ano)
#table(isp$ano)
#table(pop$ano)

isp_novo <- isp_pop |> filter(between(ano, 2005, 2021)
)

nomes_dp <- fread("0. Docs/Relacao_RISPxAISPxCISP.csv", encoding = "Latin-1") |> janitor::clean_names() |> select(-risp, -aisp)

isp_dp <- left_join(isp_novo, nomes_dp)

# taxa de letatidade violenta
# hominicidio doloso 
# autos de resistencias 

nomes_isp <- isp_dp |>
  group_by(cisp) |>
    summarise(
      mun = paste(municipio, collapse = "-"),
      ter = paste(unidade_territorial, collapse = "-"),
    )

#x <- c("Universidade Norte Fluminense Darcy Ribeiro", "UENF")
#x2 <- paste(x, collapse = "-")

#print(x2)
#isp_novo_0$pop_circ

isp_dp_nomes <- left_join(isp_dp, nomes_isp, by = "cisp")

# População
pop_ano <-  isp_dp_nomes |>
  select(munic, ano, mes, pop_circ) |>
  filter(mes == 12) |>
  group_by(ano) |>
  summarise(sum_pop = sum(pop_circ))

#crimes 
crimes <- isp_dp_nomes |>
  group_by(ano) |>
  summarise(
    Count = n(),
    sum_ltd_violenta = sum(letalidade_violenta, na.rm = TRUE),
    sum_autos_resis = sum(hom_por_interv_policial, na.rm = TRUE),
    sum_hom_doloso = sum(hom_doloso, na.rm = TRUE),
    sum_hom_interv_pm = sum(hom_por_interv_policial)
  )

crimes_pop_ano <- left_join(crimes, pop_ano) |>
  mutate(perct_ltd_violenta = (sum_ltd_violenta/sum_pop)*100000,
         perct_autos_resis = (sum_autos_resis/sum_pop)*100000,
         perct_hom_doloso = (sum_hom_doloso/sum_pop)*100000,
         perct_interv_pm = (sum_hom_interv_pm/sum_pop)*100000,
         ind_violencia_pm = (sum_hom_interv_pm/sum_ltd_violenta),
  ) |>
  mutate(ind_violencia_pm_100000 = (ind_violencia_pm/sum_pop)*100000)

  
