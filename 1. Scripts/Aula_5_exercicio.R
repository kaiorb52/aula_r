
isp <- read_csv2("0. Docs/BaseDPEvolucaoMensalCisp.csv", locale = locale(encoding = "windows-1252"))

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

#exercicio:
pop_ano <- isp_nomes |>
  select(munic, ano,  mes, pop_circ) |> 
  filter(mes == 12) |> 
  group_by(ano) |> 
  summarise(pop = sum(pop_circ))

calc_taxa <- function(crim, pop){
  t <- (crim/pop)
  t*100000
}

# sum_crimes <- function(x){
#   sum(x, na.rm = T)
# }

select_crimes <- function(df, pop_df, group, a, b){
  df %>%
    group_by({{group}}) %>%
    summarise(
      across(
        {{a}}:{{b}}, ~ sum(.x, na.rm = TRUE),
        .names="{col}"
      )
    ) %>%
    left_join(pop_df) %>%
    mutate(
      across(
        {{a}}:{{b}}, ~ calc_taxa(.x, pop = pop),
        .names="taxa_{col}"
      )
    )
}

todos_crimes_fun <- 
  select_crimes(df = isp_nomes, pop_df = pop_ano, ano, hom_doloso, registro_ocorrencias)

taxa_longer <- todos_crimmes_fun %>%
  pivot_longer(
    cols = hom_doloso:taxa_registro_ocorrencias, 
    names_to = "Crimes", 
    values_to = "N"   
  )

taxa_wider <- taxa_longer %>%
  pivot_wider(
    names_from = "Crimes", 
    values_from = "N"
  )

taxa_longer %>%
  #filter(str_detect(Crimes, "taxa")) %>%
  filter(Crimes %in% c("taxa_hom_doloso", "taxa_hom_por_interv_policial")) %>%
  ggplot(aes(x=ano, y=N, color=Crimes)) +
  geom_point()+
  geom_line()+
  theme_classic()

