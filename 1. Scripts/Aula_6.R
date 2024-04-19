
pacman::p_load(
  tidyverse,
  data.table,
  here, 
  janitor,
  RColorBrewer
)

taxa_longer <- readRDS("~/aula_r/taxa_longer.rds")


#Grafico 1 --------

taxa_hom_doloso <- taxa_longer %>%
  filter(Crimes == "taxa_hom_doloso") 


ggplot(data=taxa_hom_doloso, aes(y=N, x=ano)) +
    geom_point(size=3.2, shape=10) +
    geom_line(size=1.5, linetype="dotted") +
    geom_smooth(color="red", method = "glm", se=FALSE) +
    scale_x_continuous("Ano", breaks = scales::breaks_extended(17),                          
      #seq(2005, 2021, by=1)
    ) +
    scale_y_continuous("Taxa pro 100.000 hab.", 
                       breaks = scales::breaks_extended(17),                          
    ) +
    ggtitle("Homicídio doloso por 100.000 hab.", subtitle = "Rio de janeiro, 2005-2021") +
    # ggthemes::theme_calc() +
    #labs(y=NULL)+ 
    theme_classic() +
    theme(
      axis.text.x = element_text(angle=45, vjust=1,hjust=1)
    )


#Grafico 2 --------

tax
a_hom_doloso <- taxa_longer %>%
  filter(Crimes == "taxa_hom_por_interv_policial") 

ggplot(data=taxa_hom_doloso, aes(y=N, x=ano)) +
  geom_point(size=3.2, shape=10) +
  geom_line(size=1.5, linetype="dotted") +
  geom_smooth(color="red", method = "glm", se=FALSE) +
  scale_x_continuous("Ano", breaks = scales::breaks_extended(17),                          
                     #seq(2005, 2021, by=1)
  ) +
  scale_y_continuous("Taxa pro 100.000 hab.", 
                     breaks = scales::breaks_extended(17),                          
  ) +
  ggtitle("MIAE por 100.000 hab.", subtitle = "Rio de janeiro, 2005-2021") +
  # ggthemes::theme_calc() +
  #labs(y=NULL)+ 
  theme_classic() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1,hjust=1)
  )


#Grafico 3 --------

taxa_longer %>%
  filter(Crimes %in% c("taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(y=N, x=ano, color=Crimes, linetype=Crimes, shape=Crimes)) +
  geom_point(size=3.25) +
  geom_line(size=1.5) +
  #geom_smooth(color="red", method = "glm", se=FALSE) +
  scale_x_continuous("Ano", breaks = scales::breaks_extended(17),                          
                     #seq(2005, 2021, by=1)
  ) +
  scale_y_continuous("Taxa pro 100.000 hab.", 
                     breaks = scales::breaks_extended(17),
                    
  ) +
  ggtitle("Homicídio doloso & MIAE por 100.000 hab.", subtitle = "Rio de janeiro, 2005-2021") +
  #labs(y=NULL)+ 
  theme_classic() +
  #ggthemes::theme_calc() +
  theme(
    axis.text.x = element_text(angle=45, vjust=1,hjust=1),
    legend.location = "bottom" 
  ) +
  scale_color_manual(
    values = c(
      "orange1",
      "darkgreen"))+
  # scale_color_brewer("Crimes", type = "qual", palette = "Set1") +
  facet_wrap(.~Crimes, scales = "free_y")

  # RColorBrewer::brewer.pal.info
  # RColorBrewer::display.brewer.all()


# taxas por ano e regiao --------------------------------------------------

isp <- read_csv2("0. Docs/BaseDPEvolucaoMensalCisp.csv", locale = locale(encoding = "windows-1252"))
pop <- read_csv2("0. Docs/PopulacaoEvolucaoMensalCisp.csv",
                 locale = locale(encoding = "windows-1252"))

isp2 <- left_join(isp, pop,
                  by = c("cisp" = "circ", "mes", "ano"))
any(is.na(isp2$pop_circ))

isp3 <- inner_join(isp, pop, 
                   by = c("cisp" = "circ", "mes", "ano"))
isp_na <- isp2 |> filter(is.na(pop_circ))#só entre 2005 e 2021

isp_3_21 <- isp2 |> 
  filter(between(ano, 2005, 2021))

nomes <- read_csv2("0. Docs/Relacao_RISPxAISPxCISP.csv",
                   locale = locale(encoding = "windows-1252")) |> 
  janitor::clean_names()

nomes2 <- nomes |> 
  group_by(cisp) |> 
  summarise(
    mun = paste(municipio, collapse = "-"),
    ter = paste(unidade_territorial, collapse = "-")
  )

isp_nomes <- left_join(isp_3_21, nomes2, by = "cisp")

pop_ano <- isp_nomes |>
  select(regiao, ano,  mes, pop_circ) |> 
  filter(mes == 12) |> 
  group_by(ano, regiao) |> 
  summarise(pop = sum(pop_circ))

calc_taxa <- function(crim, pop){
  t <- (crim/pop)
  t*100000
}

select_crimes <- function(df, pop_df, group , a, b){
  df %>%
    group_by( {{group}}, regiao) %>% #Revisar dps
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

taxa_longer <- todos_crimes_fun %>%
  pivot_longer(
    cols = hom_doloso:taxa_registro_ocorrencias, 
    names_to = "Crimes", 
    values_to = "N"   
  )

taxa_longer %>%
  filter(ano == 2018 & Crimes == "taxa_letalidade_violenta") %>%
  ggplot(aes(y=reorder(regiao, N), fill=N, x=N)) +
    geom_bar(stat = "identity", color="black") +
    theme_classic()+
    scale_fill_distiller(palette = "Reds", direction = 1)

taxa_longer |>
  filter(ano == 2018 & Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) |>
  mutate( 
    regiao= factor(regiao) |>
      forcats::fct_reorder(.x=N,
                           .fun=min)
  ) |>
  ggplot(aes(x=regiao, fill=Crimes, y=N)) +
    #geom_bar(stat = "identity", color="black") +
    geom_col(color="black", position = "dodge")+
    coord_flip()+
    theme_classic() +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
