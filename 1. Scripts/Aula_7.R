
pacman::p_load(
  tidyverse,
  data.table,
  here, 
  janitor,
  RColorBrewer,
  ggpubr
)

taxa_longer_reg <- readRDS("~/aula_r/taxa_longer_reg.rds")


taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, fill=regiao))+
    geom_histogram(binwidth = 5, color="black")+
    coord_cartesian(expand = FALSE, xlim = c(15, 65))+
    facet_wrap(.~regiao)+
    ggthemes::theme_clean()

taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, fill=regiao))+
    geom_density(color="black")+
    coord_cartesian(expand = FALSE, xlim = c(15, 65))+
    facet_wrap(.~regiao) +
    ggthemes::theme_clean()

taxa_letalidade_violenta_df <- taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ungroup()

taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N))+
  geom_density(color="black")+
  coord_cartesian(expand = FALSE, xlim = c(15, 65))+
  geom_vline(xintercept = mean(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed") +
  geom_vline(xintercept = median(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed", color="red") +
  ggthemes::theme_clean()

taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, fill=regiao))+
  geom_density(color="black")+
  coord_cartesian(expand = FALSE, xlim = c(15, 65))+
  facet_wrap(.~regiao) +
  geom_vline(xintercept = mean(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed") +
  geom_vline(xintercept = median(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed", color="red") +
  ggthemes::theme_clean()

taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N))+
  geom_boxplot(color="black", width = 0.70) +
  ggthemes::theme_clean()


taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, y=reorder(regiao, N)))+
  geom_boxplot(color="black", width = 0.70) +
  ggthemes::theme_clean()

taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, y=reorder(regiao, N)))+
  geom_boxplot(color="black", width = 0.70) +
  geom_vline(xintercept = mean(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed") +
  geom_vline(xintercept = median(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed", color="tomato") +
  ggthemes::theme_clean()

taxa_longer_reg %>%
  janitor::tabyl(Crimes)

str_detect(taxa_longer_reg$Crimes, pattern = "taxa")

taxa_longer_reg %>%
  filter(str_detect(Crimes, "taxa"))

taxa_longer_reg %>%
  #filter(str_detect(Crimes, "taxa")) %>%
  filter(Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(x=N, y=reorder(Crimes, N)))+
  geom_boxplot()

taxa_longer_reg %>%
  #filter(str_detect(Crimes, "taxa")) %>%
  filter(Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(x=Crimes, y=N, fill=regiao))+
  #facet_grid(.~Crimes, scales = "free")+
  geom_boxplot(width = 0.5)

quantile(taxa_letalidade_violenta_df$N)
length(taxa_letalidade_violenta_df)
length(taxa_letalidade_violenta_df$N)
quantile(taxa_letalidade_violenta_df$N)

x <- quantile(taxa_letalidade_violenta_df$N)

x |> View()

taxa_longer_reg %>%
  #filter(str_detect(Crimes, "taxa")) %>%
  filter(Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(x=reorder(regiao, N), y=N, color=regiao))+
  facet_grid(.~Crimes, scales = "free")+
  stat_summary(
    fun.data = "mean_cl_boot",
    size=1.5
  )
  
taxa_longer_reg %>%
  #filter(str_detect(Crimes, "taxa")) %>%
  filter(Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(x=reorder(regiao, N), y=N, fill=regiao))+
  facet_grid(.~Crimes, scales = "free")+
  geom_violin(aes())+
  geom_boxplot(width = 0.5) 


taxas_todos_2 <- readRDS("~/aula_r/taxas_todos_2.rds")

todos_crimes_fun <- readRDS("~/aula_r/todos_crimes_fun.rds")

taxas_todos_2 %>%
  ggplot(aes(x=hom_doloso, y=taxa_hom_por_interv_policial)) +
  geom_point()+
  geom_smooth(se=FALSE, color="red")

todos_crimes_fun %>%
  ggplot(aes(x=hom_doloso, y=taxa_hom_por_interv_policial)) +
  geom_point()+
  geom_smooth(se=FALSE, color="red")


# Exercicicios Graficos

todos_crimes_fun %>%
  ggplot(aes(x=hom_doloso, y=taxa_hom_por_interv_policial)) +
  facet_wrap(regiao~., scales = "free") +
  geom_point()+
  geom_smooth(se=FALSE, color="red", method = "lm") +
  stat_cor(method = "pearson") +
  ggthemes::theme_clean() +
  labs(x="Homicidio Doloso", y="Homicidio por intervenção policial")

 taxa_longer_reg %>%
  filter(Crimes =="taxa_letalidade_violenta") %>%
  ggplot(aes(x=N, y=reorder(regiao, N)))+
  geom_violin(width = 1.00)+
  geom_jitter()+
  geom_boxplot(color="black", width = 0.5)+
  stat_summary(
    fun.data = "mean_cl_boot",
    geom = "point",
    colour = "red", 
    size=8
  ) +
  labs(y=NULL,x=NULL) + 
  geom_vline(xintercept = mean(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed") +
  geom_vline(xintercept = median(taxa_letalidade_violenta_df$N), size=1.25, linetype="dashed", color="tomato2") +
  ggthemes::theme_clean()

todos_crimes_fun %>%
    pivot_longer(
    cols = hom_doloso:taxa_registro_ocorrencias, 
    names_to = "Crimes", 
    values_to = "N"   
  ) %>%
  filter(Crimes %in% c("taxa_letalidade_violenta", "taxa_hom_por_interv_policial", "taxa_hom_doloso")) %>%
  ggplot(aes(x=ano, y=N, color=Crimes, shape=Crimes)) +
  facet_wrap(regiao~., scales = "free") +
  geom_point(size=3.0)+
  geom_line(size=1.2) +
  ggthemes::theme_clean() +
  labs(y=NULL,x="Anos") + 
  scale_x_continuous(breaks = seq(2005, 2022, by = 2 )) +
  theme(legend.position = "bottom", axis.title.x = element_text(size=10, face = "bold")) +
  scale_color_manual(values = c(
    "gold1", "tomato", "darkgreen"
    )
  )

seq(3)

rep("Kaio", 3)


isp <- read_csv2("0. Docs/BaseDPEvolucaoMensalCisp.csv", locale = locale(encoding = "windows-1252"))
pop <- read_csv2("0. Docs/PopulacaoEvolucaoMensalCisp.csv",
                 locale = locale(encoding = "windows-1252"))

isp2 <- left_join(isp, pop,
                  by = c("cisp" = "circ", "mes", "ano"))
any(is.na(isp2$pop_circ))

isp3 <- inner_join(isp, pop, 
                   by = c("cisp" = "circ", "mes", "ano"))

isp3 %>%
  paste0(mes_e_ano = isp3$mes, isp3$ano) 

paste0("Kaio", "10")

isp3$mes_e_ano <- paste0(isp3$mes, ".", isp3$ano)
isp3$mes_e_ano

isp3 %>% #Revisar dps
  group_by(mes_e_ano, munic) %>%
  summarise(sum_hom_doloso=sum(hom_doloso)) %>%
  #mutate(mes_e_ano=as.numeric(mes_e_ano)) %>%
  #arrange(-sum_hom_doloso) %>%
  filter(munic %in% c("Rio de Janeiro", "Macaé", "Campos dos Goytacazes")) %>%
  #View()
  ggplot(aes(x=mes_e_ano, y=sum_hom_doloso, color=munic)) +
    #facet_wrap(munic~., scales = "free") +
    geom_point(size=3.0)+
    geom_line(size=1.2) +
    ggthemes::theme_clean()
