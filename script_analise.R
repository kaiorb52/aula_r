
# setup -------------------------------------------------------------------


instalados <- installed.packages() 

instalados <- instalados[ ,1]
if(!"pacman" %in% instalados){
  install.packages("pacman")
  library(pacman)
} else {
  library(pacman)
}

p_load(
  readr, #ler dados
  dplyr  #manejo e limpeza
)





# importação dos dados ----------------------------------------------------
url <- "https://www.ispdados.rj.gov.br/Arquivos/BaseDPEvolucaoMensalCisp.csv"
  
isp <- read_delim(url, delim = ";", quote = '"', col_names = T,
           na = "", locale = locale(decimal_mark = ".",
                                    encoding = "windows-1252"))
isp <- read_csv2(url, locale = locale(encoding = "windows-1252"))

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





