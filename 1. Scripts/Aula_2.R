instalados <- installed.packages()

str(instalados)
head(instalados)
row.names(instalados)
instalados <- instalados [ ,1]

"pacaman" %in% instalados

if(!"pacman" %in% instalados){
  install.packages("pacman")
  library (pacman)
} else { 
  library(pacman)
}

library(pacman)

p_load(
   readr,
   dplyr
)

#importação de dados ------------------------------------------

url <- "https://www.ispdados.rj.gov.br/Arquivos/BaseDPEvolucaoMensalCisp.csv"

isp <- read_delim(url, delim = ";", quote = '"', col_names = T,  
           na = "", locale = locale(decimal_mark = ".",
                                    encoding = "windows-1252"))
# isp <- read_csv2(url, locale = locale(encoding = "windows-1252"))

# guess_encoding(url)

library(dplyr)

#select ---------------------------------

glimpse(isp)
names(isp)

#var1:var2
isp |> select( cisp, mes, ano, 21:33) |> glimpse()


#2) starts_with

isp |> select( cisp, mes, ano, starts_with ("roubo")) |> glimpse()


#3 ends_with

isp |> select( cisp, mes, ano, ends_with ("transeunte")) |> glimpse()


#4 contains

#5 matches 

#6 num_range

#7 all_of

#8 any_of

#9 everything

#10 last_col

#11 where




