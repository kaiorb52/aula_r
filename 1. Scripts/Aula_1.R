library(tidyverse)
library(janitor)

install.packages("pacman")

library(pacman)
p_load(tidyverse)

lista <-list(alunos,
             c(1:10),
             letters)

str(lista)
x <-

#Aula 01: Introdução ao R ----
#Logical
T
F
NA
#numeric
##double
1.2
1.3
###integer
1
2
3
#character
"1"
"David"
"Trololo"
#conversões
as.numeric(T)
as.numeric(F)
as.logical(1)
as.logical(0)
as.numeric("1") + as.numeric("1")
as.character(1000)
as.character(1)


hila <- 82
valmir<- 89
hila+valmir
class(hila)
typeof(hila)


#vetores
#estrutura unidimensional
#todos os valores do mesmo tipo
idades <- c(27, 24, 20, 20, 21, 33, 22, 23, 24)
nomes <- c ("Luciano", "Raphael", "Caio", 'Davi', 'Bruna', 'Suzamar', "Heitor", "Arthur", "Jaiany")
matriculados <- c(rep(T,9))
dput(nomes)
c(T,F,F,T,1,2,4)

#MATRIZES-------
#bidimensional
#todos os valores do mesmo tipo
matrix(c(2,2,2,2), ncol=2)



#array----
#n-dimensional
#todos valores do mesmo tipo
x <- array (c(2,2,2,2,2,2,2,2,2), dim = 2)
##  data.frame----
#bidimensional
#colunas de diferentes tipos
alunos <- data.frame
nomes = nomes,
idade=idade,
matriculados = matriculados,
data.frame(nomes = nomes <- c ("Luciano", "Raphael", "Caio", 'Davi', 'Bruna', 'Suzamar', "Heitor", "Arthur", "Jaiany"), 
           idades <- c(27, 24, 20, 20, 21, 33, 22, 23, 24),
           origem <- ("campos", "rio de janeiro", "campos", " são gonçalo", "campos", "trajano de moares", "cambuci", "Itaocara", "araruama"), stringsAsfac
           
           
           
##listas-----
lista <- list(
   alunos,
   1,
   2,
   3,
   c('a', "b", "a"),
   list(c("Brasil", "Suécia", "Mianmar"))
)


##factor----
factor('M', "F", "M" , "F" , "F"),
levels= C("M", "F"),
labels= c("Masculino", "Feminino")




x_fator <- factor(
  c("S","M", "F", "F" , "E"), 
  levels = c ("E", "F", "M", "S"),
  labels = c ("Sem escolaridade", "Fundamental", "Médio", "Superior"),
  ordered = T
)
           
           
           
x_character <- c(rep("F", 5))  

x_fator
table(x_fator)
x_character
table(x_character)


#operações
 vet <- 1:100
 mean(vet)
 median(vet)
 sd(vet)
 sum(vet)
 length(vet)
 sum (vet)/length(vet) 


nomes <- aluno$nomes

nomes
length(nomes)
nomes[1:5]

nomes[2] <- "Rafael"
nomes [nomes == "Rafael"]
ind <- nomes != "Bruna"
nomes [ind]


#operadores lógicos(booleanos)----

nomes[2] <- "Rafael"
nomes [nomes == "Rafael"]
ind <- nomes != "Bruna"
nomes [ind]
nomes
nomes <- nomes [ind]
nomes


alunos$origem
alunos ["campos" , "rio de janeiro"]


data("mtcars")
mtcars$mpg
mtcars[mtcars$mpg > 20,]


data("crimtab")
crimtab

head(crimtab)


install.packages("tidyverse")
install.packages("janitor")
library(tidyverse)
library(janitor)
