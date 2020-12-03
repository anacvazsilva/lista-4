#Carregando pacotes

library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(knitr)
library(psych)

# Carregando as bases. state_const (base U.S. State Constitutions ) e a base States and Abbreviations para auxiliar a colocar as abreviações dos estados na base original.

state_const <- read_excel("C:/Users/barba/Dropbox/Academicos/Doutorado/Disciplinas/Metodos/Trabalho Final - Fred/dados/state_const.xlsx")
States_and_Abbreviations <- read_xlsx("C:/Users/barba/Dropbox/Academicos/Doutorado/Disciplinas/Metodos/Trabalho Final - Fred/States and Abbreviations.xlsx")

# Manipulando a base

constituicoes_poderes <- state_const %>% #criando um novo data frame com as manipulações
  select(state, constitution_year,current_constitution, 
         total_words, legislative, executive, judicial) %>% #selecionando as variáveis de interesse
  left_join(States_and_Abbreviations, constituicoes_poderes, by = "state") %>% #criando uma nova coluna de abreviacoes a partir do data.frame States and Abbreviations
   mutate (poderes = (legislative + executive + judicial), #criando uma nova coluna com os valores somados das categorias executivo, legislativo e judiciário
          imp_l = (legislative/poderes), #criando o índice de proporcionalidade do legislativo em relação à soma dos poderes
          imp_e = (executive/poderes), #criando o índice de proporcionalidade do executivo em relação à soma dos poderes
          imp_j = (judicial/poderes)) #criando o índice de proporcionalidade do judiciário em relação à soma dos poderes

#TESTE DE CONFIANÇA - Alpha de Cronbach

tabela_para_alpha <- constituicoes_poderes %>% #criando um novo objeto para o cálculo do alpha
  select(imp_l, imp_e, imp_j) %>% #selecionadno as variáveis com os valores do índice 
  psych::alpha(tabela_para_alpha, check.keys = TRUE) #aplicando a função alpha de Cronbach, já definida pelo pacote psych 

# GRÁFICO 1 - Poderes por ano constitucional

constituicoes_poderes %>%
  ggplot(aes(x = constitution_year,
             y = poderes)) + #determinando os eixos x e y
  geom_point(aes(color = abbreviation)) + #determinando a cor por abreviacao/estado
  theme_classic() #deixando clean


# GRAFÍCOS 2 - Índices Legislativo, Executivo e Judiciário por ano constitucional e estado

constituicoes_poderes %>%
  ggplot(aes(x = constitution_year,
             y = imp_l)) + #determinando os eixos x e y
  geom_line(aes(color = "red")) + #determinando a cor por abreviacao/estado
  geom_point(color = "red") +
  facet_wrap(~ abbreviation) + #separando por estado
  theme_classic() + #deixando clean
  labs(title = "IMP do Legislativo por Ano Constitucional", #determinando os labels
     y = "IMP_L", x="Constitution_Year")

constituicoes_poderes %>%
  ggplot(aes(x = constitution_year,
             y = imp_e)) + #determinando os eixos x e y
  geom_line(aes(color = "red")) + #determinando a cor por abreviacao/estado
  geom_point(color = "red") +
  facet_wrap(~ abbreviation) + #separando por estado
  theme_classic() + #deixando clean
  labs(title = "IMP do Executivo por Ano Constitucional", #determinando os labels
       y = "IMP_E", x="Constitution_Year")


constituicoes_poderes %>%
  ggplot(aes(x = constitution_year,
             y = imp_j)) + #determinando os eixos x e y
  geom_line(aes(color = "red")) + #determinando a cor por abreviacao/estado
  geom_point(color = "red") +
  facet_wrap(~ abbreviation) + #separando por estado
  theme_classic() + #deixando clean
  labs(title = "IMP do Judiciário por Ano Constitucional", #determinando os labels
       y = "IMP_J", x="Constitution_Year")



# GRÁFICO 2 - Poderes por ano constitucional separado por estado

constituicoes_poderes %>%
  ggplot(aes(constitution_year, poderes)) + #selecionando os eixos ano da constituicao e da variavel de interesse (poderes)
  geom_line(color = "steelblue", size = 1) + #determinando a cor e tamanho da linha
  geom_point(color = "steelblue") + #determinando a cor do ponto
  labs(title = "Powerss by Constitution Year", #determinando os labels
       y = "Powers", x="Constitution_Year") +
  facet_wrap(~ abbreviation) + #separando por estado
  theme_classic() #deixando mais clean

constituicoes_poderes %>%
  ggplot(aes(constitution_year, imp_l)) + #selecionando os eixos ano da constituicao e da variavel de interesse (imp_l)
  geom_line(color = "steelblue", size = 1) + #determinando a cor e tamanho da linha
  geom_point(color = "steelblue") + #determinando a cor do ponto
  labs(title = "Powerss by Constitution Year", #determinando os labels
       y = "Powers", x="Constitution_Year") +
  facet_wrap(~ abbreviation) + #separando por estado
  theme_classic() #deixando mais clean
