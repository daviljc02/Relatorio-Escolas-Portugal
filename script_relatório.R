####################################ISABELLA###################################
library(readr)
dados_portugues_grupo9 <- read_csv("dados_portugues_grupo9.csv")
dados <-dados_portugues_grupo9
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("expss")
library(expss)
library(dplyr)

novo_vetor <- dados %>%
  select( schoolsup, famsup,failures, G3)
dados2=novo_vetor |> select(schoolsup,famsup,G3)
dados2=dados2 |> rename(Sup.Escolar=schoolsup,
                        Sup.Familiar.Escolar=famsup,
                        Notas=G3)#renomeando a base de dados e selecionando as variáveis utilizadas


dados2$Sup.Escolar= as.factor(dados$schoolsup)
dados2$Sup.Familiar.Escolar= as.factor(dados$famsup)#transformando em fatores


dados2$Sup.Familiar.Escolar=factor(x=dados2$Sup.Familiar.Escolar,
                                   levels = c("no","yes"),
                                   labels = c("Não","Sim"))#transformando em fatores
dados2$Sup.Escolar=factor(x=dados2$Sup.Escolar,
                          levels = c("no","yes"),
                          labels = c("Não","Sim"))#transformando em fatores

#média das notas
dados_tibble <- tibble(Notas = dados2$Notas)
mean(dados_tibble$Notas, na.rm = TRUE)
#11.89687

#alunos sem suporte escolar
sum(dados2$Sup.Escolar == "Não")
#572

#alunos com suporte escolar
sum(dados2$Sup.Escolar == "Sim")
#68

#alunos que não recebem suporte em casa para com a escola
sum(dados2$Sup.Familiar.Escolar == "Não")
#247

#alunos que recebem suporte em casa para com a escola
sum(dados2$Sup.Familiar.Escolar == "Sim")
#393

#média de notas de alunos que tiveram suporte escolar
sup_escolar<- dados2 %>% filter(Sup.Escolar== "Sim")
mean(sup_escolar$Notas)
#11.27941

#média de notas de alunos que não tiveram suporte escolar 
sup_escolar<- dados2 %>% filter(Sup.Escolar== "Não")
mean(sup_escolar$Notas)
#11.97028

#média de notas de pessoas que não tiveram ajudas dos pais nos estudos 
sup_familiar_escolar<- dados2 %>% filter(Sup.Familiar.Escolar== "Não")
mean(sup_familiar_escolar$Notas)
#11.66802

#média de notas de pessoas que tiveram ajuda dos pais nos estudos
sup_familiar_escolar<- dados2 %>% filter(Sup.Familiar.Escolar== "Sim")
mean(sup_familiar_escolar$Notas)
#12.04071

library(ggplot2)

dados2 |>
  select(Sup.Familiar.Escolar, Notas) |>
  ggplot(mapping = aes(y = Notas, x = Sup.Familiar.Escolar)) +
  geom_boxplot(fill = "white") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "black") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5, 
    color = "black"
  ) +
  theme_classic() +
  xlab("Tem Suporte Familiar") +
  ylab("Notas")#fazendo o bloxplot


dados2 |> 
  select(Sup.Escolar,Notas) |> 
  ggplot(mapping = aes(y=Notas, x=Sup.Escolar))+
  geom_boxplot(fill = "white")+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "black") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5, 
    color = "black"
  ) +
  theme_classic() +
  xlab("Tem Suporte Escolar")#fazendo o bloxplot

#com a analise dos gráficos podemos perceber que há pouca ou nenhuma relação entre o 
#que o aluno tem de suporte com as suas notas


####associaçao apoio escolar x notas



# Calcular a variância das Notas
Var_Notas <- var(novo_vetor$G3)

# Resumir os dados
resumo <- novo_vetor %>%
  group_by(schoolsup) %>%
  summarise(
    n = n(),
    Var = var(G3)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) * 100


R2
# 0.3432353



######associaçao apoio familiar x notas

# Calcular a variância das Notas
Var_Notas <- var(novo_vetor$G3)

# Resumir os dados
resumo <- novo_vetor %>%
  group_by(famsup) %>%
  summarise(
    n = n(),
    Var = var(G3)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas)*100 


R2
# 0.1424124



#Desvios
desvios_sup_e <-dados2 %>%
  group_by(Sup.Escolar) %>%
  summarise(desvio_padrao = sd(Notas))
desvios_sup_e

desvios_sup_f <-dados2 %>%
  group_by(Sup.Familiar.Escolar) %>%
  summarise(desvio_padrao = sd(Notas))
desvios_sup_f


#################################################PAOLLA###########################
#trabalho_de_analise_de_dados
library(tidyverse)
library(dplyr)
library(expss)
library(ggplot2)

dados=read_delim(file ='dados_portugues_grupo9.csv')
data.frame(dados)
dados1=dados |> select (G3,school,sex,age,address,internet,studytime,traveltime,higher)
head(dados1)
dados1=dados1|> rename (Nota=G3,
                        Escola=school,
                        Sexo=sex,
                        Idade=age,
                        Endereco=address,
                        Internet=internet,
                        Tempo.de.estudo=studytime,
                        Tempo.deslocam=traveltime,
                        Quer.Ens.Sup=higher)

##ANÁLISES GERAIS

#NOTAS ACIMA DA MÉDIA GERAL ENTRE AS DUAS ESCOLAS 
#MAIOR DESEMPENHO APARENTE DA ESCOLA GP-> 68.4% DOS ALUNOS QUE TIRARAM UMA NOTA ACIMA DA MÉDIA GERAL SÃO DESSA ESCOLA
dados1 %>% filter(Nota >"11.89") %>% select (Escola) %>% fre()
# |      . | Count | Valid percent | Percent | Responses, % | Cumulative responses, % |
#  | ------ | ----- | ------------- | ------- | ------------ | ----------------------- |
#  |     GP |   290 |          68.4 |    68.4 |         68.4 |                    68.4 |
#  |     MS |   134 |          31.6 |    31.6 |         31.6 |                   100.0 |
#  | #Total |   424 |         100.0 |   100.0 |        100.0 |                         |


calc.media.gp=dados1 %>% filter (Escola=="GP") 
mean(calc.media.gp$Nota)
#media de notas da escola GP= 12.57

calc.media.ms=dados1 %>% filter (Escola=="MS")
mean(calc.media.ms$Nota)
#media de notas da escola MS= 10.65

#de acordo com a média de GP:
calc.media.gp %>% filter(Nota >"12.57")%>%select(Idade) %>%fre()
#238 notas acima da média (57.5%)

calc.media.gp %>% filter(Nota <"12.57")%>%select(Idade) %>%fre()
#176 notas abaixo da média (42.5%)


#de acordo com a média de MS:
calc.media.ms %>% filter(Nota >"10.65")%>%select(Idade) %>%fre()
#168 notas acima da média (74.4%)

calc.media.ms %>% filter(Nota <"10.65")%>%select(Idade) %>%fre()
#58 notas acima da média (25,6%)





#relacao de endereço com notas (a media de nota dos alunos da regiao urbana é maior que a dos alunos da zona rural)
notarural = dados1 %>% filter(Endereco == "R")
mean(notarural$Nota)
#11.05155

notaurbano=dados1 %>% filter(Endereco == "U")
mean(notaurbano$Nota)
# 12.26457





#relaçao de sexo com notas(media de notas das mulheres maior que a dos homens)
notafem  <- dados1 %>% filter(Sexo== "F")
mean(notafem$Nota)
#12.24339
notamasc<- dados1 %>% filter(Sexo == "M")
mean(notamasc$Nota) 
#11.39695




#relaçao de internet com notas (é perceptivel que a media de quem tem internet é maior que quem não tem)
media_por_internet <-dados1 %>% group_by(Internet) %>% summarize(media_Nota=mean(Nota,na.rm = TRUE))
View(media_por_internet)
# não tem internet
#11.00671
#tem internet
#12.16701





#relaçao de lugar que mora e se tem internet: é visivel que a maioria das pessoas que moram na regiao urbana tem internet, o que colabora para um maior desempenho dessas pessoas na prova.Por outro lado,uma parte significativadas pessoas que vivem na zona rural não tem internet, e isso afeta parte do desempenho deles na prova

dados_urbano <- dados1 %>%
  filter(Endereco == "U")


# Contar o número de pessoas sem internet na região urbana
sem_internet_urbano <- sum(dados_urbano$Internet == "no")

# Calcular a porcentagem de pessoas sem internet na região urbana
porcentagem_sem_internet_urbano <- (sem_internet_urbano / nrow(dados_urbano)) * 100

# Repetir o mesmo processo para a zona rural
dados_rural <- dados1 %>%
  filter(Endereco == "R")

sem_internet_rural <- sum(dados_rural$Internet == "no")
porcentagem_sem_internet_rural <- (sem_internet_rural / nrow(dados_rural)) * 100

# Exibir os resultados
cat("Porcentagem de pessoas sem internet na região urbana:", porcentagem_sem_internet_urbano, "%\n")
#18.38565 %
cat("Porcentagem de pessoas sem internet na zona rural:", porcentagem_sem_internet_rural, "%\n")
# 34.53608 %


# Filtrar os dados para região urbana
dados_urbano <- dados1 %>%
  filter(Endereco == "U")

# Contar o número de pessoas com internet na região urbana
com_internet_urbano <- sum(dados_urbano$Internet == "yes")

# Calcular a porcentagem de pessoas com internet na região urbana
porcentagem_com_internet_urbano <- (com_internet_urbano / nrow(dados_urbano)) * 100

# Repetir o mesmo processo para a zona rural
dados_rural <- dados1 %>%
  filter(Endereco == "R")

com_internet_rural <- sum(dados_rural$Internet == "yes")
porcentagem_com_internet_rural <- (com_internet_rural / nrow(dados_rural)) * 100

# Exibir os resultados
cat("Porcentagem de pessoas com internet na região urbana:", porcentagem_com_internet_urbano, "%\n")
# 81.61435 %
cat("Porcentagem de pessoas com internet na zona rural:", porcentagem_com_internet_rural, "%\n")
# 65.46392 %






##ANÁLISES MAIS ESPECÍFICAS



#relaçao de tempo de deslocamento com notas (aparentemente quanto maior o tempo de deslocamento,menor a média de notas dos alunos,mas de acordo com o boxplot,é perceptível a variação da média de notas dentro de cada grupo,mas de forma global essa variação não é relevante. 

dados1$Tempo.deslocam = as.factor(dados1$Tempo.deslocam)
tempdesloc1<- dados1 %>% filter(Tempo.deslocam== "1")
mean(tempdesloc1$Nota)
#12.24444


tempdesloc2<- dados1 %>% filter(Tempo.deslocam== "2")
mean(tempdesloc2$Nota)
#11.56667

tempdesloc3<- dados1 %>% filter(Tempo.deslocam== "3")
mean(tempdesloc3$Nota)
# 11.16667

tempdesloc4<- dados1 %>% filter(Tempo.deslocam== "4")
mean(tempdesloc4$Nota)
# 10.875 


dados1 |> 
  select(Tempo.deslocam,Nota) |> 
  ggplot(mapping = aes(y=Nota, x=Tempo.deslocam))+
  geom_boxplot(fill = "white")+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "black") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5, 
    color = "black"
  ) +
  theme_classic() + # Escolher um tema básico
  xlab("Tempo de Deslocamento")




#relacao de tempo de estudo com notas(aparentemente quanto maior o tempo de estudo,maior a media de nota dos alunos,VERIFICAR COM BOXPLOT)
dados1$Tempo.de.estudo = as.factor(dados1$Tempo.de.estudo)

tempoestudo1 <- dados1 %>% filter(Tempo.de.estudo == "1")
tempoestudo2 <- dados1 %>% filter(Tempo.de.estudo == "2") 
tempoestudo3=dados1 %>% filter (Tempo.de.estudo=="3")
tempoestudo4 <- dados1 %>% filter(Tempo.de.estudo == "4")

mean(tempoestudo1$Nota) 
# 10.85308
mean(tempoestudo2$Nota)
# 12.07309
mean(tempoestudo3$Nota)
# 13.24468
mean(tempoestudo4$Nota)
# 13.08824

dados1 |> 
  select(Tempo.de.estudo,Nota) |> 
  ggplot(mapping = aes(y=Nota, x=Tempo.de.estudo))+
  geom_boxplot(fill = "white")+
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "black") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5, 
    color = "black"
  ) +
  theme_classic() + # Escolher um tema básico
  xlab("Tempo de Estudo") # Definir os rótulos no eixo x

#é notório no gráfico do boxplot uma ligeira variação da média de notas conforme o aumento do tempo de estudo dos alunos




##Relação entre Tempo de deslocamento e Tempo de estudo
# Gráfico de barras 
# AO ANALISAR SE AS PESSOAS QUE LEVAM MAIS TEMPO PARA SE DESLOCAR DE CASA ATÉ A ESCOLA POSSUEM MENOS TEMPO DE ESTUDO,PERCEBEMOS QUE ESSAS VARIÁVEIS NÃO TÊM RELAÇÃO RELEVANTE,MOSTRANDO NO GRÁFICO QUE MESMO TENDO POUCO TEMPO DE DESLOCAMENTO ,MUITAS PESSOAS CONTINUAM TENDO POUCO TEMPO DE ESTUDO)
#grafico_barras <- ggplot(dados1, aes(x =dados1$Tempo.de.estudo,fill=dados1$Tempo.deslocam)) +
#geom_bar() +  # Usamos geom_col() ao invés de geom_bar()
#labs(title =  "Tempo de deslocamento em relação ao tempo de estudo",
#  x = "Tempo de estudo",
#y = "Contagem",
# fill="Tempo de deslocamento")

#grafico_barras  





#ANÁLISE DA RELAÇÃO DE INTERNET COM NOTA: 80.4% DOS ALUNOS QUE TIRARAM NOTA ACIMA DA MÉDIA GERAL DAS ESCOLAS TEM INTERNET
dados1$Internet=factor(dados1$Internet)
levels(dados1$Internet)

dados1 %>% select(Internet) %>% fre()

dados1 %>% filter(Nota >"11.89") %>% select(Internet)%>%  fre()

#TABELA DE PESSOAS QUE TIRARAM NOTA ACIMA DA MÉDIA GERAL (ENTRE AS DUAS ESCOLAS) EM RELAÇÃO A TER OU NÃO INTERNET
#|      . | Count | Valid percent | Percent | Responses, % | Cumulative responses, % |
# | ------ | ----- | ------------- | ------- | ------------ | ----------------------- |
# |     no |    83 |          19.6 |    19.6 |         19.6 |                    19.6 |
# |    yes |   341 |          80.4 |    80.4 |         80.4 |                   100.0 |
# | #Total |   424 |         100.0 |   100.0 |        100.0 |                         |
#|   <NA> |     0 |               |     0.0 |              |                         |

mean(dados1$Nota)
#11.89 


####associação internet x nota

# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Internet) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#2.117495

####associação internet x nota

# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Internet) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#2.782024

###assosiaçao tempo de estudo x nota

# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Tempo.de.estudo) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#6.411851
#associaçao tempo deslocam x nota
# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Tempo.deslocam) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#1.278773

#associaçao Escola x nota
# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Escola) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#7.892937

#associaçao tempo ensino superior x nota
# Calcular a variância das Notas
Var_Notas <- var(dados1$Nota)

# Resumir os dados
resumo <- dados1 %>%
  group_by(Quer.Ens.Sup) %>%
  summarise(
    n = n(),
    Var = var(Nota)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas) *100


R2
#10.91164



#Desvios

desvios_local <-dados1 %>%
  group_by(Endereco) %>%
  summarise(desvio_padrao = sd(Nota))

desvios_internet <-dados1 %>%
  group_by(Internet) %>%
  summarise(desvio_padrao = sd(Nota))

desvios_temp_deslocam <-dados1 %>%
  group_by(Tempo.deslocam) %>%
  summarise(desvio_padrao = sd(Nota))

desvios_temp_est <-dados1 %>%
  group_by(Tempo.de.estudo) %>%
  summarise(desvio_padrao = sd(Nota))



desvios_escola <-dados1 %>%
  group_by(Escola) %>%
  summarise(desvio_padrao = sd(Nota))

desvio_global <-dados1 %>% summarise(Variancia=var(Nota),Dp=sd(Nota))


desvios_sex <-dados1 %>%
  group_by(Sexo) %>%
  summarise(desvio_padrao = sd(Nota))



desvios_ens_sup<-dados1 %>%
  group_by(Quer.Ens.Sup) %>%
  summarise(desvio_padrao = sd(Nota))


desvios_pago<-dados%>%
  group_by(paid) %>%
  summarise(desvio_padrao = sd(G3))


#######resumo escola#############
resumo_escola <-dados1 %>% group_by(Escola) %>% summarize(media_Nota=mean(Nota,na.rm = TRUE))
View(resumo_escola)

Escolagp<- dados1%>% filter(Escola == "GP")

resumo_ens_sup <-dados1 %>% group_by(Quer.Ens.Sup) %>% summarize(media_Nota=mean(Nota,na.rm = TRUE))

table(dados1$Quer.Ens.Sup)
#no yes 
#67 573 


#########################################################JULIANA###################

#trabalho_de_analise_de_dados
library(tidyverse)
library(dplyr)
library(expss)
library(ggplot2)

#setwd("C:/Users/Administrator/Documents/UFF/IA/TRAB1")
dados <- read_csv("dados_portugues_grupo9.csv")
data.frame(dados)
head(dados)
#a base de dados foi importada com um cabeçalho único, então tive que separa-lo
dados<-read.csv("dados_portugues_grupo9.csv", header=TRUE, sep =",")

dados1=dados |> select (G3,school,Walc,Dalc,goout,freetime,romantic,health,studytime)




#RELAÇÃO DO CONSUMO DE ALCOOL AOS FDS COM NOTAS
#(Queda progressiva da média com o aumento do consumo de alcool aos finais de semana)

notaWalc1 = dados1 %>% filter(Walc == "1")
mean(notaWalc1$G3)
#12.36364

notaWalc2 = dados1 %>% filter(Walc == "2")
mean(notaWalc2$G3)
#12.26174

notaWalc3 = dados1 %>% filter(Walc == "3")
mean(notaWalc3$G3)
#11.66102

notaWalc4 = dados1 %>% filter(Walc == "4")
mean(notaWalc4$G3)
#10.97674

notaWalc5 = dados1 %>% filter(Walc == "5")
mean(notaWalc5$G3)
#10.55556


#Criando o boxplot entre consumo de alcool aos fds x notas

labels <- c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")
dados1$Walc <- factor(dados1$Walc, levels = 1:5, labels = labels)
ggplot(dados1, aes(x = Walc, y = G3)) +
  geom_boxplot() +
  stat_summary(fun=mean,geom = "point",shape=20,size=3, color="black")
labs(x = "Consumo de Álcool aos Finais de Semana", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels)  





#RELAÇÃO ENTRE CONSUMO DIÁRIO DE ÀLCOOL E NOTAS
#não achei relação clara

notaDalc1 = dados1 %>% filter(Dalc == "1")
mean(notaDalc1$G3)
#12.29505

notaDalc2 = dados1 %>% filter(Dalc == "2")
mean(notaDalc2$G3)
#11.38333

notaDalc3 = dados1 %>% filter(Dalc == "3")
mean(notaDalc3$G3)
#11.02381

notaDalc4 = dados1 %>% filter(Dalc == "4")
mean(notaDalc4$G3)
#8.941176

notaDalc5 = dados1 %>% filter(Dalc == "5")
mean(notaDalc5$G3)
#10.23529


#Achei estranho o valor progressivamente cair e no nível 5 a média subir. 
#Resolvi ver se talvez o grupo 5 tivesse poucas pessoas e talvez a média de alguns influenciasse 
#mais no valor final
# Contando quantas pessoas se enquadram em cada nível de consumo diário de álcool (Dalc)
counts <- table(dados1$Dalc)
print(counts)
#Muito Baixo       Baixo    Moderado     Alto    Muito Alto 
#    444            120          42       17          17 

#Então resolvi fazer um boxplot procuranto por outliers no grupo de alto consumo(5)
#que pudessem puxar a média para cima. Encontrei um outlier com nota 16
print(head(notaDalc5))
ggplot(notaDalc5, aes(x = factor(Dalc), y = G3)) +
  geom_boxplot() +
  labs(x = "Consumo de Álcool diário (Muito Alto)", y = "Notas") +
  theme_minimal() +
  scale_x_discrete(labels = "Muito Alto")

#excluí o outlier e recalculei a média do grupo 5 Dalc sem ele


# Calculando o primeiro e terceiro quartil e o IQR
Q1 <- quantile(notaDalc5$G3, 0.25)
Q3 <- quantile(notaDalc5$G3, 0.75)
IQR <- Q3 - Q1

# Definindo os limites para identificar os outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Removendo os outliers
notaDalc5_no_outliers <- notaDalc5 %>%
  filter(G3 >= lower_bound & G3 <= upper_bound)

# Calculando a média sem os outliers
mean_no_outliers <- mean(notaDalc5_no_outliers$G3)
print(paste("Média sem outliers:", mean_no_outliers))
#"Média sem outliers: 9.78571428571429"

#A média deu mais baixa após retirar o outlier do grupo 5, mas ainda deu mais alta que a média do grupo 4
#como a queda da média do grupo 3 para o grupo 4 foi muito mais significativa que do 1 pro 2 e do 2 pro 3,
#resolvi avaliar se não tinha algum outlier puxando a média do grupo 4 pra baixo. E TINHA:
#segue o boxplot:
ggplot(notaDalc4, aes(x = factor(Dalc), y = G3)) +
  geom_boxplot() +
  labs(x = "Consumo de Álcool diário (Alto)", y = "Notas") +
  theme_minimal() +
  scale_x_discrete(labels = "Alto")

#Fiz o mesmo processo de retirar os outliers e recalcular a média


# Calculando o primeiro e terceiro quartil e o IQR
Q1 <- quantile(notaDalc4$G3, 0.25)
Q3 <- quantile(notaDalc4$G3, 0.75)
IQR <- Q3 - Q1

# Definindo os limites para identificar os outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Removendo os outliers
notaDalc4_no_outliers <- notaDalc4 %>%
  filter(G3 >= lower_bound & G3 <= upper_bound)

# Calculando a média sem os outliers
mean_no_outliers <- mean(notaDalc4_no_outliers$G3)
print(paste("Média sem outliers:", mean_no_outliers))
#"Média sem outliers: 11.6153846153846"


#Então fui fazer com o grupo 3 também, para ver se, retirando os outliers, encontrariamos
#alguma associação clara, mas não aconteceu ://

# Calculando o primeiro e terceiro quartil e o IQR
Q1 <- quantile(notaDalc3$G3, 0.25)
Q3 <- quantile(notaDalc3$G3, 0.75)
IQR <- Q3 - Q1

# Definindo os limites para identificar os outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Removendo os outliers
notaDalc3_no_outliers <- notaDalc3 %>%
  filter(G3 >= lower_bound & G3 <= upper_bound)

# Calculando a média sem os outliers
mean_no_outliers <- mean(notaDalc3_no_outliers$G3)
print(paste("Média sem outliers:", mean_no_outliers))
#"Média sem outliers: 10.7" -> Média menor que do grupo 4://


#Criando o boxplot entre consumo diário de álcool x notas


labels <- c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")
dados1$Dalc <- factor(dados1$Dalc, levels = 1:5, labels = labels)
ggplot(dados1, aes(x = Dalc, y = G3)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black", fill = "black") +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = round(after_stat(y), 2)),
    vjust = -0.5, 
    color = "black"
  )+
  labs( x = "Consumo de Álcool Diário", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels)  






#RELAÇÃO ENTRE NAMORAR E NOTAS 
#os que não namoram tem nota ligeiramente superior

Notanamoram = dados1 %>% filter(romantic == "yes")
mean(Notanamoram$G3)
#11.49787

NotaNAOnamoram = dados1 %>% filter(romantic == "no")
mean(NotaNAOnamoram$G3)
#12.1284


#Criando o boxplot do status de relacionamento x notas

labels <- c("Não Namoram", "Namoram")
dados1$romantic <- factor(dados1$romantic, labels = labels)
ggplot(dados1, aes(x = romantic, y = G3)) +
  geom_boxplot() +
  labs(x = "Relacionamento", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels)  

#contando romantic
counts <- table(dados1$romantic)
print(counts)
# no yes 
#405 235






#RELAÇÃO ENTRE SAIR COM AMIGOS E NOTAS

notagoout1 = dados1 %>% filter(goout == "1")
mean(notagoout1$G3)
#10.70213

notagoout2 = dados1 %>% filter(goout == "2")
mean(notagoout2$G3)
#12.66901

notagoout3 = dados1 %>% filter(goout == "3")
mean(notagoout3$G3)
#12.14356

notagoout4 = dados1 %>% filter(goout == "4")
mean(notagoout4$G3)
#11.99286

notagoout5 = dados1 %>% filter(goout == "5")
mean(notagoout5$G3)
#10.82569


#Criando boxplot sair com amigos x notas
labels <- c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")
dados1$goout <- factor(dados1$goout, levels = 1:5, labels = labels)
ggplot(dados1, aes(x = goout, y = G3)) +
  geom_boxplot() +
  labs(x = "Sair com os amigos", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels) 

#contando goout
counts <- table(dados1$goout)
print(counts)
#Muito Baixo   Baixo    Moderado    Alto    Muito Alto 
#     47         142         202     140         109





#RELAÇÃO ENTRE TEMPO LIVRE DEPOIS DA ESCOLA E NOTAS
#sem relação aparente

notafreetime1 = dados1 %>% filter(freetime == "1")
mean(notafreetime1$G3)
#11.65909

notafreetime2 = dados1 %>% filter(freetime == "2")
mean(notafreetime2$G3)
#12.68269

notafreetime3 = dados1 %>% filter(freetime == "3")
mean(notafreetime3$G3)
#12.06048

notafreetime4 = dados1 %>% filter(freetime == "4")
mean(notafreetime4$G3)
#11.72727

notafreetime5 = dados1 %>% filter(freetime == "5")
mean(notafreetime5$G3)
#10.69118

#Criando boxplot tempo livre após a aula x notas
labels <- c("Muito Pouco", "Pouco", "Moderado", "Alto", "Muito Alto")
dados1$freetime <- factor(dados1$freetime, levels = 1:5, labels = labels)
ggplot(dados1, aes(x = freetime, y = G3)) +
  geom_boxplot() +
  labs(x = "Tempo livre após a aula", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels)


#contagem freetime
counts <- table(dados1$freetime)
print(counts)
#1   2   3   4   5 
#44 104 248 176  68 







#ESTADO DE SAÚDE ATUAL X NOTAS
#sem relação aparente

notahealth1 = dados1 %>% filter(health == "1")
mean(notafreetime1$G3)
#11.65909

notahealth2 = dados1 %>% filter(health == "2")
mean(notahealth2$G3)
#12.16

notahealth3 = dados1 %>% filter(health == "3")
mean(notahealth3$G3)
#11.83471

notahealth4 = dados1 %>% filter(health == "4")
mean(notahealth4$G3)
#12.33019

notahealth5 = dados1 %>% filter(health == "5")
mean(notahealth5$G3)
#11.46988


#contando health
counts <- table(dados1$health)
print(counts)
#  1  2   3    4   5 
# 89 75  121  106 249 


#Criando boxplot Estado Atual de Saúde x notas
labels <- c("Muito Ruim", "Ruim", "Moderado", "Bom","Muito Bom") 
dados1$health <- factor(dados1$health, levels = 1:5, labels = labels)
ggplot(dados1, aes(x = health, y = G3)) +
  geom_boxplot() +
  labs(x = "Estado Atual de Saúde", y = "Notas") +
  theme_minimal() +  
  scale_x_discrete(labels = labels)  

#Desvios

desvios_al_diario <-dados1 %>%
  group_by(Dalc) %>%
  summarise(desvio_padrao = sd(G3))

desvios_al_sem <-dados1 %>%
  group_by(Walc) %>%
  summarise(desvio_padrao = sd(G3))

desvios_relacionamento <-dados1 %>%
  group_by(romantic) %>%
  summarise(desvio_padrao = sd(G3))

desvios_temp_livre <-dados1 %>%
  group_by(freetime) %>%
  summarise(desvio_padrao = sd(G3))

desvios_saude <-dados1 %>%
  group_by(health) %>%
  summarise(desvio_padrao = sd(G3))

desvios_saidas <-dados1 %>%
  group_by(goout) %>%
  summarise(desvio_padrao = sd(G3))

#Grau de associaçao DIARIO
Var_Notas <- var(dados1$G3)

# Resumir os dados
resumo <- dados1%>%
  group_by(Dalc) %>%
  summarise(
    n = n(),
    Var = var(G3)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas)*100 


R2
#4.149981

###############ASSOCIAÇAO SEMANAL
Var_Notas <- var(dados1$G3)

# Resumir os dados
resumo <- dados1%>%
  group_by(Walc) %>%
  summarise(
    n = n(),
    Var = var(G3)
  )

#variância ponderada
Var_Barra <- sum(resumo$n * resumo$Var) / sum(resumo$n)

#r^2
R2 <- (1 - Var_Barra / Var_Notas)*100 


R2
#2.827569


###################analise nota zero###########
#trabalho_de_analise_de_dados
library(tidyverse)
library(dplyr)
library(expss)
library(ggplot2)

#setwd("C:/Users/Administrator/Documents/UFF/IA/TRAB1")
dados <- read_csv("dados_portugues_grupo9.csv")
data.frame(dados)
head(dados)
#a base de dados foi importada com um cabeçalho único, então tive que separa-lo
dados<-read.csv("dados_portugues_grupo9.csv", header=TRUE, sep =",")

#ANALISE ALUNOS NOTA ZERO

dados$schoolsup=factor(x=dados$schoolsup,
                       levels = c("no","yes"),
                       labels = c("Não","Sim"))

dados$paid=factor(x=dados$paid,
                  levels = c("no","yes"),
                  labels = c("Não","Sim"))

alunosnota0<-subset(dados, G3==0)
head(alunosnota0)
summary(alunosnota0)

count_(alunosnota0)
# 15 alunos



# Tabela resumo para a variável "school"
#TODOS SAO DA ESCOLA MS MENOS 1S 
school_summary <- as.data.frame(table(alunosnota0$school))
colnames(school_summary) <- c("school", "Freq")
print(school_summary)
#School Freq
#     GP    1
#     MS   14


# Tabela resumo para a variável "studytime"
#TODOS OS ALUNOS NOTA ZERO TEM 1 OU 2 DE STUDYTIME
studytime_summary <- as.data.frame(table(alunosnota0$studytime))
colnames(studytime_summary) <- c("studytime", "Freq")
print(studytime_summary)
#  studytime Freq
#         1    8
#         2    7


# Tabela resumo para a variável "schoolsup"
#SÓ 1 TEM SCHOOLSUP
schoolsup_summary <- as.data.frame(table(alunosnota0$schoolsup))
colnames(schoolsup_summary) <- c("schoolsup", "Freq")
print(schoolsup_summary)
# schoolsup Freq
#        no   14
#       yes    1


# Tabela resumo para a variável "paid"
#SÓ 1 TEM PAID
paid_summary <- as.data.frame(table(alunosnota0$paid))
colnames(paid_summary) <- c("paid", "Freq")
print(paid_summary)
#  paid Freq
#   no   14
#  yes    1



bar_width <- 0.3


# Gráfico de barras para a variável "school"
#ggplot(data = school_summary, aes(x = school, y = Freq)) +
#geom_bar(stat = "identity", width = bar_width) +
# labs(title = "Distribuição de Alunos com Nota Zero por Escola", x = "Escola", y = "Frequência") +
# theme_minimal()+
ggplot(data = school_summary, aes(x = school, y = Freq)) +
  geom_bar(stat = "identity", width = bar_width) +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = "Escola", y = "Frequência") +
  theme_minimal()

# Gráfico de barras para a variável "studytime"

ggplot(data = studytime_summary, aes(x = studytime, y = Freq)) +
  geom_bar(stat = "identity", width = bar_width) +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = "Tempo de Estudo", y = "Frequência") +
  theme_minimal()

# Gráfico de barras para a variável "schoolsup"
ggplot(data = schoolsup_summary, aes(x = schoolsup, y = Freq)) +
  geom_bar(stat = "identity", width = bar_width) +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3.5) +
  labs( x = "Suporte Escolar", y = "Frequência") +
  theme_minimal()


# Gráfico de barras para a variável "paid"
ggplot(data = paid_summary, aes(x = paid, y = Freq)) +
  geom_bar(stat = "identity", width = bar_width) +
  geom_text(aes(label = Freq), vjust = -0.5, color = "black", size = 3.5) +
  labs(x = "Pagou por Aulas Extras", y = "Frequência") +
  theme_minimal()

