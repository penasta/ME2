#-------------------------------------------------------------------------------

# Atividade 3.3: Testes de aderência: Kolmogorov, Shapiro-Wilk e Anderson- Darling 

#-------------------------------------------------------------------------------
# 0.1: Carregando pacotes

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,nortest)

#-------------------------------------------------------------------------------
# 0.2: Seed

# Meu código é feito para cada vez executado, todas as amostras são geradas aleatoriamente
# e cada vez executado produzirá um resultado único. Caso deseje fixar a seed, rode o comando abaixo:

# set.seed(*insira um inteiro aqui*)

# Caso tenha inserido uma seed mas queira voltar a aleatoriedade, execute o comando abaixo:

# set.seed(NULL)

#-------------------------------------------------------------------------------

# 1. Testes de Kolmogorov para normalidade

#-------------------------------------------------------------------------------

# Gere 1000 (mil) amostras de tamanho 15 de uma variável normalmente distribuída com média μ e desvio padrão σ.
## A média μ deverá ser gerada aleatoriamente no intervalo entre 100 e 200
## Para o cálculo do desvio padrão considere um CV igual a 10%.

amostras <- list()
medias <- list()
desvios <- list()
for (i in 1:1000) {
  media <- sample(100:200, size=1)
  s <- 10*media 
  amostras[[i]] <- rnorm(15, mean = media, sd = s)
  medias[[i]] <- media
  desvios[[i]] <- s
}

#-------------------------------------------------------------------------------

# Para cada amostra, obtenha a Estatística de Teste, tipo Kolmogorov (considere teste bicaudal)

#-------------------------------------------------------------------------------

## CASO 1: considere que a Função de distribuição Normal está totalmente especificada, ou seja, 
#utilize os valores da média e desvio padrão considerados para a geração das amostras

amostras2 <- list()
for (i in 1:1000) {
  media <- medias[[i]]
  s <- desvios[[i]]
  amostras2[[i]] <- rnorm(15, mean = media, sd = s)
}

testes <- list()
for(i in 1:1000) {
  testes[[i]] <- ks.test(amostras[[i]],amostras2[[i]])
}

#-------------------------------------------------------------------------------

## CASO 2: considere que a Função de distribuição Normal não foi especificada, 
#e você deverá estimar a média e o desvio padrão a partir dos dados amostrais.

amostras3 <- list()
for (i in 1:1000) {
  media <- mean(amostras[[i]])
  s <- sd(amostras[[i]])
  amostras3[[i]] <- rnorm(15, mean = media, sd = s)
}

testes2 <- list()
for(i in 1:1000) {
  testes2[[i]] <- ks.test(amostras[[i]],amostras3[[i]])
}

#-------------------------------------------------------------------------------

## Você terá dois conjuntos de 1000 estimativas da estatística de teste, tipo Kolmogorov

# testes
# testes2

#-------------------------------------------------------------------------------

# Para cada conjunto de estimativas da estatística de teste, obtenha os quantis 
# 80%, 85%, 90%, 95%, 97,5%, 99%, e compare com os quantis tabelados 
# (Tabelas do Conover) segundo cada caso.

# valores tabelados p/ teste bicaudal p/ n=15
valortabelaconover80 <- 0.266
valortabelaconover90 <- 0.304
valortabelaconover95 <- 0.338
valortabelaconover98 <- 0.377
valortabelaconover99 <- 0.404

# Valores não tabelados pelo Conover, irei estimar tirando médias dos valores dados.

valortabelaconover85 <- (valortabelaconover80+valortabelaconover90)/2
valortabelaconover975 <- valortabelaconover95+(valortabelaconover95-valortabelaconover90)

#-------------------------------------------------------------------------------

# QUANTIS DOS TESTES DO CASO 1

# 80%
proptestescaso180 <- NA
for(i in 1:1000) {
proptestescaso180 <- c(proptestescaso180,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover80, 1,0))
}
proptestescaso180 <- proptestescaso180[2:1001]
proptestescaso180 <- sum(proptestescaso180)/1000
proptestescaso180


# 85%
valortabelaconover85 <- (valortabelaconover80+0.304)/2
proptestescaso185 <- NA
for(i in 1:1000) {
  proptestescaso185 <- c(proptestescaso185,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover85, 1,0))
}
proptestescaso185 <- proptestescaso185[2:1001]
proptestescaso185 <- sum(proptestescaso185)/1000
proptestescaso185


# 90%
proptestescaso190 <- NA
for(i in 1:1000) {
  proptestescaso190 <- c(proptestescaso190,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover90, 1,0))
}
proptestescaso190 <- proptestescaso190[2:1001]
proptestescaso190 <- sum(proptestescaso190)/1000
proptestescaso190


# 95%
proptestescaso195 <- NA
for(i in 1:1000) {
  proptestescaso195 <- c(proptestescaso195,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover95, 1,0))
}
proptestescaso195 <- proptestescaso195[2:1001]
proptestescaso195 <- sum(proptestescaso195)/1000
proptestescaso195

# 97,5%
proptestescaso1975 <- NA
for(i in 1:1000) {
  proptestescaso1975 <- c(proptestescaso1975,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover975, 1,0))
}
proptestescaso1975 <- proptestescaso1975[2:1001]
proptestescaso1975 <- sum(proptestescaso1975)/1000
proptestescaso1975

# 99%
proptestescaso199 <- NA
for(i in 1:1000) {
  proptestescaso199 <- c(proptestescaso199,ifelse(as.numeric(testes[[i]]$statistic) >= valortabelaconover99, 1,0))
}
proptestescaso199 <- proptestescaso199[2:1001]
proptestescaso199 <- sum(proptestescaso199)/1000
proptestescaso199

labels <- c("80%","85%","90%","95%","97,5%","99%")
valores <- c(proptestescaso180,proptestescaso185,proptestescaso190,proptestescaso195,proptestescaso1975,proptestescaso199)
proporcoesquantiscaso1 <- data.frame(valores,row.names = labels)
proporcoesquantiscaso1 <- t(proporcoesquantiscaso1)
proporcoesquantiscaso1 <- as.data.frame(proporcoesquantiscaso1)
rownames(proporcoesquantiscaso1) <- "Proporção"

# Proporção de testes dentre os 1000 cuja estatística de teste é igual ou maior
# que o valor tabelado:
proporcoesquantiscaso1

#-------------------------------------------------------------------------------

# QUANTIS DOS TESTES DO CASO 2

# 80%
proptestescaso280 <- NA
for(i in 1:1000) {
  proptestescaso280 <- c(proptestescaso280,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover80, 1,0))
}
proptestescaso280 <- proptestescaso280[2:1001]
proptestescaso280 <- sum(proptestescaso280)/1000
proptestescaso280


# 85%

proptestescaso285 <- NA
for(i in 1:1000) {
  proptestescaso285 <- c(proptestescaso285,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover85, 1,0))
}
proptestescaso285 <- proptestescaso285[2:1001]
proptestescaso285 <- sum(proptestescaso285)/1000
proptestescaso285


# 90%
proptestescaso290 <- NA
for(i in 1:1000) {
  proptestescaso290 <- c(proptestescaso290,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover90, 1,0))
}
proptestescaso290 <- proptestescaso290[2:1001]
proptestescaso290 <- sum(proptestescaso290)/1000
proptestescaso290


# 95%
proptestescaso295 <- NA
for(i in 1:1000) {
  proptestescaso295 <- c(proptestescaso295,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover95, 1,0))
}
proptestescaso295 <- proptestescaso295[2:1001]
proptestescaso295 <- sum(proptestescaso295)/1000
proptestescaso295

# 97,5%
proptestescaso2975 <- NA
for(i in 1:1000) {
  proptestescaso2975 <- c(proptestescaso2975,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover975, 1,0))
}
proptestescaso2975 <- proptestescaso2975[2:1001]
proptestescaso2975 <- sum(proptestescaso2975)/1000
proptestescaso2975

# 99%
proptestescaso299 <- NA
for(i in 1:1000) {
  proptestescaso299 <- c(proptestescaso299,ifelse(as.numeric(testes2[[i]]$statistic) >= valortabelaconover99, 1,0))
}
proptestescaso299 <- proptestescaso299[2:1001]
proptestescaso299 <- sum(proptestescaso299)/1000
proptestescaso299

valores2 <- c(proptestescaso280,proptestescaso285,proptestescaso290,proptestescaso295,proptestescaso2975,proptestescaso299)
proporcoesquantiscaso2 <- data.frame(valores2,row.names = labels)
proporcoesquantiscaso2 <- t(proporcoesquantiscaso2)
proporcoesquantiscaso2 <- as.data.frame(proporcoesquantiscaso2)
rownames(proporcoesquantiscaso2) <- "Proporção"

# Proporção de testes dentre os 1000 cuja estatística de teste é igual ou maior
# que o valor tabelado:
proporcoesquantiscaso2

#-------------------------------------------------------------------------------

# faça uma análise descritiva (apresente gráficos). Comente os resultados.

#-------------------------------------------------------------------------------
# Gráficos:

# Caso 1:
estatísticaskolmogorov1 <- NA
for (i in 1:1000){
  estatísticaskolmogorov1 <- c(estatísticaskolmogorov1,as.numeric(testes[[i]]$statistic))
}
estatísticaskolmogorov1 <- estatísticaskolmogorov1[2:1001]

# Histograma:
hist(estatísticaskolmogorov1
     ,main="Histograma estatísticas de Kolmogorov observadas - Caso 1"
     ,xlab="Valor da estatística de teste (0 a 1)"
     ,ylab="Frequência"
     )

# Boxplot:
boxplot(estatísticaskolmogorov1
        ,main="Boxplot estatísticas de Kolmogorov observadas"
        ,xlab="Caso 1"
        ,ylab="Valor da estatística de teste (0 a 1)"
        )

# Caso 2:
estatísticaskolmogorov2 <- NA
for (i in 1:1000){
  estatísticaskolmogorov2 <- c(estatísticaskolmogorov2,as.numeric(testes2[[i]]$statistic))
}
estatísticaskolmogorov2 <- estatísticaskolmogorov2[2:1001]

# Histograma:
hist(estatísticaskolmogorov2
     ,main="Histograma estatísticas de Kolmogorov observadas - Caso 2"
     ,xlab="Valor da estatística de teste (0 a 1)"
     ,ylab="Frequência"
     )

# Boxplot:
boxplot(estatísticaskolmogorov2
        ,main="Boxplot estatísticas de Kolmogorov observadas"
        ,xlab="Caso 2"
        ,ylab="Valor da estatística de teste (0 a 1)"
        )


#-------------------------------------------------------------------------------

# Análise descritiva e comentários: (...) Markdown

#-------------------------------------------------------------------------------
# Dando uma limpada no ambiente (objetos temporários)

rm(i,labels,media,proptestescaso180,proptestescaso185,proptestescaso190,
   proptestescaso195,proptestescaso1975,proptestescaso199,s,valores,
   valortabelaconover80,valortabelaconover85,valortabelaconover90,
   valortabelaconover95,valortabelaconover975,valortabelaconover98,
   valortabelaconover99,proptestescaso280,proptestescaso285,proptestescaso290,
   proptestescaso295,proptestescaso2975,proptestescaso299,valores2)

#-------------------------------------------------------------------------------

# 2. Testes para normalidade: Shapiro-Wilk e Anderson-Darling

#-------------------------------------------------------------------------------

# Escolha 5 amostras geradas anteriormente, e teste normalidade utilizando Shapiro-Wilk e Anderson-Darling.

#-------------------------------------------------------------------------------

testesSW <- list()
testesAD <- list()
valoressorteados <- 0
for (i in 1:5){
  
sorteada <- sample(1:1000,1)
# Apesar de improvável pelo número de possibilidades, quero me certificar que
# não selecionar e testar a mesma amostra mais de 1 vez:
if (sorteada %in% valoressorteados){
  sorteada <- sample(1:1000,1)
} else {
  valoressorteados <- c(valoressorteados,sorteada)
}

testesSW[[i]] <- shapiro.test(amostras[[sorteada]])
testesAD[[i]] <- ad.test(amostras[[sorteada]])

}

rm(valoressorteados,sorteada,i)

#-------------------------------------------------------------------------------

# Compare com os resultados anteriores e comente os resultados

