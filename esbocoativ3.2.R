#------------------------------------------------------------------------------

# Atividade 3.2 - Análise de dados: testes de aderência

# Escolha duas subamostras geradas na atividade 2.2 (uma com n= 20 e a outra com n=200):
  
# Para a amostra com n=200, construa uma distribuição de frequências para uma 
# das variáveis nota em língua portuguesa ou em matemática (como na Atividade 
# 1.3; escolher uma das variáveis) e verifique se os dados podem ser descritos 
# pela distribuição Normal.

# Teste normalidade das variáveis notas em língua portuguesa e matemática para 
# a amostra (n=20). Apresente os testes de Shapiro-Wilk, Anderson-Darling e 
# Kolmogorov (Lilliefors). 

# Comente os resultados obtidos, à luz da análise descritiva realizada com a 
# amostra com 2000 observações (Atividade 1.3).

#------------------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
p_load(installr,readr,tidyverse,nortest)
# updateR()

#------------------------------------------------------------------------------
# Carregando dados

amostra <- read_csv("./banco/amostra_150167636.csv")

#------------------------------------------------------------------------------
# Pré ETL

amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="1998 ou antes",1999) 
amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="2005 ou depois",2004)
amostra$ANO_NASC<-as.numeric(amostra$ANO_NASC)

#------------------------------------------------------------------------------
# Gerando amostras

amostras20 <- list()
for (i in 1:50) {
  amostras20[[i]] <- sample_n(amostra,20)
}

amostras200 <- list()
for (i in 1:50) {
  amostras200[[i]] <- sample_n(amostra,200)
}

rm(i)

#------------------------------------------------------------------------------
# Selecionando uma amostra de tamanho 20 e uma de tamanho 200

amostra20 <- amostras20[[sample(1:50, size=1)]]

amostra200 <- amostras200[[sample(1:50, size=1)]]

#------------------------------------------------------------------------------
# Limpando o ambiente

rm(amostra,amostras20,amostras200)

#------------------------------------------------------------------------------
# Distribuição de frequência para a variável Nota em Matemática para a amostra n=200

hist(amostra200$NOTA_MT
     ,main="Histograma Notas Matemática - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
)

#------------------------------------------------------------------------------
# Verificação de normalidade para esta variável

# O próprio formato do histograma é um primeiro teste de normalidade.
# Como na atividade anterior, farei mais testes para confirmar.

# Gráfico Quantil-Quantil

qqnorm(amostra200$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       )
qqline(amostra200$NOTA_MT)

##

# Realizando os testes de normalidade
# Teste Shapiro-Wilk

swt1 <- shapiro.test(amostra200$NOTA_MT)

# Teste Anderson-Darling

adt1 <- ad.test(amostra200$NOTA_MT)

# Teste Lilliefors (Kolmogorov-Smirnov)

lt1 <- lillie.test(amostra200$NOTA_MT)

# Análise: (...)

#------------------------------------------------------------------------------
# Testando a normalidade para a amostra n=20

# Variável Nota em Matemática

hist(amostra20$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
)

qqnorm(amostra20$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
)
qqline(amostra20$NOTA_MT)

# Realizando os testes de normalidade
# Teste Shapiro-Wilk

swt2mt <- shapiro.test(amostra20$NOTA_MT)

# Teste Anderson-Darling

adt2mt <- ad.test(amostra20$NOTA_MT)

# Teste Lilliefors (Kolmogorov-Smirnov)

lt2mt <- lillie.test(amostra20$NOTA_MT)

# Análise: (...)

#####

# Variável Nota em Língua Portuguesa

hist(amostra20$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
)

qqnorm(amostra20$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
)
qqline(amostra20$NOTA_LP)

# Realizando os testes de normalidade
# Teste Shapiro-Wilk

swt2lp <- shapiro.test(amostra20$NOTA_LP)

# Teste Anderson-Darling

adt2lp <- ad.test(amostra20$NOTA_LP)

# Teste Lilliefors (Kolmogorov-Smirnov)

lt2lp <- lillie.test(amostra20$NOTA_LP)

# Análise: (...)
#------------------------------------------------------------------------------