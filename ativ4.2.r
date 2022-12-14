#-------------------------------------------------------------------------------

# 1. A partir de sua amostra dos resultados do SAEB 9o. ano, considere amostras de tamanho 20 e 200. Para cada amostra, explore a associação entre as seguintes variáveis:
#  
#   NOTA_MT e LOCALIZAÇÃO (Urbana e Rural)
#
#   NOTA_LP e Ano de nascimento (2001 ou antes ; 2002 ou depois)
#
# Para avaliar essas relações construa os gráficos adequados e medidas de posição 
# e variabilidade segundo categorias das variáveis qualitativas. Você diria que 
# a proficiência em matemática é maior em escolas urbanas? Existe diferença entre 
# as proficiências em língua portuguesa segundo grupo de idade do estudante? 
# Considere os testes: t de Student, Wilcoxon-Mann-Whitney e Kolmogorov-Smirnov.
# (Não se esqueça de testar normalidade e homocedasticidade - fazer referência, 
# se vc já analisou esses aspectos anteriormente)
#
# 2. Para a amostra de tamanho 20, verifique se há diferença entre as notas de língua portuguesa e matemática. Considere os testes: t de Student, Wilcoxon e Sinais. Comente os resultados.

#-------------------------------------------------------------------------------

# Carregando pacotes

if (!require("pacman")) install.packages("pacman")
p_load(installr,readr,tidyverse,nortest,twosamples,BSDA,sfsmisc,DescTools)
# updateR()

#-------------------------------------------------------------------------------

# Carregando dados

amostra <- read_csv("./banco/amostra_150167636.csv")

# Dessa vez, quero garantir que as subamostras não terão valores NA
amostra <- na.omit(amostra)

#-------------------------------------------------------------------------------

# ETL e seleção de variáveis de interesse
# Variáveis de interesse: NOTA_MT LOCALIZAÇÃO NOTA_LP Ano de nascimento (2001 ou antes ; 2002 ou depois)
# LOCALIZACAO ; NOTA_MT ; NOTA_LP ; ANO_NASC .

amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="1998 ou antes",1999) 
amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="2005 ou depois",2004)
amostra$ANO_NASC<-as.numeric(amostra$ANO_NASC)

amostra <- amostra %>%
  select(LOCALIZACAO,NOTA_MT,NOTA_LP,ANO_NASC)

amostra <- amostra %>%
  mutate(ANO_NASC = ifelse(ANO_NASC <= 2001, "2001 ou antes","2002 ou depois"))

amostra$LOCALIZACAO <- factor(amostra$LOCALIZACAO)
amostra$ANO_NASC <- factor(amostra$ANO_NASC)

#-------------------------------------------------------------------------------

#Gerando amostras

set.seed(150167636)
amostra20 <- sample_n(amostra,20)

set.seed(150167636)
amostra200 <- sample_n(amostra,200)

#-------------------------------------------------------------------------------

# Testes: t de Student, Wilcoxon-Mann-Whitney e Kolmogorov-Smirnov.
# Testar normalidade e homocedasticidade
# Construir gráficos adequados e medidas de posição e variabilidade segundo categorias das variáveis qualitativas.

# Dando uma olhada nos dados
summary(amostra)
summary(amostra20)
summary(amostra200)

#-------------------------------------------------------------------------------
# Transformações para trabalhar
rural20 <- amostra20 %>%
  filter(LOCALIZACAO == 'Rural')
urbana20 <- amostra20 %>%
  filter(LOCALIZACAO == 'Urbana')

rural200 <- amostra200 %>%
  filter(LOCALIZACAO == 'Rural')
urbana200 <- amostra200 %>%
  filter(LOCALIZACAO == 'Urbana')

#

antes20 <- amostra20 %>%
  filter(ANO_NASC == '2001 ou antes')
depois20 <- amostra20 %>%
  filter(ANO_NASC == '2002 ou depois')

antes200 <- amostra200 %>%
  filter(ANO_NASC == '2001 ou antes')
depois200 <- amostra200 %>%
  filter(ANO_NASC == '2002 ou depois')
#-------------------------------------------------------------------------------

# Considere os testes: t de Student, Wilcoxon-Mann-Whitney e Kolmogorov-Smirnov.

#-------------------------------------------------------------------------------

# Proeficiência em matemática pela localização da escola:
# Amostra n=20
t.test(x=rural20$NOTA_MT,
       y=urbana20$NOTA_MT,
       alternative="two.sided",
       mu=0,
       conf.level=.95)

wilcox.test(rural20$NOTA_MT,
            urbana20$NOTA_MT,
            conf.level=.95) 

ks.test(x=rural20$NOTA_MT,
        y=urbana20$NOTA_MT,
        alternative="two.sided")

# Amostra n=200
t.test(x=rural200$NOTA_MT,
       y=urbana200$NOTA_MT,
       alternative="two.sided",
       mu=0,
       conf.level=.95)

wilcox.test(rural200$NOTA_MT,
            urbana200$NOTA_MT,
            conf.level=.95) 

ks.test(x=rural200$NOTA_MT,
        y=urbana200$NOTA_MT,
        alternative="two.sided")

#-------------------------------------------------------------------------------

# Proeficiência em língua portuguesa pelo ano de nascimento:
# Amostra n=20
t.test(x=antes20$NOTA_LP,
       y=depois20$NOTA_LP,
       alternative="two.sided",
       mu=0,
       conf.level=.95)

wilcox.test(antes20$NOTA_LP,
            depois20$NOTA_LP,
            conf.level=.95) 

ks.test(x=antes20$NOTA_LP,
        y=depois20$NOTA_LP,
        alternative="two.sided")

# Amostra n=200
t.test(x=antes200$NOTA_LP,
       y=depois200$NOTA_LP,
       alternative="two.sided",
       mu=0,
       conf.level=.95)

wilcox.test(antes200$NOTA_LP,
            depois200$NOTA_LP,
            conf.level=.95) 

ks.test(x=antes200$NOTA_LP,
        y=depois200$NOTA_LP,
        alternative="two.sided")

#-------------------------------------------------------------------------------

# Todos os testes confirmam a aceitação de h0 em todos os cenários estudados.

#-------------------------------------------------------------------------------

# Alguns gráficos:

#-------------------------------------------------------------------------------

# Amostra n=20, variável Nota em língua portuguesa
hist(amostra20$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub=''
)

qqnorm(amostra20$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub=''
)
qqline(amostra20$NOTA_LP)

# Amostra n=20, variável Nota em língua portuguesa
# Nascidos em 2001 ou antes
hist(antes20$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Nascidos em 2001 ou antes'
)

qqnorm(antes20$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Nascidos em 2001 ou antes'
)
qqline(antes20$NOTA_LP)

# Amostra n=20, variável Nota em língua portuguesa
# Nascidos em 2002 ou depois
hist(depois20$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Nascidos em 2002 ou depois'
)

qqnorm(depois20$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Nascidos em 2002 ou depois'
)
qqline(depois20$NOTA_LP)

#-------------------------------------------------------------------------------

# Amostra n=200, variável Nota em língua portuguesa
hist(amostra200$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub=''
)

qqnorm(amostra200$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub=''
)
qqline(amostra200$NOTA_LP)

# Amostra n=200, variável Nota em língua portuguesa
# Nascidos em 2001 ou antes
hist(antes200$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Nascidos em 2001 ou antes'
)

qqnorm(antes200$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Nascidos em 2001 ou antes'
)
qqline(antes200$NOTA_LP)

# Amostra n=200, variável Nota em língua portuguesa
# Nascidos em 2002 ou depois
hist(depois200$NOTA_LP
     ,main="Histograma Notas em Língua Portuguesa - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Nascidos em 2002 ou depois'
)

qqnorm(depois200$NOTA_LP
       ,main="Gráfico Q-Q da variável Nota em Língua Portuguesa - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Nascidos em 2002 ou depois'
)
qqline(depois200$NOTA_LP)

#-------------------------------------------------------------------------------

# Amostra n=20, variável Nota em Matemática
hist(amostra20$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub=''
)

qqnorm(amostra20$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub=''
)
qqline(amostra20$NOTA_MT)

# Amostra n=20, variável Nota em Matemática
# Escolas rurais
hist(rural20$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Escolas em zonas rurais'
)

qqnorm(rural20$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Escolas em zonas rurais'
)
qqline(rural20$NOTA_MT)

# Amostra n=20, variável Nota em Matemática
# Escolas urbanas
hist(urbana20$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=20"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Escolas em zonas urbanas'
)

qqnorm(urbana20$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=20"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Escolas em zonas urbanas'
)
qqline(urbana20$NOTA_MT)

#-------------------------------------------------------------------------------

# Amostra n=200, variável Nota em Matemática
hist(amostra200$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub=''
)

qqnorm(amostra200$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub=''
)
qqline(amostra200$NOTA_MT)

# Amostra n=200, variável Nota em Matemática
# Escolas rurais
hist(rural200$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Escolas em zonas rurais'
)

qqnorm(rural200$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Escolas em zonas rurais'
)
qqline(rural200$NOTA_MT)

# Amostra n=200, variável Nota em Matemática
# Escolas urbanas
hist(urbana200$NOTA_MT
     ,main="Histograma Notas em Matemática - Amostra n=200"
     ,xlab="Nota"
     ,ylab="Frequência"
     ,sub='Escolas em zonas urbanas'
)

qqnorm(urbana200$NOTA_MT
       ,main="Gráfico Q-Q da variável Nota em Matemática - Amostra n=200"
       ,xlab="Quantis esperados para uma variável com distribuição normal"
       ,ylab="Quantis observados na amostra"
       ,sub='Escolas em zonas urbanas'
)
qqline(urbana200$NOTA_MT)

#-------------------------------------------------------------------------------

# Testando a normalidade das variáveis NOTA_MT e NOTA_LP
# Testes: | Qui-quadrado | Shapiro-Wilk | Anderson-Darling |
# OBS: A normalidade desses dados foi melhor explorada nos exercícios anteriores.
# aqui se encontra apenas um resumo, com o resultado dos testes de aderência.

# Qui-quadrado: [OBS: Não é um bom teste para as amostras n=20]
PearsonTest(x=amostra20$NOTA_LP)
PearsonTest(x=amostra200$NOTA_LP)
PearsonTest(x=amostra20$NOTA_MT)
PearsonTest(x=amostra200$NOTA_MT)

# Shapiro-Wilk:
shapiro.test(x=amostra20$NOTA_LP)
shapiro.test(x=amostra200$NOTA_LP)
shapiro.test(x=amostra20$NOTA_MT)
shapiro.test(x=amostra200$NOTA_MT)

#Anderson-Darling
ad.test(x=amostra20$NOTA_LP)
ad.test(x=amostra200$NOTA_LP)
ad.test(x=amostra20$NOTA_MT)
ad.test(x=amostra200$NOTA_MT)

#-------------------------------------------------------------------------------

# Testando a homocedasticidade das variáveis NOTA_MT e NOTA_LP
var.test(x=rural20$NOTA_MT,
         y=urbana20$NOTA_MT,
         alternative = "two.sided")
var.test(x=rural20$NOTA_MT,
         y=urbana20$NOTA_MT,
         alternative = "two.sided")
var.test(x=antes200$NOTA_LP,
         y=depois200$NOTA_LP,
         alternative = "two.sided")
var.test(x=antes20$NOTA_LP,
         y=depois20$NOTA_LP,
         alternative = "two.sided")
# Portando, os dados apresentam homocedasticidade.

#-------------------------------------------------------------------------------
# Para a amostra n=20, Comparar as variáveis NOTA_MT e NOTA_LP pareadas.
# Testes: t de Student, Wilcoxon e Sinais. Comentar.

t.test(x=amostra20$NOTA_MT,
       y=amostra20$NOTA_LP,
       alternative="greater",
       mu=0,
       conf.level=.95,
       paired=TRUE)

wilcox.test(amostra20$NOTA_MT,
            amostra20$NOTA_LP,
            paired=TRUE,
            conf.level=.95) 

SIGN.test(amostra20$NOTA_MT, 
          amostra20$NOTA_LP, 
          md=0,
          alternative="greater",
          conf.level = 0.95)

#------------------------------------------------------------------------------