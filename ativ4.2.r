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
pacman::p_load(installr,readr,tidyverse,nortest)
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

#set.seed()
amostra20 <- sample_n(amostra,20)

#set.seed()
amostra200 <- sample_n(amostra,200)

#-------------------------------------------------------------------------------
# Comparar dois a dois as variáveis
# NOTA_MT com LOCALIZAÇÃO
# NOTA_LP com ANO_NASC
#
# Testes: t de Student, Wilcoxon-Mann-Whitney e Kolmogorov-Smirnov.
# Testar normalidade e homocedasticidade
# Construir gráficos adequados e medidas de posição e variabilidade segundo categorias das variáveis qualitativas.
summary(amostra)
summary(amostra20)
summary(amostra200)

#-------------------------------------------------------------------------------
# Para a amostra n=20, Comparar dois a dois as variáveis NOTA_MT e NOTA_LP.
# Testes: t de Student, Wilcoxon e Sinais. Comentar.


#------------------------------------------------------------------------------