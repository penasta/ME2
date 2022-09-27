
#-------------------------------------------------------------------------------

# 1. Considere uma amostra de tamanho 500. Relacione as seguintes variáveis:
#    
#  NOTA_MT e Região
# NOTA_LP e Uso do tempo de tela: considerar as seguintes categorias
# Não vê TV..... + Menos de 1 hora (juntar as duas categorias)
# Entre 1 e 2 horas
# Mais de 2, até 3 horas
# Mais de 3 horas
# * Exclui os valores faltantes
# 
# 
# Para avaliar essas relações construa os gráficos adequados e medidas de posição e variabilidade segundo categorias das variáveis qualitativas. 
# 
# Você diria que existem diferenças entre as proficiências em matemática segundo a região geográfica da escola? 
#   
#   Existe diferença entre as proficiências em língua portuguesa segundo categoria de uso do tempo de tela?
#   
#   Apresente dois tipos de testes de hipóteses (ANOVA e Kruskal-Wallis), defina as hipóteses, pressupostos e comente os resultados. 
# 
# Caso haja evidências que levam você a afirmar que existem diferenças, identifique essas diferenças. (Não se esqueça de testar normalidade e homocedasticidade, caso já tenha feito esses testes, comente).

#-------------------------------------------------------------------------------

seed <- 150167636

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,readr,nortest,ggpubr)

amostra <- read_csv("banco/amostra_150167636.csv")
amostra <- na.omit(amostra)

set.seed(seed)
amostra <- sample_n(amostra, 500)

amostra <- amostra %>%
  select(NOTA_LP,NOTA_MT,REGIAO,USO_TEMPO_TELAS)%>%
  mutate(REGIAO = as.factor(REGIAO)) %>%
  mutate(USO_TEMPO_TELAS=as.factor(USO_TEMPO_TELAS))

amostra <- amostra %>%
  mutate(USO_TEMPO_TELAS = case_when(USO_TEMPO_TELAS == 'Não vejo TV, não navego na internet e não jogo jogos eletrônicos' ~ 'Até 1h',
                                     USO_TEMPO_TELAS == 'Menos de 1 hora' ~ 'Até 1h',
                                     USO_TEMPO_TELAS == 'Entre 1 e 2 horas' ~ '1 a 2 horas',
                                     USO_TEMPO_TELAS == 'Mais de 2 horas, até 3 horas' ~ '2 a 3 horas',
                                     USO_TEMPO_TELAS == 'Mais de 3 horas' ~ 'Mais de 3 horas')) %>%
  mutate(USO_TEMPO_TELAS = as.factor((USO_TEMPO_TELAS)))

summary(amostra)

#-------------------------------------------------------------------------------

# 
# Para avaliar essas relações construa os gráficos adequados e medidas de posição e variabilidade segundo categorias das variáveis qualitativas. 
# 

#-------------------------------------------------------------------------------

# Gerando tabelas de medidas de posição e variabilidade das variáveis qualitativas

#-------------------------------------------------------------------------------

# Região X nota em matemática;

# Média por categoria:

m1 <- amostra %>%
  group_by(REGIAO) %>%
  dplyr::summarize(Média = mean(NOTA_MT, na.rm=TRUE))

# Variância por categoria:

v1 <- amostra %>%
  group_by(REGIAO) %>%
  dplyr::summarize(Variância = var(NOTA_MT, na.rm=TRUE))

# Desvio padrão por categoria:

d1 <- amostra %>%
  group_by(REGIAO) %>%
  dplyr::summarize(Desvio = sd(NOTA_MT, na.rm=TRUE))

# Mediana por categoria:

md1 <- amostra %>%
  group_by(REGIAO) %>%
  dplyr::summarize(Mediana = median(NOTA_MT, na.rm=TRUE))

tabela1 <- inner_join(m1,v1,by='REGIAO')
tabela1 <- inner_join(tabela1,d1,by='REGIAO')
tabela1 <- inner_join(tabela1,md1,by='REGIAO')
colnames(tabela1)[4] <- "Desvio padrão"

#-------------------------------------------------------------------------------

# Uso de tela X nota em pt;

# Média por categoria:

m2 <- amostra %>%
  group_by(USO_TEMPO_TELAS) %>%
  dplyr::summarize(Variância = var(NOTA_LP, na.rm=TRUE))

# Variância por categoria:

v2 <- amostra %>%
  group_by(USO_TEMPO_TELAS) %>%
  dplyr::summarize(Média = mean(NOTA_LP, na.rm=TRUE))

# Desvio padrão por categoria:

d2 <- amostra %>%
  group_by(USO_TEMPO_TELAS) %>%
  dplyr::summarize(Desvio = sd(NOTA_LP, na.rm=TRUE))

# Mediana por categoria:

md2 <- amostra %>%
  group_by(USO_TEMPO_TELAS) %>%
  dplyr::summarize(Mediana = median(NOTA_LP, na.rm=TRUE))

tabela2 <- inner_join(v2,m2,by='USO_TEMPO_TELAS')
tabela2 <- inner_join(tabela2,d2,by='USO_TEMPO_TELAS')
tabela2 <- inner_join(tabela2,md2,by='USO_TEMPO_TELAS')
colnames(tabela2)[4] <- "Desvio padrão"

#-------------------------------------------------------------------------------

#
# Você diria que existem diferenças entre as proficiências em matemática segundo a região geográfica da escola? 
#

#-------------------------------------------------------------------------------
# Pergunta: As média da nota em matemática das escolas são diferentes segundo região geográfica?
# h0) h1)

# primeiro testar se seguem distribuição normal
# h0) h1)
# estatística de teste:

#-------------------------------------------------------------------------------
# Centro-Oeste:

CO <- amostra %>%
  select(NOTA_MT,REGIAO) %>%
  group_by(REGIAO) %>%
#  tally() %>%
  filter(REGIAO == 'Centro-Oeste')

ks.test(x=CO$NOTA_MT,y=pnorm)
ad.test(x=CO$NOTA_MT)
shapiro.test(x=CO$NOTA_MT)
ggdensity(CO$NOTA_MT, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(CO$NOTA_MT)

# É Normal

#-------------------------------------------------------------------------------
# Nordeste:

NE <- amostra %>%
  select(NOTA_MT,REGIAO) %>%
  group_by(REGIAO) %>%
  #  tally() %>%
  filter(REGIAO == 'Nordeste')

ks.test(x=NE$NOTA_MT,y=pnorm)
ad.test(x=NE$NOTA_MT)
shapiro.test(x=NE$NOTA_MT)
ggdensity(NE$NOTA_MT, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(NE$NOTA_MT)

# É Normal
#-------------------------------------------------------------------------------
# Norte:

NO <- amostra %>%
  select(NOTA_MT,REGIAO) %>%
  group_by(REGIAO) %>%
  #  tally() %>%
  filter(REGIAO == 'Norte')

ks.test(x=NO$NOTA_MT,y=pnorm)
ad.test(x=NO$NOTA_MT)
shapiro.test(x=NO$NOTA_MT)
ggdensity(NO$NOTA_MT, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(NO$NOTA_MT)

# É Normal
#-------------------------------------------------------------------------------
# Sudeste:

SE <- amostra %>%
  select(NOTA_MT,REGIAO) %>%
  group_by(REGIAO) %>%
  #  tally() %>%
  filter(REGIAO == 'Sudeste')

ks.test(x=SE$NOTA_MT,y=pnorm)
ad.test(x=SE$NOTA_MT)
shapiro.test(x=SE$NOTA_MT)
ggdensity(SE$NOTA_MT, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(SE$NOTA_MT)

# É Normal
#-------------------------------------------------------------------------------
# Sul:

SUL <- amostra %>%
  select(NOTA_MT,REGIAO) %>%
  group_by(REGIAO) %>%
  #  tally() %>%
  filter(REGIAO == 'Sul')

ks.test(x=SUL$NOTA_MT,y=pnorm)
ad.test(x=SUL$NOTA_MT)
shapiro.test(x=SUL$NOTA_MT)
ggdensity(SUL$NOTA_MT, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(SUL$NOTA_MT)

# É Normal

#-------------------------------------------------------------------------------

# Portanto, a distribuição das notas de matemática segundo cada região geográfica respeita a distribuição normal, conforme esperado e em concordância com os exercícios anteriormente realizados sob essa variável

#-------------------------------------------------------------------------------

# segundo, vamos testar se médias são iguais, após, testar se variâncias são iguais.
# h0) h1)
# nível de significância: 95%
# Estatística de teste:
# Teste de hipótese para igualdade de médias de várias populações que seguem distribuição normal:
# ANOVA: 

mod1 <- aov(amostra$NOTA_MT~amostra$REGIAO)
summary(mod1)

# boxplot: média das notas segundo região geográfica:

ggplot(amostra, aes(x=REGIAO, y=NOTA_MT)) + 
  geom_boxplot() + 
  scale_x_discrete(limits=c("Nordeste", "Norte", "Sudeste","Centro-Oeste","Sul")) +
  labs(title = "Boxplot - Notas em matemática por região",
       subtitle = "",
       x = "Macrorregião geográfica",
       y = "Nota em Matemática")

# Portanto, rejeitamos a hipótese h0) de igualdade das médias. Ou seja, existe diferença na proeficiência segundo região geográfica.

# Descobrindo aonde estão as desigualdades, utilizando o método de ajustamento de Bonferroni:
pairwise.t.test(x=amostra$NOTA_MT, g=as.factor(amostra$REGIAO), p.adjust.method="bonferroni", paired = FALSE)

# concluimos portanto que:
# média centro oeste = média norte = média sudeste = média sul 
# média centro oeste =/ média nordeste
# média sudeste =/ média nordeste
# média sul =/ média nordeste
# média sul =/ média sudeste

# A ANOVA tem como pressuposto a homocedasticidade das variâncias entre as variáveis. Verificaremos isto com um boxplot e com o teste de Bartlett:
# as variâncias são iguais?
# Boxplot das variâncias:

boxplot(mod1$res~amostra$REGIAO)

# Teste de Bartlett:

bartlett.test(mod1$res~amostra$REGIAO)

# Notamos que o boxplot e o teste nos sugerem a homocedasticidade das variâncias.

#-------------------------------------------------------------------------------

#
# Existe diferença entre as proficiências em língua portuguesa segundo categoria de uso do tempo de tela?
#

#-------------------------------------------------------------------------------
# Pergunta: As média da nota em língua portuguesa das escolas são diferentes segundo categoria de uso do tempo de tela?
# h0) h1)

# primeiro testar se seguem distribuição normal
# h0) h1)
# estatística de teste:

#-------------------------------------------------------------------------------
# Até 1h:

menos1 <- amostra %>%
  select(NOTA_LP,USO_TEMPO_TELAS) %>%
  group_by(USO_TEMPO_TELAS) %>%
  #  tally() %>%
  filter(USO_TEMPO_TELAS == 'Até 1h')

ks.test(x=menos1$NOTA_LP,y=pnorm)
ad.test(x=menos1$NOTA_LP)
shapiro.test(x=menos1$NOTA_LP)
ggdensity(menos1$NOTA_LP, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(menos1$NOTA_LP)

# É Normal

#-------------------------------------------------------------------------------
# 1 a 2 horas:

umaa2 <- amostra %>%
  select(NOTA_LP,USO_TEMPO_TELAS) %>%
  group_by(USO_TEMPO_TELAS) %>%
  #  tally() %>%
  filter(USO_TEMPO_TELAS == '1 a 2 horas')

ks.test(x=umaa2$NOTA_LP,y=pnorm)
ad.test(x=umaa2$NOTA_LP)
shapiro.test(x=umaa2$NOTA_LP)
ggdensity(umaa2$NOTA_LP, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(umaa2$NOTA_LP)

# É Normal

#-------------------------------------------------------------------------------
# 2 a 3 horas:

duasa3 <- amostra %>%
  select(NOTA_LP,USO_TEMPO_TELAS) %>%
  group_by(USO_TEMPO_TELAS) %>%
  #  tally() %>%
  filter(USO_TEMPO_TELAS == '2 a 3 horas')

ks.test(x=duasa3$NOTA_LP,y=pnorm)
ad.test(x=duasa3$NOTA_LP)
shapiro.test(x=duasa3$NOTA_LP)
ggdensity(duasa3$NOTA_LP, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(duasa3$NOTA_LP)

# É Normal

#-------------------------------------------------------------------------------
# Mais de 3 horas:

mais3 <- amostra %>%
  select(NOTA_LP,USO_TEMPO_TELAS) %>%
  group_by(USO_TEMPO_TELAS) %>%
  #  tally() %>%
  filter(USO_TEMPO_TELAS == 'Mais de 3 horas')

ks.test(x=mais3$NOTA_LP,y=pnorm)
ad.test(x=mais3$NOTA_LP)
shapiro.test(x=mais3$NOTA_LP)
ggdensity(mais3$NOTA_LP, 
          main = "Densidade de probabilidade da nota",
          xlab = "Nota",
          ylab = "Densidade")
ggqqplot(mais3$NOTA_LP)

# É Normal

#-------------------------------------------------------------------------------

# Portanto, a distribuição das notas de língua portuguesa segundo categorias de tempo de uso de tela respeita a distribuição normal, conforme esperado e em concordância com os exercícios anteriormente realizados sob essa variável

#-------------------------------------------------------------------------------

# segundo, vamos testar se médias são iguais, após, testar se variâncias são iguais.
# h0) h1)
# nível de significância: 95%
# Estatística de teste:
# Teste de hipótese para igualdade de médias de várias populações que seguem distribuição normal:
# ANOVA: 

mod2 <- aov(amostra$NOTA_LP~amostra$USO_TEMPO_TELAS)
summary(mod2)

# boxplot: média das notas segundo categoria de tempo de uso de tela:

ggplot(amostra, aes(x=USO_TEMPO_TELAS, y=NOTA_LP)) + 
  geom_boxplot() + 
  scale_x_discrete(limits=c("Até 1h", "1 a 2 horas", "2 a 3 horas","Mais de 3 horas")) +
  labs(title = "Boxplot - Notas em Língua Portuguesa por tempo de exposição à telas",
       subtitle = "",
       x = "Tempo de exposição à telas",
       y = "Nota em Língua Portuguesa")

# Portanto, rejeitamos a hipótese h0) de igualdade das médias. Ou seja, existe diferença na proeficiência segundo categoria de tempo de uso de telas.

# Descobrindo aonde estão as desigualdades, utilizando o método de ajustamento de Bonferroni:
pairwise.t.test(x=amostra$NOTA_LP, g=as.factor(amostra$USO_TEMPO_TELAS), p.adjust.method="bonferroni", paired = FALSE)

# concluimos portanto que:
# média 1a2horas = média maisde3horas = média 2a3horas
# média até1h =/ 1a2horas 
# média até1h =/ 2a3horas
# média maisde3horas =/ até1h

# A ANOVA tem como pressuposto a homocedasticidade das variâncias entre as variáveis. Verificaremos isto com um boxplot e com o teste de Bartlett:
# as variâncias são iguais?
# Boxplot das variâncias:

boxplot(mod2$res~amostra$USO_TEMPO_TELAS)

# Teste de Bartlett:

bartlett.test(mod2$res~amostra$USO_TEMPO_TELAS)

# Notamos que o boxplot e o teste nos sugerem a homocedasticidade das variâncias.
