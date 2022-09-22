library(pacman)
p_load(tidyverse,car)

#-------------------------------------------------------------------------------

# 1 - questão 13 bussab

m1 <- c(3,5,2,4,8,4,3,9)
meanm1 <- mean(m1)
varnm1 <- var(m1)

m2 <- c(4,4,3,8,7,4,2,5)
meanm2 <- mean(m2)
varnm2 <- var(m2)

m3 <- c(6,7,8,6,7,9,10,9)
meanm3 <- mean(m3)
varnm3 <- var(m3)

grupo <- c("Método 1","Método 2","Método 3")

mean <- c(meanm1,meanm2,meanm3)
var <- c(varnm1,varnm2,varnm3)

tam <- c(8,8,8)

tabelaq13 <- data.frame(grupo,tam,mean,var)
colnames(tabelaq13) <- c("Grupo","Tamanho","Média","Variância")

metodo <- rep(grupo,each=8)
valores <- c(m1,m2,m3)

tabela1 <- data.frame(valores,metodo)

model1 <- aov(valores ~ metodo, data = tabela1)
summary(model1)

# Ou seja, rejeitamos h0
pwtt1 <- pairwise.t.test(x=tabela1$valores, g=as.factor(tabela1$metodo), p.adjust.method="bonferroni", paired = FALSE)
pwtt1

# ou seja, média 1 = média 2 != média 3

# Teste de Levene:
levene1 <- leveneTest(y = tabela1$valores, group = as.factor(tabela1$metodo))
levene1

# Teste de Barlett
barlett1 <- bartlett.test(valores ~ metodo, tabela1)
barlett1

#-------------------------------------------------------------------------------

# 1 pt 2 - Questao 17 Bsussab

livro1 <- c(28,31,17,25,26,22,24)
meanv1 <- mean(livro1)
varnv1 <- var(livro1)

livro2 <- c(29,33,35,24,28)
meanv2 <- mean(livro2)
varnv2 <- var(livro2)

livro3 <- c(26,24,22,19,23,25,29,30)
meanv3 <- mean(livro3)
varnv3 <- var(livro3)

livro4 <- c(39,27,35,34,28,34,33)
meanv4 <- mean(livro4)
varnv4 <- var(livro4)

nlivro <- c("Livro 1","Livro 2","Livro 3","Livro 4")

meanlv <- c(meanv1,meanv2,meanv3,meanv4)
varlv <- c(varnv1,varnv2,varnv3,varnv4)

nlv <- c(7,5,8,7)

tabelaq17 <- data.frame(nlivro,nlv,meanlv,varlv)
colnames(tabelaq17) <- c("Livro","Tamanho","Média","Variância")

livro <- rep(nlivro,nlv)
qvalores <- c(livro1,livro2,livro3,livro4)

tabela2 <- data.frame(qvalores,livro)


model2 <- aov(qvalores ~ livro, data = tabela2)
summary(model2)

# Ou seja, rejeitamos h0

pwtt2 <- pairwise.t.test(x=tabela2$qvalores, g=as.factor(tabela2$livro), p.adjust.method="bonferroni", paired = FALSE)
pwtt2

# ou seja, 1=2 , 1=3 , 1 |= 4 , 2=3 , 2=4 , 3 != 4

# Teste de Levene:
levene2 <- leveneTest(y = tabela2$qvalores, group = as.factor(tabela2$livro))
levene2

# Teste de Barlett
barlett2 <- bartlett.test(qvalores ~ livro, tabela2)
barlett2

#-------------------------------------------------------------------------------

# 2

nf <- c(7.6,7.7,7.5,7.8)
fp <- c(8.9,8.2,8.1,8,8.4)
fr <- c(8,8.8,8.7,8.6,9)
fm <- c(9.9,9.1,9.2,9.8)

cat <- c("Não fumante","Fuma pouco","Fuma razoavelmente","Fuma muito")
ncat <- c(length(nf),length(fp),length(fr),length(fm))
agrup1 <- c(nf,fp,fr,fm)
agrup2 <- rep(cat,ncat) 
dbex2 <- data.frame(agrup1,agrup2)

# ANOVA: 
modelq2 <- aov(agrup1 ~ agrup2, data = dbex2)
summary(modelq2)

# Bonferroni:
pwttq2 <- pairwise.t.test(x=dbex2$agrup1, g=as.factor(dbex2$agrup2), p.adjust.method="bonferroni", paired = FALSE)
pwttq2

# Teste de Levene:
leveneq2 <- leveneTest(y = dbex2$agrup1, group = as.factor(dbex2$agrup2))
leveneq2

# Teste de Barlett
barlettq2 <- bartlett.test(agrup1 ~ agrup2, dbex2)
barlettq2

#-------------------------------------------------------------------------------

# 3

j1 <- c(4,3,2,1)
j2 <- c(4,2,3,1)
j3 <- c(3,1.5,1.5,4)
j4 <- c(3,1,2,4)
j5 <- c(4,2,1,3)
j6 <- c(2,2,2,4)
j7 <- c(1,3,2,4)
j8 <- c(2,4,1,3)
j9 <- c(3.5,1,2,3.5)
j10 <- c(4,1,3,2)
j11 <- c(4,2,3,1)
j12 <- c(3.5,1,2,3.5)

ptg <- as.data.frame(t(data.frame(j1,j2,j3,j4,j5,j6,j7,j8,j9,j10,j11,j12)))
tipograma <- rep(c("A","B","C","D"),each=12)
postograma <- c(ptg$V1,ptg$V2,ptg$V3,ptg$V4)
jardineiro <- rep(c(1:12), 4)

ex3 <- data.frame(postograma,tipograma,jardineiro)

kruskal.test(postograma ~ tipograma, data = ex3)

friedman.test(y = postograma, groups = tipograma, blocks = jardineiro)

pairwise.t.test(x=ex3$postograma, g=as.factor(ex3$tipograma), p.adjust.method="bonferroni", paired = FALSE)

# Dado esses testes, podemos admitir que há diferença entre os tipos de grama. no caso:

# GRAMA A != GRAMA B

# Testes: riA <-ex3 %>%
##filter(tipograma=="A") %>%
##  select(postograma) %>%
##  summarise(sum(postograma)) %>%
##  as.numeric()
##
##riB <- ex3 %>%
##  filter(tipograma=="B") %>%
##  select(postograma) %>%
##  summarise(sum(postograma)) %>%
##  as.numeric()
##
##riC <-ex3 %>%
##  filter(tipograma=="C") %>%
##  select(postograma) %>%
##  summarise(sum(postograma)) %>%
##  as.numeric()
##
##riD <- ex3 %>%
##  filter(tipograma=="D") %>%
##  select(postograma) %>%
##  summarise(sum(postograma)) %>%
##  as.numeric()

#-------------------------------------------------------------------------------

# Q4

pontos <- c(18,14,16,20,7,6,5,10,13,14,16,17,15,10,12,14,12,11,12,18,11,9,9,16,15,16,10,14,10,8,11,6)
aluno <- rep(c(1:8),each=4)
lista <- rep(c(1:4),8)

kruskal.test(pontos ~ lista)

friedman.test(y = pontos, groups = lista, blocks = aluno)

pairwise.t.test(x=pontos, g=as.factor(lista), p.adjust.method="bonferroni", paired = FALSE)

#-------------------------------------------------------------------------------