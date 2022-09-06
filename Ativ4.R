require(pacman)
p_load(sfsmisc,nortest,twosamples,BSDA)

#-------------------------------------------------------------------------------

# LISTA DE EXERCÍCIOS N. 4
# COMPARANDO POPULAÇÕES – VARIÁVEIS QUANTITATIVAS

#-------------------------------------------------------------------------------

# 1) Faça os seguintes exercícios do capítulo 13 do livro “Estatística Básica”
# de Bussab, W e Morettin, P, 9a. Edição, 2017 (ou edição anterior)

#-------------------------------------------------------------------------------

# a) n. 4

# resolvido no caderno

#-------------------------------------------------------------------------------

#b) n. 9 e 10. Nos dois exercícios utilizar os testes t-Student, Mann-Whitney, KolmogorovSmirnov e Cramér-von Mises. Comente os resultados.

# a primeira parte do ex n.9 foi resolvida no caderno.

#-------------------------------------------------------------------------------
# Exercício 9

# Teste de Kolmogorov-Smirnov

# Dados:
liberais <- c(6.6,10.3,10.8,12.9,9.2,12.3,7.0)
administradores <- c(8.1,9.8,8.7,10,10.2,8.2,8.7,10.1)

# Teste Kolmogorov-Smirnov
ks.test(x=liberais,y=administradores)

# Teste Cramer-von Mises
cvm_test(liberais, administradores)

#-------------------------------------------------------------------------------
# Exercício 10

# h0: Não há evidências de que o novo fertilizante aumente a produção. 
# h1: Há evidências de que o novo fertilizante aumente a produção. 

# Dados:
controle <- c(7.1,6,8,7,6.6,7.4,7,7,6.9,6.8)
tratamento <- c(6.9,6.8,7.5,6.8,6.9,6.8,6.8,6.8,6.7,6.6)

# Teste t student
t.test(x=tratamento,
       y=controle,
       alternative="greater",
       mu=0,
       conf.level=.95)

# Teste de Mann Whitney
wilcox.test(x = controle,
            y = tratamento,
            alternative = "greater",
            mu = 0, 
            conf.int = 0.95, 
            conf.level = 0.95)

# Teste Kolmogorov-Smirnov
ks.test(x=controle,
        y=tratamento,
        alternative="greater"
        )

# Teste Cramer-von Mises
cvm_test(controle, tratamento)

# Conclusão: Aceita-se h0. Não há evidências de que o novo fertilizante aumente a produção. 

#-------------------------------------------------------------------------------

# c) n. 20 e 21. Para o mesmo conjunto de dados utilizar o teste dos sinais. 
# Comente os resultados

#-------------------------------------------------------------------------------
# Exercício 20

# h0: O cartaz não produz um aumento na médias das vendas
# h1: O cartaz produz um aumento na médias das vendas

# Dados:

scartaz <- c(13,18,14,16,19,12,22)
ccartaz <- c(16,24,18,14,26,17,29)

t.test(x=ccartaz,
       y=scartaz,
       alternative="greater",
       mu=0,
       conf.level=.95,
       paired=TRUE)

# Como t pertence à região crítica, rejeitamos H0. Ou seja, há evidências de que o cartaz produz um aumento na média das vendas. 

# teste dos sinais:
SIGN.test(ccartaz, 
          scartaz, 
          md=0,
          alternative="greater",
          conf.level = 0.95)

# Como s pertence à região crítica, rejeitamos H0. Ou seja, há evidências de que o cartaz produz um efeito positivo nas vendas médias. 

#-------------------------------------------------------------------------------
# Exercício 21

wilcox.test(ccartaz,
            scartaz,
            paired=TRUE,
            conf.level=.95) 

# Como p-valor < 0.05 e V > 13, rejeitamos H0. Ou seja, há evidências de que o cartaz produz um efeito positivo nas vendas médias. 

#-------------------------------------------------------------------------------

# 2) Selecionou-se uma amostra aleatória de 20 condutores de automóveis com o
# objetivo de verificar se o tempo de reação era afetado pelo consumo de álcool.
# Foi medido o tempo de reação a um mesmo estímulo de cada condutor antes e
# depois de consumir uma dose de bebida alcoólica. Os tempos de reação antes e
# depois do consumo da bebida estão na tabela abaixo:

Condutor <- c(1:20)

Antes <- c(0.68,0.64,0.68,0.82,0.58,0.80,0.72,0.65,0.84,0.73,0.65,0.59,0.78,0.67,0.65,0.76,0.61,0.86,0.74,0.88)
  
Depois <- c(0.73,0.62,0.66,0.92,0.68,0.87,0.77,0.70,0.88,0.79,0.72,0.60,0.78,0.66,0.68,0.77,0.72,0.86,0.72,0.97)

tabela <- as.data.frame(Condutor)
tabela$Antes <- NA
tabela$Depois <- NA
tabela$Antes <- Antes
tabela$Depois <- Depois

# Você diria que o álcool afeta o tempo de reação? Apresente três tipos de
# testes e comente os resultados.

#-------------------------------------------------------------------------------
# h0: Álcool não aumenta o tempo de reação
# h1: Álcool aumenta o tempo de reação

wilcox.test(Antes,
            Depois,
            paired=TRUE,
            conf.level=.95) 

t.test(x=Antes,
       y=Depois,
       alternative="greater",
       mu=0,
       conf.level=.95,
       paired=TRUE)

ks.test(x=Antes,
        y=Depois,
        alternative="greater")

SIGN.test(Antes, 
          Depois, 
          md=0,
          alternative="greater",
          conf.level = 0.95)

cvm_test(Antes,Depois)

# Baseado no resultado dos testes, a hipótese nula é aceita. Portanto, para esse
# conjunto de dados, o álcool não aumentou o tempo de reação dos condutores.
#-------------------------------------------------------------------------------

# Tabela 2 p/ markdown:
liberaist <- c(6.6,10.3,10.8,12.9,9.2,12.3,7.0,NA)
tabela2 <- as.data.frame(liberaist)
tabela2$administradores <- NA
tabela2$administradores <- administradores
tabela2 <- as.data.frame(t(tabela2))
rownames(tabela2) <- c("Liberais","Administradores")
colnames(tabela2) <- NA

# Tabela 3 p/ markdown:
administradores2 <- c(sort(administradores),NA,NA,NA,NA,NA,NA,NA)

liberaist2 <- c(sort(liberaist),NA,NA,NA,NA,NA,NA,NA,NA)
reunida <- c("6,6(L)","7(L)","8,1(A)","8,2(A)","8,7(A)","8,7(A)","9,2(L)",
             "9,8(A)","10(A)","10,1(A)","10,2(A)","10,3(L)","10,8(L)","12,3(L)",
             "12,9(L)")
posto <- c(1,2,3,4,5.5,5.5,7,8,9,10,11,12,13,14,15)

tabela3 <- as.data.frame(liberaist2)
tabela3$administradores <- NA
tabela3$administradores <- administradores2
tabela3$reunida <- NA
tabela3$reunida <- reunida
tabela3$posto <- NA
tabela3$posto <- posto

colnames(tabela3) <- c("Liberais","Administradores","Reunida e classificada","Posto")

#-------------------------------------------------------------------------------