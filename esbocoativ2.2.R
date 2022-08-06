#------------------------------------------------------------------------------
# Carregando pacotes

if (!require("pacman")) install.packages("pacman")
pacman::p_load(installr,readr,tidyverse)
updateR()

#------------------------------------------------------------------------------
# Carregando dados

amostra <- read_csv("./banco/amostra_150167636.csv")

#------------------------------------------------------------------------------
# Pré ETL

amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="1998 ou antes",1999) 
amostra$ANO_NASC<-replace(amostra$ANO_NASC, amostra$ANO_NASC=="2005 ou depois",2004)
amostra$ANO_NASC<-as.numeric(amostra$ANO_NASC)

#------------------------------------------------------------------------------
#Gerando amostras

amostras20 <- list()
for (i in 1:50) {
  amostras20[[i]] <- sample_n(amostra,20)
}

amostras200 <- list()
for (i in 1:50) {
  amostras200[[i]] <- sample_n(amostra,200)
}

#------------------------------------------------------------------------------
# Proporção de alunos que nasceram em 2001 ou antes
# amostra tam 20

propnasc20 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$ANO_NASC,amostras20[[i]]$ANO_NASC)
  propnasc20 <- c(propnasc20,t$p.value)
}
rm(i,t)
propnasc20 <- propnasc20[!is.na(propnasc20)]
propnasc20 <- propnasc20 >=.05
table(propnasc20)

# amostra tam 200

propnasc200 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$ANO_NASC,amostras200[[i]]$ANO_NASC)
  propnasc200 <- c(propnasc200,t$p.value)
}
rm(i,t)
propnasc200 <- propnasc200[!is.na(propnasc200)]
propnasc200 <- propnasc200 >=.05
table(propnasc200)

#------------------------------------------------------------------------------
# Proporção de alunas (sexo feminino)
# amostras tam 20
P <- as.numeric(table(amostra$SEXO))[1]/sum(as.numeric(table(amostra$SEXO)))

propalunas20 <- NA
for (i in 1:50){
  
  n <- as.numeric(length(amostras20[[i]]$SEXO[!is.na(amostras20[[i]]$SEXO)]))
  p <- as.numeric(table(amostras20[[i]]$SEXO))[1]/sum(as.numeric(table(amostras20[[i]]$SEXO)))
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  vf <- lowerinterval <= P & upperinterval >= P
  propalunas20 <- c(propalunas20,vf)
  
}
rm(n,p,i,vf,lowerinterval,upperinterval,margin)
propalunas20 <- propalunas20[!is.na(propalunas20)]
table(propalunas20)

# amostras tam 200

propalunas200 <- NA
for (i in 1:50){
  
  n <- as.numeric(length(amostras200[[i]]$SEXO[!is.na(amostras200[[i]]$SEXO)]))
  p <- as.numeric(table(amostras200[[i]]$SEXO))[1]/sum(as.numeric(table(amostras200[[i]]$SEXO)))
  
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  vf <- lowerinterval <= P & upperinterval >= P
  propalunas200 <- c(propalunas200,vf)
  
}
rm(n,p,i,vf,lowerinterval,upperinterval,margin)
propalunas200 <- propalunas200[!is.na(propalunas200)]
table(propalunas200)

#------------------------------------------------------------------------------
# Média Nota_LP
# amostras tam 20

mdlp20 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$NOTA_LP,amostras20[[i]]$NOTA_LP)
  mdlp20 <- c(mdlp20,t$p.value)
}
rm(i,t)
mdlp20 <- mdlp20[!is.na(mdlp20)]
mdlp20 <- mdlp20 >=.05
table(mdlp20)

# amostras tam 200

mdlp200 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$NOTA_LP,amostras200[[i]]$NOTA_LP)
  mdlp200 <- c(mdlp200,t$p.value)
}
rm(i,t)
mdlp200 <- mdlp200[!is.na(mdlp200)]
mdlp200 <- mdlp200 >=.05
table(mdlp200)

#------------------------------------------------------------------------------
# Média Nota_MT
# amostras tam 20

mdmt20 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$NOTA_MT,amostras20[[i]]$NOTA_MT)
  mdmt20 <- c(mdmt20,t$p.value)
}
rm(i,t)
mdmt20 <- mdmt20[!is.na(mdmt20)]
mdmt20 <- mdmt20 >=.05
table(mdmt20)

# amostras tam 200

mdmt200 <- NA
for (i in 1:50) {
  t <- ks.test(amostra$NOTA_MT,amostras200[[i]]$NOTA_MT)
  mdmt200 <- c(mdmt200,t$p.value)
}
rm(i,t)
mdmt200 <- mdmt200[!is.na(mdmt200)]
mdmt200 <- mdmt200 >=.05
table(mdmt200)

#------------------------------------------------------------------------------
# Gráficos

ggplot(as.data.frame(table(propnasc20)), aes(x=propnasc20, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Proporção de alunos que nasceram em 2001 ou antes - Amostras n=20",
       x="",
       y="Frequência",
       caption="IC 95%"
       )

ggplot(as.data.frame(table(propnasc200)), aes(x=propnasc200, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Proporção de alunos que nasceram em 2001 ou antes - Amostras n=200",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(propalunas20)), aes(x=propalunas20, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Proporção de alunas (sexo feminino) - Amostras n=20",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(propalunas200)), aes(x=propalunas200, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Proporção de alunas (sexo feminino) - Amostras n=200",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(mdlp20)), aes(x=mdlp20, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Nota em língua portuguesa - Amostras n=20",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(mdlp200)), aes(x=mdlp200, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Nota em língua portuguesa - Amostras n=200",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(mdmt20)), aes(x=mdmt20, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Nota em matemática - Amostras n=20",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

ggplot(as.data.frame(table(mdmt200)), aes(x=mdmt200, y = Freq, )) + 
  geom_bar(stat="identity")+
  labs(title="Estatísticas conferem com os parâmetros?",
       subtitle = "Nota em matemática - Amostras n=200",
       x="",
       y="Frequência",
       caption="IC 95%"
  )

#------------------------------------------------------------------------------