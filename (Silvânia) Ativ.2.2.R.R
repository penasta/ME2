# A partir de sua amostra de dados do SAEB 9o. ano, extraia 50 amostras aleatórias de tamanho 20, e outras 50 amostras aleatórias de tamanho 200 (total 100 amostras).

#Para cada amostra construa um IC 95% para:

#Proporção de alunos que nasceram em 2001 ou antes
#Proporção de alunas (sexo feminino)
#Média Nota_LP
# Média Nota_MT

if (!require("pacman")) install.packages("pacman")
pacman::p_load(purrr,tidyverse,ggcorrplot,scales,stringr,
               cowplot,RColorBrewer,xtable,readxl,readxl,
               lubridate,janitor,e1071,moments,statsr)


amostra_180065271 <- read.csv("C:\\Users\\toled\\Documents\\GitHub\\ME2\\banco/amostra_150167636.csv")

#Média Nota_LP

z_95 <- qnorm(0.975)
qt_95_19gl <- qt(0.975, df=19) 
qt_95_199gl <- qt(0.975, df=199)

media_lp <- mean(amostra_180065271$NOTA_LP)

media_lp
#254.055974067

# Média Nota_MT

media_mt <- mean(amostra_180065271$NOTA_MT)

media_mt
#251.663275561

#Proporção de alunos que nasceram em 2001 ou antes


antes_2001 <- amostra_180065271 %>% # criando um novo banco de dados com nascidos antes de 2001
  filter(ANO_NASC == "1998 ou antes" | ANO_NASC == "1999" | ANO_NASC == "2000"
         | ANO_NASC == "2001")

NAs <- colSums(is.na(amostra_180065271))*100/nrow(amostra_180065271)
NAs

#das duas mil amostras, tivemos 3,75% das observações da variável sexo como faltantes. Para o cálculo da
#proporção usada como parâmetro, resolvi desconsiderar os nas no denominador (2000-75)

prop_aluno_antes_2001 <-(nrow(antes_2001[antes_2001$ANO_NASC,])/1925)
prop_aluno_antes_2001
#0.2244156

#Proporção de alunas (sexo feminino)

prop_sexo_feminino <- (nrow(amostra_180065271[amostra_180065271$SEXO == "Feminino", ])/1925)
prop_sexo_feminino 
#0.5501299

amostras_20 <- amostra_180065271 %>% rep_sample_n(size = 20, reps = 50, replace = FALSE) #AMOSTRAS TAMANHO 20

amostras_200 <- amostra_180065271 %>% rep_sample_n(size = 200, reps = 50, replace = TRUE) #AMOSTRAS TAMANHO 200


#INTERVALOS PARA A MEDIA DE LP (AMOSTRAS TAMANHO 20)

notaslp_amostra20 <- amostras_20 %>% summarise(lower = mean(NOTA_LP) - qt_95_19gl * (sd(NOTA_LP) / sqrt(20)), 
                                               upper = mean(NOTA_LP) + qt_95_19gl * (sd(NOTA_LP) / sqrt(20)))



notaslp_amostra20 <- notaslp_amostra20  %>%
  mutate(legenda = ifelse(lower < media_lp & upper > media_lp, "Contém", "Não Contém"))

banco_lp20 <- data.frame(lp_id = c(1:50, 1:50),
                         lp_intervalo = c(notaslp_amostra20$lower, notaslp_amostra20$upper),
                         legenda = c(notaslp_amostra20$legenda, notaslp_amostra20$legenda))

ggplot(data = banco_lp20, aes(x = lp_intervalo, y = lp_id, 
                              group = lp_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_lp, color = "#000000") + 
  annotate(x = media_lp,y=-3, label = expression(mu == 254.0283) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))



# INTERVALOS PARA A MÉDIA DE NOTA DE PORTUGUÊS (AMOSTRAS TAMANHO 200)


notaslp_amostra200 <- amostras_200 %>% summarise(lower = mean(NOTA_LP) - qt_95_199gl * (sd(NOTA_LP) / sqrt(200)), 
                                                 upper = mean(NOTA_LP) + qt_95_199gl * (sd(NOTA_LP) / sqrt(200)))



notaslp_amostra200 <- notaslp_amostra200  %>%
  mutate(legenda = ifelse(lower < media_lp & upper > media_lp, "Contém", "Não Contém"))

banco_lp200 <- data.frame(lp_id = c(1:50, 1:50),
                          lp_intervalo = c(notaslp_amostra200$lower, notaslp_amostra200$upper),
                          legenda = c(notaslp_amostra200$legenda, notaslp_amostra200$legenda))

ggplot(data = banco_lp200, aes(x = lp_intervalo, y = lp_id, 
                               group = lp_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_lp, color = "#000000") + 
  annotate(x = media_lp,y=-3, label = expression(mu == 254.0283) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))



#INTERVALOS PARA A MEDIA DE Matemática (AMOSTRAS TAMANHO 20)

notasmt_amostra20 <- amostras_20 %>% summarise(lower = mean(NOTA_MT) - qt_95_19gl * (sd(NOTA_MT) / sqrt(20)), 
                                               upper = mean(NOTA_MT) + qt_95_19gl * (sd(NOTA_MT) / sqrt(20)))



notasmt_amostra20 <- notasmt_amostra20  %>%
  mutate(legenda = ifelse(lower < media_lp & upper > media_lp, "Contém", "Não Contém"))

banco_mt20 <- data.frame(lp_id = c(1:50, 1:50),
                         lp_intervalo = c(notasmt_amostra20$lower, notasmt_amostra20$upper),
                         legenda = c(notasmt_amostra20$legenda, notasmt_amostra20$legenda))

ggplot(data = banco_mt20, aes(x = lp_intervalo, y = lp_id, 
                              group = lp_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_lp, color = "#000000") + 
  annotate(x = media_lp,y=-3, label = expression(mu == 252.0926) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))


# INTERVALOS PARA MatemáticaT (AMOSTRAS TAMANHO 200)




notasmt_amostra200 <- amostras_200 %>% summarise(lower = mean(NOTA_MT) - qt_95_19gl * (sd(NOTA_MT) / sqrt(200)), 
                                                 upper = mean(NOTA_MT) + qt_95_19gl * (sd(NOTA_MT) / sqrt(200)))



notasmt_amostra200 <- notasmt_amostra200  %>%
  mutate(legenda = ifelse(lower < media_mt & upper > media_mt, "Contém", "Não Contém"))

banco_mt200 <- data.frame(mt_id = c(1:50, 1:50),
                          mt_intervalo = c(notasmt_amostra200$lower, notasmt_amostra200$upper),
                          legenda = c(notasmt_amostra200$legenda, notasmt_amostra200$legenda))

ggplot(data = banco_mt200, aes(x = mt_intervalo, y = mt_id, 
                               group = mt_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) + 
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = media_mt, color = "#000000") + 
  annotate(x = media_mt,y=-3, label = expression(mu == 252.342) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))


#intervalo de prop para sexo (amostras tamanho 20)

#antes
amostras_20_f <- amostras_20[amostras_20$SEXO == "Feminino",c(1, 11) ]
amostras_20_f
any(is.na(amostras_20_f))
amostras_20_f <- na.omit(amostras_20_f)
#depois


propsexo_amostra20 <- amostras_20_f %>% 
  summarise(lower = (length(replicate)/20) - z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20)),
                                                  upper = (length(replicate)/20) + z_95 * sqrt(((length(replicate)/20)*(1 - (length(replicate)/20))/20)))

propsexo_amostra20 <- propsexo_amostra20  %>%
  mutate(legenda = ifelse(lower < prop_sexo_feminino & upper > prop_sexo_feminino, "Contém", "Não Contém"))

banco_sexo20 <- data.frame(sexo_id = c(1:50, 1:50),
                           sexo_intervalo = c(propsexo_amostra20$lower, propsexo_amostra20$upper),
                           legenda = c(propsexo_amostra20$legenda, propsexo_amostra20$legenda))

ggplot(data = banco_sexo20, aes(x = sexo_intervalo, y = sexo_id, 
                                group = sexo_id, color = legenda)) +
  geom_point(size = 2) +  
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_sexo_feminino, color = "black") + 
  annotate(x = prop_sexo_feminino,y=-3, label = expression(mu == 0.526) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))




#Proporção de ALUNOS nascidos em 2001 ou antes:

#amostra_180065271$ANO_NASC<-replace(amostra_180065271$ANO_NASC, amostra_180065271$ANO_NASC=="1998 ou antes",1999) 
#amostra_180065271$ANO_NASC<-replace(amostra_180065271$ANO_NASC, amostra_180065271$ANO_NASC=="2005 ou depois",2004)
#amostra_180065271$ANO_NASC<-as.numeric(amostra_180065271$ANO_NASC)

# Testes:

amostras_200_f <- amostras_200[amostras_200$SEXO == "Feminino",c(1, 11) ]
amostras_200_f
any(is.na(amostras_200_f))
amostras_200_f <- na.omit(amostras_200_f)



propsexo_amostra200 <- amostras_200_f %>% summarise(lower = (length(replicate)/200) - z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200)),
                                                    upper = (length(replicate)/200) + z_95 * sqrt(((length(replicate)/200)*(1 - (length(replicate)/200))/200)))



propsexo_amostra200 <- propsexo_amostra200  %>%
  mutate(legenda = ifelse(lower < prop_sexo_feminino & upper > prop_sexo_feminino, "Contém", "Não Contém"))






banco_sexo200 <- data.frame(sexo_id = c(1:50, 1:50),
                            sexo_intervalo = c(propsexo_amostra200$lower, propsexo_amostra200$upper),
                            legenda = c(propsexo_amostra200$legenda, propsexo_amostra200$legenda))







ggplot(data = banco_sexo200, aes(x = sexo_intervalo, y = sexo_id, 
                                 group = sexo_id, color = legenda)) +
  geom_point(size = 2) +
  geom_line(size = 1.05) +
  scale_colour_manual(name="O intervalo contém o parâmetro?", values = c("#7B68EE", "#FF1493"))+
  labs(x="Intervalos de Confiança", y="Amostras") +
  geom_vline(xintercept = prop_sexo_feminino, color = "black") + 
  annotate(x = prop_sexo_feminino,y=-3, label = expression(p == 0.55) ,
           geom = "text", angle = 0, vjust = 1, hjust=-0.1, size = 3.5) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(size=14, face="bold"))
