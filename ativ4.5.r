
seed <- 150167636

if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,readr)

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

bp1 <- ggplot(amostra, aes(x=REGIAO, y=NOTA_MT)) + 
  geom_boxplot() + 
  scale_x_discrete(limits=c("Nordeste", "Norte", "Sudeste","Centro-Oeste","Sul")) +
  labs(title = "Boxplot - Notas em matemática por região",
       subtitle = "",
       x = "Macrorregião geográfica",
       y = "Nota em Matemática")

bp2 <- ggplot(amostra, aes(x=USO_TEMPO_TELAS, y=NOTA_LP)) + 
  geom_boxplot() + 
  scale_x_discrete(limits=c("Até 1h", "1 a 2 horas", "2 a 3 horas","Mais de 3 horas")) +
  labs(title = "Boxplot - Notas em Língua Portuguesa por tempo de exposição à telas",
     subtitle = "",
       x = "Tempo de exposição à telas",
       y = "Nota em Língua Portuguesa")

# bp1
# bp2

