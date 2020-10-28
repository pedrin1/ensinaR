library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(tidyverse)
library(ggthemes)

dados <- read.csv("C:/Users/pedro/Desktop/MICRODADOS.csv", sep = ";")

dados <- filter(dados, Classificacao=="Confirmados") 

dados <- dados[sample(1:dim(dados)[1], 20000), ]


for(i in 1:nrow(dados)){
  if(dados$Sexo[i]=="F"){dados$Sexo[i] <- "Feminino"
  }
}
for(i in 1:nrow(dados)){
  if(dados$Sexo[i]=="M"){dados$Sexo[i] <- "Masculino"
  }
}
for(i in 1:nrow(dados)){
  if(dados$Sexo[i]=="F"){dados$Sexo[i] <- "Feminino"
  }
}


dados <- mutate(dados,"Idade"=IdadeNaDataNotificacao)
for(i in 1:nrow(dados)){
  dados$Idade[i] <- str_sub(dados$Idade[i], end = 2)
} 

mortes <- dados %>% 
  filter(Evolucao=="Óbito pelo COVID-19") %>% 
  group_by(Municipio, RacaCor,Classificacao) %>% 
  summarise(Ã³bitos=n()) %>% 
  arrange(desc(Ã³bitos)) %>% 
  ungroup()

mortes <- mortes %>% 
  filter(RacaCor=="Amarela") %>% 
  arrange(desc(Ã³bitos)) %>% 
  ungroup()

length(dados$Classificacao) # quantidade de casos confirmados 


attach(dados)
regiao <- rep(nrow(dados))

for (i in 1:nrow(dados)){
  if(Municipio[i] =='VITORIA' |Municipio[i] =="DOMINGOS MARTINS"| Municipio[i] =="SANTA LEOPOLDINA"| Municipio[i] =="MARECHAL FLORIANO"|  Municipio[i] =='FUNDAO' | Municipio[i] =='SERRA' | Municipio[i] =='VILA VELHA' | Municipio[i] =='CARIACICA' | Municipio[i] =='VIANA' | Municipio[i] =='GUARAPARI'| Municipio[i] ==" SANTA LEOPOLDINA"| Municipio[i] =="DOMIGOS MARTINS"| Municipio[i] =="MARCHAL FLORIANO"| Municipio[i] =="SANTA TERESA"| Municipio[i] =="SANTA MARIA DO JETIBA"| Municipio[i] =="ITAGUACU"| Municipio[i] =="LARANJA DA TERRA"| Municipio[i] =="ITARANA"| Municipio[i] =="AFONSO CLAUDIO"| Municipio[i] =="VENDA NOVA DO IMIGRANDE"| Municipio[i] =="BREJETUBA"| Municipio[i] =="CONCEICAO DO CASTELO"){regiao[i]<-'Região Metropolitana'}
  if(Municipio[i] =="AGUIA BRANCA"|Municipio[i] =="VENDA NOVA DO IMIGRANTE"|  Municipio[i] =="SANTA MARIA DE JETIBA"| Municipio[i] =="MANTENOPOLIS"| Municipio[i] =="JAGUARE"| Municipio[i] =="SAO MATEUS"| Municipio[i] =="NOVA VENECIA"| Municipio[i] =="BARRA DE SAO FRANCISCO"| Municipio[i] =="VILA PAVAO"| Municipio[i] =="BOA ESPERANCA"| Municipio[i] =="AGUA DOCE DO NORTE"| Municipio[i] =="CONCEICAO DA BARRA"| Municipio[i] =="PINHEIROS"| Municipio[i] =="PEDRO CANARIO"| Municipio[i] =="MONTANHA"| Municipio[i] =="MUCURICI"| Municipio[i] =="PONTO BELO"| Municipio[i] =="ECOPORANGA"){regiao[i]<-'Região Norte'}
  if(Municipio[i] =="ARACRUZ"| Municipio[i] =="IBIRACU"| Municipio[i] =="JOAO NEIVA"| Municipio[i] =="SAO ROQUE DO CANAA"| Municipio[i] =="BAIXO GUANDU"| Municipio[i] =="COLATINA"| Municipio[i] =="MARILANDIA"| Municipio[i] =="LINHARES"| Municipio[i] =="RIO BANANAL"| Municipio[i] =="PANCAS"| Municipio[i] =="SOORETAMA"| Municipio[i] =="GOVERNADOR LINDENBERG"| Municipio[i] =="SAO DOMINGOS DO NORTE" | Municipio[i] =="ALTO RIO NOVO"| Municipio[i] =="SAO GABRIEL DA PALHA"| Municipio[i] =="VILA VALERIO"){regiao[i]<- "Região Central"}
  if(Municipio[i] == 'PRESIDENTE KENNEDY'	|Municipio[i] =="MUQUI"|  Municipio[i] =='MARATAIZES'	| Municipio[i] =='ITAPEMIRIM'	| Municipio[i] =='MIMOSO DO SUL' | Municipio[i] =='APIACA' | Municipio[i] =='BOM JESUS DO NORTE' | Municipio[i] =='SAO JOSE DO CALCADO' | Municipio[i] =="MUQI" | Municipio[i] =="ATILIO VIVACQUA" | Municipio[i] =="PIUMA "| Municipio[i] =="RIO NOVO DO SUL"| Municipio[i] =="CACHOEIRO DE ITAPEMIRIM"| Municipio[i] =="JERONIMO MONTEIRO" | Municipio[i] =="ALEGRE"| Municipio[i] =="GUACUI"| Municipio[i] =="DORES DO RIO PRETO"| Municipio[i] =="DIVINO DE SAO LOURENCO"| Municipio[i] =="IBITIRAMA" | Municipio[i] == "IUNA"| Municipio[i] =="IBATIBA"| Municipio[i] =="IRUPI"| Municipio[i] =="MUNIZ FREIRE"| Municipio[i] =="CASTELO"| Municipio[i] =="VARGEM ALTA"| Municipio[i] =="ALFREDO CHAVES"| Municipio[i] =="ANCHIETA"| Municipio[i] ==" ICONHA"| Municipio[i] =="RIO NOVO DO SUL"| Municipio[i] =="PIUMA"| Municipio[i] =="ICONHA"){regiao[i]<-'Região Sul'}
}
dados$regiao <- regiao

# METROPOLITANA
# NORTE 
# CENTRAL 
# SUL 
########################################################################################

#ggplot(dados) + geom_bar(aes(x=Sexo, fill= Sexo)) + scale_y_continuous(breaks = seq(0,100000,2000)) +
 #labs(y="Frequência") + theme_bw() + scale_fill_manual(values=c("yellow","red","purple"))
  

ggplot(dados) + geom_bar(aes(x=RacaCor, fill= RacaCor)) + scale_y_continuous(breaks = seq(0,100000,1000)) +
  labs(y="Frequência") + theme_bw() #+ scale_fill_manual(values=c("yellow","red","purple"))


ggplot(dados) + geom_bar(aes(x=Escolaridade, fill= Escolaridade)) + scale_y_continuous(breaks = seq(0,100000,2000)) +
  labs(y="Frequência") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) #+ scale_fill_manual(values=c("yellow","red","purple"))


dados2 <- dados %>%
  filter(regiao=="Região Central" |regiao== "Região Metropolitana" |regiao=="Região Norte"| regiao=="Região Sul") 

dados2 <- rename(dados2, região="regiao")

ggplot(dados2) + geom_bar(aes(x=região,fill=região)) + scale_y_continuous(breaks = seq(0,100000,2000)) +
  labs(y="Frequência") + theme_bw() 

#######
regiao_m <-dados2 %>% 
  filter(dados2$região=="Região Metropolitana", dados2$Sexo=="Masculino"|dados2$Sexo=="Feminino") 

regiao_c <-dados2 %>% 
  filter(dados2$região=="Região Central",dados2$Sexo=="Masculino"|dados2$Sexo=="Feminino")

regiao_n <-dados2 %>% 
  filter(dados2$região=="Região Norte",dados2$Sexo=="Masculino"|dados2$Sexo=="Feminino")

regiao_s <-dados2 %>% 
  filter(dados2$região=="Região Sul",dados2$Sexo=="Masculino"|dados2$Sexo=="Feminino")

########
ggplot(regiao_m) + geom_bar(aes(RacaCor, fill=Sexo), position = "dodge") + scale_y_continuous(breaks = seq(0,100000,500)) +
  labs(y="Frequência",x="Raça/Cor",title = "Região Metropolitana ") + theme_bw() + scale_fill_manual(values=c("yellow","purple"))

ggplot(regiao_c) + geom_bar(aes(RacaCor, fill=Sexo), position = "dodge") + scale_y_continuous(breaks = seq(0,100000,100)) +
  labs(y="Frequência",x="Raça/Cor",title = "Região Central ") + theme_bw() + scale_fill_manual(values=c("yellow","purple")) 

ggplot(regiao_n) + geom_bar(aes(RacaCor, fill=Sexo), position = "dodge") + scale_y_continuous(breaks = seq(0,100000,100)) +
  labs(y="Frequência",x="Raça/Cor",title = "Região Norte") + theme_bw() + scale_fill_manual(values=c("yellow","purple"))

ggplot(regiao_s) + geom_bar(aes(RacaCor, fill=Sexo), position = "dodge") + scale_y_continuous(breaks = seq(0,100000,100)) +
  labs(y="Frequência",x="Raça/Cor",title = "Região Sul") + theme_bw() + scale_fill_manual(values=c("yellow","purple"))

#ggplot(regiao_s) + geom_bar(aes(Sexo, fill=RacaCor), position = "dodge") + scale_y_continuous(breaks = seq(0,100000,100)) +
#  labs(y="Frequência") + theme_bw()





xbarra<-replicate(20,mean(rbinom(20,3,0.2)))
x_hist<-hist(xbarra,plot=F)
x_density<-density(xbarra)
hist(xbarra,probability = T,xlim=range(c(x_hist$breaks,x_density$x))
     ,ylim = range(c(x_hist$density,x_density$y)),col = "lightblue",main ="Binominal(n=10)")
lines(x_density,lwd=2)




x1 <- mean(rpois(10,3))
c(10,50,500,1000,10000)

for(i in c(10,50,500,1000,10000)){
  x1 <- mean(rpois(i,3))
}

x <- mean(rpois(10,3))
x2 <- replicate(10000,mean(rpois(100,3)))

hist(x2)



library(ggplot2)
library(reshape2)


set.seed(100)

# diferentes tamanhos amostrais que iremos simular
n <- c(1, 5, 10, 100, 500, 1000)

# número de replicações para cada n
n.rep <- 1000

sims <- lapply(n, function(n) replicate(n.rep, (mean(rexp(n)) - 1)*sqrt(n)))
names(sims) <- as.character(n)


str(sims)

sims[[6]]
sims[["1000"]]
sims.df <- as.data.frame(do.call("cbind", sims))

## Empilha para o ggplot2
sims.df <- melt(sims.df,
                variable.name = "n",
                value.name = "Valor")
sims.df$n <- paste("n =", sims.df$n)
sims.df$n <- factor(sims.df$n, levels = unique(sims.df$n))

# Histogramas vs Densidade Normal (ggplot2)
ggplot(sims.df, aes(x = Valor)) +
  # Histograma
  geom_histogram(aes(y = ..density..),
                 fill = "lightblue",
                 col = "black",
                 binwidth = 0.5) + xlim(c(-6, 6)) +
  # Uma faceta para cada n
  facet_wrap(~n) +
  ## Densidade da normal(0,1) para comparação
  stat_function(fun = dnorm,
                col = "red", size = 0.8) +
  # Titulo principal e do eixo Y
  ggtitle("Teorema Central do Limite\nDistribuição Expoencial") +
  ylab("Densidade") +
  ## Tema em preto e branco
  theme_bw()



set.seed(100)

# diferentes tamanhos amostrais que iremos simular
n <- c(1, 5, 10, 100, 500, 1000)

# número de replicações para cada n
n.rep <- 1000

sims <- lapply(n, function(n) replicate(n.rep, (mean(rgeom(n,0.5)) - 1)*sqrt(n)))
names(sims) <- as.character(n)


str(sims)

sims[[6]]
sims[["1000"]]
sims.df <- as.data.frame(do.call("cbind", sims))

## Empilha para o ggplot2
sims.df <- melt(sims.df,
                variable.name = "n",
                value.name = "Valor")
sims.df$n <- paste("n =", sims.df$n)
sims.df$n <- factor(sims.df$n, levels = unique(sims.df$n))

# Histogramas vs Densidade Normal (ggplot2)
ggplot(sims.df, aes(x = Valor)) +
  # Histograma
  geom_histogram(aes(y = ..density..),
                 fill = "lightblue",
                 col = "black",
                 binwidth = 0.5) + xlim(c(-6, 6)) +
  # Uma faceta para cada n
  facet_wrap(~n) +
  ## Densidade da normal(0,1) para comparação
  stat_function(fun = dnorm,
                col = "red", size = 0.5) +
  # Titulo principal e do eixo Y
  ggtitle("Teorema Central do Limite\nDistribuição Geometrica") +
  ylab("Densidade") +
  ## Tema em preto e branco
  theme_bw()


set.seed(100)

# diferentes tamanhos amostrais que iremos simular
n <- c(1, 5, 10, 100, 500, 1000)

# número de replicações para cada n
n.rep <- 1000

sims <- lapply(n, function(n) replicate(n.rep, (mean(rpois(n,0.9)) - 1)*sqrt(n)))
names(sims) <- as.character(n)


str(sims)

sims[[6]]
sims[["1000"]]
sims.df <- as.data.frame(do.call("cbind", sims))

## Empilha para o ggplot2
sims.df <- melt(sims.df,
                variable.name = "n",
                value.name = "Valor")
sims.df$n <- paste("n =", sims.df$n)
sims.df$n <- factor(sims.df$n, levels = unique(sims.df$n))

# Histogramas vs Densidade Normal (ggplot2)
ggplot(sims.df, aes(x = Valor)) +
  # Histograma
  geom_histogram(aes(y = ..density..),
                 fill = "lightblue",
                 col = "black",
                 binwidth = 0.5) + xlim(c(-10, 10)) +
  # Uma faceta para cada n
  facet_wrap(~n) +
  ## Densidade da normal(0,1) para comparação
  stat_function(fun = dnorm,
                col = "red", size = 0.8) +
  # Titulo principal e do eixo Y
  ggtitle("Teorema Central do Limite\nDistribuição Poisson") +
  ylab("Densidade") +
  ## Tema em preto e branco
  theme_bw()


