library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(tidyverse)
library(ggthemes)
library(forcats)

dados <- read.csv("C:/Users/pedro/Desktop/MICRODADOS.csv", sep = ";")
dados <- filter(dados, Classificacao=="Confirmados") 

dados <- dados[sample(1:dim(dados)[1], 20000), ] # não mudei o nome para "dados_amostra" pois já tinha feito o código todo quando achei melhor resumir os dados


for(i in 1:nrow(dados)){
  if(dados$Sexo[i]=="F"){dados$Sexo[i] <- "Feminino"}
  if(dados$Sexo[i]=="M"){dados$Sexo[i] <- "Masculino"}
  if(dados$Sexo[i]=="I"){dados$Sexo[i] <- "Ignorados"
  }
}

dados$Idade <- str_sub(dados$IdadeNaDataNotificacao, end = 2)
# eu sei que poderia ter feito tudo dentro de um if só kkk

dados <- mutate(dados,"Idade"=IdadeNaDataNotificacao)
for(i in 1:nrow(dados)){
  dados$Idade[i] <- str_sub(dados$Idade[i], end = 2)
} ## essa função pega apenas os dois primeiros digitos da idade

# questão 4 e 5 
  
mortes <- dados %>% 
  filter(Evolucao=="Óbito pelo COVID-19") %>% 
  group_by(Municipio, RacaCor,Classificacao) %>% 
  summarise(óbitos=n()) %>% 
  arrange(desc(óbitos)) %>% 
  ungroup()

mortes <- mortes %>% 
  filter(RacaCor=="Amarela") %>% 
  arrange(desc(óbitos)) %>% 
  ungroup()

length(dados$Classificacao) # quantidade de casos confirmados 

#### gráficos ####

ggplot(dados) + geom_bar(aes(x=FaixaEtaria), fill= "purple")+ 
  labs(y= "Frequência") + scale_y_continuous(breaks = seq(0,100000,500)) + theme_calc()

dados$Idade <- as.numeric(dados$Idade)


ggplot(dados, aes(x = Idade, fill=Sexo)) +
  geom_histogram(color= "white",bins = 50) + theme_bw() +
  ylab("Frequência") + scale_fill_manual(values=c("black","red","purple"))
 


attach(dados)
regiao <- rep(nrow(dados))
for (i in 1:nrow(dados)){
  if(Municipio[i] =='VITORIA' |Municipio[i] =="SANTA LEOPOLDINA"| Municipio[i] =="MARECHAL FLORIANO"|  Municipio[i] =='FUNDAO' | Municipio[i] =='SERRA' | Municipio[i] =='VILA VELHA' | Municipio[i] =='CARIACICA' | Municipio[i] =='VIANA' | Municipio[i] =='GUARAPARI'| Municipio[i] ==" SANTA LEOPOLDINA"| Municipio[i] =="DOMIGOS MARTINS"| Municipio[i] =="MARCHAL FLORIANO"| Municipio[i] =="SANTA TERESA"| Municipio[i] =="SANTA MARIA DO JETIBA"| Municipio[i] =="ITAGUACU"| Municipio[i] =="LARANJA DA TERRA"| Municipio[i] =="ITARANA"| Municipio[i] =="AFONSO CLAUDIO"| Municipio[i] =="VENDA NOVA DO IMIGRANDE"| Municipio[i] =="BREJETUBA"| Municipio[i] =="CONCEICAO DO CASTELO"){regiao[i]<-'Região Metropolitana'}
  if(Municipio[i] =='PRESIDENTE KENNEDY'	|Municipio[i] =="MUQUI"|  Municipio[i] =='MARATAIZES'	| Municipio[i] =='ITAPEMIRIM'	| Municipio[i] =='MIMOSO DO SUL' | Municipio[i] =='APIACA' | Municipio[i] =='BOM JESUS DO NORTE' | Municipio[i] =='SAO JOSE DO CALCADO' | Municipio[i] =="MUQI" | Municipio[i] =="ATILIO VIVACQUA" | Municipio[i] =="PIUMA "| Municipio[i] =="RIO NOVO DO SUL"| Municipio[i] =="CACHOEIRO DE ITAPEMIRIM"| Municipio[i] =="JERONIMO MONTEIRO" | Municipio[i] =="ALEGRE"| Municipio[i] =="GUACUI"| Municipio[i] =="DORES DO RIO PRETO"| Municipio[i] =="DIVINO DE SAO LOURENCO"| Municipio[i] =="IBITIRAMA" | Municipio[i] == "IUNA"| Municipio[i] =="IBATIBA"| Municipio[i] =="IRUPI"| Municipio[i] =="MUNIZ FREIRE"| Municipio[i] =="CASTELO"| Municipio[i] =="VARGEM ALTA"| Municipio[i] =="ALFREDO CHAVES"| Municipio[i] =="ANCHIETA"| Municipio[i] ==" ICONHA"| Municipio[i] =="RIO NOVO DO SUL"| Municipio[i] =="PIUMA"| Municipio[i] =="ICONHA"){regiao[i]<-'Região Sul'}
  if(Municipio[i] =="ARACRUZ"| Municipio[i] =="IBIRACU"| Municipio[i] =="JOAO NEIVA"| Municipio[i] =="SAO ROQUE DO CANAA"| Municipio[i] =="BAIXO GUANDU"| Municipio[i] =="COLATINA"| Municipio[i] =="MARILANDIA"| Municipio[i] =="LINHARES"| Municipio[i] =="RIO BANANAL"| Municipio[i] =="PANCAS"| Municipio[i] =="SOORETAMA"| Municipio[i] =="GOVERNADOR LINDENBERG"| Municipio[i] =="SAO DOMINGOS DO NORTE" | Municipio[i] =="ALTO RIO NOVO"| Municipio[i] =="SAO GABRIEL DA PALHA"| Municipio[i] =="VILA VALERIO"){regiao[i]<- "Região Central"}
  if(Municipio[i] =="AGUIA BRANCA"|Municipio[i] =="VENDA NOVA DO IMIGRANTE"|  Municipio[i] =="SANTA MARIA DE JETIBA"| Municipio[i] =="MANTENOPOLIS"| Municipio[i] =="JAGUARE"| Municipio[i] =="SAO MATEUS"| Municipio[i] =="NOVA VENECIA"| Municipio[i] =="BARRA DE SAO FRANCISCO"| Municipio[i] =="VILA PAVAO"| Municipio[i] =="BOA ESPERANCA"| Municipio[i] =="AGUA DOCE DO NORTE"| Municipio[i] =="CONCEICAO DA BARRA"| Municipio[i] =="PINHEIROS"| Municipio[i] =="PEDRO CANARIO"| Municipio[i] =="MONTANHA"| Municipio[i] =="MUCURICI"| Municipio[i] =="PONTO BELO"| Municipio[i] =="ECOPORANGA"){regiao[i]<-'Região Norte'}
}
dados$regiao <- regiao


dados <- dados %>% 
  mutate(DataNotificacao = as.Date(DataDiagnostico))
casos_t <- dados %>%
  group_by(Sexo, DataNotificacao) %>%
  summarise(n_casos = n()) %>%
  tidyr::complete(tidyr::nesting(Sexo), 
                  DataNotificacao = seq.Date(min(dados$DataNotificacao),
                                             max(dados$DataNotificacao), 
                                             by = "day"), 
                  fill = list(n_casos = 0)) %>%
  mutate(casos_acumulados = cumsum(n_casos))
ggplot(casos_t) + 
  theme_minimal() + 
  aes(x = DataNotificacao, y = casos_acumulados, color = Sexo) +
  geom_line() +
  scale_color_viridis_d() + 
  labs(y = "Número de casos acumulados", 
       x = "Data Diagnóstico")



regiao_m <-dados %>%
  filter(dados$regiao=="Região Metropolitana")
regiao_c <-dados %>%
  filter(dados$regiao=="Região Central")
regiao_n <-dados %>%
  filter(dados$regiao=="Região Norte")
regiao_s <-dados %>%
  filter(dados$regiao=="Região Sul")




#filtrar confirmados por sexo, obitos por covid, relação

#mutate morte, se for obito =1 se for diferente = 0




attach(dados)
prop <- rep(nrow(dados))
for (i in 1:nrow(dados)){
  if(Evolucao[i]=="Óbito pelo COVID-19"){prop[i] <- "1"
  }else{prop[i] <- "0"}
  
}
dados$prop <- prop
  

taxa <- dados %>% 
  group_by(Sexo,RacaCor,prop) %>% 
  summarise(n=n()) %>% 
  filter(prop =="1") %>% 
  mutate(indice_mortalidade = (n/length(dados$prop))*100 )


ggplot(taxa) + geom_col(aes(x= RacaCor, y=indice_mortalidade, fill= Sexo),position = "dodge") + 
  labs( x="Pessoas",
        y="Letalidade(%)",
        title="Taxa de letalidade da COVID-19 no ES") + 
  scale_fill_manual(values = c("yellow","purple")) + theme_bw()
                             
  
                         








