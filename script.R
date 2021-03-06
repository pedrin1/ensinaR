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



dados$Sexo <- fct_recode(dados$Sexo, 
                    Masculino ="M",
                    Feminino = "F",
                    Ignorados = "I")

dados$Idade <- str_sub(dados$IdadeNaDataNotificacao, end = 2)

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



#### gráficos ####

ggplot(dados) + geom_bar(aes(x=FaixaEtaria), fill= "purple")+ 
  labs(y= "Frequência") + scale_y_continuous(breaks = seq(0,100000,2500)) + theme_calc()

dados$Idade <- as.numeric(dados$Idade)


ggplot(dados, aes(x = Idade, fill=Sexo)) +
  geom_histogram(color= "white",bins = 50) + theme_bw() +
  ylab("Frequência") + scale_fill_manual(values=c("black","red","purple"))
 

regiao <- rep(nrow(dados))
classifica_regiao <- function(Municipio){
  reg_metropolitana <- c(
    'VITORIA',
    "DOMINGOS MARTINS" ,
    "SANTA LEOPOLDINA" ,
    "MARECHAL FLORIANO" ,
    'FUNDAO' ,
    'SERRA' ,
    'VILA VELHA' ,
    'CARIACICA' ,
    'VIANA' ,
    'GUARAPARI' ,
    "SANTA LEOPOLDINA" ,
    "DOMIGOS MARTINS" ,
    "MARCHAL FLORIANO" ,
    "SANTA TERESA" ,
    "SANTA MARIA DO JETIBA" ,
    "ITAGUACU" ,
    "LARANJA DA TERRA" ,
    "ITARANA" ,
    "AFONSO CLAUDIO" ,
    "VENDA NOVA DO IMIGRANDE" ,
    "BREJETUBA" ,
    "CONCEICAO DO CASTELO"
  )
  reg_norte <- c(
    "AGUIA BRANCA " ,
    "VENDA NOVA DO IMIGRANTE" ,
    "SANTA MARIA DE JETIBA" ,
    "MANTENOPOLIS" ,
    "JAGUARE" ,
    "SAO MATEUS" ,
    "NOVA VENECIA" ,
    "BARRA DE SAO FRANCISCO" ,
    "VILA PAVAO" ,
    "BOA ESPERANCA" ,
    "AGUA DOCE DO NORTE" ,
    "CONCEICAO DA BARRA" ,
    "PINHEIROS" ,
    "PEDRO CANARIO" ,
    "MONTANHA" ,
    "MUCURICI" ,
    "PONTO BELO" ,
    "ECOPORANGA"
  )
  
  reg_central <- c(
    "ARACRUZ" ,
    "IBIRACU" ,
    "JOAO NEIVA" ,
    "SAO ROQUE DO CANAA" ,
    "BAIXO GUANDU" ,
    "COLATINA" ,
    "MARILANDIA" ,
    "LINHARES" ,
    "RIO BANANAL" ,
    "PANCAS" ,
    "SOORETAMA" ,
    "GOVERNADOR LINDENBERG" ,
    "SAO DOMINGOS DO NORTE" ,
    "ALTO RIO NOVO" ,
    "SAO GABRIEL DA PALHA" ,
    "VILA VALERIO"
  )
  
  reg_sul <- c(
    'PRESIDENTE KENNEDY'	,
    "MUQUI" ,
    'MARATAIZES'	,
    'ITAPEMIRIM'	,
    'MIMOSO DO SUL' ,
    'APIACA' ,
    'BOM JESUS DO NORTE' ,
    'SAO JOSE DO CALCADO' ,
    "MUQI" ,
    "ATILIO VIVACQUA" ,
    "PIUMA " ,
    "RIO NOVO DO SUL" ,
    "CACHOEIRO DE ITAPEMIRIM" ,
    "JERONIMO MONTEIRO" ,
    "ALEGRE" ,
    "GUACUI" ,
    "DORES DO RIO PRETO" ,
    "DIVINO DE SAO LOURENCO" ,
    "IBITIRAMA" ,
    "IUNA" ,
    "IBATIBA" ,
    "IRUPI" ,
    "MUNIZ FREIRE" ,
    "CASTELO" ,
    "VARGEM ALTA" ,
    "ALFREDO CHAVES" ,
    "ANCHIETA" ,
    "ICONHA" ,
    "RIO NOVO DO SUL" ,
    "PIUMA" ,
    "ICONHA"
  )
  
  regiao <- 
    ifelse(any(Municipio == reg_metropolitana),
           "Região Metropolitana", 
           ifelse(any(Municipio == reg_norte), 
                  "Região Norte", 
                  ifelse(any(Municipio == reg_central), 
                         "Regiao Central", 
                         ifelse(any(Municipio == reg_sul), 
                                "Regiao Sul", "Outro Estado")
                  )
           )
    )
  return(regiao)
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



casos <- dados %>%
  group_by(RacaCor,Sexo) %>%
  summarise(n_casos = n())
mortes <- dados %>% 
  filter(Evolucao ==  "Óbito pelo COVID-19") %>%
  group_by(RacaCor,Sexo) %>%
  summarise(n_mortes = n())
letalidade <- inner_join(casos, mortes) %>%
  mutate(taxa_l =   ifelse(n_casos == 0, 0, 
                               round(100 * n_mortes / n_casos, 1)))


ggplot(letalidade) + geom_col(aes(x= RacaCor, y=taxa_l, fill= Sexo),position = "dodge") + 
  labs( x="Pessoas",
        y="Letalidade(%)",
        title="Taxa de letalidade da COVID-19 no ES") + 
  scale_fill_manual(values = c("yellow","purple")) + theme_bw()
                             
   
                         




