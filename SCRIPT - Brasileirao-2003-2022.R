################################################################################
# IDENTIFICACAO                            
################################################################################
# 13.07.2023 - José Caetano Beuker <https://www.linkedin.com/in/jcbeuker/>
# Análise de todos os times e suas respectivas posicoes no Brasileirao 
# de 2003 a 2019
# Adaptado da Base de dados disponível em:
#   <https://www.kaggle.com/datasets/josevitormichelin/brazilian-football-champi
#    onship-brasileiro>
#
################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            
################################################################################
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic", "readr")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
################################################################################
# DOWNLOAD E SELECAO DAS VARIAVEIS DESEJADAS              
################################################################################
# Convertendo o arquivo para o encoding UFT-F
# writeLines(iconv(readLines("202301_SeguroDefeso.csv"), 
#                 from = "WINDOWS-1252", to = "UTF-8"), 
#                file("202301_SeguroDefeso_UTF8.csv", encoding = "UTF-8"))
#
# Carregando com o arquivo convertido para UTF-8
#favorecidos <- read.csv2("202301_SeguroDefeso_UTF8.csv", 
#                           sep= ";", dec = ",") 

brasileirao <- read.csv( "dataset-Posicoes-Times-Brasileirao-2003-2022.csv"
                        , sep = ";"
                        , dec = ","
                        , encoding = "UTF-8")

################################################################################
# ANALISE EXPLORATORIA DA BASE DE DADOS            
################################################################################

# Head dos dados
head(brasileirao, 5)

# Tail dos dados
tail(brasileirao, 5)

# Estatistica descritiva
summary(brasileirao)

# Estrutura da tabela
str(brasileirao)

# Dimensoes da base
dim(brasileirao)

################################################################################
# ANALISE EXPLORATORIA DOS CAMPEOES DE CADA ANO            
################################################################################
# Separa os campeoes de cada ano
campeoes <- brasileirao[brasileirao$position == 1,]
print(campeoes)

# Estatistica descritiva
summary(campeoes)

campeoes_colunasMaisImportantes <- campeoes[,c("year","team","points","games",
                                               "victories","draws","losses",
                                               "goals_scored","goals_against",
                                               "goals_difference",
                                               "perc_points_won")]

summary(campeoes_colunasMaisImportantes)
