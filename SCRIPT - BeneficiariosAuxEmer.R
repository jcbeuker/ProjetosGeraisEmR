################################################################################
# IDENTIFICACAO                            
################################################################################
# 30.05.2023 - José Caetano Beuker <https://www.linkedin.com/in/jcbeuker/>
# Análise dos beneficiários do Auxílio Emergencial
# Base de dados disponível em:
#   https://portaldatransparencia.gov.br/download-de-dados/auxilio-emergencial
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
# beneficiarios <- read.csv("202004_AuxilioEmergencial.csv", sep= ";", dec = ",")
beneficiarios <-
  readr::read_csv("202004_AuxilioEmergencial.csv") %>%
  dplyr::select("UF", "NOME MUNICÍPIO", "NIS BENEFICIÁRIO", "CPF BENEFICIÁRIO", 
                "NOME BENEFICIÁRIO", "CPF RESPONSÁVEL" ,"NOME RESPONSÁVEL" )
#
# Head dos dados
head(beneficiarios, 5) %>%  knitr::kable()
#
# Estatistica descritiva
summary(beneficiarios)
