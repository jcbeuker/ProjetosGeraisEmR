################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            
################################################################################
pacotes <- c("ade4", #função para matriz de distâncias em variáveis binárias
             "cluster", #função 'agnes' para elaboração de clusters hierárquicos
             "plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
             #evitar sobreposição de textos
             "fastDummies",
             "knitr","kableExtra", #formatação de tabelas
             "splines",
             "reshape2", #função 'melt'
             "factoextra", #função 'fviz_dend' para elaboração de dendogramas
             "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
             "correlation",
             "see",
             "ggraph",
             "psych",
             "nortest",
             "rgl",
             "car",
             "ggside",
             "tidyquant",
             "olsrr",
             "jtools",
             "ggstance",
             "magick",
             "cowplot",
             "emojifont",
             "beepr",
             "Rcpp",
             "equatiomatic", 
             "readr", 
             "esquisse", 
             "nycflights13", 
             "AggregateR") # função 'Aggregate' para agregar dados de um DF ou 
# tabela

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