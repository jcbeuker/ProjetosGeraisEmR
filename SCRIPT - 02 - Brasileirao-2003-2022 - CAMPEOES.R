################################################################################
# IDENTIFICACAO                            
################################################################################
# 13.07.2023 - José Caetano Beuker <https://www.linkedin.com/in/jcbeuker/>
# Análise de todos os times campeões do Campeonato Brasileiro - Série A 
# de 2003 a 2022
# Adaptado da Base de dados disponível em:
#   <https://www.kaggle.com/datasets/josevitormichelin/brazilian-football-champi
#    onship-brasileiro>
#
# ANÁLISE DE REGRESSAO
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
# CARREGAMENTO DOS DADOS              
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
# ANALISE EXPLORATORIA DOS CAMPEOES DE CADA ANO            
################################################################################
# Separa os campeoes de cada ano
campeoes <- brasileirao[brasileirao$position == 1,]
print(campeoes)

# Estatistica descritiva
summary(campeoes)

campeoes %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

# Gráfico de linhas
ggplot(campeoes) +
  geom_line(aes(x = points, y = goals_scored, color = team))


################################################################################
#                            GRÁFICO DE DISPERSÃO                              #
################################################################################
# Pontos em função de gols marcados
ggplotly(
  ggplot(campeoes, aes(x = points, y = goals_scored)) +
    geom_point(color = "blue", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(campeoes$points)) +
    ylim(0, max(campeoes$goals_scored)) +
    labs(x = "Pontos",
         y = "Goals Marcados",
         title = paste("R²:",
                       round(((cor(campeoes$points, 
                                   campeoes$goals_scored))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)

# Pontos em função de gols sofridos
ggplotly(
  ggplot(campeoes, aes(x = points, y = goals_against)) +
    geom_point(color = "blue", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(campeoes$points)) +
    ylim(0, max(campeoes$goals_against)) +
    labs(x = "Pontos",
         y = "Goals Marcados",
         title = paste("R²:",
                       round(((cor(campeoes$points, 
                                   campeoes$goals_against))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)


# Pontos em função do saldo de gols
ggplotly(
  ggplot(campeoes, aes(x = points, y = goals_difference)) +
    geom_point(color = "green", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(campeoes$points)) +
    ylim(0, max(campeoes$goals_difference)) +
    labs(x = "Pontos",
         y = "Saldo de Gols",
         title = paste("R²:",
                       round(((cor(campeoes$points, 
                                   campeoes$goals_difference))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)

