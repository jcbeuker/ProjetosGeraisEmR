################################################################################
# IDENTIFICACAO                            
################################################################################
# 13.07.2023 - José Caetano Beuker <https://www.linkedin.com/in/jcbeuker/>
# Análise de todos os times e suas respectivas posicoes no Brasileirao 
# de 2003 a 2022
# Adaptado da Base de dados disponível em:
#   <https://www.kaggle.com/datasets/josevitormichelin/brazilian-football-champi
#    onship-brasileiro>
#
# ANÁLISE DE EXPLORATÓRIA
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
             "PerformanceAnalytics",
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

# Converte a coluna goals_difference para numérica
#brasileirao <- transform(brasileirao,
 #                        goals_difference = as.numeric(goals_difference))


# Estrutura da tabela após a conversão
#str(brasileirao)

# Dimensoes da base
dim(brasileirao)

# Lista
brasileirao %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 14)

# Histograma
ggplot(data = brasileirao) + 
  geom_histogram(aes(x = points), color = "black", fill = "light green", 
                 bins = 20) +
  labs(x = "Pontos",
       y = "Frequência") +
  theme_bw()

# Gráfico de pontos 
ggplot(brasileirao) +
geom_point(aes(x = points, y = goals_scored, size = goals_difference, 
               color = position == 1, shape = points > 80)) +
  geom_smooth(aes(x = points, y = goals_scored), method = "loess", se = FALSE) +
  labs(title = "Times no Brasileirao 2003 a 2023",
       x = "Pontos",
       y = "Gols Marcados") +
  theme_bw()

# Gráfico de calor das correlações
  brasileirao_correlacoes <- cor(brasileirao[, 4:9])
  
  brasileirao_correlacoes
  
  brasileirao_correlacoes_reorg <- melt(brasileirao_correlacoes)
  
  brasileirao_correlacoes_reorg
  
  ggplotly(
  ggplot(brasileirao_correlacoes_reorg) +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) +
    geom_text(aes(x = Var1, y = Var2, label = value), size = 5) +
    labs(x = "Variáveis",
         y = "Variáveis",
         fill = "Coef. Correl.") +
    scale_fill_gradient2(low = "blue",
                        mid = "yellow",
                        high = "red",
                        midpoint = 0)
  )
  
# Gráfico Boxplot
  ggplotly(
    ggplot(brasileirao) + 
      geom_boxplot(aes(y = points), fill = "gray", color = "blue") + 
      labs(x = "Pontos",
           y = "valores")
  )
  

# Ferramenta Interativa
esquisser(brasileirao, viewer = "browser")

################################################################################
# ANÁLISE DE CLUSTER
################################################################################
# Separando as estatísticas de cada time em uma lista
# Funcao 'Aggregate' do pacote 'AggregateR'
tabelaTimesBrasileiraoAggregate <-Aggregate(x=brasileirao, by='team')

tabelaTimesBrasileiraoTodos <- select(tabelaTimesBrasileiraoAggregate, 
                                      team, 
                                      points_sum,
                                      points_mean,
                                      games_sum,
                                      victories_sum,
                                      draws_sum,
                                      losses_sum,
                                      goals_scored_sum,
                                      goals_scored_mean,
                                      goals_against_sum,
                                      goals_against_mean,
                                      goals_difference_sum)

## As variáveis apresentam unidades de medida e amplitudes distintas
# Padronizando as variáveis
tabelaTimesBrasileiraoTodosPadronizado <- as.data.frame(scale(
  tabelaTimesBrasileiraoTodos[,2:12]))
rownames(tabelaTimesBrasileiraoTodosPadronizado) <- 
  tabelaTimesBrasileiraoTodos$team

## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. Por exemplo:
round(mean(tabelaTimesBrasileiraoTodosPadronizado$points_sum),3)
round(sd(tabelaTimesBrasileiraoTodosPadronizado$points_sum),3)

## Esquema de aglomeração hierárquico

# Matriz de dissimilarides
matriz_D <- tabelaTimesBrasileiraoTodosPadronizado %>% 
  dist(method = "euclidean")

# 1º Teste: elaboração da clusterização hierárquica como 'single linkage'
cluster_hier_single <- agnes(x = matriz_D, method = "single")

# Construção do dendograma 'single linkage'
dev.off()
fviz_dend(x = cluster_hier_single, show_labels = T)

## O método de encadeamento single linkage não permite uma clusterização útil
## Pode-se interpretar que as observações estão muito próximas umas das outras

# 2º Teste: elaboração da clusterização hierárquica com 'complete linkage'
cluster_hier_complete <- agnes(x = matriz_D, method = "complete")

# Construção do dendograma 'complete linkage'
fviz_dend(x = cluster_hier_complete, show_labels = T)

## O método de encadeamento complete linkage melhora significativamente

# 3º Teste: elaboração da clusterização hierárquica como 'average linkage'
cluster_hier_average <- agnes(x = matriz_D, method = "average")

# Construção do dendograma 'average linkage'
fviz_dend(x = cluster_hier_average, show_labels = T)

## Vamos optar pelo complete linkage (average cria clusters com menos 
# observações)

# Dendograma com visualização dos clusters selecionando por 'altura'
fviz_dend(x = cluster_hier_complete,
          h = 5.5,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = T,
          ggtheme = theme_bw())

# Formam 8 clusters cortando o dendograma em 5.5

# Detalhando o esquema hierárquico
coeficientes <- sort(cluster_hier_complete$height, decreasing = F)
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Portanto, vamos gerar uma variável indicando 8 clusters
tabelaTimesBrasileiraoTodos$cluster_Hier <- 
  factor(cutree(tree = cluster_hier_complete, k = 8))

tabelaTimesBrasileiraoTodosPadronizado$cluster_Hier <-
  factor(cutree(tree = cluster_hier_complete, k = 8))

# Verificando se todas as variáveis ajudam na formação dos grupos
summary(anova_points_sum <- aov(formula = points_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))


################################################################################
# REGRESSÃO LINEAR SIMPLES
################################################################################
# Pontos em função de gols marcados
ggplotly(
  ggplot(brasileirao, aes(x = points, y = goals_scored)) +
    geom_point(color = "blue", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(brasileirao$points)) +
    ylim(0, max(brasileirao$goals_scored)) +
    labs(x = "Pontos",
         y = "Goals Marcados",
         title = paste("R²:",
                       round(((cor(brasileirao$points, 
                                   brasileirao$goals_scored))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)

# Pontos em função de gols sofridos
ggplotly(
  ggplot(brasileirao, aes(x = points, y = goals_against)) +
    geom_point(color = "blue", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(brasileirao$points)) +
    ylim(0, max(brasileirao$goals_against)) +
    labs(x = "Pontos",
         y = "Goals Marcados",
         title = paste("R²:",
                       round(((cor(brasileirao$points, 
                                   brasileirao$goals_against))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)

# Pontos em função do saldo de gols
ggplotly(
  ggplot(brasileirao, aes(x = points, y = goals_difference)) +
    geom_point(color = "green", size = 2.0) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~x, se = F, linewidth = 1) +
    xlim(0, max(brasileirao$points)) +
    ylim(0, max(brasileirao$goals_difference)) +
    labs(x = "Pontos",
         y = "Saldo de Gols",
         title = paste("R²:",
                       round(((cor(brasileirao$points, 
                                   brasileirao$goals_difference))^2),4))) +
    scale_color_manual("Legenda:",
                       values = "grey",) +
    theme_classic()
)

################################################################################
#           MODELAGEM DE UMA REGRESSÃO LINEAR SIMPLES PARA O EXEMPLO 01        
################################################################################
# Estimando o modelo
modelo_brasileirao_PontosGolsPro <- lm(formula = points ~ goals_scored,
                         data = brasileirao)

# Observando os parâmetros
summary(modelo_brasileirao_PontosGolsPro)

# Plotando intervalo de confiança de 95%
ggplotly(
  ggplot(modelo_brasileirao_PontosGolsPro, aes(x = points, y = goals_scored)) +
    geom_point(color = "#39568CFF") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x,
                level = 0.95) +
    labs(x = "Pontos",
         y = "Gols Marcados") +
    scale_color_manual("Legenda:",
                       values = "grey50")+
    theme_bw()
)
