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
             "ggplot2",
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

brasileirao <- 
  read.csv( "dataset-Posicoes-Times-Brasileirao-Temporadas-Encerradas.csv"
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
                                      games_sum,
                                      victories_sum,
                                      draws_sum,
                                      losses_sum,
                                      goals_scored_sum,
                                      goals_against_sum,
                                      goals_difference_sum)

## As variáveis apresentam unidades de medida e amplitudes distintas
# Padronizando as variáveis
tabelaTimesBrasileiraoTodosPadronizado <- as.data.frame(scale(
  tabelaTimesBrasileiraoTodos[,2:9]))
rownames(tabelaTimesBrasileiraoTodosPadronizado) <- 
  tabelaTimesBrasileiraoTodos$team

## Todas as variáveis passam a ter média = 0 e desvio padrão = 1. Por exemplo:
round(mean(tabelaTimesBrasileiraoTodosPadronizado$points_sum),3)
round(sd(tabelaTimesBrasileiraoTodosPadronizado$points_sum),3)

#------------------- Esquema de aglomeração hierárquico ------------------------

# Matriz de dissimilarides
matriz_D <- tabelaTimesBrasileiraoTodosPadronizado %>% 
  dist(method = "euclidean")

# Method: parametrização da distância a ser utilizada:
## "euclidean": distância euclidiana
## "euclidiana quadrática": elevar ao quadrado matriz_D (matriz_D^2)
## "maximum": distância de Chebychev;
## "manhattan": distância de Manhattan (ou distância absoluta ou bloco);
## "canberra": distância de Canberra;
## "minkowski": distância de Minkowski

# Visualizando a matriz de dissimilarides
data.matrix(matriz_D) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# 1º Teste: elaboração da clusterização hierárquica como 'single linkage'
cluster_hier_single <- agnes(x = matriz_D, method = "single")

# As distâncias do esquema hierárquico de aglomeração
coeficientes <- sort(cluster_hier_single$height, decreasing = FALSE)
coeficientes

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
          h = 1.7,
          color_labels_by_k = F,
          rect = T,
          rect_fill = T,
          rect_border = "black",
          lwd = 1,
          show_labels = T,
          ggtheme = theme_bw())

# Formam 7 clusters cortando o dendograma em 1.7

# Detalhando o esquema hierárquico
coeficientes <- sort(cluster_hier_complete$height, decreasing = F)
esquema <- as.data.frame(cbind(cluster_hier_complete$merge, coeficientes))
names(esquema) <- c("Cluster1", "Cluster2", "Coeficientes")
esquema

# Portanto, vamos gerar uma variável indicando 7 clusters
tabelaTimesBrasileiraoTodos$cluster_Hier <- 
  factor(cutree(tree = cluster_hier_complete, k = 7))

tabelaTimesBrasileiraoTodosPadronizado$cluster_Hier <-
  factor(cutree(tree = cluster_hier_complete, k = 7))

# Análise de variância de um fator (ANOVA). Interpretação do output:

## Mean Sq do cluster_H: indica a variabilidade entre grupos
## Mean Sq dos Residuals: indica a variabilidade dentro dos grupos
## F value: estatística de teste (Sum Sq do cluster_H / Sum Sq dos Residuals)
## Pr(>F): p-valor da estatística 
## p-valor < 0.05: pelo menos um cluster apresenta média estatisticamente 
## diferente dos demais

## A variável mais discriminante dos grupos contém maior estatística F 
## (e significativa)

summary(anova_points_sum <- aov(formula = points_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_games_sum <- aov(formula = games_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_victories_sum <- aov(formula = victories_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_draws_sum <- aov(formula = draws_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_losses_sum <- aov(formula = losses_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_goals_scored_sum <- aov(formula = goals_scored_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_goals_against_sum <- 
          aov(formula = goals_against_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

summary(anova_goals_difference_sum <- 
          aov(formula = goals_difference_sum ~ cluster_Hier,
                                data = tabelaTimesBrasileiraoTodosPadronizado))

# Todas auxiliam na formação de pelo menos um cluster

# O que os clusters indicam? Vamos interpretar algumas variáveis médias:

analise <- group_by(tabelaTimesBrasileiraoTodos, cluster_Hier) %>% 
  summarise(points_sum = mean(points_sum, na.rm = TRUE),
            games_sum = mean(games_sum, na.rm = TRUE),
            victories_sum = mean(victories_sum, na.rm = TRUE),
            draws_sum = mean(draws_sum, na.rm = TRUE),
            losses_sum = mean(losses_sum, na.rm = TRUE),
            goals_scored_sum = mean(goals_scored_sum, na.rm = TRUE),
            goals_against_sum = mean(goals_against_sum, na.rm = TRUE),
            goals_difference_sum = mean(goals_difference_sum, na.rm = TRUE))

## Por exemplo, os times dos clusters 1 e 2 apresentam:
## Alta soma de pontos, maiores somas de jogos.
## Os times do cluster 3 apresentam a mais baixa soma de pontos.

#------------ Esquema de aglomeração não hierárquico K-MEANS--------------------
# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(tabelaTimesBrasileiraoTodos[,2:9],
                         centers = 4)
# Criando variável categórica para indicação do cluster no banco de dados
tabelaTimesBrasileiraoTodos$cluster_K <- factor(cluster_kmeans$cluster)

# Método de Elbow para identificação do número ótimo de clusters
fviz_nbclust(tabelaTimesBrasileiraoTodos[,2:9], kmeans, 
             method = "wss", k.max = 10)

# Visualização da base de dados
tabelaTimesBrasileiraoTodos %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Análise de variância de um fator (ANOVA)

# ANOVA da variável 'points_sum'
summary(anova_points_sum <- aov(formula = points_sum ~ cluster_K,
                                data = tabelaTimesBrasileiraoTodos))

# ANOVA da variável 'games_sum'
summary(anova_games_sum <- aov(formula = games_sum ~ cluster_K,
                                data = tabelaTimesBrasileiraoTodos))

# ANOVA da variável 'victories_sum'
summary(anova_victories_sum <- aov(formula = victories_sum ~ cluster_K,
                                data = tabelaTimesBrasileiraoTodos))

# Comparando os resultados dos esquemas hierárquico e não hierárquico
tabelaTimesBrasileiraoTodos %>% 
  select(team, cluster_Hier, cluster_K) %>% 
  arrange(team) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

