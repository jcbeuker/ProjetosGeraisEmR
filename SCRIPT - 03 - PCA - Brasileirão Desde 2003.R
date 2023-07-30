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
# ANÁLISE FATORIAL POR COMPONENTES PRINCIPAIS
# 
################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            
################################################################################
pacotes <- c("plotly", #plataforma gráfica
              "tidyverse", #carregar outros pacotes do R
              "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a
              #evitar sobreposição de textos
              "knitr", "kableExtra", #formatação de tabelas
              "reshape2", #função 'melt'
              "PerformanceAnalytics", #função 'chart.Correlation' para plotagem
              "psych", #elaboração da fatorial e estatísticas
              "ltm", #determinação do alpha de Cronbach pela função 'cronbach.alpha'
              "Hmisc", # matriz de correlações com p-valor
              "readxl") # importar arquivo Excel

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% install.packages())) != 0){
 instalador <- pacotes[!pacotes %in% install.packages()] 
 for (i in 1:length(instalador)) {
   install.packages(instalador, dependencies = T)
   break()
   }
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

# Scatter e ajuste linear entre as variáveis 'points' e 'goals_scored'
brasileirao %>% 
  ggplot() +
  geom_point(aes(x = points, y = goals_scored),
             color = "darkorchid",
             size = 3) +
  geom_smooth(aes(x = points, y = goals_scored),
              color = "orange",
              method = "lm",
              formula = y ~ x,
              se = FALSE,
              size = 1.3)+
  labs(x = "Pontos",
       y = "Gols Marcados") +
  theme_bw()

# Coeficiente de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(brasileirao[,4:12]), type = "pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) #Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  brasileirao[,4:12] %>% 
    cor() %>% 
    melt() %>% 
    rename(Correlação = value) %>% 
    ggplot() +
    geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
    geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 1)),
              size = 5) +
  scale_fill_viridis_b() +
  labs(x = NULL, y = NULL) +
    theme_bw()
)

# Visualização das distribuições das variáveis, scatters, valores das correlações
chart.Correlation(brasileirao[,4:12], histogram = TRUE, pch = "+")

################################################################################
# ANÁLISE FATORIAL POR COMPONENTES PRINCIPAIS
################################################################################
# Teste de esfericidade de Bartlett
cortest.bartlett(brasileirao[,4:12])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(brasileirao[,4:12],
                      nfactors = length(brasileirao[,4:12]),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues

# Soma dos eigenvalues = 9 (quantidade de variáveis na análise)
# Também representa a quantidade máxima de possíveis fatores na análise
round(sum(eigenvalues), 2)

# Identificação da variância compartilhada em cada fator
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Variância compartilhada pelas variáveis originais para a formação de cada fator
round(variancia_compartilhada, 3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Cálculo dos fatores propriamente ditos
fatores <- as.data.frame(fatorial$scores)
print(fatores)

# Coeficiente de correlação de Pearson para cada par de fatores (ortogonais)
rho <- rcorr(as.matrix(fatores), type = "pearson")
round(rho$r, 4)

# Cálculo das cargas fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualização das cargas fatoriais
round(cargas_fatoriais, 3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Cálculo das comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>% 
  rename(comunalidades = 1)

# Visualização das comunalidades (aqui são iguais a 1 para todas as variáveis)
# Foram extraídos 9 fatores neste primeiro momento
round(comunalidades, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 20)

### Elaboração da Análise Fatorial por Componentes Principais ###
### Fatores extraídos a partir de autovalores maiores que 1 ###

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais
# com quantidade 'k' de fatores com eigenvalues maiores que 1
fatorial2 <-  principal(brasileirao[,4:12],
                        nfactors = k,
                        rotate = "none",
                        scores = TRUE)
fatorial2

# Cálculo das comunalidades com apenas 'k' primeiros fatores
comunalidades2 <- as.data.frame(unclass(fatorial2$communality)) %>% 
  rename(comunalidades = 1)

# Visualização das comunalidades com apenas os 'k' primeiros fatores
round(comunalidades2, 3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = 'striped',
                full_width = FALSE,
                font_size = 14)

# Loading plot com as cargas dos 'k' primeiros fatores
cargas_fatoriais[,1:3] %>% 
  data.frame() %>% 
  rownames_to_column("variáveis") %>% 
  ggplot(aes(x = PC1, y = PC2, label = variáveis)) +
  geom_point(color = "darkorchid",
             size = 3) +
  geom_text_repel() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "orange") +
  geom_hline(aes(yintercept = 0), linetype = "dashed", color = "orange") +
  expand_limits(x = c(-1.25, 0.25), y = c(-0.25,1)) +
  theme_bw()

# Adicionando os fatores extraídos no banco de dados original
brasileirao <- bind_cols(brasileirao,
                         "fator 1" = fatores$PC1,
                         "fator 2" = fatores$PC2,
                         "fator 3" = fatores$PC3)

# Criação de um rankin (Critério da soma ponderada e ordenamento)
brasileirao$ranking <- fatores$PC1 * variancia_compartilhada$PC1[2] +
                       fatores$PC2 * variancia_compartilhada$PC2[2] +
                       fatores$PC3 * variancia_compartilhada$PC3[2] 

# Visualizando o ranking final
brasileirao %>% 
  arrange(desc(ranking)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)

# Fim  