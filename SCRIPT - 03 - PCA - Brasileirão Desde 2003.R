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

# Coeficiente de correlação de Pearson para cada par de variáveis
rho <- rcorr(as.matrix(brasileirao[,4:11]), type = "pearson")

corr_coef <- rho$r # Matriz de correlações
corr_sig <- round(rho$P, 5) #Matriz com p-valor dos coeficientes

# Elaboração de um mapa de calor das correlações de Pearson entre as variáveis
ggplotly(
  brasileirao[,4:11] %>% 
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
chart.Correlation(brasileirao[,4:11], histogram = TRUE, pch = "+")

################################################################################
# ANÁLISE FATORIAL POR COMPONENTES PRINCIPAIS
################################################################################
# Teste de esfericidade de Bartlett
cortest.bartlett(brasileirao[,4:11])

# Elaboração da análise fatorial por componentes principais
fatorial <- principal(brasileirao[,4:11],
                      nfactors = length(brasileirao[,4:11]),
                      rotate = "none",
                      scores = TRUE)
fatorial

# Eigenvalues (autovalores)
eigenvalues <- round(fatorial$values, 5)
eigenvalues

# Soma dos eigenvalues = 8 (quantidade de variáveis na análise)
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

## Fatores extraídos a partir de autovalores maiores que 1

# Definição da quantidade de fatores com eigenvalues maiores que 1
k <- sum(eigenvalues > 1)
print(k)

# Elaboração da análise fatorial por componentes principais (k = 3)
fatorial2 <- principal(brasileirao[,4:11],
                       nfactors = k,
                       rotate = "none",
                       scores = TRUE)
fatorial2

# Cálculo dos scores fatoriais
scores_fatoriais <- as.data.frame(fatorial2$weights)

# Visualização dos scores fatoriais
round(scores_fatoriais, 3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 14)
