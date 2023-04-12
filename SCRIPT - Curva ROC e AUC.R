################################################################################
# IDENTIFICACAO                            
################################################################################
# 25.03.2023 - José Caetano Beuker <https://www.linkedin.com/in/jcbeuker/>
# Aprendendo de <https://cienciaenegocios.com/curva-roc-e-auc-em-machine-learning/>
# Curva ROC e AUC
#
# Analisaremos um conjunto de dados dos passageiros do navio Titanic, que naufragou em 1912. Então aplicaremosos modelos
# (regressão logística e rede neural) para calcular o probabilidade de um passageiro, ao embarcar no navio, vir a
# sobreviver à tragédia (y, variável dependente).
#
# O dicionário dos dados, que mostra o significado de cada variável, é o seguinte:
#  
# survived: sobreviveu = 1; não sobreviveu =0 (y, variável dependente);
# pclass: classe (1, 2 e 3, indicando primeira, segunda e terceira classe);
# sex: masculino (male) e feminino (female);
# age: idade do passageiro;
# sibsp: número de irmãos e cônjuge (siblings and spouse) a bordo;
# parch: número de pais e filhos (parents and children) a bordo;
# fare: tarifa paga pelo passageiro.
#
################################################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS            
################################################################################
# Básicos / genéricos
install.packages("knitr", "tidyverse", "neuralnet", "pROC", "magrittr", "dplyr")
library(knitr) # Criar HTML
library(tidyverse) # Transformar dados
#
# How to create a basic neural network in R

# First you will need to install a package called "neuralnet", but if you try installing it the traditional way with
# the install.packages() function it won't load due to being made under an older version of R. Instead we will install
# a different package that will let us install the neuralnet package that we want:
install.packages("remotes")
library(remotes)
install_github("cran/neuralnet")
#
# Específicos deste script
library(neuralnet) # Redes Neurais
library(pROC) # Curva ROC
#
library(magrittr) # package installations are only needed the first time you use it
library(dplyr)    # alternative installation of the %>%
#
#
################################################################################
# DEFININDO FÓRMULAS QUE SERÃO UTILIZADAS NOS MODELOS              
################################################################################
# Para garantir que seja a mesma, onde survived é a variável dependente (y) e as
# demais são as variáveis preditoras (x)
fml_y_survived <- formula(survived ~ pclass2 + pclass3 + sexmale + age +sibsp +
                            parch + fare)
#
################################################################################
# DOWNLOAD E SELECAO DAS VARIAVEIS DESEJADAS              
################################################################################
df_titanic <-
  readr::read_csv("https://gitlab.com/dados/open/raw/master/titanic.csv") %>%
  dplyr::select(passengerid, survived, pclass, sex, age, sibsp, parch, fare)
#
# Head dos dados
head(df_titanic, 5) %>%  knitr::kable()
#
# Estatistica descritiva
summary(df_titanic)
#
################################################################################
# TRATAMENTO DOS DADOS                            
################################################################################
df_titanic_tratado <-
  df_titanic %>%
  dplyr::mutate_if(is.character, as.factor) %>% # Transformando de character para factor
  dplyr::mutate(survived = as.factor(survived), # Transformando de numéricas para factor
                pclass = as.factor(pclass)
  ) %>%
  bind_cols(as_tibble(model.matrix(~ sex + pclass - 1, data = .))) %>% # Criando variáveis binárias
  na.omit() # Excluindo NA's
df_titanic_tratado$passengerid <- as.character(df_titanic_tratado$passengerid) # Transformando de numérica para character
#
# Estatistica descritiva apos tratamento
summary(df_titanic_tratado)
#
################################################################################
# DIVIDINDO O DATASET EM TREINO E TESTE                                  
################################################################################
# O seed é um número que garante a geração "aleatória" do computador será sempre
# a mesma. Garantindo que este exemplo é repoduzível.
set.seed(123)
#
# Criar o subset de treino
train <- df_titanic_tratado %>% dplyr::sample_frac(.7)
#
# Criar o subset de teste com antijoin (pega tudo que não pertence)
test <- dplyr::anti_join(df_titanic_tratado, train, by = 'passengerid')
#
################################################################################
#  MODELO 1
# REGRESSÃO LOGÍSTICA
#################################################################################
# Rodando o modelo
fit_reg_log <-
  glm(
    fml_y_survived,
    family = binomial(link = 'logit'),
    data = train
  )
#
# Fazendo as predições
pred_reg_log <-
  predict(
    fit_reg_log, newdata = test, type = 'response'
  )
#
# Organizando a tabela de dados para calcular as métricas da curva ROC
pred_roc_reg_log <-
  dplyr::tibble(
    pred_reg_log,
    "survived" = as.factor(as.numeric(test$survived)-1)
  ) %>%
  arrange(desc(pred_reg_log))
#
# Curva ROC (Receiver Operating Characteristic) para Regressão Logística
# Criando objeto com as métricas para curva ROC
roc_reg_log <-
  pROC::roc(
    pred_roc_reg_log$survived,
    pred_roc_reg_log$pred_reg_log,
    percent = TRUE
  )
#
# Utilizando o pacote pROC para plotar a curva ROC
par(pty = "s")
plot.roc(
  roc_reg_log,
  print.auc = T,
  legacy.axes = T,
  xlab = "Taxa de Falso Positivo (100 - Especificidade)",
  ylab = "Taxa de Verdadeiro Positivo (Sensibilidade)"
)
#
################################################################################
#  MODELO 2
# REDE NEURAL
#################################################################################
# Estimando uma Rede Neural com duas camadas (hidden layers) de três neurônios
# cada.
set.seed(123)
#
# Estimando o modelo
fit_net <-
  neuralnet(
    fml_y_survived,
    train,
    hidden = c(3,3),
    threshold = .2, # Aumentar o threshold reduz o tempo para rodar, mas
    # compromete a acurácia do modelo.
    err.fct = "sse",
    linear.output = FALSE
  )
#
# Fazendo as predições
pred_net <-
  predict(
    fit_net,
    newdata = test
  )[,2] # Segunda coluna para pegar apenas survived = 1
#
# Curva ROC para Redes Neurais Artificiais
pred_roc_net <-
  tibble(
    pred_net,
    "survived" = as.factor(as.numeric(test$survived)-1)
  ) %>%
  dplyr::arrange(desc(pred_net))
#
roc_net <-
  roc(
    pred_roc_net$survived,
    pred_roc_net$pred_net,
    percent = T
  )
#
par(pty = "s")
plot.roc(
  roc_net,
  print.auc = T,
  legacy.axes = T,
  xlab = "Taxa de Falso Positivo (100 - Especificidade)",
  ylab = "Taxa de Verdadeiro Positivo (Sensibilidade)"
)
#
################################################################################
# COMPARANDO A CURVA ROC DOS DOIS MODELOS
#################################################################################
# Criando um gráfico mesclando as duas curvas e apresentando as AUC
par(pty = "s")
plot(
  roc_reg_log,
  print.auc = T,
  col = "blue",
  main = "ROC",
  legacy.axes = T,
  xlab = "% de Falso Positivo (100- Especificidade",
  ylab = "% de Verdadeiro Positivo (Sensibilidade)"
)
plot(
  roc_net,
  print.auc = T,
  col = "green",
  print.auc.y = 40,
  add = T,
  legacy.axes = T
)
legend(
  "bottomright",
  legend = c("Regressão Logística", "Rede Neural"),
  col = c("blue", "green"),
  lwd = 2
)
#
# Verificando se a AUC (Area Under the Curve) dos dois modelos são
#  estatisticamente diferentes.
# Teste de DeLong. Se p-valor < 0.10, então há diferença, caso contrário não há.
# O teste possui a hipótese nula (h0) de que a AUC dos testes são iguais.
roc.test(
  roc_reg_log,
  roc_net,
  method = "delong"
)
# OBS.: O p-valor do Teste de DeLong mostra que não houve diferença estatística
# nas Curvas ROC para os dois modelos testados. Sendo assim, tanto faz, neste
# caso, escolhermos qualquer um dos modelos.
#
################################################################################
# FIM DO SCRIPT
#################################################################################