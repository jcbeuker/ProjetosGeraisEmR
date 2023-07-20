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
