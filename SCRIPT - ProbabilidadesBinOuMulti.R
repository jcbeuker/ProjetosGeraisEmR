# Função para Calcular probabilidade em modelo logístico binário
probabilidadeBi <- function(alfa, beta1, x1, beta2, x2){
probabilidadeEv = 1 / (1 + exp(-(alfa + (beta1 * x1) + (beta2 * x2))))
print(paste("Probabilidade de evento = ", probabilidadeEv))
probabilidadeNEv = 1 / (1 + exp(alfa + (beta1 * x1) + (beta2 * x2)))
print(paste("Probabilidade de não evento = ", probabilidadeNEv))
}

probabilidadeBi(alfa = -26.16654, beta1 = 0.19038, x1 = 6, beta2 = 2.36288, x2= 10)


# Função para Calcular probabilidade em modelo logístico multinomial (3 possibilidades)
probabilidadeMulti <- function(alfaz1, beta1z1, x1z1, beta2z1, x2z1, alfaz2, 
                               beta1z2, x1z2, beta2z2, x2z2){
  probabilidadeEvento0 = (1 / (1 + exp(alfaz1 + (beta1z1 * x1z1) + (beta2z1 * x2z1)) 
                            + exp((alfaz2 + (beta1z2 * x1z2) + (beta2z2 * x2z2)))))
  print(paste("Evento0 = ", format(round(probabilidadeEvento0, 5), nsmall = 5)))
  probabilidadeEvento1 = (exp(alfaz1 + (beta1z1 * x1z1) + (beta2z1 * x2z1)) 
                          / (1 + exp(alfaz1 + (beta1z1 * x1z1) + (beta2z1 * x2z1)) 
                              + exp((alfaz2 + (beta1z2 * x1z2) + (beta2z2 * x2z2)))))
  print(paste("Evento1 = ", format(round(probabilidadeEvento1, 5), nsmall = 5)))
  probabilidadeEvento2 = (exp(alfaz2 + (beta1z2 * x1z2) + (beta2z2 * x2z2)) 
                          / (1 + exp(alfaz1 + (beta1z1 * x1z1) + (beta2z1 * x2z1)) 
                             + exp((alfaz2 + (beta1z2 * x1z2) + (beta2z2 * x2z2)))))
  print(paste("Evento2 = ", format(round(probabilidadeEvento2, 5), nsmall = 5)))
}

probabilidadeMulti(alfaz1 = -33.06853, beta1z1 = 0.5574586, x1z1 = 22, beta2z1 = 1.666924, x2z1 = 12, 
                   alfaz2 = -62.21623 , beta1z2 = 1.0766952, x1z2 = 22, beta2z2 = 2.891689, x2z2 = 12)

                   