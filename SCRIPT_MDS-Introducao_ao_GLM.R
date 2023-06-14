# Curso disponível em https://sites.google.com/view/msdias/courses

# Carrega dados
d <- read_delim("3_Binomial_selecaoModelos.csv", 
                             delim = ";", escape_double = FALSE, trim_ws = TRUE)

head(cbind(d$Morte, d$Total-d$Morte))
