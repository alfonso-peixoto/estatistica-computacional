# Alguns comentários

#install.packages("gamlss")
#install.packages("hnp")
#install.packages("ExpDes.pt")

require(gamlss) # deve-se instalar esse pacote
require(hnp) # deve-se instalar esse pacote
require(ExpDes.pt) # deve-se instalar esse pacote
setwd("C:/Users/alfon/OneDrive/Área de Trabalho/ANDRÉ/ESTATÍSTICA COMPUTACIONAL/AVALIAÇÃO/QUESTÃO 4") # deve-se mudar esse diretório 
dados <-read.table("20211031_ANOTAÇÕES_AVALIAÇÃO_N1_Q4.csv", h = T, sep=",") # função para importar os dados
dados
attach(dados) # esse função permite trabalhar com as colunas do cojunto de dados
  doses <- with(dados,Doses) # nomeando os dados com a função with ()
 vivos <- with(dados,Vivos)
mortos <- c(50-vivos)
 total <- with(dados,M)
  resp <- cbind(mortos,total-mortos)
     y <- mortos 
     n <- length(y)
attach(dados)
plot(doses,mortos/total, xlab = "Doses", ylab = "Proporções observadas", pch="*",col="red")

m1 <- glm(resp~doses, family = 'binomial') # pacote glm para trabalhar os dados de proporção.
 x <- model.matrix(m1);x
summary(m1)


plot(c(10,45), c(0,0.5), type="n", xlab="Doses", ylab ="Proporções observadas",col="red")
points(doses,mortos/total,pch="*",col="red")
x1 <-seq(10,200,0.2)
lp <- -3.774090 + 0.13953*x1
pe <- exp(lp)/(1 + exp(lp))
lines(x1, pe,lty=1, col = "blue")
title("Valores observados e curva ajustada")

# utilizando o pacote hnp para verificar a qualidade do ajuste.

 residuo = residuals(m1)
 
 hnp( residuo , sim = 1000,  halfnormal = F)

 




