#title: "Questão 3"
#author: "André Alfonso Peixoto"
#date: "07/11/2021"
#output:
#html_document:
#df_print: paged
#pdf_document: default

######## ----------------- Entrada de dados  ---------------- ########

#install.packages("ExpDes")
#require(ExpDes)
#require(Rmisc)

setwd("C:/Users/alfon/OneDrive/Área de Trabalho/ANDRÉ/ESTATÍSTICA COMPUTACIONAL/AVALIAÇÃO/QUESTÃO 3")

Y <- c(83, 86, 103, 116, 132,
       63, 69, 79, 81, 98,
       55, 61, 79, 79, 91)
Y

Tra <- gl(3,
          5,
          labels = LETTERS[1:3])
Tra

r   <- rep(1:5,
           3)

r

dados <- data.frame(Tra,
                    r,
                    Y)
dados

attach(dados)

# Análise descritiva:  gráfico do box_plot por tratamento 

boxplot(Y~Tra, data = dados, col = c(1,2,3))

M1 = aov(Y ~ Tra, data=dados)

summary.lm(M1)
anova(M1)


T1 = c(83, 86, 103, 116, 132)
T2 = c(63, 69, 79, 81, 98)
T3 = c(55, 61, 79, 79, 91)


T1 = sum(T1)
T2 = sum(T2)
T3 = sum(T3)

T1;T2;T3

## Vamos fazer a tabela da Análise de Variância:

## Soma de quadrado total

Y = dados$Y

# Fórmula SQT = Soma (Y^2) - (Soma(Y))^2/I*J

SQT = sum(Y^2) - (sum(Y))^2/20
SQT


## Soma de quadrado de tratamento


# Fórmula SQTrat = (1/J)*Soma (Ti^2) - (Soma(Y))^2/I*J

SQTrat = (1/5)*(T1^2+T2^2+T3^2) - (sum(Y))^2/15
SQTrat

##########################################################

#  Quadro: Análise de Variância
#  ________________________________________________________
#  FV       GL         SQ            QM        F
#  ________________________________________________________
#  Trat      3         163.75    54.58       7.7976 ^**
#  Res      16         112.00     7.00
#  ________________________________________________________
#  Total    19         275.75
#  _________________________________________________________



# Vamos fazer os teste Tukey:
# Fórmula;
# Delta = q(I,Gl do residuo)*sqrt(QMRes/J)
# Então: q(I,Gl do residuo) = q(5,25)= 4,17
# Ordena as médias:+

Delta = 4.05*sqrt(7/5)
Delta

# Ordena as médias:

MT1 = sum(T1)/5
MT2 = sum(T2)/5
MT3 = sum(T3)/5

MT1;MT2;MT3

# Ordenar as médias

MT2; MT3; MT1
# MT2 = 27 a b
# MT3 = 26   b
# MT1 = 23   b

# Conclusão: Médias seguidas de mesma letra não diferem pelo teste de Tukey a 5% de probabilidade

# Utilizando o pacote  ExpDes

require(ExpDes)

crd(Tra, Y, quali = TRUE, sigF = 0.05)


##¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
## Contrastes ortogonais
##¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
#A   B   C   D
#
#cmat <- rbind('A vs B, C e D'  = c( 3, -1, -1, -1),   # Define a matriz dos contrastes ortogonais
#              'B vs C'         = c( 0,  2, 1,  -1),
#              'C vs D'         = c( 0,  0, 1,  -1))
#cmat
#
#
#
## Carrega pacote necessário

#require(MASS)
#require(gmodels)

#M2 <- aov(Y ~ Tra,
#           data = dados,
#           contrasts = list(Tra=make.contrasts(cmat)))  # make.contrasts (gmodels): gera matriz dos contrastes


#summary(M2,                                          # ANOVA com a SQDtra e GLtra desdobrados em contrastes ortogonais
#        split=list(Tra=list('(A) vs. (B,C,D)' =3,
#                             'B vs. C '       =2,
#                             'C vs. D'        =1)))