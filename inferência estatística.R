
###-----Distribui??o Amostral / Estima??o-----###

n=2000
set.seed(100)
dados1=rnorm(n,mean=3, sd=1)
dados1     # Popula??o de 1000 elementos

hist(dados1)


amostra1=sample(dados1, 20) # amostra tamanho 20
amostra2=sample(dados1, 80) # amostra tamanho 50
amostra3=sample(dados1, 150) # amostra tamanho 150

# medias amostrais:
xbar1=mean(amostra1)
xbar1
xbar2=mean(amostra2)
xbar2
xbar3=mean(amostra3)
xbar3
  

# erros padrão amostrais: 1/sqrt(n) = 0.02236
sd(amostra1)/sqrt(20)
sd(amostra2)/sqrt(80)
sd(amostra3)/sqrt(150)


###------Intervalo de Confian?a------####

#---exercício 1:----#

n = 100 #resmas
X_bar = 1.5 #minutos/resmas
dp = 0.30 #desvio padrão
alfa = 0.05 #significância
qt.z=qnorm(alfa/2, lower.tail = FALSE) # quantil da distrbuição normal
qt.z
IC_Med=c(X_bar-qt.z*dp/sqrt(n),X_bar+qt.z*dp/sqrt(n))
IC_Med

#---Exercício 2:----#

n=500 #amostra de válvulas
X_bar= 700 #média amostral 700horas 
s= 90 #desvio padrão amostral
alfa=0.1 #significância
qt.t=qt(alfa/2,(n-1), lower.tail = FALSE)  #quantil da distrbuição t
qt.t

IC_Med=c(X_bar-qt.t*s/sqrt(n), X_bar+qt.t*s/sqrt(n))
IC_Med

#---Exercício 3:----#

n = 500 # tamanho da amostra do eleitorado
p = 260/500 # proporção estimada de candidatos de b)
alfa = 0.10 # significância do teste
qt.z=qnorm(alfa/2, lower.tail = FALSE) # quantil da distrbuição normal
qt.z
IC_prop=c(p-qt.z*sqrt((p*(1-p))/n),
          p+qt.z*sqrt((p*(1-p))/n))
IC_prop

#---Exercício 4:----#

n = 625 # tamanho da amostra 
p = 0.70*625/625 # proporção estimada de marca A)
alfa = 0.10 # significância do teste
qt.z=qnorm(alfa/2, lower.tail = FALSE) # quantil da distrbuição normal
qt.z

IC_prop=c(p-qt.z*sqrt((p*(1-p))/n),
          p+qt.z*sqrt((p*(1-p))/n))
IC_prop

#---Exercício 5:----#
n = 20 #garrafas
s2 = 0.0153 #desvio padrão
alfa = 0.05 #significância

qt.chi1=qchisq(1-alfa/2, (n-1), lower.tail = FALSE)
qt.chi2=qchisq(alfa/2, (n-1), lower.tail = FALSE)

IC_var=c((n-1)*s2/qt.chi2,
         (n-1)*s2/qt.chi1)
IC_var


#---Exemplo 4:----#

diametro=c(10, 13, 14, 11, 13, 14, 11, 13, 14, 15,
           12, 14, 15, 13, 14, 12, 12, 11, 15, 16,
           13, 15, 14, 14, 15, 15, 16, 12, 10, 15)

n=length(diametro)
mean(diametro)
s2=var(diametro)
alfa=0.02
quant.chi1=qchisq(1-alfa/2, (n-1), lower.tail = FALSE)
quant.chi2=qchisq(alfa/2, (n-1), lower.tail = FALSE)

IC_var=c((n-1)*s2/quant.chi2,
         (n-1)*s2/quant.chi1)

IC_var


###------Teste de Hip?teses-------####

#Exemplo 5 (a): Teste para m?dia com vari?ncia conhecida
tempo=c(6.8, 7.1, 5.9, 7.5, 6.3, 6.9, 7.2, 7.6, 6.6, 6.3)
mean(tempo)
var(tempo)

require(BSDA)
z.test(tempo, mu=7.4, sigma.x=sqrt(1.3), alternative ="less", conf.level = 0.99)

#Exemplo 5 (b): Teste para m?dia com vari?ncia desconhecida
t.test(tempo, mu=7.4, alternative ="less", conf.level = 0.99)


#Exemplo 5 (c):
# teste para vari?ncia
require(TeachingDemos)
sigma.test(tempo,sigma=sqrt(1.3), alternative ="two.sided", conf.level = 0.95) # rejeita 


#Exemplo 6:
Tipo_1=c(3250, 3268, 4302, 3184, 3266, 3297, 3332, 3502, 3064, 3116)
Tipo_2=c(3094, 3268, 4302, 3184, 3266, 3124, 3316, 3212, 3380, 3018)
mean(Tipo_1)
mean(Tipo_2)

require(BSDA)
z.test(x=Tipo_1, y=Tipo_2, mu = 0, sigma.x=353, sigma.y=363, alternative ="two.sided", conf.level = 0.95)

#Exemplo 7:
X=c(54, 55, 58, 50, 61)
Y=c(51, 54, 55, 52, 53)

mean(X)
mean(Y)
var(X)
var(Y)

# teste para vari?ncias
var.test(X, Y, alternative ="two.sided", conf.level = 0.9) # rejeita igualdade de var 

# teste de compara??o de m?dias
t.test(X, Y, alternative ="greater",var.equal = FALSE, conf.level = 0.95)


#Exemplo 8:
CAT_A=c(45, 51, 50, 62, 43, 42, 53, 50, 48, 55)
CAT_B=c(45, 35, 43, 59, 48, 45, 41, 43, 49, 39)

mean(CAT_A)
mean(CAT_B)
var(CAT_A)
var(CAT_B)

# teste para vari?ncias
var.test(CAT_A, CAT_B, alternative ="two.sided", conf.level = 0.9) # N?o rejeita igualdade de var 

# teste de compara??o de m?dias
t.test(CAT_A, CAT_B, alternative ="greater",var.equal = TRUE, conf.level = 0.95)



#Exemplo 9 - Amostras Pareadas
Antes= c(1505, 1419, 1504, 1494, 1510, 1506)
Depois=c(1625, 1511, 1459, 1441, 1472, 1521)

# teste de compara??o de m?dias
t.test(Antes, Depois, alternative ="less", mu = 0, paired = TRUE, conf.level = 0.99)


