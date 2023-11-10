library(ggplot2)
#usaremos as variáveis mpg (milhas por galão) e disp - Deslocamento do motor (em polegadas cúbicas).
mtcarss = mtcars[, c(1,3)]
summary(mtcarss)

modelo_MRLS = lm(mtcarss$disp~mtcarss$mpg, data = mtcars)
summary(modelo_MRLS)

coeficintes_do_MRLS = coef(modelo_MRLS)
coeficintes_do_MRLS

ggplot(mtcars) + aes(x = mpg, y = disp, fill = 'blue') + geom_point(shape = 17, cex = 1.5, colour = "red") + geom_smooth(method = "lm") + 
theme_bw() + xlab("mpg-milhas por galão") + ylab("disp - Deslocamento do motor (em polegadas cúbicas)") + annotate("text", x = 30, y = 400, label = "MRLS: Y = 580,88-17,43X")#+ xlim(c(0,40)) + ylim(c(0,400)) +
#scale_x_continuous(breaks = seq(0,40,2))

# plot(mtcars$hp~mtcars$mpg, sub = "modelo de regressão", xlab("mpg-milhas por galão") + ylab("disp - Deslocamento do motor (em polegadas cúbicas)")
# abline(coeficintes_do_MRS[1], coeficintes_do_MRS[2])

#Ánalise de variância (anova) verificar se é significativa a relação entre as variáveis --------
anova(modelo_MRLS)
#o p-value representa a probabilidade que a variável não seja relevante para o modelo, vamos utilizar a análise de variância (ANOVA) para visualizarmos o p value. 
#o p-value (Pr(>F)) tem um valor menor que 0,05, ou seja, a relação entre as duas variáveis é significativa.

##análise de residuos

## linearidade das variáveis -----------------------------------------------
cor(mtcarss, method = "kendall")
ggplot(mtcarss) + aes(x = mpg, y = disp) + geom_point(shape = 17, cex = 1.5, colour = "red") +
  theme_bw() + xlab("mpg-milhas por galão") + ylab("disp - Deslocamento do motor (em polegadas cúbicas)") +
  annotate("text", x = 30, y = 400, label = "MRLS: COR: 0.7681311 ")


# Homocedasticidade dos resíduos ------------------------------------------
# plot(rstudent(modelo_RLS) ~ fitted(modelo_RLS), pch = 17)
# abline(h = 0, lty = 3, col = "red")
# 
Dados_MRLS = data.frame(residuos_padron = rstudent(modelo_RLS), 
                       Y_estimado = modelo_RLS$fitted.values)

#gráfico resíduos x valor previsto
ggplot(Dados_MRLS) + aes(x = Y_estimado, y = residuos_padron) + geom_point(shape = 17, cex = 1.5, colour = "red") + 
theme_bw() + ylab("Resíduos estudentizados") + xlab("valor estimado pelo MRLS") + geom_hline(yintercept = 0, lty = 5, colour = "blue")


#gráfico de resíduos x variável (disp)
Dados_MRLS$disp = mtcarss$disp
ggplot(Dados_MRLS) + aes(x = Dados_MRLS$residuos_padron, y = Dados_MRLS$disp) + geom_point(shape = 17, cex = 1.5, colour = "red") + 
  theme_bw() + ylab("Resíduos estudentizados") + xlab("disp - Deslocamento do motor (em polegadas cúbicas)") 
#+ scale_y_continuous(limit = c(-4,4),breaks = seq(-4,4,2)) + geom_hline(yintercept = 0)

#gráfico de resíduos x variável (mpg)
Dados_MRLS$disp = mtcarss$mpg
ggplot(Dados_MRLS) + aes(x = Dados_MRLS$residuos_padron, y = Dados_MRLS$mpg) + geom_point(shape = 17, cex = 1.5, colour = "red") + 
  theme_bw() + ylab("Resíduos estudentizados") + xlab("mpg-milhas por galão") 
#+ scale_y_continuous(limit = c(-4,4),breaks = seq(-4,4,2)) + geom_hline(yintercept = 0)


#gráfico de resíduos x observações
Dados_MRLS$disp = mtcarss$mpg
ggplot(Dados_MRLS) + aes(x = Dados_MRLS$residuos_padron, x = ) + geom_point(shape = 17, cex = 1.5, colour = "red") + 
  theme_bw() + ylab("Resíduos estudentizados") + xlab("mpg-milhas por galão") 
#+ scale_y_continuous(limit = c(-4,4),breaks = seq(-4,4,2)) + geom_hline(yintercept = 0)



#normalidade dos resíduos -----------------------------------------------
 ##teste de normalidade dos resíduos
shapiro.test(modelo_RLS$residuals)
#como p-valor é maior que 0.5 não rejietamos a hipótese de normalidade dos resíduos

 ## histograma normalidade dos resíduos
quebras = pretty(range(modelo_RLS$residuals),
                 n = nclass.Sturges(modelo_RLS$residuals))


ggplot(modelo_RLS, aes(x = modelo_RLS$residuals)) +
  ylab("densidade")+ xlab("resíduos") +
  labs(title = "Histograma", subtitle = "") +  
  geom_histogram(aes(y = stat(density)), fill ="#00FA9A", breaks = quebras) +
  geom_density(col = "red")+
  theme_bw()

#teste de normalidade das variáveis
shapiro.test(mtcars$mpg) #segue distribuição
shapiro.test(mtcars$disp) #não segue distribuição




