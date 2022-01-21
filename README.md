```{r}
library(readxl)
library(car)
library(lattice)
library(corrgram)
library(lmtest)
library(MASS) # Box y Cox
library(ggplot2)
library(lattice)
```


 + Cargar base
 

```{r}
load("Ansiedad.Rdata")
```


## Análisis de valores extremos 

  ## casos de influencia 


```{r}
modex = lm(AR ~ edad + sexo + estud + empleo + satisfaccion + ocio + dormir + fisica + aliment + miedo, data = Ansiedad)
```

 
```{r}
n=nrow(Ansiedad)
p=length(modex)

dffits = dffits(modex) 

plot(abs(dffits),ylab="DFFITS",main="DFFITS")

lim = 2**sqrt((p+1)/(n-p-1))

abline(h=lim)
```
 
  R/ Vemos que 1 punto sobresale. Es decir, es posible que este valor respresente un caso de influencia sobre su propia estimacion.
  
  
 Grafico de de las distancias con las líneas de probabilidad en 0.1, 0.2 y 0.5 
 
```{r}
t=rstudent(modex)
cook = cooks.distance(modex) 
plot(cook, ylab = "Distancia de Cook", main = "Distancias de Cook")
q=qf(c(0.1, 0.2, 0.5), p, n-p)
abline(h = q, col = c(1, 4, 2))

# el estudiante que tiene un residual estudentizado más alto es:
max(t)
```
 
 
 Nota caracteristicas de valor extremo encontrado:
 24, Mujer, 40 horas estudio semanal, no tiene empleo, satisfaccion 7, dormir 7 horas, fisica 5 horas, aliment 4 veces, AR 69, miedo 30
 
 R/ La estudiante numero 30 tiene influencia sobre las estimaciones generales y esta influencia es mediana.

 
  Identificar en cuáles de los coeficientes hay mayor influencia por parte de los estudiantes que se han identificado.
  
```{r}
dfbetas = dfbetas(modex)
head(dfbetas)
```

```{r}
matplot(abs(dfbetas[,-1]),ylab="Cambio en coeficientes",pch=18)
lim=2/sqrt(n)
abline(h=lim)
```

 R/ Según los gráficos la presencia del estudiante 30 hace que cambien todos los coeficientes.

```{r}
basef =  Ansiedad[-30,]
```


```{r}
modex = lm(AR ~ edad + sexo + estud + empleo + satisfaccion + ocio + dormir + fisica + aliment + miedo, data = basef)
```


 + Eliminacion de valores extremos para ajustar linealidad
 
```{r}
crPlots(modex, main="Residuales parciales", ylab="")
```

 R/ Observamos valores extremos en miedo y aliment ya que alteran la linealidad de los residuales contra el predictor.

  
```{r}
basef = na.omit(basef)
```
  
  
```{r}
which(basef$edad > 26)
which(as.numeric(basef$ocio) > 40)
which(basef$aliment > 10)
which(basef$satisfaccion < 5)

modex = lm(AR ~ edad + sexo + estud + empleo + satisfaccion + ocio + dormir + fisica + aliment + miedo + I(miedo^2), data = basef[-c(4, 11, 16, 28, 31, 32, 35, 36), ])

drop1(modex, test = 'F')
```

 R/ Se observa que hay existencia de algunos valores extremos que pueden estar afectando la muestra. Al final la muestra elegida es de 28 estudiantes
 
```{r}
basex = basef[-c(4, 11, 16, 28, 31, 32, 35, 36), ]
```
 
 
```{r}
modsup = lm(AR ~ edad + sexo + estud + empleo + satisfaccion + ocio + dormir + fisica + aliment + miedo + I(miedo^2), data = basex)
```

 R/ modsup es el modelo a ultilizar para analisis de supuestos 


## Analisis descriptivo

   # Correlacciones 


```{r}
base1 = basex[,-c(2, 4)]
scatterplotMatrix(base1)
```

```{r}
corrgram(base1, diag.panel=panel.density, upper.panel=panel.conf)
```

 R/ Las conclusiones acerca de estas correlaciones se haran en el documento escrito.

 ## Diagnosticos analisis de supuestos 

 + Homocedasticidad

 + Grafico para analizar homocedasticidad
 
```{r}
plot(modsup$fit,modsup$res,xlab = "Valores ajustados", ylab = "Residuales")
```
 
 R/ Los residuales parecen seguir un comportamiento aleatorio y de varianza constante.
 
 
 + Grafico de las variables predictoras contra los residuales para analizar homocedasticidad
 
```{r}
# Variables cuantitativas
plot(basex$edad, modsup$residuals)
plot(basex$estud, modsup$residuals)
plot(basex$ocio, modsup$residuals)
plot(basex$dormir, modsup$residuals)
plot(basex$fisica, modsup$residuals)
plot(basex$aliment, modsup$residuals)
plot(basex$miedo, modsup$residuals)

# Variables categoricas
ggplot(basex, aes(x=basex$sexo, y=modsup$residuals)) +  
  geom_boxplot()
ggplot(basex, aes(x=basex$empleo, y=modsup$residuals)) +  
  geom_boxplot()
ggplot(basex, aes(x=basex$satisfaccion, y=modsup$residuals)) +  
  geom_boxplot()
```
 
 R/ Graficos presentan homosedasticidad 
 
 + Prueba formal Breush Pagan
 
 H0: Hay homocedasticidad
 H1: No hay homocedasticidad
 
```{r}
bptest(modsup, studentize = F)
```
 
 R/ Con una siginificancia del 5%, no hay suficiente evidencia estadistica para rechazar la H0 de que hay homocedasticidad. Es decir, se puede asumir homocedasticidad.
 
 + Normalidad
 
```{r}
qqPlot(modsup$residuals, ylab = "Residuales")
```

 R/ Aqui observamos que se cumple el supuesto de normalidad. Los valores se encuentran dentro de las bandas de confianza de la distribucion teorica por ende se asume normalidad.
 
 
 
 ## Seleccion de variables
 
```{r}
modsup = lm(AR ~ edad + sexo + estud + empleo + satisfaccion + ocio + dormir + fisica + aliment + miedo + I(miedo^2), data = basex)

modA = modsup

step(modA)
```
 

 + Modelo final
 
```{r}
modf = lm(AR ~ sexo + empleo + satisfaccion + ocio + dormir + fisica + miedo + I(miedo^2), data = basex)
```
 

 + Prueba de hipotesis de no-interaccion

 Graficos interaccion 
 
```{r}
xyplot(AR ~ ocio, groups = empleo, type = c("p","r"), pch = 18 ,ylab = "Ansiedad rasgo" , xlab = "Promedio de horas de ocio pasivo a la semana", auto.key = T , data = basex)
```
 
 R/ Parece haber interaccion entre sexo y ocio, sexo y dormir, sexo y fisica.
 
 
 + Prueba de hipotesis formal (comparacion de modelos) 
  
  H0 = se prefiere el modelo pequeño modf, el modelo pequeño aporta lo mismo que el grande 
  H1 = el modelo grande aporta mas que el modelo pequeño, se prefiere el modelo grande 
 
```{r}
mods = lm(AR ~ ocio + sexo, data = basex)
modc = lm(AR ~ ocio + ocio*sexo, data = basex)
anova(mods, modc) # No hay interaccion entre sexo y ocio.

mods1 = lm(AR ~ dormir + sexo, data = basex)
modc1 = lm(AR ~ dormir + dormir*sexo, data = basex)
anova(mods1, modc1) # No hay interaccion entre sexo y dormir

mods2 = lm(AR ~ fisica + sexo, data = basex)
modc2 = lm(AR ~ fisica + fisica*sexo, data = basex)
anova(mods2, modc2) # No hay interaccion entre sexo y fisica
```
 
  
  + Eleccion del modelo (modelo sin interaccion)
  
  Modelo escrito:
  
  $X_1=sexo,X_2 = empleo,X_3=satisfaccion,X_4=ocio,X_5=dormir,X_6=fisica,X_7=miedo,X_8=miedo^2$
  
  $\mu_{AR|X}= \beta_0+\beta_1X_1+\beta_2X_2+\beta_3X_3+\beta_4X_4+\beta_5X_5+\beta_6X_6+\beta_7X_7+\beta_8X_8$
  
  + Estimacion de coeficientes del modelo e interpretacion:
  
  
```{r}
modfi= lm(AR ~ sexo + empleo + satisfaccion + ocio + dormir + fisica + miedo + I(miedo^2), data = basex)
```
  
  
 + Interpretacion miedo^2 y grafico para comprender como se comporta este predictor contra la respuesta.

```{r}
# Grafico sin centrar miedo
b = modfi$coefficients

summary(basex$satisfaccion)  # Se escoge 6
summary(basex$ocio)
summary(basex$dormir)
summary(basex$fisica)

xh = c(1, 0, 0, 6, 10, 8, 4)%*%b[1:7]

curve((xh + b[8]*x + b[9]*x^2), 25, 50, xlab = "Miedo a la evaluacion negativa", ylab = "Ansiedad rasgo promedio",ylim=c(20,75)) # Asi se comporta la respuesta promedio dada la escala de miedo


xh1 = c(1, 1, 0, 6, 10, 8, 4)%*%b[1:7]

curve((xh1 + b[8]*x + b[9]*x^2), add = T, col = 4)

legend("topleft", c("Mujeres", "Hombres"), col = c(1,4), lty = 1, bty = "n")
```

 + Como miedo y miedo^2 estan muy correlacionadas entonces se decide centrar la variable para eliminar este problema.

```{r}
basex$miedoc = basex$miedo - mean(basex$miedo)
modfi= lm(AR ~ sexo + empleo + satisfaccion + ocio + dormir + fisica + miedoc + I(miedoc^2), data = basex)
vif(modfi)
```



