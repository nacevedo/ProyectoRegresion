datos <- Wikipedia_Views

datos$GENERO <- as.numeric(datos$GENERO)
datos$FACULTAD <- as.numeric(datos$FACULTAD)
datos$PhD <- as.numeric(datos$PhD)
datos$CARGO <- as.numeric(datos$CARGO)
datos$LOGINWIKI <- as.numeric(datos$LOGINWIKI)

datos$GENERO <- factor(datos$GENERO)
datos$FACULTAD <- factor(datos$FACULTAD)
datos$PhD <- factor(datos$PhD)
datos$CARGO <- factor(datos$CARGO)
datos$LOGINWIKI <- factor(datos$LOGINWIKI)

datos$GENERO <- relevel(datos$GENERO, ref = 1)
datos$FACULTAD <- relevel(datos$FACULTAD, ref = 6)
datos$PhD <- relevel(datos$PhD, ref = 1)
datos$CARGO <- relevel(datos$CARGO, ref = 6)
datos$LOGINWIKI <- relevel(datos$LOGINWIKI, ref = 1)

reg1 = lm(VISTAS ~., data = datos)

#Multicolinealidad
#pairs(~ VISTAS + EDAD + GENERO + FACULTAD + PhD + EXPERIENCIA + 
       # CARGO +SALARIO +LOGINWIKI+ PEU1 + PEU2 +ENJ1 + ENJ2 +QU+VIS+IM+SA+USE1+USE2+USE3+PF+EXP1+EXP2+EXP3, datos = datos)

library(car)

vif(reg1)

cMulticolinealidad <- lm (VISTAS ~. -ENJ1, data = datos)
vif(cMulticolinealidad)

#Heterocedasticidad
#plot(cMulticolinealidad)

library(lmtest)

bptest(cMulticolinealidad)

bptest(VISTAS ~ EDAD, data = datos)
bptest(VISTAS ~ GENERO, data = datos)
bptest(VISTAS ~ FACULTAD, data = datos)
bptest(VISTAS ~ PhD, data = datos)
bptest(VISTAS ~ EXPERIENCIA, data = datos)
bptest(VISTAS ~ CARGO, data = datos)
bptest(VISTAS ~ SALARIO, data = datos)
bptest(VISTAS ~ LOGINWIKI, data = datos)
bptest(VISTAS ~ PEU1, data = datos)
bptest(VISTAS ~ PEU2, data = datos)
bptest(VISTAS ~ ENJ2, data = datos)
bptest(VISTAS ~ QU, data = datos)
bptest(VISTAS ~ VIS, data = datos)
bptest(VISTAS ~ IM, data = datos)
bptest(VISTAS ~ SA, data = datos)
bptest(VISTAS ~ USE1, data = datos)
bptest(VISTAS ~ USE2, data = datos)
bptest(VISTAS ~ USE3, data = datos)
bptest(VISTAS ~ PF, data = datos)
bptest(VISTAS ~ EXP1, data = datos)
bptest(VISTAS ~ EXP2, data = datos)
bptest(VISTAS ~ EXP3, data = datos)

View(datos)
datosCorregidos1 <- datos[,6:24]/sqrt(datos$IM)
datosCorregidos <- c(datos[,1:5],datosCorregidos1)

regCorregida <- lm (VISTAS ~. -ENJ1 -IM, data = datosCorregidos)
summary(regCorregida)
bptest(regCorregida)


#Comparación de residuales
resi<-matrix(residuals(regCorregida))
resi1<-matrix(c(resi[2:910,],0))
#Gráfico
plot(resi,resi1)

dwtest(regCorregida)




