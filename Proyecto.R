data <- Wikipedia_Views
data$GENERO <- factor(data$GENERO)
data$FACULTAD <- factor(data$FACULTAD)
data$PhD <- factor(data$PhD)
data$CARGO <- factor(data$CARGO)
data$LOGINWIKI <- factor(data$LOGINWIKI)

data$GENERO <- relevel(data$GENERO, ref = "0")
data$FACULTAD <- relevel(data$FACULTAD, ref = 6)
data$PhD <- relevel(data$PhD, ref = "0")
data$CARGO <- relevel(data$CARGO, ref = 6)
data$LOGINWIKI <- relevel(data$LOGINWIKI, ref = "0")

reg1 = lm(VISTAS ~., data = data)

#Multicolinealidad
pairs(~ VISTAS + EDAD + GENERO + FACULTAD + PhD + EXPERIENCIA + CARGO +SALARIO +LOGINWIKI+ PEU1 + PEU2 +ENJ1 + ENJ2 +QU+VIS+IM+SA+USE1+USE2+USE3+PF+EXP1+EXP2+EXP3, data = data)

