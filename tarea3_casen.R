library(haven)
library(dplyr)
library(ggplot2)
library(lmtest)     # bptest, resettest, coeftest
library(sandwich)   # vcovHC (White)
library(car)        # vif

casen <- read_sav("../casen_2024.sav",
                  col_select = c(sexo, edad, esc, ytrabajocor, activ,
                                 o10, o19, o32)) |>
  zap_labels()

# o10 = horas semanales habituales
# o19 = tiene contrato escrito (1 = sí, firmó)
# o32 = cotizó el mes pasado en algún sistema previsional (1 a 5 = sí)
dd <- casen |>
  filter(activ == 1, edad >= 15, ytrabajocor > 0, !is.na(esc),
         o10 > 0, o10 <= 100) |>
  mutate(ln_ingreso = log(ytrabajocor),
         mujer      = as.integer(sexo == 2),
         horas      = o10,
         contrato   = as.integer(o19 %in% 1),
         cotiza     = as.integer(o32 %in% 1:5))

set.seed(106)
dd <- slice_sample(dd, n = 1000, replace = FALSE)

# Base limpia para los estudiantes: la muestra y solo las variables de la tarea
dd <- dd |> transmute(ingreso = ytrabajocor, ln_ingreso, esc, edad, mujer,
                      horas, contrato, cotiza) |>
  zap_label() |> zap_formats() |> zap_widths() |> as.data.frame()
saveRDS(dd, "casen_tarea3.rds")

# Desde aquí es lo que hacen los estudiantes
dd <- readRDS("casen_tarea3.rds")

#a)
m_a <- lm(ln_ingreso ~ esc + mujer + log(horas) + contrato + cotiza, data = dd)
summary(m_a)
# contrato no es significativo: tener contrato no subiría el ingreso?

#b) Multicolinealidad
cor(dd$contrato, dd$cotiza)

vif(m_a)
# contrato tiene el VIF más alto: comparte información con cotiza.

# El VIF "a mano" para contrato: regresión auxiliar contra los demás regresores
aux <- lm(contrato ~ esc + mujer + log(horas) + cotiza, data = dd)
1 / (1 - summary(aux)$r.squared)

# Corrección: índice de formalidad (0 a 2)
dd <- dd |> mutate(formal = contrato + cotiza)
table(dd$formal)

m_b <- lm(ln_ingreso ~ esc + mujer + log(horas) + formal, data = dd)
summary(m_b)
vif(m_b)

#c) Variable omitida: edad
m_c <- lm(ln_ingreso ~ esc + mujer + log(horas) + formal + edad, data = dd)
summary(m_c)                                      # test t de edad: H0 = coeficiente de edad es cero

# Lo mismo como test F (con una restricción, F = t^2)
anova(m_b, m_c)                                   # rechaza: la edad es relevante

# Dirección del sesgo en esc = (efecto de edad en ln_ingreso) x (relación de edad con esc)
coef(m_c)["edad"]                                 # positivo
cor(dd$esc, dd$edad)                              # negativa: los mayores tienen menos escolaridad
coef(lm(edad ~ esc, data = dd))["esc"]
coef(m_b)["esc"]; coef(m_c)["esc"]
# positivo x negativo = sesgo negativo: sin la edad, el retorno a la escolaridad está subestimado.

#d) Especificación
resettest(m_c, power = 2:3, type = "fitted")   # rechaza

# ¿Es lineal el efecto de la escolaridad sobre ln(ingreso)?
dd |> group_by(esc) |> summarise(n = n(), ln_ingreso = mean(ln_ingreso)) |>
  ggplot(aes(esc, ln_ingreso)) + geom_point(aes(size = n)) + geom_smooth(se = FALSE)

# El retorno de un año más de escolaridad es mayor en la educación superior:
# el perfil es convexo. Corrección: agregar escolaridad al cuadrado.
m_d <- lm(ln_ingreso ~ esc + I(esc^2) + mujer + log(horas) + formal + edad,
          data = dd)
summary(m_d)
resettest(m_d, power = 2:3, type = "fitted")   # ya no rechaza

#e) Heterocedasticidad
bptest(m_d)                                       # rechaza H0
coeftest(m_d, vcov = vcovHC(m_d, type = "HC1"))   # errores estándar robustos (White)

#f) Comparación con a)
summary(m_a)$r.squared
summary(m_d)$r.squared
