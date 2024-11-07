# Definir el factor (dosis de fertilización nitrogenada)
dosis <- c(0, 50, 100)  # kg/ha

# Repeticiones
repeticiones <- 5

# Crear el diseño DCA
dca <- design.crd(trt = dosis, r = repeticiones, seed = 123)

# Ver el diseño generado
dca$book


# Definir los factores
dosis <- c(0, 50, 100)  # Factor 1: Dosis de fertilización nitrogenada (kg/ha)
cultivares <- c("Peruanita", "Canchan")  # Factor 2: Cultivares de papa

# Número de repeticiones
repeticiones <- 4

# Crear el diseño DBCA
dbca <- design.rcbd(trt = list(dosis, cultivares), r = repeticiones, seed = 123)

# Mostrar el diseño experimental
print(dbca$book)


# Asegurarse de que agricolae está instalado y cargado
if (!require(agricolae)) {
  install.packages("agricolae")
}
library(agricolae)

# Definir los factores manualmente
dosis <- c(0, 50, 100)  # Factor 1: Dosis de fertilización nitrogenada (kg/ha)
cultivares <- c("Peruanita", "Canchan")  # Factor 2: Cultivares de papa

# Número de repeticiones
repeticiones <- 4

# Generar el diseño DBCA con dos factores
dbca <- design.rcbd(trt = as.factor(interaction(dosis, cultivares)), r = repeticiones, seed = 123)

# Mostrar el diseño experimental
print(dbca$book)


dosis <- c(0, 50, 100)  # Factor 1: Dosis de fertilización nitrogenada (kg/ha)
cultivares <- c("Peruanita", "Canchan")  # Factor 2: Cultivares de papa

# Crear una tabla de combinaciones de los factores
tratamientos <- expand.grid(Dosis = dosis, Cultivares = cultivares)

# Número de repeticiones
repeticiones <- 4

# Generar el diseño DBCA
dbca <- design.rcbd(trt = tratamientos, r = repeticiones, seed = 123)

# Mostrar el diseño experimental
print(dbca$book)

# DCA ---------------------------------------------------------------------

# DBCA --------------------------------------------------------------------




# Mi primera pagina web

1.Tener mi proyecto
1.Conectar mi proyecto a github
1.Tener un archivo en formato html 'index.html'
1.Activar Github pages

source('https://inkaverse.com/setup.r')


# Modelo lineal

## Importar data xlsx

# Leer el archivo de Excel y asignarlo a una variable


{r}
library(openxlsx)

data_de_tesis_prof <- openxlsx::read.xlsx("LA MOLINA 2014 POTATO WUE (FB).xlsx", sheet = "fb")



## Modelo lineal

{r}
modelo <- lm(formula = lfa ~ bloque + geno + riego + riego*geno, data = data_de_tesis_prof)

anova(modelo)


## Grafico en boxplot

```{r}
#cargamos el paquete ggplot2
library(ggplot2)

# Crear un boxplot para visualizar los efectos de geno y riego
ggplot(data_de_tesis_prof, aes(x = factor(geno), y = lfa, fill = factor(riego))) +
  geom_boxplot() +
  labs(x = "Genotipo", y = "LFA (Respuesta)", fill = "Riego") +
  theme_minimal() +
  ggtitle("Boxplot de LFA por Genotipo y Tratamiento de Riego")


# Cargar el paquete agricolae
library(agricolae)

# Realizar ANOVA usando el modelo ajustado
anova_result <- anova(modelo)

# Realizar prueba de comparación de medias (ejemplo con Tukey) para los efectos de "geno"
tukey_result_geno <- HSD.test(modelo, "geno", group = TRUE)

# También puedes realizar la prueba para el factor "riego"
tukey_result_riego <- HSD.test(modelo, "riego", group = TRUE)

# Imprimir resultados
print(tukey_result_geno)
print(tukey_result_riego)


# Graficar las comparaciones de medias para el factor "geno"
plot(tukey_result_geno, main = "Comparación de Medias para Genotipo", ylab = "Media de LFA", xlab = "Genotipo")

# Graficar las comparaciones de medias para el factor "riego"
plot(tukey_result_riego, main = "Comparación de Medias para Riego", ylab = "Media de LFA", xlab = "Riego")

#interraccion 

library(agricolae)

# Test de Tukey para el factor "riego"
tukey_riego <- HSD.test(modelo, "riego", group = TRUE)
print(tukey_riego)

# Test de Tukey para el factor "geno"
tukey_geno <- HSD.test(modelo, "geno", group = TRUE)
print(tukey_geno)

# Test de Tukey para la interacción "riego:geno"
tukey_interaccion <- HSD.test(modelo, c("riego", "geno"), group = TRUE)
print(tukey_interaccion)

# Ver la estructura de tukey_interaccion
str(tukey_interaccion)

# Convertir las filas en columnas y separar los factores
mc <- tukey_interaccion$groups %>%
  rownames_to_column("trat") %>%
  separate(trat, c("riego", "variedad"))

# Ver la estructura del nuevo data frame mc
str(mc)

#grafico ggplot2:

library(ggplot2)

# Crear el gráfico de barras
ggplot(mc, aes(x = reorder(groups, lfa), y = lfa, fill = groups)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Voltear el gráfico para que las barras estén horizontales
  labs(x = "Grupo", y = "LFA", title = "LFA por Grupo de Riego") +
  theme_minimal() +
  theme(legend.position = "none")  # Ocultar la leyenda si no es necesaria



# Gráfico en ggplot

ggplot(mc, aes(x = variedad, y = lfa, fill = riego)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = groups), position = position_dodge(width = 1), vjust = -0.1) +
  labs(title = "Producción por variedad y Riego",
       x = "Variedad",
       y = "LFA") +
  theme_minimal()

# Comparación de medias: emmeans
library(emmeans)
mc <- emmeans(modelo, ~ riego * geno)
mc <- emmeans(modelo, ~ riego | geno)

mc <- emmeans(modelo, ~ riego | geno) %>%
  as.data.frame()

mc




library(emmeans)



modelo <- lme4::lmer(hi ~ (1|bloque) + geno*riego
                     , data = fb)
anova(modelo)
source('https://inkaverse.com/setup.r')


library(emmeans)
library(multcomp)
library(multcompView)
library(lme4)
library(lmerTest)



cm1 <- emmeans(modelo, ~ geno | riego) %>% 
  cld(Letters = letters, reversed = T)
cm1


cm2 <- emmeans(modelo, ~ riego | geno) %>% 
  cld(Letters = letters, reversed = T)
cm2

cm3 <- emmeans(modelo, ~ riego * geno) %>% 
  cld(Letters = letters, reversed = T)
cm3

