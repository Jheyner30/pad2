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




