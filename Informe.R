remove(list = ls())

library(data.table)
library(dplyr)
library(tidyr) 
library(ggplot2)
#En este trabajo se analiza el comportamiento de la mortalidad en el estado de Campeche durante los años 2010, 2019 y 2021. Estos años permiten comparar tres momentos importantes: 2010 como punto inicial de referencia censal, 2019 como año previo a la pandemia por COVID-19 y 2021 como un año afectado por sus efectos en la mortalidad.
#Campeche es una entidad ubicada en el sureste de México, dentro de la península de Yucatán. Se caracteriza por tener una población relativamente pequeña y una baja densidad poblacional, lo que puede influir en la distribución de los servicios de salud y en el acceso oportuno a la atención médica. En 2010, el estado tenía 822,441 habitantes, mientras que para 2020 alcanzó 928,363 habitantes, lo que muestra un crecimiento moderado de la población.
#Otro aspecto importante es el cambio en la estructura por edad. En 2010, Campeche tenía una edad mediana de 25 años, mientras que en 2020 aumentó a 29 años, lo cual indica un avance gradual hacia el envejecimiento demográfico. Este dato es relevante porque una población con mayor proporción de adultos mayores tiende a presentar niveles más altos de mortalidad.
#Además, el año 2021 debe interpretarse con cuidado, ya que la mortalidad estuvo influida por la pandemia por COVID-19. Por ello, al comparar 2019 con 2021, se puede observar no solo el comportamiento habitual de la mortalidad, sino también el efecto de un evento sanitario extraordinario.
#En conjunto, estos elementos permiten contextualizar los resultados del análisis, considerando que la mortalidad en Campeche puede estar relacionada tanto con su estructura poblacional como con factores territoriales y de acceso a servicios de salud. En particular, se considerarán elementos como la estructura por edad, la distribución por sexo, el crecimiento poblacional y la densidad demográfica, ya que estos factores sirven como base para interpretar posteriormente las tasas de mortalidad y sus posibles diferencias dentro del estado.


#cargamos la información de la proyección a mitad de año de 1950 a 2070
pop <- fread("https://repodatos.atdt.gob.mx/CONAPO/proyecciones/00_Pob_Mitad_1950_2070.csv")


#creamos un nuevo data frame con el formato que deseamos para obtener las poblaciones iniciales
cpe_ancho <- pop %>%
  filter(
    ENTIDAD == "Campeche",
    ANIO %in% c(2010, 2019, 2021)
  ) %>%
  select(EDAD, SEXO, POBLACION, ANIO) %>%
  mutate(
    GRUPO_EDAD = cut(
      EDAD, 
      breaks = c(0, 1, 5, seq(10, 85, by = 5), Inf), 
      right = FALSE,
      labels = c("0", "1", seq(5, 85, by = 5))
    )
  ) %>%
  group_by(GRUPO_EDAD, SEXO, ANIO) %>%
  summarise(POBLACION = sum(POBLACION, na.rm = TRUE), .groups = "drop") %>%
  
  # 4. Separamos la columna SEXO en dos columnas de población
  pivot_wider(
    names_from = SEXO,       # De aquí toma los nombres de las nuevas columnas ("Hombres", "Mujeres")
    values_from = POBLACION  # De aquí toma los valores para rellenar las columnas
  )


#obtenemos la información de la mortalidad observada con los datos reportados por la INEGI
#En el siguiente análisis no se consideraron los no especificados ni las defunciones registradas fuera del año de ocurrencia, esto debido a que representaban una proporción muy pequeña.
#poner la ruta del archivo
setwd("C:/Users/angel/OneDrive/Escritorio/2026-2/Demografia/Nueva carpeta/Entrega 1/Data")

#consultamos llos archivos de la ruta
dir()

#guardamos el archivo de nuestro directorio
lt <- fread("mortalidad_2010_2019_2021.csv")
#corregimos el error al contruir el csv
lt <- lt[,-5][,-5][,-5]



names(lt)
#cambiar nombres y ordenar si es necesario

#Cambieamos los nombres a una forma estandar
#setnames(lt, c("Edad", "Hombre","Mujer"), c("x", "HDx","MDx"))
#lt <- lt %>%
#  arrange(x,AÑO)


#lt[ , Dx := HDx + MDx] #Construimos las defunciones totales



#con la información de las defunciones, ademas de la informacíon de los nacidos proyectados en 2010, 2019 y 20221

dir()

lt1 <- fread("Mortaliad_y_proyección.csv")

#calculamos la q_x
lt1[ , q_x := Dx/lx]
setDT(lt1)

lt1[, qx_h := HDx / Hlx]

lt1[, qx_m := MDx / Mlx]

#calculamos las P_x
lt1[ , p_x := 1-q_x]

lt1[, px_h := 1-qx_h]

lt1[, px_h := 1-qx_m] 



# Extraemos el q0 de cada año para usarlo en las ecuaciones de Coale-Demeny
lt1[, q0_h_anio := qx_h[x == 0], by = AÑO]
lt1[, q0_m_anio := qx_m[x == 0], by = AÑO]
lt1[, q0_anio := q_x[x == 0], by = AÑO]


#obtenemos los ax_n
lt1[, ax_h := ifelse(x == 0,
                     ifelse(q0_h_anio >= 0.100, 0.330, 0.0425 + 2.875 * q0_h_anio), # Edad 0
                     ifelse(x == 1,
                            ifelse(q0_h_anio >= 0.100, 1.352, 1.653 - 3.013 * q0_h_anio), # Edad 1
                            2.5))] # Mayores de 5 años

# Para Mujeres (ax_m)
lt1[, ax_m := ifelse(x == 0,
                     ifelse(q0_m_anio >= 0.100, 0.350, 0.050 + 2.800 * q0_m_anio), # Edad 0
                     ifelse(x == 1,
                            ifelse(q0_m_anio >= 0.100, 1.361, 1.524 - 1.622 * q0_m_anio), # Edad 1
                            2.5))]
#en general
lt1[, ax_gen := ifelse(x == 0,
                       ifelse(q0_anio >= 0.100, 0.330, 0.0425 + 2.875 * q0_anio), # Edad 0
                       ifelse(x == 1,
                              ifelse(q0_anio >= 0.100, 1.352, 1.653 - 3.013 * q0_anio), # Edad 1
                              2.5))] # Mayores de 5 años                            
#apv

lt1[, HLx := n * Hlx + ax_h * HDx] #Hombre
lt1[, MLx := n * Mlx + ax_m * MDx] #mujeres
lt1[, Lx := n * lx + ax_gen * Dx] # En general

#Tx
# 1. Tx General
lt1[, Tx := rev(cumsum(rev(Lx))), by = AÑO]

# 2. Tx para Hombres (si calculaste HLx previamente)
lt1[, HTx := rev(cumsum(rev(HLx))), by = AÑO]

# 3. Tx para Mujeres (si calculaste MLx previamente)
lt1[, MTx := rev(cumsum(rev(MLx))), by = AÑO]

#Esperanzas de vida

#general
lt1[, ex := Tx / lx, by = AÑO]
#hombres
lt1[, ex_h := HTx / Hlx, by = AÑO]
#mujeres
lt1[, ex_m := MTx / Mlx, by = AÑO]

#cuadro con las esperanzas de vida
cuadro_e0 <- lt1[x == 0, .(AÑO, ex_h, ex_m)]

lt1[, AÑO := as.factor(AÑO)] # Nos asegura colores fijos por año

# Paleta básica y limpia de colores
colores <- c("2010" = "#4682B4", "2019" = "#2E8B57", "2021" = "#CD5C5C")
gHlx <- ggplot(lt1, aes(x = x, y = Hlx, color = AÑO)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Sobrevivientes Hombres (Hlx) por Año",
       subtitle = "Muestra cuántos siguen vivos de la cohorte inicial",
       x = "Edad", y = "Número de personas vivas", color = "Año") +
  theme_minimal()


ggsave("grafica_lx_hombres.png", plot = g_sobrevivientes_h, width = 7, height = 4.5, dpi = 300)

gMlx <- ggplot(lt1, aes(x = x, y = Mlx, color = AÑO)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Sobrevivientes Mujeres (Mlx) por Año",
       subtitle = "Muestra cuántos siguen vivos de la cohorte inicial",
       x = "Edad", y = "Número de personas vivas", color = "Año") +
  theme_minimal()

ggsave("grafica_lx_Mujeres.png", plot = g_sobrevivientes_h, width = 7, height = 4.5, dpi = 300)


#grafica de probabilidad de morir de hombre
gqx_h <- ggplot(lt1, aes(x = x, y = log(qx_h), color = AÑO)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Probabilidad de morir de hombres por Año",
       subtitle = "escala logarítmica",
       x = "Edad", y = "Número de personas vivas", color = "Año") +
  theme_minimal()

ggsave("grafica_qx_h.png", plot = g_sobrevivientes_h, width = 7, height = 4.5, dpi = 300)


#grafica de probabilidad de morir de mujeres
gqx_h <- ggplot(lt1, aes(x = x, y = log(qx_m), color = AÑO)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Probabilidad de morir de mujeres por Año",
       subtitle = "escala logarítmica",
       x = "Edad", y = "Número de personas vivas", color = "Año") +
  theme_minimal()

ggsave("grafica_qx_m.png", plot = g_sobrevivientes_h, width = 7, height = 4.5, dpi = 300)
