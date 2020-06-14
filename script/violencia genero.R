
# librerias ---------------------------------------------------------------

library(readr)
library(readxl)
library(dplyr) # manipulacion datos
library(tidyr) # manipulacion datos 2
library(magrittr) # Pipe %>%
library(readr) # Importar csv
library(ggplot2) # Gráficos
library(lubridate) # Manipular fechas
library(cowplot) # Grid de Gráficos
library(scales) # Escalas en los gráficos
library(purrr) # mapear funciones a las columnas 
library(factoextra)
library(FactoMineR)
library(dplyr)
library(tidyr)
library(patchwork)
library(foreing)
library(tibble)
library(stringr)
library(openxlsx)
library(stringi)







vio_df <- read_delim("git/Reduccion de dimenciones/Reduccion_Dimenciones/data/violencia.csv", ";")[-1]
# Pesos de la encuesta

vio_mfa <- MFA(vio_df[-72], group = c(15, 18, 14, 22, 1, 1),
               type = c(rep("n", 4), "s", "n"),
               name.group = c("educacion", "laboral", "familiar", "social", "edad", "civil"),
               num.group.sup = c(5, 6), graph = FALSE)






