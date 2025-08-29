if(! require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, srvyr, janitor, stringr, readxl, writexl, tools, dplyr, jsonlite)


# #variables de interés
#   DIS_VER
# DIS_OIR
# DIS_CAMINAR
# DIS_RECORDAR 
# DIS_BANARSE
# DIS_HABLAR
# DIS_MENTAL
#De las anteriores variables:
  # 3= lo hace con mucha dificultad
  # 4= no puede hacerlo
Personas09 <- read_csv("Personas09.CSV")
View(Personas09)
  
datos_dis <- Personas09 %>% 
  mutate(
    discapacidad = if_else(
      DIS_VER %in% c(3, 4) |
        DIS_OIR %in% c(3, 4) |
        DIS_CAMINAR %in% c(3, 4) |
        DIS_RECORDAR %in% c(3, 4) |
        DIS_BANARSE %in% c(3, 4) |
        DIS_HABLAR %in% c(3, 4) |
        DIS_MENTAL %in% c(3, 4),
      1, 0
    ),
    grupo_EDAD = case_when(
      EDAD >= 0  & EDAD <= 6  ~ "0-6",
      EDAD >= 7  & EDAD <= 11 ~ "7-11",
      EDAD >= 12 & EDAD <= 17 ~ "12-17",
      EDAD >= 18 & EDAD <= 59 ~ "18-59",
      EDAD >= 60 & EDAD <= 64 ~ "60-64",
      EDAD >= 65              ~ "65+",
      TRUE ~ NA_character_
    )
  )

tabla_discapacidad <- datos_dis %>%
  group_by(grupo_EDAD) %>%
  summarise(
    con_discapacidad = sum(if_else(discapacidad == 1, FACTOR, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = round(100 * con_discapacidad / sum(con_discapacidad), 1)
  )

tabla_discapacidad
write_xlsx(tabla_discapacidad, "pob_discapacidad_fin.xlsx")
