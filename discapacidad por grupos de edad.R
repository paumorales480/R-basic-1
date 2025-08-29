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
#write_xlsx(tabla_discapacidad, "pob_discapacidad_fin.xlsx")


#Población para 2050 con los poderosos datos de CONAPO
X0_Pob_Mitad_1950_2070 <- read_excel("0_Pob_Mitad_1950_2070.xlsx")
head(X0_Pob_Mitad_1950_2070)
datos <- X0_Pob_Mitad_1950_2070 %>%
  mutate(grupo_edad = case_when(
    EDAD < 12 ~ "menores de 12",
    EDAD >= 60 ~ "60+",
    EDAD >= 12 & EDAD < 60 ~ "otros"
  )) %>% 
  filter(ENTIDAD == "Ciudad de México",
         AÑO == 2050) %>% 
  group_by(grupo_edad) %>% 
  summarise(
    total = sum(POBLACION),
    .groups = "drop"
  ) %>% 
  mutate(
    porcentaje = round(100 * total / sum(total), 1)
  )

datos



datos_2 <- X0_Pob_Mitad_1950_2070 %>%
  filter(ENTIDAD == "Ciudad de México",
         AÑO == 2050)
str(datos_2$EDAD)
summary(datos_2$EDAD)
datos_2
#edad mediana de 2050
median(datos_2$EDAD, na.rm = TRUE) #sale 54.5
mean(datos_2$EDAD, na.rm = TRUE) #54.5


#####
#índice de envejecimiento 
#Dato de Fide: 267 personas mayores por cada 100 menores de 15 años
datos_3 <- X0_Pob_Mitad_1950_2070 %>% 
  mutate(grupo_edad = case_when(
    EDAD < 15 ~ "menores de 15",
    EDAD >= 60 ~ "60+",
    EDAD >= 15 & EDAD < 60 ~ "otros"
  )) %>% 
  filter(ENTIDAD == "Ciudad de México",
         AÑO == 2050) %>% 
  group_by(grupo_edad) %>% 
  summarise(total = sum(POBLACION), .groups = "drop")
datos_3

#índice de envejecimiento
indice_envejecimiento <- round((datos_3$total[datos_3$grupo_edad == "60+"] /
                            datos_3$total[datos_3$grupo_edad == "menores de 15"]) * 100,1)

print(paste0("El índice de envejecimiento es de: ",indice_envejecimiento)) #344.3


#porcentaje de la pob de 30 a 59 años, respecto al total de la pob en 2050
datos_4 <- X0_Pob_Mitad_1950_2070 %>% 
  mutate(grupo_edad = case_when(
    EDAD <= 11 ~ "infancias",
    EDAD >= 60 ~ "adultos mayores",
    EDAD >= 0 & EDAD <= 17 ~ "infancias y adolescentes",
    EDAD >= 30 & EDAD <=59 ~ "pob adulta",
    EDAD >= 12 & EDAD <=29 ~ "jóvenes"
  )) %>% 
  filter(ENTIDAD == "Ciudad de México",
         AÑO == 2050) %>% 
  group_by(grupo_edad) %>% 
  summarise(
    total = sum(POBLACION),
    .groups = "drop"
  ) %>% 
  mutate(
    porcentaje = round(100 * total / sum(total), 1)
  )  
datos_4







