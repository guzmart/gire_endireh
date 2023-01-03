# analisis manifestaciones de violencia ----
#------------------------------------------------------------------------------#
# Proyecto:                   ANÁLISIS DE ENDIREH PARA GIRE v2
# Objetivo:                   Analizar la Encuesta Nacional de la Dinámica de 
#                             las Relaciones en los Hogares, 2016 y 2021
#
# Encargadas:                 Katia Guzmán Martínez; Lorenzo León Robles
# Correos:                    katia.guzmart@gmail.com
# 
# Fecha de creación:          25 de agosto de 2022
# Última actualización:       25 de agosto de 2022
#------------------------------------------------------------------------------#
# Fuentes:
# - ENDIREH 2016:         https://www.inegi.org.mx/programas/endireh/2016/


Sys.setlocale("LC_TIME", "es_ES")
options(scipen=999)

# Ajuste por grupos pequeños en el cálculo de estratificados
options(survey.lonely.psu="adjust")
# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")
if(!require("ggalt")) install.packages("ggalt") & require("ggalt")
require(tidyverse)

# Directorios ----
paste_inp       <- function(x){paste0("01_datos_crudos/" , x)}
paste_out       <- function(x){paste0("02_datos_limpios/", x)}
paste_plot      <- function(x){paste0("03_gráficas/", x)}
source("00_códigos/00_funciones.R")

# Colores ----
v_gire_cols_2 <- c("#a8c589", "#73AD40")
# Datos ----
d_endireh_vob <- readRDS(paste_out("01_endireh_2016_vob_no_filter_lolo.rds")) %>% 
  bind_rows(
    readRDS(paste_out("01_endireh_2021_vob_no_filter_lolo.rds"))
  ) %>%
  mutate(cruce_g_edad = ifelse(cruce_g_edad=="40 a 99 años", "40 a 49 años", cruce_g_edad)) %>% 
  glimpse

# 18. Mujeres con discapacidad (sólo 2021) ----
d_disc <- d_endireh_vob %>% 
  filter(anio==2021) %>% 
  mutate(
    cruce_discapacidad_alguna_dummy = case_when(
      cruce_discapacidad_dummy_física_caminar == T ~ T,
      cruce_discapacidad_dummy_física_ver == T ~ T,
      cruce_discapacidad_dummy_física_mover_brazos == T ~ T,
      cruce_discapacidad_dummy_física_escuchar == T ~ T,
      cruce_discapacidad_dummy_física_bañar_vestirse_o_comer == T ~ T,
      cruce_discapacidad_dummy_física_hablar == T ~ T,
      cruce_discapacidad_dummy_cognitiva_aprender_recordar_concentrarse== T ~ T,
      cruce_discapacidad_dummy_cognitiva_problemas_emocionales== T ~ T,
      T ~ F
    ),
    
    cruce_dificultad_alguna_dummy = case_when(
      cruce_dificultad_dummy_física_caminar == T & cruce_discapacidad_dummy_física_caminar == F ~ T,
      cruce_dificultad_dummy_física_ver == T & cruce_discapacidad_dummy_física_ver == F ~ T,
      cruce_dificultad_dummy_física_mover_brazos == T & cruce_discapacidad_dummy_física_mover_brazos == F ~ T,
      cruce_dificultad_dummy_física_escuchar == T & cruce_discapacidad_dummy_física_escuchar == F ~ T,
      cruce_dificultad_dummy_física_bañar_vestirse_o_comer == T & cruce_discapacidad_dummy_física_bañar_vestirse_o_comer == F ~ T,
      cruce_dificultad_dummy_física_hablar == T & cruce_discapacidad_dummy_física_hablar == F ~ T,
      cruce_dificultad_dummy_cognitiva_aprender_recordar_concentrarse== T  & cruce_discapacidad_dummy_cognitiva_aprender_recordar_concentrarse== F~ T,
      cruce_dificultad_dummy_cognitiva_problemas_emocionales== T  & cruce_discapacidad_dummy_cognitiva_problemas_emocionales== F~ T,
      T ~ F
    ),
    
    cruce_sin_discapacidad_dificultad_dummy = case_when(
      cruce_dificultad_alguna_dummy == F ~ T,
      cruce_discapacidad_alguna_dummy == F ~ T,
    )
  ) %>% 
  glimpse
## 18.1. Por grupos de edad ----
vars_to_cross <- d_disc %>% select(starts_with("v_vio_tipo_")) %>% names
data_temp <- tibble()
for (var in vars_to_cross) {
  rum <- d_disc %>% 
    mutate(v_vio_laboral_vida_discriminacion =v_vio_laboral_ahora_discriminacion) %>% 
    mutate(
      v_vob_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    mutate(
      cruce_discapacidad_cat = case_when(
        cruce_discapacidad_alguna_dummy  ~ "Mujer con discapacidad",
        cruce_dificultad_alguna_dummy  ~ "Mujer con dificultad",
        cruce_sin_discapacidad_dificultad_dummy  ~ "Mujer sin discapacidad ni dificultad",
        T ~ "Mujer sin discapacidad ni dificultad",#NA_character_ # o tal vez debería ser  "Sin Discapacidad o Dificultad",
      ),
    ) %>% 
    filter(v_vob_alguna_2) %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                                   .ponderador =  fac_muj,
                                   .strata = est_dis,
                                   .ids = upm_dis,
                                   anio, cruce_discapacidad_cat, cruce_g_edad,
    ) %>% 
    mutate(violencia = var) %>% 
    rename(tipo = !!sym(var))
  data_temp <- bind_rows(data_temp,rum)
}



  
título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por prevalencia en tipos de violencia en la vida\n y condición de discapacidad* o limitación**"
nota <- "Elaboración de GIRE con información de la ENDIREH (2021)\n*Incluye a las mujeres que tienen como respuesta 'No puede hacerlo' o\n'Lo hace con mucha dificultad' en al menos una de las actividades de la pregunta 19.1.\n**Incluye a las mujeres que únicamente tienen como respuesta 'Lo hace con poca dificultad'\nen alguna de las actividades de la pregunta 19.1."

data_temp%>% 
  filter(tipo) %>% 
  filter(preferencia) %>% 
  mutate(num_pregunta = stri_replace_all(num_pregunta, fixed ="_todas", "_alguna")) %>% 
  ggplot(
    aes(
      x = cruce_g_edad, 
      y = reorder(stri_trans_totitle(stri_extract_last_words(stri_replace_all(stri_replace_all(violencia, fixed ="_dummy", ""), fixed ="_", " "))),-v_prop),
      fill = v_prop
    )
  ) +
  facet_wrap(~str_wrap(cruce_discapacidad_cat,25))+
  geom_tile(col = "white", show.legend = F) +
  geom_text(aes(label = scales::percent(v_prop, accuracy = 0.1)), family = "Ubuntu", size = 7) +
  scale_fill_gradient("", high = "#ff6260", low = v_gire_cols_2[2])  +
  guides(label = "none") +
  scale_x_discrete(position = "bottom") +
  theme_bw() +
  labs(
    title = str_wrap(título, 50),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 45),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("18_01_vob_alguna_disc_limit_física_cruce_g_edad_modalidad.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

## 18.1. Por grupos de edad y tipos ----
vars_to_cross <- d_disc %>% select(starts_with("v_vob_tipo_") ) %>% names
vars_to_cross <- c("v_vob_tipo_alguna_2", vars_to_cross)
data_temp <- tibble()
for (var in vars_to_cross) {
  print(var)
  rum <- d_disc %>% 
    mutate(
      v_vob_tipo_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    mutate(
      cruce_discapacidad_cat = case_when(
        cruce_discapacidad_alguna_dummy  ~ "1. Mujer con discapacidad",
        cruce_dificultad_alguna_dummy  ~ "2. Mujer con dificultad",
        cruce_sin_discapacidad_dificultad_dummy  ~ "3. Mujer sin discapacidad ni dificultad",
        T ~ "3. Mujer sin discapacidad ni dificultad",#NA_character_ # o tal vez debería ser  "Sin Discapacidad o Dificultad",
      ),
    ) 
  if(var != "v_vob_tipo_alguna_2"){
    rum <- rum %>% 
      filter(v_vob_tipo_alguna_2)
  }
  
  rum2 <- rum %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                         .ponderador =  fac_muj,
                         .strata = est_dis,
                         .ids = upm_dis,
                         #anio, 
                         cruce_discapacidad_cat, cruce_g_edad
    ) %>% 
    bind_rows(
      rum %>% 
        group_and_wponder_by(.variable_a_pond=!!sym(var),
                             .ponderador =  fac_muj,
                             .strata = est_dis,
                             .ids = upm_dis,
                             #anio, 
                             cruce_discapacidad_cat
        ) %>% 
        mutate(cruce_g_edad = "General")
      
    )
  
  data_temp <- bind_rows(data_temp,rum2)
}




título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por condición de discapacidad* o limitación**"
nota <- "Elaboración de GIRE con información de la ENDIREH (2021)\n*Incluye a las mujeres que tienen como respuesta 'No puede hacerlo' o\n'Lo hace con mucha dificultad' en al menos una de las actividades de la pregunta 19.1.\n**Incluye a las mujeres que únicamente tienen como respuesta 'Lo hace con poca dificultad'\nen alguna de las actividades de la pregunta 19.1."

data_temp%>% 
  filter(preferencia) %>% 
  mutate(num_pregunta = stri_replace_all(num_pregunta, fixed ="_alguna_2", "_alguna")) %>% 
  ggplot(
    aes(
      x = reorder(cruce_g_edad, ifelse(is.na(stri_extract_first(cruce_g_edad, regex ="[:digit:]")), 0, as.numeric(stri_extract_first(cruce_g_edad, regex ="[:digit:]")))), 
      y = reorder(str_wrap(stri_trans_totitle(stri_replace_all(stri_replace_all(stri_replace_all(num_pregunta, fixed ="v_vob_tipo_", ""), fixed ="_dummy", ""), fixed ="_", " ")),30),-v_prop),
      fill = v_prop
    )
  ) +
  facet_wrap(~str_wrap(cruce_discapacidad_cat,25))+
  geom_tile(col = "white", show.legend = F) +
  geom_text(aes(label = scales::percent(v_prop, accuracy = 0.1)), family = "Ubuntu", size = 7) +
  scale_fill_gradient("", high = "#ff6260", low = v_gire_cols_2[2])  +
  guides(label = "none") +
  scale_x_discrete(position = "bottom") +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 45),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("18_02_vob_alguna_disc_limit_física_cruce_g_edad_tipos.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")
## tata ----

vars_to_cross <- d_disc %>% select(starts_with("v_vob_tipo_") ) %>% names
#vars_to_cross <- c("v_vob_tipo_alguna_2", vars_to_cross)
data_temp <- tibble()
for (var in vars_to_cross) {
  print(var)
  rum <- d_disc %>% 
    mutate(
      v_vob_tipo_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    mutate(
      cruce_discapacidad_cat = case_when(
        cruce_discapacidad_alguna_dummy  ~ "1. Mujer con discapacidad",
        cruce_dificultad_alguna_dummy  ~ "2. Mujer con dificultad",
        cruce_sin_discapacidad_dificultad_dummy  ~ "3. Mujer sin discapacidad ni dificultad",
        T ~ "3. Mujer sin discapacidad ni dificultad",#NA_character_ # o tal vez debería ser  "Sin Discapacidad o Dificultad",
      ),
    ) 
  
  rum2 <- rum %>% 
    filter(v_vob_tipo_alguna_2) %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                         .ponderador =  fac_muj,
                         .strata = est_dis,
                         .ids = upm_dis,
                         #anio, 
                         cruce_discapacidad_cat
    ) 
  data_temp <- bind_rows(data_temp,rum2)
}




título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por condición de discapacidad* o limitación**"
nota <- "Elaboración de GIRE con información de la ENDIREH (2021)\n*Incluye a las mujeres que tienen como respuesta 'No puede hacerlo' o\n'Lo hace con mucha dificultad' en al menos una de las actividades de la pregunta 19.1.\n**Incluye a las mujeres que únicamente tienen como respuesta 'Lo hace con poca dificultad'\nen alguna de las actividades de la pregunta 19.1."

aca <- data_temp%>% 
  filter(preferencia) %>% 
  pivot_wider(names_from = cruce_discapacidad_cat, values_from = v_prop, id_cols = num_pregunta) 
  
aca %>% 
  mutate(diff= `3. Mujer sin discapacidad ni dificultad`-`1. Mujer con discapacidad`) %>% 
  arrange(desc(diff))

data_temp%>% 
  filter(preferencia) %>% 
  mutate(num_pregunta = stri_replace_all(num_pregunta, fixed ="_alguna_2", "_alguna")) %>% 
  ggplot(
    aes(
      x = str_wrap(cruce_discapacidad_cat,25), 
      y = reorder(str_wrap(stri_trans_totitle(stri_replace_all(stri_replace_all(stri_replace_all(num_pregunta, fixed ="v_vob_tipo_", ""), fixed ="_dummy", ""), fixed ="_", " ")),30),-v_prop),
      fill = v_prop
    )
  ) +
  #facet_wrap(~)+
  geom_tile(col = "white", show.legend = F) +
  geom_text(aes(label = scales::percent(v_prop, accuracy = 0.1)), family = "Ubuntu", size = 7) +
  scale_fill_gradient("", high = "#ff6260", low = v_gire_cols_2[2])  +
  guides(label = "none") +
  scale_x_discrete(position = "bottom") +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 0),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("18_03_vob_alguna_disc_limit_física_tipos.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# Edad --- tios de violencia
vars_to_cross <- d_disc %>% select(starts_with("v_vob_tipo_")) %>% names
data_temp <- tibble()
for (var in vars_to_cross) {
  print(var)
  rum <- d_disc %>% 
    filter(anio==2021) %>% 
    mutate(
      v_vob_tipo_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    filter(v_vob_tipo_alguna_2) 
  
  rum2 <- rum %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                         .ponderador =  fac_muj,
                         .strata = est_dis,
                         .ids = upm_dis,
                         #anio,
                        cruce_g_edad
    ) %>% 
    bind_rows(
      rum %>% 
        group_and_wponder_by(.variable_a_pond=!!sym(var),
                             .ponderador =  fac_muj,
                             .strata = est_dis,
                             .ids = upm_dis,
                             #anio, 
                             
        ) %>% 
        mutate(cruce_g_edad = "General")
      
    )
  
  data_temp <- bind_rows(data_temp,rum2)
}




título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por prevalencia en tipos de violencia en la vida"
nota <- "Elaboración de GIRE con información de la ENDIREH (2021)"

data_temp%>% 
  filter(preferencia) %>% 
  mutate(num_pregunta = stri_replace_all(num_pregunta, fixed ="_alguna_2", "_alguna")) %>% 
  ggplot(
    aes(
      x = reorder(cruce_g_edad, ifelse(is.na(stri_extract_first(cruce_g_edad, regex ="[:digit:]")), 0, as.numeric(stri_extract_first(cruce_g_edad, regex ="[:digit:]")))), 
      y = reorder(str_wrap(stri_trans_totitle(stri_replace_all(stri_replace_all(stri_replace_all(num_pregunta, fixed ="v_vob_tipo_", ""), fixed ="_dummy", ""), fixed ="_", " ")),30),-v_prop),
      fill = v_prop
    )
  ) +
  #facet_wrap(~str_wrap(cruce_discapacidad_cat,25))+
  geom_tile(col = "white", show.legend = F) +
  geom_text(aes(label = scales::percent(v_prop, accuracy = 0.1)), family = "Ubuntu", size = 7) +
  scale_fill_gradient("", high = "#ff6260", low = v_gire_cols_2[2])  +
  guides(label = "none") +
  scale_x_discrete(position = "bottom") +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 40, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 0),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("19_1_vob_cruce_g_edad_modalidad.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")
