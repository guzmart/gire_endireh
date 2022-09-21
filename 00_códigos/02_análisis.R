#------------------------------------------------------------------------------#
# Proyecto:                   ANÁLISIS DE ENDIREH PARA GIRE
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
d_endireh_vob <- readRDS(paste_out("01_endireh_2016_vob_no_filter.rds")) %>% 
  bind_rows(
    readRDS(paste_out("01_endireh_2021_vob_no_filter.rds"))
  ) %>%
  mutate(cruce_g_edad = ifelse(cruce_g_edad=="40 a 99 años", "40 a 49 años", cruce_g_edad)) %>% 
  glimpse

# 1.  Número de partos ----
## 1.1. Últimos 5 años ----
d_tot  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_cesárea_dummy,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, v_anio_ult_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_cesárea_dummy,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, v_anio_ult_parto
      ) %>%  
      ungroup() 
  ) %>% 
  glimpse()

d_tot_anio <- d_tot %>% 
  select(anio, v_anio_ult_parto, v_tot) %>% 
  group_by(anio) %>% 
  summarise(tot_partos = sum(v_tot)) %>% 
  ungroup()

título <- "Número de mujeres entre 15 y 49 años que tuvieron al menos un parto en los últimos 5 años"
subtítulo <- ""
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio,
  aes(
    x = paste0("ENDIREH - ", anio),
    y = tot_partos,
    label = scales::comma(tot_partos)
  )
) + 
  geom_col(show.legend = F, width = 0.4, aes(fill = paste0("ENDIREH - ", anio))) +
  geom_label(show.legend = F, vjust = 0.2, size = 8, family = "Ubuntu") +
  scale_y_continuous(labels = scales::comma, limits = c(0,max(d_tot_anio$tot_partos)+100000)) +
  scale_fill_manual(values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("01_01_tot_partos_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")


## 1.2. Por año de último parto ----
título <- "Número de mujeres entre 15 y 49 años que tuvieron al menos un parto*"
subtítulo <- "Por año de último parto"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)\n*La ENDIREH captura los partos ocurridos entre octubre de 2011 y octubre de 2016;\nla ENDIREH 2021, los ocurridos entre octubre de 2016 y octubre de 2021"
ggplot(
  d_tot %>% 
    group_by(anio, v_anio_ult_parto) %>% 
    summarise(tot_partos = sum(v_tot)) %>% 
    ungroup(),
  aes(
    x = v_anio_ult_parto,
    y = tot_partos,
    col = paste0("ENDIREH - ", anio),
    group = paste0("ENDIREH - ", anio),
    label = scales::comma(tot_partos)
  )
) + 
  geom_line(size = 2) + geom_point(size = 3.5) + 
  geom_label(show.legend = F, vjust = -0.8, size = 6, family = "Ubuntu") +
  scale_y_continuous(labels = scales::comma, limits = c(0,2000000)) +
  scale_color_manual("", values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )
  
  
ggsave(filename = paste_plot("01_02_tot_partos_anio_ult_parto.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 2. Cesáreas ----
## 2.1. Últimos 5 años ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_cesárea_dummy,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_cesárea_dummy,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años cuyo último parto fue por cesárea"
subtítulo <- "Últimos 5 años; entre paréntesis se indica el total de cesáreas"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    filter(preferencia==T),
  aes(
    x = paste0("ENDIREH - ", anio),
    y = v_prop,
    label = paste0( scales::percent(v_prop, accuracy = 0.1), "\n(", scales::comma(v_tot),")")
  )
) + 
  geom_col(show.legend = F, width = 0.4, aes(fill = paste0("ENDIREH - ", anio))) +
  geom_label(show.legend = F, vjust = 0.2, size = 8, family = "Ubuntu") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) +
  scale_fill_manual(values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("02_01_prop_cesáreas_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

## 2.2. Por año de último parto ----
título <- "Porcentaje de mujeres entre 15 y 49 años cuyo último parto fue por cesárea"
subtítulo <- "Por año de último parto;* entre paréntesis se indica el total de cesáreas"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)\n*La ENDIREH captura los partos ocurridos entre octubre de 2011 y octubre de 2016;\nla ENDIREH 2021, los ocurridos entre octubre de 2016 y octubre de 2021"
d_tot  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_cesárea_dummy,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, v_anio_ult_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_cesárea_dummy,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, v_anio_ult_parto
      ) %>%  
      ungroup() 
  ) %>% 
  glimpse()

ggplot(
  d_tot %>% 
    filter(preferencia==T),
  aes(
    x = v_anio_ult_parto,
    y = v_prop,
    col = paste0("ENDIREH - ", anio),
    group = paste0("ENDIREH - ", anio),
    label = paste0( scales::percent(v_prop, accuracy = 0.1), "\n(", scales::comma(v_tot),")")
  )
) + 
  geom_line(size = 2) + geom_point(size = 3.5) + 
  geom_label(show.legend = F, size = 6, family = "Ubuntu", aes(vjust = ifelse(anio == 2021, -0.3, 1.3))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) +
  scale_color_manual("", values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("02_02_prop_cesáreas_anio_ult_parto.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 3. Sufrió algún tipo de violencia obstétrica ----
## 3.1. Últimos 5 años ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    v_vob_alguna_2 = case_when(
      v_vob_alguna == T ~ T,
      v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
      v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
      T ~ F
    ),
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        v_vob_alguna_2 = case_when(
          v_vob_alguna == T ~ T,
          v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
          v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
          T ~ F
        ),
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio
      ) %>%  
      ungroup() 
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Últimos 5 años"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    filter(preferencia==T),
  aes(
    x = paste0("ENDIREH - ", anio),
    y = v_prop,
    label = scales::percent(v_prop, accuracy = 0.1)
  )
) + 
  geom_col(show.legend = F, width = 0.4, aes(fill = paste0("ENDIREH - ", anio))) +
  geom_label(show.legend = F, vjust = 0.2, size = 8, family = "Ubuntu") +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.4)) +
  scale_fill_manual(values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("03_01_prop_vob_alguna_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")





## 3.2. Por año de último parto ----
título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Por año de último parto*"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)\n*La ENDIREH captura los partos ocurridos entre octubre de 2011 y octubre de 2016;\nla ENDIREH 2021, los ocurridos entre octubre de 2016 y octubre de 2021"
d_tot  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    v_vob_alguna_2 = case_when(
      v_vob_alguna == T ~ T,
      v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
      v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
      T ~ F
    ),
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, v_anio_ult_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        v_vob_alguna_2 = case_when(
          v_vob_alguna == T ~ T,
          v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
          v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
          T ~ F
        ),
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, v_anio_ult_parto
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()

ggplot(
  d_tot %>% 
    filter(preferencia==T),
  aes(
    x = v_anio_ult_parto,
    y = v_prop,
    col = paste0("ENDIREH - ", anio),
    group = paste0("ENDIREH - ", anio),
    label = scales::percent(v_prop, accuracy = 0.1)
  )
) + 
  geom_line(size = 2) + geom_point(size = 3.5) + 
  geom_label(show.legend = F, size = 6, family = "Ubuntu", aes(vjust = ifelse(anio == 2016, -0.3, 1.3))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_color_manual("", values = v_gire_cols_2) +
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("03_02_prop_vob_alguna_anio_ult_parto.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 4. Manifestaciones de violencia obstétrica ----
## 4.1. Últimos 5 años ----
vars_to_cross <- names(d_endireh_vob)[str_detect(names(d_endireh_vob), "v_vob_tipo_")]
d_tot_anio_2016 <- data.frame()
for (var in vars_to_cross[1:10]) {
  #stop()
  print(paste0("Doing for: "  ," and var: " , var))
  rum <- d_endireh_vob %>% 
    filter(anio == 2016) %>% 
    mutate(
      v_vob_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_vob_alguna_2 == T~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_vob_alguna_2 == T~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                         .ponderador =  fac_muj,
                         .strata = estrato,
                         .ids = upm_dis,
                         #!!sym(cross_var),
                         anio) %>% 
    ungroup() 
  
  d_tot_anio_2016 <- bind_rows(d_tot_anio_2016,rum)
}

d_tot_anio_2021 <- data.frame()
for (var in vars_to_cross) {
  #stop()
  print(paste0("Doing for: "  ," and var: " , var))
  rum <- d_endireh_vob %>% 
    filter(anio == 2021) %>% 
    mutate(
      v_vob_alguna_2 = case_when(
        v_vob_alguna == T ~ T,
        v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
        v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
        T ~ F
      ),
      keep = case_when(
        anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_vob_alguna_2 == T~ 1,
        anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_vob_alguna_2 == T~ 1,
        T ~ 0
      ),
      v_parto_dummy = ifelse(filtro_parto==1, T, F)
    ) %>% 
    filter(keep == 1) %>% 
    group_and_wponder_by(.variable_a_pond=!!sym(var),
                         .ponderador =  fac_muj,
                         .strata = estrato,
                         .ids = upm_dis,
                         #!!sym(cross_var),
                         anio) %>% 
    ungroup() 
  
  d_tot_anio_2021 <- bind_rows(d_tot_anio_2021,rum)
}

d_tot_anio_2 <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    v_vob_cesárea_dummy = case_when(
      v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
      v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
      T ~ F
    ),
    v_cesárea_no_fue_informada_por_qué_dummy = ifelse(v_cesárea_informaron_por_qué_dummy==T, F, T),
    v_cesárea_no_la_autorizó_dummy = ifelse(v_cesárea_autorización_dummy==T, F, T),
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=v_cesárea_no_fue_informada_por_qué_dummy,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2016) %>% 
      mutate(
        v_vob_cesárea_dummy = case_when(
          v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
          v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
          T ~ F
        ),
        v_cesárea_no_fue_informada_por_qué_dummy = ifelse(v_cesárea_informaron_por_qué_dummy==T, F, T),
        v_cesárea_no_la_autorizó_dummy = ifelse(v_cesárea_autorización_dummy==T, F, T),
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_cesárea_no_la_autorizó_dummy,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio
      ) %>%  
      ungroup() 
  ) %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        v_vob_cesárea_dummy = case_when(
          v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
          v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
          T ~ F
        ),
        v_cesárea_no_fue_informada_por_qué_dummy = ifelse(v_cesárea_informaron_por_qué_dummy==T, F, T),
        v_cesárea_no_la_autorizó_dummy = ifelse(v_cesárea_autorización_dummy==T, F, T),
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=v_cesárea_no_fue_informada_por_qué_dummy,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio
      ) %>%  
      ungroup() %>% 
      bind_rows(
        d_endireh_vob %>% 
          filter(anio == 2021) %>% 
          mutate(
            v_vob_cesárea_dummy = case_when(
              v_cesárea_dummy == T & v_cesárea_informaron_por_qué_dummy == F ~ T,
              v_cesárea_dummy == T & v_cesárea_autorización_dummy == F ~ T,
              T ~ F
            ),
            v_cesárea_no_fue_informada_por_qué_dummy = ifelse(v_cesárea_informaron_por_qué_dummy==T, F, T),
            v_cesárea_no_la_autorizó_dummy = ifelse(v_cesárea_autorización_dummy==T, F, T),
            keep = case_when(
              anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
              anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 & !(as.numeric(cruce_lugar_atencion_parto) == 9 | as.numeric(cruce_lugar_atencion_parto) == 99) & v_cesárea_dummy == T ~ 1,
              T ~ 0
            ),
            v_parto_dummy = ifelse(filtro_parto==1, T, F)
          ) %>% 
          filter(keep == 1) %>% 
          group_and_wponder_by(
            .variable_a_pond=v_cesárea_no_la_autorizó_dummy,
            .ponderador =  fac_muj,
            .strata = est_dis,
            .ids = upm_dis,
            anio
          ) %>%  
          ungroup() 
      )
  ) %>% 
  glimpse()


d_tot_anio_complete <- bind_rows(d_tot_anio_2016, d_tot_anio_2021) %>% 
  mutate(
    tipo = "VOB general",
    num_pregunta = str_to_sentence(
      str_replace_all(
        str_remove_all(str_remove_all(num_pregunta, "v_vob_tipo_"), "_dummy"), "_", " "
      )
    )
  ) %>% 
  bind_rows(
    d_tot_anio_2 %>% 
      mutate(
        tipo = "VOB en cesárea",
        num_pregunta = case_when(
          num_pregunta == "v_cesárea_no_fue_informada_por_qué_dummy" ~ "No fue informada por qué se le practicó una cesárea",
          T ~ "No autorizó que se le realizara una cesárea"
        )
      )
  ) %>% 
  glimpse

título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por tipo de manifestación de violencia"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"

ggplot(
  d_tot_anio_complete %>% 
    filter(preferencia==T),
  aes(
    x = v_prop,
    y = reorder(str_wrap(num_pregunta, 25), v_prop),
    label = scales::percent(v_prop, accuracy = 0.1)
  )
) + 
  geom_col(width = 0.4, aes(fill = tipo)) +
  facet_wrap(~paste0("ENDIREH - ", anio)) +
  geom_label(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu") +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.4)) +
  scale_fill_manual("", values = c("#ff6260", v_gire_cols_2[2])) +
  theme_bw() +
  labs(
    title = str_wrap(título, 50),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    strip.text.x = element_text(size = 25),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "top",
    legend.text = element_text(size = 20)
  )

ggsave(filename = paste_plot("04_01_prop_vob_manifestaciones_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 5. Lugar de atención ----
## 5.1. Últimos 5 años (atención) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=cruce_lugar_atencion_parto,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=cruce_lugar_atencion_parto,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años que tuvieron al menos un parto"
subtítulo <- "Desagregación por lugar de atención del parto"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    mutate(preferencia = recode_lugar_atencion_parto(as.numeric(preferencia))) %>% 
    drop_na(preferencia),
  aes(
    x = v_prop,
    y = reorder(str_wrap(preferencia,20), v_prop),
    label = scales::percent(v_prop, accuracy = 0.1),
    col = paste0("ENDIREH - ", anio),
    fill = paste0("ENDIREH - ", anio)
  )
) + 
  geom_col(
    width = 0.4,
    position = position_dodge2()
  ) +
  geom_text(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .9)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.4)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("05_01_prop_lugar_at_parto_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

## 5.2. Por año de último parto ----
d_tot  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
  mutate(
    keep = case_when(
      anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
      anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
      T ~ 0
    ),
    v_parto_dummy = ifelse(filtro_parto==1, T, F)
  ) %>% 
  filter(keep == 1) %>% 
  group_and_wponder_by(
    .variable_a_pond=cruce_lugar_atencion_parto,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, v_anio_ult_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
      mutate(
        keep = case_when(
          anio == 2016 & as.numeric(v_anio_ult_parto) >= 2011 & as.numeric(v_anio_ult_parto) <= 2016 ~ 1,
          anio == 2021 & as.numeric(v_anio_ult_parto) >= 2016 & as.numeric(v_anio_ult_parto) <= 2021 ~ 1,
          T ~ 0
        ),
        v_parto_dummy = ifelse(filtro_parto==1, T, F)
      ) %>% 
      filter(keep == 1) %>% 
      group_and_wponder_by(
        .variable_a_pond=cruce_lugar_atencion_parto,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, v_anio_ult_parto
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()

título <- "Porcentaje de mujeres entre 15 y 49 años que tuvieron al menos un parto"
subtítulo <- "Desagregación por lugar de atención del parto"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot %>% 
    mutate(preferencia = recode_lugar_atencion_parto(as.numeric(preferencia))) %>% 
    drop_na(preferencia),
  aes(
    y = v_prop,
    x = v_anio_ult_parto,
    label = scales::percent(v_prop, accuracy = 0.1),
    group = paste0("ENDIREH - ", anio),
    col = paste0("ENDIREH - ", anio)
  )
) + 
  geom_line(size = 2) + geom_point(size = 3.5) +
  facet_wrap(~reorder(str_wrap(preferencia,20), v_prop)) +
  geom_label(show.legend = F, size = 3, family = "Ubuntu", aes(vjust = ifelse(anio == 2016, -0.3, 1.3))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )
ggsave(filename = paste_plot("05_02_prop_lugar_at_parto_anio_ult_parto.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 6. Lugar de atención ----
## 6.1. Últimos 5 años (vob) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_lugar_atencion_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_lugar_atencion_parto
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por lugar de atención del parto"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    mutate(cruce_lugar_atencion_parto = recode_lugar_atencion_parto(as.numeric(cruce_lugar_atencion_parto))) %>% 
    filter(preferencia==T),
  aes(
    x = v_prop,
    y = reorder(str_wrap(cruce_lugar_atencion_parto,20), v_prop),
    label = scales::percent(v_prop, accuracy = 0.1),
    col = paste0("ENDIREH - ", anio),
    fill = paste0("ENDIREH - ", anio)
  )
) + 
  geom_col(
    width = 0.4,
    position = position_dodge2()
  ) +
  geom_text(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .9)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("06_01_vob_alguna_prop_lugar_at_parto_5_anios.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

## 6.2. Por año de último parto (vob) ----
d_tot  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, v_anio_ult_parto, cruce_lugar_atencion_parto
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, v_anio_ult_parto, cruce_lugar_atencion_parto
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()

título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por lugar de atención del parto"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"

ggplot(
  d_tot %>% 
    mutate(cruce_lugar_atencion_parto = recode_lugar_atencion_parto(as.numeric(cruce_lugar_atencion_parto))) %>% 
    filter(preferencia==T),
  aes(
    y = v_prop,
    x = v_anio_ult_parto,
    label = scales::percent(v_prop, accuracy = 0.1),
    group = paste0("ENDIREH - ", anio),
    col = paste0("ENDIREH - ", anio)
  )
) + 
  geom_line(size = 2) + geom_point(size = 3.5) +
  facet_wrap(~reorder(str_wrap(cruce_lugar_atencion_parto,20), v_prop)) +
  geom_label(show.legend = F, size = 3, family = "Ubuntu", aes(vjust = ifelse(anio == 2016, -0.3, 1.3))) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 15, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )
ggsave(filename = paste_plot("06_02_vob_alguna_prop_lugar_at_parto_anio_ult_parto.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")


# 7. Mujeres con discapacidad (sólo 2021) ----
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
## 7.1. General ----
d_tot_anio <- 
  d_disc %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_discapacidad_alguna_dummy
  ) %>%  
  ungroup() %>% 
  mutate(tipo = ifelse(cruce_discapacidad_alguna_dummy==T, "Mujer con discapacidad", "Drop")) %>% 
  select(-cruce_discapacidad_alguna_dummy) %>% 
  bind_rows(
    
    d_disc %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_dificultad_alguna_dummy
      ) %>%  
      ungroup() %>% 
      mutate(tipo = ifelse(cruce_dificultad_alguna_dummy==T, "Mujer con dificultad", "Drop")) %>% 
      select(-cruce_dificultad_alguna_dummy)
    
  ) %>% 
  bind_rows(
    
    d_disc %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_sin_discapacidad_dificultad_dummy
      ) %>%  
      ungroup() %>% 
      mutate(tipo = ifelse(cruce_sin_discapacidad_dificultad_dummy==T, "Mujer sin discapacidad ni dificultad", "Drop")) %>% 
      select(-cruce_sin_discapacidad_dificultad_dummy)
    
  ) %>% 
  glimpse

título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por condición de discapacidad* o limitación**"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)\n*Incluye a las mujeres que tienen como respuesta 'No puede hacerlo' o\n'Lo hace con mucha dificultad' en al menos una de las actividades de la pregunta 19.1.\n**Incluye a las mujeres que únicamente tienen como respuesta 'Lo hace con poca dificultad'\nen alguna de las actividades de la pregunta 19.1."

ggplot(
  d_tot_anio_2 %>% 
    filter(preferencia==T, !tipo == "Drop"),
  aes(
    x = reorder(str_wrap(tipo,25), -v_prop),
    y = v_prop, 
    fill = v_prop,
    label = scales::percent(v_prop, accuracy = 0.1)
  )
) + 
  geom_col(width = 0.4, position = position_dodge2()) +
  geom_text(show.legend = F, vjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .9)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.6)) +
  scale_fill_gradient(high = v_gire_cols_2[2], low = "gray") +
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
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )


ggsave(filename = paste_plot("07_01_vob_alguna_disc_limit_física.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")
## 7.2. Por grupos de edad ----
d_tot_anio <- 
  d_disc %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_discapacidad_alguna_dummy, cruce_g_edad
  ) %>%  
  ungroup() %>% 
  mutate(tipo = ifelse(cruce_discapacidad_alguna_dummy==T, "1. Mujer con discapacidad", "Drop")) %>% 
  select(-cruce_discapacidad_alguna_dummy) %>% 
  bind_rows(
    
    d_disc %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_dificultad_alguna_dummy, cruce_g_edad
      ) %>%  
      ungroup() %>% 
      mutate(tipo = ifelse(cruce_dificultad_alguna_dummy==T, "2. Mujer con limitación", "Drop")) %>% 
      select(-cruce_dificultad_alguna_dummy)
    
  ) %>% 
  bind_rows(
    
    d_disc %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_sin_discapacidad_dificultad_dummy, cruce_g_edad
      ) %>%  
      ungroup() %>% 
      mutate(tipo = ifelse(cruce_sin_discapacidad_dificultad_dummy==T, "3. Mujer sin discapacidad ni dificultad", "Drop")) %>% 
      select(-cruce_sin_discapacidad_dificultad_dummy)
    
  ) %>% 
  glimpse

título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por condición de discapacidad* o limitación**"
nota <- "Elaboración de GIRE con información de la ENDIREH (2021)\n*Incluye a las mujeres que tienen como respuesta 'No puede hacerlo' o\n'Lo hace con mucha dificultad' en al menos una de las actividades de la pregunta 19.1.\n**Incluye a las mujeres que únicamente tienen como respuesta 'Lo hace con poca dificultad'\nen alguna de las actividades de la pregunta 19.1."


ggplot(
  data = 
    d_tot_anio %>% 
    filter(preferencia) %>% 
    filter(!tipo == "Drop"),
  aes(
    y = cruce_g_edad, 
    x = str_wrap(tipo,25),
    fill = v_prop
  )
) +
  geom_tile(col = "white", show.legend = F) +
  geom_text(aes(label = scales::percent(v_prop, accuracy = 0.1)), family = "Ubuntu", size = 7) +
  scale_fill_gradient("", high = "#ff6260", low = v_gire_cols_2[2])  +
  guides(label = "none") +
  scale_x_discrete(position = "top") +
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
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.position = "none"
  )

ggsave(filename = paste_plot("07_02_vob_alguna_disc_limit_física_cruce_g_edad.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 8. Grupos de edad ----
## 8.1. Últimos 5 años (vob) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_g_edad
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_g_edad
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por grupos de edad"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    filter(preferencia==T),
  aes(
    x = v_prop,
    y = fct_rev(str_wrap(cruce_g_edad,20)),
    label = scales::percent(v_prop, accuracy = 0.1),
    col = paste0("ENDIREH - ", anio),
    fill = paste0("ENDIREH - ", anio)
  )
) + 
  geom_col(
    width = 0.4,
    position = position_dodge2()
  ) +
  geom_text(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .6)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("08_01_vob_alguna_g_edad.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 9. Escolaridad ----
## 9.1. Últimos 5 años (vob) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_escolaridad
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_escolaridad
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por escolaridad"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    mutate(cruce_escolaridad = case_when(
      cruce_escolaridad == "Ninguno" ~ "1. Ninguno",
      cruce_escolaridad == "Primaria" ~ "2. Primaria",
      cruce_escolaridad == "Secundaria" ~ "3. Secundaria",
      cruce_escolaridad == "Preparatoria" ~ "4. Preparatoria",
      T ~ "5. Licenciatura o más"
    )) %>% 
    filter(preferencia==T),
  aes(
    x = v_prop,
    y = fct_rev(str_wrap(cruce_escolaridad,20)),
    label = scales::percent(v_prop, accuracy = 0.1),
    col = paste0("ENDIREH - ", anio),
    fill = paste0("ENDIREH - ", anio)
  )
) + 
  geom_col(
    width = 0.4,
    position = position_dodge2()
  ) +
  geom_text(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .6)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("09_01_vob_alguna_escolaridad.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")


# 10. Mujer ocupada en un empleo ----
## 10.1. Últimos 5 años (vob) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_ocupada_dummy
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_ocupada_dummy
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()


título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por si se encuentra ocupada en un empleo o no"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"
ggplot(
  d_tot_anio %>% 
    mutate(cruce_ocupada_dummy = ifelse(cruce_ocupada_dummy==T, "Mujeres ocupadas", "Mujeres no ocupadas")) %>% 
    filter(preferencia==T),
  aes(
    x = v_prop,
    y = cruce_ocupada_dummy,
    label = scales::percent(v_prop, accuracy = 0.1),
    col = paste0("ENDIREH - ", anio),
    fill = paste0("ENDIREH - ", anio)
  )
) + 
  geom_col(
    width = 0.4,
    position = position_dodge2()
  ) +
  geom_text(show.legend = F, hjust = -0.2, size = 8, family = "Ubuntu", position = position_dodge2(width = .6)) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("10_01_vob_alguna_muj_ocup_dummy.png"), 
       width = 15, height = 15, 
       dpi = 200, bg= "transparent")

# 11. Entidades ----
## 11.1. Últimos 5 años (vob) ----
d_tot_anio  <- 
  d_endireh_vob %>% 
  filter(anio == 2016) %>% 
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
  group_and_wponder_by(
    .variable_a_pond=v_vob_alguna_2,
    .ponderador =  fac_muj,
    .strata = est_dis,
    .ids = upm_dis,
    anio, cruce_cve_ent
  ) %>%  
  ungroup() %>% 
  bind_rows(
    d_endireh_vob %>% 
      filter(anio == 2021) %>% 
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
      group_and_wponder_by(
        .variable_a_pond=v_vob_alguna_2,
        .ponderador =  fac_muj,
        .strata = est_dis,
        .ids = upm_dis,
        anio, cruce_cve_ent
      ) %>%  
      ungroup()
  ) %>% 
  glimpse()

d <- d_tot_anio %>% 
  rename(cve_ent = cruce_cve_ent) %>% 
  left_join(
    readxl::read_excel(paste_inp("00_cve_ent.xlsx")) %>% 
      select(cve_ent, ent = ent)
  ) %>% 
  filter(preferencia==T) %>% 
  select(anio, cve_ent, ent, v_prop) %>% 
  group_by(cve_ent) %>% 
  mutate(
    diff = v_prop-lag(v_prop),
    diff= ifelse(is.na(diff), lead(v_prop)-v_prop, diff),
    diff = diff*(-1)
  ) %>% 
  glimpse

d_wide <- d %>%
  select(cve_ent, ent, anio, v_prop) %>% 
  pivot_wider(names_from = "anio", values_from = "v_prop") %>%
  ungroup() %>% 
  mutate(diff = round(`2016`-`2021`,4)) %>% 
  glimpse()

título <- "Porcentaje de mujeres entre 15 y 49 años en cuyo último parto sufrió de VOB"
subtítulo <- "Desagregación por entidad federativa"
nota <- "Elaboración de GIRE con información de la ENDIREH (2016 y 2021)"

ggplot(d_wide) +
  geom_dumbbell(
    aes(y = reorder(ent,`2021`), 
        x = `2016`, xend = `2021`, group = ent),
    col = "#D2D0CD",
    size=3, 
  ) + 
  geom_text(
    data = d,
    aes(y = reorder(ent, v_prop),x = v_prop, color =as.factor(anio), 
        label = ifelse(anio == "2021" & diff < 0, scales::percent(v_prop,accuracy = 0.01), NA)),
    hjust = -0.2, show.legend = F, size = 7, family = "Ubuntu"
  ) +
  geom_text(
    data = d,
    aes(y = reorder(ent, v_prop),x = v_prop, color =as.factor(anio), 
        label = ifelse(anio == "2016" & diff < 0, scales::percent(v_prop,accuracy = 0.01), NA)),
    hjust = 1.2, show.legend = F, size = 7, family = "Ubuntu"
  ) +
  geom_text(
    data = d,
    aes(y = reorder(ent, v_prop),x = v_prop, color =as.factor(anio), 
        label = ifelse(anio == "2021" & diff > 0, scales::percent(v_prop,accuracy = 0.01), NA)),
    hjust = 1.2, show.legend = F, size = 7, family = "Ubuntu"
  ) +
  geom_text(
    data = d,
    aes(y = reorder(ent, v_prop),x = v_prop, color =as.factor(anio), 
        label = ifelse(anio == "2016" & diff > 0, scales::percent(v_prop,accuracy = 0.01), NA)),
    hjust = -0.2, show.legend = F, size = 7, family = "Ubuntu"
  ) +
  geom_point(
    data = d ,
    aes(y = reorder(ent, v_prop),x = v_prop, color =as.factor(anio)), size = 7
  ) +
  scale_x_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_manual("", values = v_gire_cols_2) +
  scale_color_manual("", values = v_gire_cols_2) +
  theme_bw() +
  labs(
    title = str_wrap(título, 45),
    subtitle = subtítulo,
    caption = nota
  ) +
  theme(
    plot.title = element_text(size = 38, face = "bold", colour = "#777777", hjust = 0.5),
    plot.subtitle = element_text(size = 30, colour = "#777777", hjust = 0.5),
    plot.caption = element_text(size = 24),
    panel.background = element_rect(fill = "transparent",colour = NA),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    text = element_text(family = "Ubuntu"),
    legend.text = element_text(size = 20),
    legend.position = "top"
  )

ggsave(filename = paste_plot("11_01_vob_alguna_ent.png"), 
       width = 15, height = 20, 
       dpi = 200, bg= "transparent")

