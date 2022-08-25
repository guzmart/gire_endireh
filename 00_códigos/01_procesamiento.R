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

require(tidyverse)

# Directorios ----
paste_inp       <- function(x){paste0("01_datos_crudos/" , x)}
paste_out       <- function(x){paste0("02_datos_limpios/", x)}
paste_plot      <- function(x){paste0("03_gráficas/", x)}

# Datos ----
## Selección de variables de interés ----
d_sdem <- read.dbf(paste_inp("bd_sd_endireh2016_sitioinegi_dbf/TSDem.DBF"), as.is = T) %>% 
  janitor::clean_names() %>% 
  mutate(
    anio = 2016,
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_tipo_loc = dominio,
  ) %>% 
  select(
    llave, anio,
    cruce_cve_ent = cve_ent, cruce_edad = edad, cruce_niv = niv, cruce_gra = gra, 
    cruce_alfabet = p2_8, cruce_auto_indig = p2_10, cruce_lengua_indig = p2_11,
    cruce_ocupada = p2_13, cruce_pnea_pea = p2_14, cruce_pos_ocu = p2_15,
    cruce_edo_civl = p2_16, cruce_tipo_loc, fac_viv:upm_dis
  ) %>% 
  glimpse

d_sec_iii <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_III.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = 2016,
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
   llave, anio, 
   cruce_edo_civil_verif = p3_1, cruce_edo_civil_2 = p3_8,
  ) %>% 
  glimpse

d_sec_iv<- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_IV.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, 
    cruce_ingreso_dummy = p4_1, sucio_ingreso = p4_2, sucio_ingreso_periodo = p4_2_1,
    cruce_ingreso_pareja_dummy = p4_6_ab, cruce_ingreso_pareja_mensual = p4_7_ab
  )
  glimpse
