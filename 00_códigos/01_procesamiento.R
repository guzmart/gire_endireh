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
  rename_at(
   vars(starts_with("p4_8_")),
   funs(rename_p4_8)
  ) %>% 
  rename_at(
    vars(starts_with("p4_9_")),
    funs(rename_p4_9)
  ) %>% 
  select(
    llave, anio, 
    cruce_ingreso_dummy = p4_1, sucio_ingreso = p4_2, sucio_ingreso_periodo = p4_2_1,
    cruce_ingreso_pareja_dummy = p4_6_ab, cruce_ingreso_pareja_mensual = p4_7_ab,
    starts_with("cruce_ingreso_otro_"), cruce_cuenta_dinero_utilizar_como_quiera = p4_11,
  ) %>% 
  glimpse

d_sec_ix <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_IX.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  recode_dummy("cruce_afiliación_pública_dummy", p9_1_1:p9_1_6, p9_1_8) %>% 
  recode_dummy("cruce_afiliación_privada_dummy", p9_1_7) %>% 
  recode_dummy("cruce_afiliación_ninguna_dummy", p9_1_9) %>% 
  recode_dummy("v_vob_tipo_posiciones_incómodas_dummy", p9_8_1) %>% 
  recode_dummy("v_vob_tipo_gritos_o_regaños_dummy", p9_8_2) %>% 
  recode_dummy("v_vob_tipo_ofensas_dummy", p9_8_3) %>% 
  recode_dummy("v_vob_tipo_fue_ignorada_dummy", p9_8_4) %>% 
  recode_dummy("v_vob_tipo_anestecia_denegada_dummy", p9_8_5) %>% 
  recode_dummy("v_vob_tipo_atención_tardada_por_gritos_o_quejas_dummy", p9_8_6) %>% 
  recode_dummy("v_vob_tipo_método_anticonceptivo_o_esterlización_forzada_dummy", p9_8_7) %>% 
  recode_dummy("v_vob_tipo_presión_para_aceptar_anticoncepción_o_esterilización_dummy", p9_8_8) %>% 
  recode_dummy("v_vob_tipo_firma_involuntaria_de_papeles_dummy", p9_8_9) %>% 
  recode_dummy("v_vob_tipo_fue_aislada_de_su_bebé_por_más_de_5_horas_dummy", p9_8_10) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_afiliación = case_when(
      cruce_afiliación_privada == T ~ "Servicios médicos privados",
      cruce_afiliación_pública == T ~ "Servicios médicos públicos",
      T ~ "Ningún tipo de servicio"
    ),
    v_vob_alguna = case_when(
      p9_8_1 == "1" ~ T,
      p9_8_2 == "1" ~ T,
      p9_8_3 == "1" ~ T,
      p9_8_4 == "1" ~ T,
      p9_8_5 == "1" ~ T,
      p9_8_6 == "1" ~ T,
      p9_8_7 == "1" ~ T,
      p9_8_8 == "1" ~ T,
      p9_8_9 == "1" ~ T,
      p9_8_10 == "1" ~ T,
      T ~ F
    )
      
      # ifelse(
      #   p9_8_1 > 1 &
      #     p9_8_2 > 1 &
      #     p9_8_3 > 1 &
      #     p9_8_4 > 1 &
      #     p9_8_5 > 1 &
      #     p9_8_6 > 1 &
      #     p9_8_7 > 1 & 
      #     p9_8_8 > 1 &
      #     p9_8_9 > 1 & 
      #     p9_8_10 > 1,
      #   0,
      #   1
      # )
  ) %>% 
  select(
    llave, anio, 
    filtro_embarazo = p9_2, 
    starts_with("cruce_afiliación"), cruce_lugar_atencion_parto = p9_7,
    starts_with("v_vob"), v_cesárea_dummy= p9_8_11, 
    v_cesárea_informaron_por_qué_dummy = p9_8_12,
    v_cesárea_autorización_dummy = p9_8_13
    
  ) %>%
  glimpse()







