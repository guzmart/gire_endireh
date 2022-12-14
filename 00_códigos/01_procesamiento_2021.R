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

require(tidyverse)

# Directorios ----
paste_inp       <- function(x){paste0("01_datos_crudos/" , x)}
paste_out       <- function(x){paste0("02_datos_limpios/", x)}
paste_plot      <- function(x){paste0("03_gráficas/", x)}
source("00_códigos/00_funciones.R")
# Datos ----
## 2016 - Selección de variables de interés ----
v_carpeta <- "bd_endireh_2021_dbf/"
v_carpeta_2 <- "bd_endireh_2021_dbf/"
v_anio <- 2021
### TSDEM - Sociodemográficos ----
d_sdem <- read.dbf(paste_inp(paste0(v_carpeta, "TSDem.DBF")), as.is = T) %>% 
  janitor::clean_names() %>% 
  drop_na(ren_muj_el) %>% 
  mutate(
    anio = v_anio,
    llave = paste0(upm, viv_sel, hogar, ren_muj_el),
    cruce_tipo_loc = recode_tipo_loc(dominio),
    cruce_auto_indig_dummy = ifelse(p2_10 < 3, "1", "2"),
    cruce_g_edad = recode_g_edad(as.numeric(edad)),
    cruce_escolaridad = recode_escolaridad(as.numeric(niv)),
    cruce_pea_pnea = recode_pea_pnea(as.numeric(p2_14)),
    cruce_pos_ocu = recode_pos_ocu(as.numeric(p2_15))
  ) %>% 
  select(
    llave, anio, cruce_cve_ent = cve_ent, 
    keep_edad_num = edad, cruce_g_edad,
    cruce_escolaridad, keep_escolaridad_num = gra, 
    cruce_alfabet_dummy = p2_8, cruce_auto_indig_dummy, 
    cruce_lengua_indig_dummy = p2_11, cruce_ocupada_dummy = p2_13, 
    cruce_pea_pnea, cruce_pos_ocu,
    cruce_tipo_loc, fac_viv:upm_dis
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_III - Elegibilidad y situación conyugal ----
d_sec_iii <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_III.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio,
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
   llave, anio, 
   cruce_edo_civil = p3_1, cruce_edo_civil_2 = p3_8,
  ) %>% 
  mutate(
    cruce_edo_civil = recode_cruce_edo_civil(cruce_edo_civil),
    cruce_edo_civil_2 = recode_cruce_edo_civil_2(cruce_edo_civil_2)
  ) %>% 
  glimpse

### TSEC_IV - Situación de ingresos y recursos ----
d_sec_iv <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_IV.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  rename_at(
   vars(starts_with("p4_8_")),
   funs(rename_p4_8_2021)
  ) %>% 
  rename_at(
    vars(starts_with("p4_9_")),
    funs(rename_p4_9_2021)
  ) %>% 
  select(
    llave, anio, 
    cruce_ingreso_dummy = p4_1, sucio_ingreso = p4_2, sucio_ingreso_periodo = p4_2_1,
    cruce_ingreso_pareja_dummy = p4_6_ab, cruce_ingreso_pareja_mensual = p4_7_ab,
    starts_with("cruce_ingreso_otro_"), cruce_cuenta_dinero_utilizar_como_quiera_dummy = p4_11,
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_VII - Ámbito escolar ----
d_sec_vii <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_VII.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_escuela_dummy = p7_6_15
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_VIII - Ámbito laboral ----
d_sec_viii <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_VIII.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_trabajo_dummy = p8_9_14
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_IX - Ámbito comunitario----
d_sec_ix <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_IX.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_comunitario_dummy = p10_1_14
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_X - Atención obstétrica ----
d_sec_x <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_X.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  recode_dummy("cruce_afiliación_pública_dummy", p10_1_1:p10_1_6, p10_1_8) %>% 
  recode_dummy("cruce_afiliación_privada_dummy", p10_1_7) %>% 
  recode_dummy("cruce_afiliación_ninguna_dummy", p10_1_9) %>% 
  recode_dummy("v_vob_tipo_posiciones_incómodas_dummy", p10_8_1) %>% 
  recode_dummy("v_vob_tipo_gritos_o_regaños_dummy", p10_8_2) %>% 
  recode_dummy("v_vob_tipo_jalones_o_pellizcos", p10_8_3) %>% 
  recode_dummy("v_vob_tipo_ofensas_dummy", p10_8_4) %>% 
  recode_dummy("v_vob_tipo_fue_ignorada_dummy", p10_8_5) %>% 
  recode_dummy("v_vob_tipo_anestecia_denegada_dummy", p10_8_6) %>% 
  recode_dummy("v_vob_tipo_atención_tardada_por_gritos_o_quejas_dummy", p10_8_7) %>% 
  recode_dummy("v_vob_tipo_método_anticonceptivo_o_esterlización_forzada_dummy", p10_8_8) %>% 
  recode_dummy("v_vob_tipo_presión_para_aceptar_anticoncepción_o_esterilización_dummy", p10_8_9) %>% 
  recode_dummy("v_vob_tipo_firma_involuntaria_de_papeles_dummy", p10_8_10) %>% 
    recode_dummy("v_vob_tipo_fue_aislada_de_su_bebé_por_más_de_5_horas_dummy", p10_8_11) %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_afiliación = case_when(
      cruce_afiliación_privada_dummy == T ~ "Servicios médicos privados",
      cruce_afiliación_pública_dummy == T ~ "Servicios médicos públicos",
      T ~ "Ningún tipo de servicio"
    ),
    v_vob_alguna = case_when(
      p10_8_1 == "1" ~ T,
      p10_8_2 == "1" ~ T,
      p10_8_3 == "1" ~ T,
      p10_8_4 == "1" ~ T,
      p10_8_5 == "1" ~ T,
      p10_8_6 == "1" ~ T,
      p10_8_7 == "1" ~ T,
      p10_8_8 == "1" ~ T,
      p10_8_9 == "1" ~ T,
      p10_8_10 == "1" ~ T,
      T ~ F
    ),
    filtro_parto = case_when(
      as.numeric(p10_4_1) >= 1 ~ 1,
      as.numeric(p10_4_2) >= 1 ~ 1,
      T ~ 2
    )
  ) %>% 
  select(
    llave, anio, 
    filtro_embarazo = p10_2, filtro_parto,
    starts_with("cruce_afiliación"), cruce_lugar_atencion_parto = p10_7,
    starts_with("v_vob"), v_cesárea_dummy= p10_8_12, 
    v_cesárea_informaron_por_qué_dummy = p10_8_13,
    v_cesárea_autorización_dummy = p10_8_14,
    v_anio_ult_parto = p10_6anio, v_mes_ult_parto = p10_6mes
  ) %>%
  glimpse()
beepr::beep(2)

### TSEC_XI - Ámbito familiar----
d_sec_xi <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_XI.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_familiar_dummy = case_when(
      p11_1_3 < 4 ~ "1",
      T ~ "2"
    )
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_familiar_dummy
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_XII - Familia de origen (infancia) ----
d_sec_xii <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_XII.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_infancia_dummy = case_when(
      p12_14_5 == "1" ~ "1",
      p12_14_6 == "1" ~ "1",
      T ~ "2"
    )
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_infancia_dummy
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_XIV- Relación actual o última relación ----
d_sec_xiv <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_XIV.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_pareja_expareja_dummy = case_when(
      p14_1_25 < 4 ~ "1",
      p14_1_26 < 4 ~ "1",
      p14_1_27 < 4 ~ "1",
      p14_1_29 < 4 ~ "1",
      T ~ "2"
    )
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_pareja_expareja_dummy
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_XIX - discapacidad ----
d_sec_xix<- foreign::read.dbf(
  paste_inp(paste0("bd_endireh_2021_dbf/TB_SEC_XIX.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  #head(50) %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  transmute(
    llave, 
    anio, 
    cruce_discapacidad_dummy_física_caminar = case_when(
      is.na(p19_1_1) ~ F,
      as.numeric(p19_1_1)==1 ~ T,
      as.numeric(p19_1_1)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_física_ver = case_when(
      is.na(p19_1_2) ~ F,
      as.numeric(p19_1_2)==1 ~ T,
      as.numeric(p19_1_2)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_física_mover_brazos = case_when(
      is.na(p19_1_3) ~ F,
      as.numeric(p19_1_3)==1 ~ T,
      as.numeric(p19_1_3)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_cognitiva_aprender_recordar_concentrarse = case_when(
      is.na(p19_1_4) ~ F,
      as.numeric(p19_1_4)==1 ~ T,
      as.numeric(p19_1_4)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_física_escuchar = case_when(
      is.na(p19_1_5) ~ F,
      as.numeric(p19_1_5)==1 ~ T,
      as.numeric(p19_1_5)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_física_bañar_vestirse_o_comer = case_when(
      is.na(p19_1_6) ~ F,
      as.numeric(p19_1_6)==1 ~ T,
      as.numeric(p19_1_6)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_física_hablar = case_when(
      is.na(p19_1_7) ~ F,
      as.numeric(p19_1_7)==1 ~ T,
      as.numeric(p19_1_7)==2 ~ T,
      T ~ F
    ),
    cruce_discapacidad_dummy_cognitiva_problemas_emocionales = case_when(
      is.na(p19_1_8) ~ F,
      as.numeric(p19_1_8)==1 ~ T,
      as.numeric(p19_1_8)==2 ~ T,
      T ~ F
    ),
    
    cruce_dificultad_dummy_física_caminar = case_when(
      is.na(p19_1_1) ~ F,
      as.numeric(p19_1_1)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_física_ver = case_when(
      is.na(p19_1_2) ~ F,
      as.numeric(p19_1_2)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_física_mover_brazos = case_when(
      is.na(p19_1_3) ~ F,
      as.numeric(p19_1_3)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_cognitiva_aprender_recordar_concentrarse = case_when(
      is.na(p19_1_4) ~ F,
      as.numeric(p19_1_4)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_física_escuchar = case_when(
      is.na(p19_1_5) ~ F,
      as.numeric(p19_1_5)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_física_bañar_vestirse_o_comer = case_when(
      is.na(p19_1_6) ~ F,
      as.numeric(p19_1_6)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_física_hablar = case_when(
      is.na(p19_1_7) ~ F,
      as.numeric(p19_1_7)==3 ~ T,
      T ~ F
    ),
    cruce_dificultad_dummy_cognitiva_problemas_emocionales = case_when(
      is.na(p19_1_8) ~ F,
      as.numeric(p19_1_8)==3 ~ T,
      T ~ F
    ),
  ) %>% 
  glimpse

# Unir base de datos ----
d_endireh_vob <- d_sec_iii %>% 
  left_join(
    d_sdem
  ) %>% 
  left_join(
    d_sec_iv
  ) %>% 
  left_join(
    d_sec_vii
  ) %>% 
  left_join(
    d_sec_viii
  ) %>% 
  left_join(
    d_sec_ix
  ) %>% 
  left_join(
    d_sec_x
  ) %>% 
  left_join(
    d_sec_xi
  ) %>% 
  left_join(
    d_sec_xii 
  ) %>% 
  left_join(
    d_sec_xiv
  ) %>%
  left_join(d_sec_xix)
  mutate(
    across(
      contains("cesárea"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  select(
    llave, anio, starts_with("keep"), starts_with("filtro"), 
    starts_with("cruce_"), starts_with("v_"),
    starts_with("sucio"), fac_viv:upm_dis
  ) %>% 
  glimpse

saveRDS(d_endireh_vob, paste_out("01_endireh_2021_vob_no_filter.rds"))
