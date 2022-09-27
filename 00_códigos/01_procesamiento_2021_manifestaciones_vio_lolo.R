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
paste_inp       <- function(x){paste0("../equis_endireh/01_datos_crudos/" , x)}
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
  paste_inp("bd_endireh_2021_dbf/TB_SEC_VII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_endireh_2021_dbf/TB_SEC_VII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() 
  ) %>%
  #head(50) %>%  
  
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_vio_sex_escuela_dummy = p7_6_15,
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    ),
  ) %>% 
  mutate(across(
    c(starts_with("p7_6_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_7_")),
      .fns =  list("quien_vid_Maestro"     = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_vid_Maestra"     = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_vid_Compañero"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_vid_Compañera"   = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_vid_Directore"   = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_vid_Trabajador"  = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_vid_Trabajadora" = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_vid_desconocido" = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_vid_Otra"        = ~ case_when(. == "9" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_8_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_9_")),
      .fns =  list("quien_ahora_Maestro"     = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_ahora_Maestra"     = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_ahora_Compañero"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_ahora_Compañera"   = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_ahora_Directore"   = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_ahora_Trabajador"  = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_ahora_Trabajadora" = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_ahora_desconocido" = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_ahora_Otra"        = ~ case_when(. == "9" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_10_")),
      .fns =  list("donde_ahora_escuela"           = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_ahora_cerca"             = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_ahora_lejos"             = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_ahora_transportepúblico" = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_ahora_particular"        = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_ahora_Otro"              = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_escuela_apoyo = case_when(p7_14_1 == "1" ~ T,T ~ F),
    v_vio_escuela_denuncia = case_when(p7_14_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p7_25_")),
      .fns =  list("que_sucedio_castigaron"         = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nocastigaron" = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez" = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_recomendacion"      = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_noRatifico"        = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_nada"               = ~ case_when(. == "6" ~ T,T ~ F),
                   "que_sucedio_nosabe"             = ~ case_when(. == "7" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_16_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  )  

d_sec_vii$v_vio_escuela_vida_quien_Maestro     = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Maestro_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Maestra     = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Maestra_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Compañero   = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Compañero_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Compañera   = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Compañera_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Directore   = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Directore_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Trabajador  = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Trabajador_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Trabajadora = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Trabajadora_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_desconocido = rowSums(d_sec_vii %>% select(starts_with("quien_vid_desconocido_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_quien_Otra        = rowSums(d_sec_vii %>% select(starts_with("quien_vid_Otra_p7_7_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_fisica           = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_8_",c(1,2,6))), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_emocional        = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_8_",c(4,9,13,16,18))), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_sexual           = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_8_",c(3,5,7,8,10,11,12,14,15,17))), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Maestro     = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Maestro_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Maestra     = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Maestra_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Compañero   = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Compañero_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Compañera   = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Compañera_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Directore   = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Directore_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Trabajador  = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Trabajador_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Trabajadora = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Trabajadora_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_desconocido = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_desconocido_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_quien_Otra        = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_Otra_p7_9_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_escuela            = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_escuela_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_cerca              = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_cerca_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_lejos              = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_lejos_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_transportepúblico  = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_transportepúblico_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_particular         = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_particular_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_ahora_donde_Otro               = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_Otro_p7_10_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_tipo_apoyo_orientacion = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_orientacion_p7_16_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_tipo_apoyo_legal       = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_legal_p7_16_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_tipo_apoyo_psicologico = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_psicologico_p7_16_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_tipo_apoyo_medica      = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_medica_p7_16_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_tipo_apoyo_Otro        = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_Otro_p7_16_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_castigaron       = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_castigaron_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_nocastigaron     = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_nocastigaron_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_consignaAnteJuez = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_consignaAnteJuez_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_recomendacion    = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_recomendacion_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_noRatifico       = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_noRatifico_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_nada             = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_nada_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_denuncia_que_sucedio_nosabe           = d_sec_vii$v_vio_escuela_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_nosabe_p7_25_")), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_fisica    = d_sec_vii$v_vio_escuela_ahora_fisica | rowSums(d_sec_vii %>% select(paste0("mod_p7_6_",c(1,2,6))), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_emocional = d_sec_vii$v_vio_escuela_ahora_emocional | rowSums(d_sec_vii %>% select(paste0("mod_p7_6_",c(4,9,13,16,18))), na.rm = T)>0
d_sec_vii$v_vio_escuela_vida_sexual    = d_sec_vii$v_vio_escuela_ahora_sexual | rowSums(d_sec_vii %>% select(paste0("mod_p7_6_",c(3,5,7,8,10,11,12,14,15,17))), na.rm = T)>0

d_sec_vii <- d_sec_vii %>% 
  mutate(
    v_vio_escuela_ahora_todas    = v_vio_escuela_ahora_fisica | v_vio_escuela_ahora_emocional|v_vio_escuela_ahora_sexual,
    v_vio_escuela_vida_todas     = v_vio_escuela_vida_fisica | v_vio_escuela_vida_emocional|  v_vio_escuela_vida_sexual,
  ) %>% 
  ungroup() %>% 
  mutate(
    v_vio_escuela_institucion_INM              = case_when(p7_15_1 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_linea            = case_when(p7_15_2 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_ac               = case_when(p7_15_3 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_justicia_mujeres = case_when(p7_15_4 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_defensoria       = case_when(p7_15_5 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_clinica_publica  = case_when(p7_15_6 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_clinica_privada  = case_when(p7_15_7 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_dif              = case_when(p7_15_8 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_otro             = case_when(p7_15_9 == "1" ~ T,T ~ F),
    
    v_vio_escuela_institucion_trato_INM              = p7_20_1,
    v_vio_escuela_institucion_trato_linea            = p7_20_2,
    v_vio_escuela_institucion_trato_ac               = p7_20_3,
    v_vio_escuela_institucion_trato_justicia_mujeres = p7_20_4,
    v_vio_escuela_institucion_trato_defensoria       = p7_20_5,
    v_vio_escuela_institucion_trato_clinica_publica  = p7_20_6,
    v_vio_escuela_institucion_trato_clinica_privada  = p7_20_7,
    v_vio_escuela_institucion_trato_dif              = p7_20_8,
    v_vio_escuela_institucion_trato_otro             = p7_20_9,
    
    v_vio_escuela_denuncia_en_escuela          = case_when(p7_21_1 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_mp               = case_when(p7_21_2 == "1" ~T, T ~ F),
    v_vio_escuela_denuncia_en_policia          = case_when(p7_21_3 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_municipio        = case_when(p7_21_4 == "1" ~T, T ~ F),     
    
    v_vio_escuela_denuncia_que_en_escuela          = paste(p7_25_1_1,p7_25_1_2,p7_25_1_3,sep=","),     
    v_vio_escuela_denuncia_que_en_mp               = paste(p7_25_2_1,p7_25_2_2,p7_25_2_3,sep=","),
    v_vio_escuela_denuncia_que_en_policia          = paste(p7_25_3_1,p7_25_3_2,p7_25_3_3,sep=","),     
    v_vio_escuela_denuncia_que_en_municipio        = paste(p7_25_4_1,p7_25_4_2,p7_25_4_3,sep=","),   
    
    v_vio_escuela_no_denuncia_en_verguenza        = case_when(p7_26_1  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_noLeIbanACreer   = case_when(p7_26_2  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_miedo            = case_when(p7_26_3  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_familia          = case_when(p7_26_4  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_convencieron     = case_when(p7_26_5  == "1" ~T, T ~ F), 
    v_vio_escuela_no_denuncia_en_sinImportancia   = case_when(p7_26_6  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_costumbres       = case_when(p7_26_7  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_NoSabía          = case_when(p7_26_8  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_tiempo           = case_when(p7_26_9  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_desconfianza     = case_when(p7_26_10 == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_otra             = case_when(p7_26_11 == "1" ~T, T ~ F),  
  ) %>%    
  select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
### TSEC_VIII - Ámbito laboral ----

d_sec_viii <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_VIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  as_tibble() %>% 
  
  #rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_endireh_2021_dbf/TB_SEC_VIII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() #%>% rename(n_ren = ren_m_ele)
  ) %>% 
  #head(50) %>% 
  
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_trabajo_dummy = p8_9_14
  ) %>%
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p8_8_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_9_")),
      .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_10_")),
      .fns =  list("quien_vid_patron"          = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_vid_supervisor"      = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_vid_gerente"         = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_vid_compañere"       = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_vid_cliente"         = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_vid_desconocido"     = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_vid_familiar_patron" = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_vid_otro"            = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_11_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_12_")),
      .fns =  list("quien_ahora_patron"           = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_ahora_supervisor"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_ahora_gerente"          = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_ahora_compañere"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_ahora_cliente"          = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_ahora_desconocido"      = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_ahora_familiar_patron"  = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_ahora_otro"             = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_13_")),
      .fns =  list("donde_ahora_instalaciones"     = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_ahora_cerca"             = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_ahora_lejos"             = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_ahora_transportepúblico" = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_ahora_particular"        = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_ahora_Otro"              = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_laboral_apoyo = case_when(p8_17_1 == "1" ~ T,T ~ F),
    v_vio_laboral_denuncia = case_when(p8_17_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p8_28_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nocastigaron"            = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "7" ~ T,T ~ F),
                   "que_sucedio_recomendacion"           = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_despidocambio_ella_area" = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_despidocambio_el_area"   = ~ case_when(. == "6" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "8" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "9" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_20_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  ) 

d_sec_viii$v_vio_laboral_vida_quien_patron           = rowSums(d_sec_viii %>% select(starts_with("quien_vid_patron_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_supervisor       = rowSums(d_sec_viii %>% select(starts_with("quien_vid_supervisor_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_gerente          = rowSums(d_sec_viii %>% select(starts_with("quien_vid_gerente_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_compañere        = rowSums(d_sec_viii %>% select(starts_with("quien_vid_compañere_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_cliente          = rowSums(d_sec_viii %>% select(starts_with("quien_vid_cliente_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_desconocido      = rowSums(d_sec_viii %>% select(starts_with("quien_vid_desconocido_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_familiar_patron  = rowSums(d_sec_viii %>% select(starts_with("quien_vid_familiar_patron_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_quien_otro             = rowSums(d_sec_viii %>% select(starts_with("quien_vid_otro_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_fisica                = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_11_",c(8,9,19))), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_emocional             = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_11_",c(11,12,2,7,11,17,18))), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_sexual                = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_11_",c(1,3,4,5,6,10,13,14,15,16))), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_discriminacion        = rowSums(d_sec_viii %>% select(paste0("mod_p8_8_",c(1:9))), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_patron          = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_patron_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_supervisor      = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_supervisor_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_gerente         = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_gerente_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_compañere       = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_compañere_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_cliente         = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_cliente_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_desconocido     = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_desconocido_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_familiar_patron = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_familiar_patron_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_quien_otro            = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_otro_p8_12")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_instalaciones      = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_instalaciones_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_cerca              = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_cerca_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_lejos              = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_lejos_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_transportepúblico  = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_transportepúblico_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_particular         = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_particular_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_ahora_donde_Otro               = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_Otro_p8_13_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_tipo_apoyo_orientacion = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_orientacion_p8_20_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_tipo_apoyo_legal       = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_legal_p8_20_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_tipo_apoyo_psicologico = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_psicologico_p8_20_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_tipo_apoyo_medica      = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_medica_p8_20_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_tipo_apoyo_Otro        = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_Otro_p8_20_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_castigaron               = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_castigaron_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_nocastigaron             = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_nocastigaron_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_consignaAnteJuez         = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_consignaAnteJuez_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_recomendacion            = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_recomendacion_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_despidocambio_ella_area  = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_despidocambio_ella_area_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_despidocambio_el_area    = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_despidocambio_el_area_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_noRatifico               = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_noRatifico_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_nada                     = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_nada_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_denuncia_que_sucedio_nosabe                   = d_sec_viii$v_vio_laboral_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_nosabe_p8_28_")), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_fisica         = d_sec_viii$v_vio_laboral_ahora_fisica | rowSums(d_sec_viii %>% select(paste0("mod_p8_9_",c(8,9,19))), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_emocional      = d_sec_viii$v_vio_laboral_ahora_emocional | rowSums(d_sec_viii %>% select(paste0("mod_p8_9_",c(11,12,2,7,11,17,18))), na.rm = T)>0
d_sec_viii$v_vio_laboral_vida_sexual         = d_sec_viii$v_vio_laboral_ahora_sexual | rowSums(d_sec_viii %>% select(paste0("mod_p8_9_",c(1,3,4,5,6,10,13,14,15,16))), na.rm = T)>0
d_sec_viii <- d_sec_viii %>% 
  mutate(
    v_vio_laboral_ahora_todas          = v_vio_laboral_ahora_fisica|v_vio_laboral_ahora_emocional|v_vio_laboral_ahora_sexual|v_vio_laboral_ahora_discriminacion,
    v_vio_laboral_vida_discriminacion = v_vio_laboral_ahora_discriminacion,
    v_vio_laboral_vida_todas          = v_vio_laboral_vida_fisica|v_vio_laboral_vida_emocional|v_vio_laboral_vida_sexual|v_vio_laboral_vida_discriminacion,
    v_vio_laboral_institucion_INM                 = case_when(p8_18_1  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_linea               = case_when(p8_18_2  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_ac                  = case_when(p8_18_3  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_justicia_mujeres    = case_when(p8_18_4  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_defensoria          = case_when(p8_18_5  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_clinica_publica     = case_when(p8_18_6  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_clinica_privada     = case_when(p8_18_7  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_dif                 = case_when(p8_18_8  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_otro                = case_when(p8_18_9  == "1" ~ T,T ~ F),
    
    v_vio_laboral_institucion_trato_INM                 = p8_23_1 ,
    v_vio_laboral_institucion_trato_linea               = p8_23_2 ,
    v_vio_laboral_institucion_trato_ac                  = p8_23_3 ,
    v_vio_laboral_institucion_trato_justicia_mujeres    = p8_23_4 ,
    v_vio_laboral_institucion_trato_defensoria          = p8_23_5 ,
    v_vio_laboral_institucion_trato_clinica_publica     = p8_23_6 ,
    v_vio_laboral_institucion_trato_clinica_privada     = p8_23_7 ,
    v_vio_laboral_institucion_trato_dif                 = p8_23_8 ,
    v_vio_laboral_institucion_trato_otro                = p8_23_9 ,
    
    v_vio_laboral_denuncia_en_sindicato           = case_when(p8_24_1 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_fiscalia            = case_when(p8_24_2 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_policia             = case_when(p8_24_3 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_procuraduria        = case_when(p8_24_4 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_municipio           = case_when(p8_24_5 == "1" ~T, T ~ F), 
    
    v_vio_laboral_denuncia_que_en_sindicato           = paste(p8_28_1_1,p8_28_1_2,p8_28_1_3,sep=","),
    v_vio_laboral_denuncia_que_en_fiscalia            = paste(p8_28_2_1,p8_28_2_2,p8_28_2_3,sep=","),
    v_vio_laboral_denuncia_que_en_policia             = paste(p8_28_3_1,p8_28_3_2,p8_28_3_3,sep=","),
    v_vio_laboral_denuncia_que_en_procuraduria        = paste(p8_28_4_1,p8_28_4_2,p8_28_4_3,sep=","),
    v_vio_laboral_denuncia_que_en_municipio           = paste(p8_28_5_1,p8_28_5_2,p8_28_5_3,sep=","),
    
    v_vio_laboral_no_denuncia_en_verguenza        = case_when(p8_29_01  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_noLeIbanACreer   = case_when(p8_29_02  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_miedo            = case_when(p8_29_03  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_familia          = case_when(p8_29_04  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_convencieron     = case_when(p8_29_05  == "1" ~T, T ~ F), 
    v_vio_laboral_no_denuncia_en_sinImportancia   = case_when(p8_29_06  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_costumbres       = case_when(p8_29_07  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_NoSabía          = case_when(p8_29_08  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_tiempo           = case_when(p8_29_09  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_desconfianza     = case_when(p8_29_10 == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_otra             = case_when(p8_29_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

### TSEC_IX - Ámbito comunitario----
d_sec_ix <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_IX.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  #rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_endireh_2021_dbf/TB_SEC_IX_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() #%>% rename(n_ren = ren_m_ele)
  ) %>% 
  #head(50) %>% 
  
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_comunitario_dummy = p9_1_14,
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p9_1_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p9_2_")),
      .fns =  list("quien_vid_conocido"    = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_vid_amigo"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_vid_vecino"      = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_vid_policia"     = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_vid_militar"     = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_vid_sacerdote"   = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_vid_conductor"   = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_vid_desconodico" = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_vid_otro"        = ~ case_when(. == "9" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p9_3_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p9_4_")),
      .fns =  list("quien_ahora_conocido"    = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_ahora_amigo"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_ahora_vecino"      = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_ahora_policia"     = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_ahora_militar"     = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_ahora_sacerdote"   = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_ahora_conductor"   = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_ahora_desconodico" = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_ahora_otro"        = ~ case_when(. == "9" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p9_5_")),
      .fns =  list("donde_ahora_calle"      = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_ahora_mercado"    = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_ahora_autobús"    = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_ahora_metro"      = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_ahora_metrobus"   = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_ahora_taxi"       = ~ case_when(. == "6" ~ T,T ~ F),
                   "donde_ahora_iglesia"    = ~ case_when(. == "7" ~ T,T ~ F),
                   "donde_ahora_cantina"    = ~ case_when(. == "8" ~ T,T ~ F),
                   "donde_ahora_feria"      = ~ case_when(. == "9" ~ T,T ~ F),
                   "donde_ahora_particular" = ~ case_when(. == "10" ~ T,T ~ F),
                   "donde_ahora_otro"       = ~ case_when(. == "11" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_comunitario_apoyo    = case_when(p9_8_1 == "1" ~ T,T ~ F),
    v_vio_comunitario_denuncia = case_when(p9_8_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p9_10_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p9_19_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_nocastigaron"            = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    
  )  

d_sec_ix$v_vio_comunitario_vida_quien_conocido                   = rowSums(d_sec_ix %>% select(starts_with("quien_vid_conocido_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_amigo                      = rowSums(d_sec_ix %>% select(starts_with("quien_vid_amigo_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_vecino                     = rowSums(d_sec_ix %>% select(starts_with("quien_vid_vecino_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_policia                    = rowSums(d_sec_ix %>% select(starts_with("quien_vid_policia_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_militar                    = rowSums(d_sec_ix %>% select(starts_with("quien_vid_militar_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_sacerdote                  = rowSums(d_sec_ix %>% select(starts_with("quien_vid_sacerdote_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_conductor                  = rowSums(d_sec_ix %>% select(starts_with("quien_vid_conductor_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_desconodico                = rowSums(d_sec_ix %>% select(starts_with("quien_vid_desconodico_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_quien_otro                       = rowSums(d_sec_ix %>% select(starts_with("quien_vid_otro_p9_2")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_fisica                          = rowSums(d_sec_ix %>% select(paste0("freq_ahora_p9_3_",c(4,6,12,11))), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_emocional                       = rowSums(d_sec_ix %>% select(paste0("freq_ahora_p9_3_",c(2,3,8,15))), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_sexual                          = rowSums(d_sec_ix %>% select(paste0("freq_ahora_p9_3_",c(1,5,7,9,10,13,14,16))), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_conocido                  = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_conocido_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_amigo                     = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_amigo_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_vecino                    = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_vecino_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_policia                   = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_policia_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_militar                   = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_militar_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_sacerdote                 = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_sacerdote_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_conductor                 = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_conductor_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_desconodico               = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_desconodico_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_quien_otro                      = rowSums(d_sec_ix %>% select(starts_with("quien_ahora_otro_p9_4")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_calle               = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_calle_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_mercado             = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_mercado_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_autobús             = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_autobús_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_metro               = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_metro_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_metrobus            = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_metrobus_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_taxi                = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_taxi_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_iglesia             = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_iglesia_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_cantina             = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_cantina_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_feria               = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_feria_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_particular          = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_particular_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_ahora_donde_otro                = rowSums(d_sec_ix %>% select(starts_with("donde_ahora_otro_p9_5")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_tipo_apoyo_orientacion                = rowSums(d_sec_ix %>% select(starts_with("apoyo_inst_orientacion_p9_10")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_tipo_apoyo_legal                      = rowSums(d_sec_ix %>% select(starts_with("apoyo_inst_legal_p9_10")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_tipo_apoyo_psicologico                = rowSums(d_sec_ix %>% select(starts_with("apoyo_inst_psicologico_p9_10")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_tipo_apoyo_medica                     = rowSums(d_sec_ix %>% select(starts_with("apoyo_inst_medica_p9_10")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_tipo_apoyo_Otro                       = rowSums(d_sec_ix %>% select(starts_with("apoyo_inst_Otro_p9_10")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_castigaron       = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_castigaron_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_consignaAnteJuez = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_consignaAnteJuez_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_noRatifico       = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_noRatifico_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_nocastigaron     = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_nocastigaron_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_nada             = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_nada_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_denuncia_que_sucedio_nosabe           = d_sec_ix$v_vio_comunitario_denuncia & rowSums(d_sec_ix %>% select(starts_with("que_sucedio_nosabe_p9_19")), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_fisica                           = d_sec_ix$v_vio_comunitario_ahora_fisica | rowSums(d_sec_ix %>% select(paste0("mod_p9_1_",c(4,6,12))), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_emocional                        = d_sec_ix$v_vio_comunitario_ahora_emocional | rowSums(d_sec_ix %>% select(paste0("mod_p9_1_",c(2,3,8,15))), na.rm = T)>0
d_sec_ix$v_vio_comunitario_vida_sexual                           = d_sec_ix$v_vio_comunitario_ahora_sexual | rowSums(d_sec_ix %>% select(paste0("mod_p9_1_",c(1,5,7,9,10,11,13,14,16))), na.rm = T)>0

d_sec_ix <- d_sec_ix %>% 
  mutate(
    v_vio_comunitario_ahora_todas                           = v_vio_comunitario_ahora_fisica|v_vio_comunitario_ahora_emocional|v_vio_comunitario_ahora_sexual,
    v_vio_comunitario_vida_todas                            = v_vio_comunitario_vida_fisica|v_vio_comunitario_vida_emocional|v_vio_comunitario_vida_sexual,
    
  ) %>% 
  ungroup() %>% 
  mutate(
    v_vio_comunitario_institucion_INM                 = case_when(p9_9_1  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_linea               = case_when(p9_9_2  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_ac                  = case_when(p9_9_3  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_justicia_mujeres    = case_when(p9_9_4  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_defensoria          = case_when(p9_9_5  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_clinica_publica     = case_when(p9_9_6  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_clinica_privada     = case_when(p9_9_7  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_dif                 = case_when(p9_9_8  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_otro                = case_when(p9_9_9  == "1" ~ T,T ~ F),
    
    v_vio_comunitario_institucion_trato_INM                 =p9_14_1 ,
    v_vio_comunitario_institucion_trato_linea               =p9_14_2 ,
    v_vio_comunitario_institucion_trato_ac                  =p9_14_3 ,
    v_vio_comunitario_institucion_trato_justicia_mujeres    =p9_14_4 ,
    v_vio_comunitario_institucion_trato_defensoria          =p9_14_5 ,
    v_vio_comunitario_institucion_trato_clinica_publica     =p9_14_6 ,
    v_vio_comunitario_institucion_trato_clinica_privada     =p9_14_7 ,
    v_vio_comunitario_institucion_trato_dif                 =p9_14_8 ,
    v_vio_comunitario_institucion_trato_otro                =p9_14_9 ,
    
    v_vio_comunitario_denuncia_en_policia             = case_when(p9_15_1 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_mp                  = case_when(p9_15_2 == "1" ~T, T ~ F),     
    v_vio_comunitario_denuncia_en_municipio           = case_when(p9_15_3 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_autoridadesLocales  = case_when(p9_15_4 == "1" ~T, T ~ F),
    
    v_vio_comunitario_denuncia_que_en_policia             = paste(p9_19_1_1,p9_19_1_2,p9_19_1_3,sep=","),
    v_vio_comunitario_denuncia_que_en_mp                  = paste(p9_19_2_1,p9_19_2_2,p9_19_2_3,sep=","),
    v_vio_comunitario_denuncia_que_en_municipio           = paste(p9_19_3_1,p9_19_3_2,p9_19_3_3,sep=","),
    v_vio_comunitario_denuncia_que_en_autoridadesLocales  = paste(p9_19_4_1,p9_19_4_2,p9_19_4_3,sep=","),
    
    v_vio_comunitario_no_denuncia_en_verguenza        = case_when(p9_20_1  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_noLeIbanACreer   = case_when(p9_20_2  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_miedo            = case_when(p9_20_3  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_familia          = case_when(p9_20_4  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_convencieron     = case_when(p9_20_5  == "1" ~T, T ~ F), 
    v_vio_comunitario_no_denuncia_en_sinImportancia   = case_when(p9_20_6  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_costumbres       = case_when(p9_20_7  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_NoSabía          = case_when(p9_20_8  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_tiempo           = case_when(p9_20_9  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_desconfianza     = case_when(p9_20_10 == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_otra             = case_when(p9_20_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
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
  paste_inp("bd_endireh_2021_dbf/TB_SEC_XI.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  #rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_endireh_2021_dbf/TB_SEC_XI_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names()# %>% rename(n_ren = ren_m_ele)
  ) %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_familiar_dummy = case_when(
      p11_1_3 < 4 ~ "1",
      T ~ "2"
    )
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p11_1_")),
    .fns =  list("mod" = ~ case_when(as.numeric(.) < 4 ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p11_2_")),
      .fns =  list("quien_padre"       = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_madre"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_padrastro"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_abuelo"      = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_hijo"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_hermano"     = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_tio"         = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_primo"       = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_suegro"      = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_cuñado"      = ~ case_when(. == "9" ~ T,T ~ F),
                   "quien_sobrino"     = ~ case_when(. == "10" ~ T,T ~ F),
                   "quien_yerno"       = ~ case_when(. == "11" ~ T,T ~ F),
                   "quien_otro"        = ~ case_when(. == "11" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p11_3_")),
      .fns =  list("donde_casa_propia"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_casa_familiar" = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_calle"         = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_mercado"       = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_lugar_publico" = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_otro"          = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_familiar_apoyo    = case_when(p11_6_1 == "1" ~ T,T ~ F),
    v_vio_familiar_denuncia = case_when(p11_6_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p11_8_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p11_15_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nocastigaron"            = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    
  )


d_sec_xi$v_vio_familiar_fisica                           = rowSums(d_sec_xi %>% select(paste0("mod_p11_1_",c(5,10,11,17))), na.rm = T)>0
d_sec_xi$v_vio_familiar_emocional                        = rowSums(d_sec_xi %>% select(paste0("mod_p11_1_",c(1,7,12,20))), na.rm = T)>0
d_sec_xi$v_vio_familiar_sexual                           = rowSums(d_sec_xi %>% select(paste0("mod_p11_1_",c(2,3,4,13,18,19))), na.rm = T)>0
d_sec_xi$v_vio_familiar_patrimonial                      = rowSums(d_sec_xi %>% select(paste0("mod_p11_1_",c(6,8,9,14,15,16))), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_conocido                   = rowSums(d_sec_xi %>% select(starts_with("quien_conocido_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_amigo                      = rowSums(d_sec_xi %>% select(starts_with("quien_amigo_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_vecino                     = rowSums(d_sec_xi %>% select(starts_with("quien_vecino_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_policia                    = rowSums(d_sec_xi %>% select(starts_with("quien_policia_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_militar                    = rowSums(d_sec_xi %>% select(starts_with("quien_militar_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_sacerdote                  = rowSums(d_sec_xi %>% select(starts_with("quien_sacerdote_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_conductor                  = rowSums(d_sec_xi %>% select(starts_with("quien_conductor_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_desconodico                = rowSums(d_sec_xi %>% select(starts_with("quien_desconodico_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_quien_otro                       = rowSums(d_sec_xi %>% select(starts_with("quien_otro_p11_2")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_casa_propia                = rowSums(d_sec_xi %>% select(starts_with("donde_casa_propia_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_casa_familiar              = rowSums(d_sec_xi %>% select(starts_with("donde_casa_familiar_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_calle                      = rowSums(d_sec_xi %>% select(starts_with("donde_calle_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_mercado                    = rowSums(d_sec_xi %>% select(starts_with("donde_mercado_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_lugar_publico              = rowSums(d_sec_xi %>% select(starts_with("donde_lugar_publico_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_donde_otro                       = rowSums(d_sec_xi %>% select(starts_with("donde_otro_p11_3")), na.rm = T)>0
d_sec_xi$v_vio_familiar_tipo_apoyo_orientacion                = rowSums(d_sec_xi %>% select(starts_with("apoyo_inst_orientacion_p11_8")), na.rm = T)>0
d_sec_xi$v_vio_familiar_tipo_apoyo_legal                      = rowSums(d_sec_xi %>% select(starts_with("apoyo_inst_legal_p11_8")), na.rm = T)>0
d_sec_xi$v_vio_familiar_tipo_apoyo_psicologico                = rowSums(d_sec_xi %>% select(starts_with("apoyo_inst_psicologico_p11_8")), na.rm = T)>0
d_sec_xi$v_vio_familiar_tipo_apoyo_medica                     = rowSums(d_sec_xi %>% select(starts_with("apoyo_inst_medica_p11_8")), na.rm = T)>0
d_sec_xi$v_vio_familiar_tipo_apoyo_Otro                       = rowSums(d_sec_xi %>% select(starts_with("apoyo_inst_Otro_p11_8")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_castigaron       = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_castigaron_p11_15")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_nocastigaron     = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_nocastigaron_p11_15")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_consignaAnteJuez = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_consignaAnteJuez_p11_15")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_noRatifico       = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_noRatifico_p11_15")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_nada             = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_nada_p11_15")), na.rm = T)>0
d_sec_xi$v_vio_familiar_denuncia_que_sucedio_nosabe           = d_sec_xi$v_vio_familiar_denuncia & rowSums(d_sec_xi %>% select(starts_with("que_sucedio_nosabe_p11_15")), na.rm = T)>0

d_sec_xi <- d_sec_xi %>% 
  mutate(
    v_vio_familiar_todas                            = v_vio_familiar_fisica|v_vio_familiar_emocional|v_vio_familiar_sexual|v_vio_familiar_patrimonial,
    v_vio_familiar_institucion_INM                 = case_when(p11_7_1  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_linea               = case_when(p11_7_2  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_ac                  = case_when(p11_7_3  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_cavi                = case_when(p11_7_4  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_justicia_mujeres    = case_when(p11_7_5  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_defensoria          = case_when(p11_7_6  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_clinica_publica     = case_when(p11_7_7  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_clinica_privada     = case_when(p11_7_8  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_dif                 = case_when(p11_7_9  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_otro                = case_when(p11_7_10  == "1" ~ T,T ~ F),
    
    v_vio_familiar_institucion_trato_INM                 = p11_11_1 ,
    v_vio_familiar_institucion_trato_linea               = p11_11_2 ,
    v_vio_familiar_institucion_trato_ac                  = p11_11_3 ,
    v_vio_familiar_institucion_trato_cavi                = p11_11_4 ,
    v_vio_familiar_institucion_trato_justicia_mujeres    = p11_11_5 ,
    v_vio_familiar_institucion_trato_defensoria          = p11_11_6 ,
    v_vio_familiar_institucion_trato_clinica_publica     = p11_11_7 ,
    v_vio_familiar_institucion_trato_clinica_privada     = p11_11_8 ,
    v_vio_familiar_institucion_trato_dif                 = p11_11_9 ,
    v_vio_familiar_institucion_trato_otro                = p11_11_10,
    
    v_vio_familiar_denuncia_en_policia             = case_when(p11_12_1 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_mp                  = case_when(p11_12_2 == "1" ~T, T ~ F),   
    v_vio_familiar_denuncia_en_municipio           = case_when(p11_12_3 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_autoridadesLocales  = case_when(p11_12_4 == "1" ~T, T ~ F),   
    
    v_vio_familiar_denuncia_que_en_policia             = paste(p11_15_1_1,p11_15_1_2,p11_15_1_3,sep=","),
    v_vio_familiar_denuncia_que_en_mp                  = paste(p11_15_2_1,p11_15_2_2,p11_15_2_3,sep=","),   
    v_vio_familiar_denuncia_que_en_municipio           = paste(p11_15_3_1,p11_15_3_2,p11_15_3_3,sep=","),
    v_vio_familiar_denuncia_que_en_autoridadesLocales  = paste(p11_15_4_1,p11_15_4_2,p11_15_4_3,sep=","),   
    
    v_vio_familiar_no_denuncia_en_verguenza        = case_when(p11_16_1  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_noLeIbanACreer   = case_when(p11_16_2  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_miedo            = case_when(p11_16_3  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_familia          = case_when(p11_16_4  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_convencieron     = case_when(p11_16_5  == "1" ~T, T ~ F), 
    v_vio_familiar_no_denuncia_en_sinImportancia   = case_when(p11_16_6  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_costumbres       = case_when(p11_16_7  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_NoSabía          = case_when(p11_16_8  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_tiempo           = case_when(p11_16_9  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_desconfianza     = case_when(p11_16_10 == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_otra             = case_when(p11_16_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
### TSEC_XII - Familia de origen (infancia) ----
d_sec_xii <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_XII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  #rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_infancia_dummy = case_when(
      p12_14_5 == "1" ~ "1",
      p12_14_6 == "1" ~ "1",
      T ~ "2"
    )
  ) %>%
  #head(50) %>% 
  
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p12_14_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p12_15_")),
      .fns =  list("quien_padre"       = ~ case_when(. == "1" ~ T,T ~ F),
                   "quien_madre"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "quien_padrastro"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "quien_abuelo"      = ~ case_when(. == "4" ~ T,T ~ F),
                   "quien_hermano"     = ~ case_when(. == "5" ~ T,T ~ F),
                   "quien_tio"         = ~ case_when(. == "6" ~ T,T ~ F),
                   "quien_primo"       = ~ case_when(. == "7" ~ T,T ~ F),
                   "quien_familiar"    = ~ case_when(. == "8" ~ T,T ~ F),
                   "quien_nofamiliar"  = ~ case_when(. == "9" ~ T,T ~ F),
                   "quien_desconocido" = ~ case_when(. == "10" ~ T,T ~ F),
                   "quien_otro"        = ~ case_when(. == "11" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  ) 
d_sec_xii$v_vio_familiaOrigen_Osexual                           = rowSums(d_sec_xii %>% select(paste0("mod_p12_14_",c(1:6))), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_padre                       = rowSums(d_sec_xii %>% select(starts_with("quien_padre_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_madre                       = rowSums(d_sec_xii %>% select(starts_with("quien_madre_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_padrastro                   = rowSums(d_sec_xii %>% select(starts_with("quien_padrastro_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_abuelo                      = rowSums(d_sec_xii %>% select(starts_with("quien_abuelo_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_hermano                     = rowSums(d_sec_xii %>% select(starts_with("quien_hermano_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_tio                         = rowSums(d_sec_xii %>% select(starts_with("quien_tio_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_primo                       = rowSums(d_sec_xii %>% select(starts_with("quien_primo_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_familiar                    = rowSums(d_sec_xii %>% select(starts_with("quien_familiar_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_nofamiliar                  = rowSums(d_sec_xii %>% select(starts_with("quien_nofamiliar_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_desconocido                 = rowSums(d_sec_xii %>% select(starts_with("quien_desconocido_p12_15_")), na.rm = T)>0
d_sec_xii$v_vio_familiaOrigen_otro                        = rowSums(d_sec_xii %>% select(starts_with("quien_otro_p12_15_")), na.rm = T)>0

d_sec_xii <- d_sec_xii %>% 
  mutate(
    v_vio_familiaOrigen_Otodas                            = v_vio_familiaOrigen_Osexual,
  ) %>% 
  ungroup() %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()


### TSEC_XIV- Relación actual o última relación ----
d_sec_xiv <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_XIV.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  #rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_endireh_2021_dbf/TB_SEC_XIV_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() #%>% rename(n_ren = ren_m_ele)
  ) %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_pareja_expareja_dummy = case_when(
      p14_1_25 < 4 ~ "1",
      p14_1_26 < 4 ~ "1",
      p14_1_27 < 4 ~ "1",
      p14_1_29 < 4 ~ "1",
      T ~ "2"
    )
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p14_1_")),
    .fns =  list("mod" = ~ case_when(
      is.na(.) ~ F,
      as.numeric(.)<4 ~ T,
      T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p14_2_")),
      .fns =  list("gravedad_muy_grave"       = ~ case_when(. == "1" ~ T,T ~ F),
                   "gravedad_grave"           = ~ case_when(. == "2" ~ T,T ~ F),
                   "gravedad_sinimportancia"  = ~ case_when(. == "3" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p14_3_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    v_vio_pareja_apoyo = case_when(p14_7_1 == "1" ~ T,T ~ F),
    v_vio_pareja_denuncia = case_when(p14_7_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p14_18_")),
      .fns =  list("que_sucedio_desalojo"         = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_detencion"        = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez" = ~ case_when(. == "7" ~ T,T ~ F),
                   "que_sucedio_ordenproteccion"  = ~ case_when(. == "6" ~ T,T ~ F),
                   "que_sucedio_acuerdo"          = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_noProcedio"       = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_noRatifico"       = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_nosabe"           = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p14_9_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  ) 


d_sec_xiv$v_vio_pareja_ahora_fisica                          = rowSums(d_sec_xiv %>% select(starts_with(paste0("freq_ahora_p14_3_",c(1:9,15,18,19)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_ahora_emocional                       = rowSums(d_sec_xiv %>% select(starts_with(paste0("freq_ahora_p14_3_",c(31, 10,11,12,13,14,16,17,21,22,23,24)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_ahora_sexual                          = rowSums(d_sec_xiv %>% select(starts_with(paste0("freq_ahora_p14_3_",c(30, 25,26,27,28,29)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_ahora_patrimonial                     = rowSums(d_sec_xiv %>% select(starts_with(paste0("freq_ahora_p14_3_",c(20,32,33,34,35,36,37,38)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_gravedad_muy_grave                    = rowSums(d_sec_xiv %>% select(starts_with("gravedad_muy_grave_p14_2")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_gravedad_grave                        = rowSums(d_sec_xiv %>% select(starts_with("gravedad_grave_p14_2")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_gravedad_sinimportancia               = rowSums(d_sec_xiv %>% select(starts_with("gravedad_sinimportancia_p14_2")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_tipo_apoyo_orientacion                = rowSums(d_sec_xiv %>% select(starts_with("apoyo_inst_orientacion_p14_9_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_tipo_apoyo_legal                      = rowSums(d_sec_xiv %>% select(starts_with("apoyo_inst_legal_p14_9_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_tipo_apoyo_psicologico                = rowSums(d_sec_xiv %>% select(starts_with("apoyo_inst_psicologico_p14_9_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_tipo_apoyo_medica                     = rowSums(d_sec_xiv %>% select(starts_with("apoyo_inst_medica_p14_9_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_tipo_apoyo_Otro                       = rowSums(d_sec_xiv %>% select(starts_with("apoyo_inst_Otro_p14_9_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_desalojo         = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_desalojo_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_detencion        = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_detencion_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_consignaAnteJuez = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_consignaAnteJuez_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_ordenproteccion  = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_ordenproteccion_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_acuerdo          = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_acuerdo_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_noProcedio       = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_noProcedio_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_noRatifico       = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_noRatifico_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_denuncia_que_sucedio_nosabe           = d_sec_xiv$v_vio_pareja_denuncia & rowSums(d_sec_xiv %>% select(starts_with("que_sucedio_nosabe_p14_18_")), na.rm = T)>0
d_sec_xiv$v_vio_pareja_vida_fisica                           = d_sec_xiv$v_vio_pareja_ahora_fisica | rowSums(d_sec_xiv %>% select(starts_with(paste0("mod_p14_1_",c(1:9,15,18,19)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_vida_emocional                        = d_sec_xiv$v_vio_pareja_ahora_emocional | rowSums(d_sec_xiv %>% select(starts_with(paste0("mod_p14_1_",c(31, 10,11,12,13,14,16,17,21,22,23,24)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_vida_sexual                           = d_sec_xiv$v_vio_pareja_ahora_sexual | rowSums(d_sec_xiv %>% select(starts_with(paste0("mod_p14_1_",c(30, 25,26,27,28,29)))), na.rm = T)>0
d_sec_xiv$v_vio_pareja_vida_patrimonial                      = d_sec_xiv$v_vio_pareja_ahora_patrimonial | rowSums(d_sec_xiv %>% select(starts_with(paste0("mod_p14_1_",c(20,32,33,34,35,36,37,38)))), na.rm = T)>0

d_sec_xiv <- d_sec_xiv %>% 
  mutate(
    v_vio_pareja_ahora_todas                           = v_vio_pareja_ahora_fisica|v_vio_pareja_ahora_emocional|v_vio_pareja_ahora_sexual|v_vio_pareja_ahora_patrimonial,
    v_vio_pareja_vida_todas                            = v_vio_pareja_vida_fisica |v_vio_pareja_vida_emocional|v_vio_pareja_vida_sexual|v_vio_pareja_vida_patrimonial,
    
    v_vio_pareja_institucion_INM                 = case_when(p14_8_1  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_linea               = case_when(p14_8_2  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_ac                  = case_when(p14_8_3  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_cavi                = case_when(p14_8_4  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_justicia_mujeres    = case_when(p14_8_5  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_defensoria          = case_when(p14_8_6  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_clinica_publica     = case_when(p14_8_7  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_clinica_privada     = case_when(p14_8_8  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_dif                 = case_when(p14_8_9  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_otro                = case_when(p14_8_10  == "1" ~ T,T ~ F),
    
    v_vio_pareja_institucion_trato_INM                 =p14_13_1 ,
    v_vio_pareja_institucion_trato_linea               =p14_13_2 ,
    v_vio_pareja_institucion_trato_ac                  =p14_13_3 ,
    v_vio_pareja_institucion_trato_cavi                =p14_13_4 ,
    v_vio_pareja_institucion_trato_justicia_mujeres    =p14_13_5 ,
    v_vio_pareja_institucion_trato_defensoria          =p14_13_6 ,
    v_vio_pareja_institucion_trato_clinica_publica     =p14_13_7 ,
    v_vio_pareja_institucion_trato_clinica_privada     =p14_13_8 ,
    v_vio_pareja_institucion_trato_dif                 =p14_13_9 ,
    v_vio_pareja_institucion_trato_otro                =p14_13_10,
    
    v_vio_pareja_denuncia_en_policia             = case_when(p14_14_1 == "1" ~T, T ~ F),     
    v_vio_pareja_denuncia_en_municipio           = case_when(p14_14_2 == "1" ~T, T ~ F),
    v_vio_pareja_denuncia_en_autoridadesLocales  = case_when(p14_14_3 == "1" ~T, T ~ F),
    v_vio_pareja_denuncia_en_mp                  = case_when(p14_14_4 == "1" ~T, T ~ F),    
    
    v_vio_pareja_denuncia_que_en_policia             = paste(p14_18_1_1,p14_18_1_2,p14_18_1_3,sep=","),     
    v_vio_pareja_denuncia_que_en_municipio           = paste(p14_18_2_1,p14_18_2_2,p14_18_2_3,sep=","),
    v_vio_pareja_denuncia_que_en_autoridadesLocales  = paste(p14_18_3_1,p14_18_3_2,p14_18_3_3,sep=","),
    v_vio_pareja_denuncia_que_en_mp                  = paste(p14_18_4_1,p14_18_4_2,p14_18_4_3,sep=","),    
    
    v_vio_pareja_no_denuncia_en_verguenza        = case_when(p14_14_1  == "1" ~T, T ~ F),     
    v_vio_pareja_no_denuncia_en_noLeIbanACreer   = case_when(p14_22_2  == "1" ~T, T ~ F),
    v_vio_pareja_no_denuncia_en_miedo            = case_when(p14_22_3  == "1" ~T, T ~ F),
    v_vio_pareja_no_denuncia_en_familia          = case_when(p14_22_4  == "1" ~T, T ~ F),     
    v_vio_pareja_no_denuncia_en_convencieron     = case_when(p14_22_5  == "1" ~T, T ~ F), 
    v_vio_pareja_no_denuncia_en_sinImportancia   = case_when(p14_22_6  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_costumbres       = case_when(p14_22_7  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_NoSabía          = case_when(p14_22_8  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_tiempo           = case_when(p14_22_9  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_desconfianza     = case_when(p14_22_10 == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_otra             = case_when(p14_22_11 == "1" ~T, T ~ F),  
    v_vio_pareja_resultado_denuncia_relacion     = case_when(
      p14_20ab == "1" ~"Separacion Temporal",
      p14_20ab == "2" ~"Separacion (Ella)",
      p14_20ab == "3" ~"Separacion (El)",
      p14_20ab == "4" ~"No Separacion",
      T ~ NA_character_),  
  ) %>%    
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
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
  left_join(d_sec_xix) %>% 
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
  
  d_endireh_vob$v_vio_tipo_apoyo_orientacion          = rowSums(d_endireh_vob %>% select(ends_with("apoyo_orientacion")),na.rm = T)>0
  d_endireh_vob$v_vio_tipo_apoyo_legal                = rowSums(d_endireh_vob %>% select(ends_with("apoyo_legal")),na.rm = T)>0
  d_endireh_vob$v_vio_tipo_apoyo_psicologico          = rowSums(d_endireh_vob %>% select(ends_with("apoyo_psicologico")),na.rm = T)>0
  d_endireh_vob$v_vio_tipo_apoyo_medica               = rowSums(d_endireh_vob %>% select(ends_with("apoyo_medica")),na.rm = T)>0
  d_endireh_vob$v_vio_tipo_apoyo_Otro                 = rowSums(d_endireh_vob %>% select(ends_with("apoyo_Otro")),na.rm = T)>0
  d_endireh_vob$v_vio_institucion_INM                 = rowSums(d_endireh_vob %>% select(ends_with("on_INM")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_INM_estatal         = rowSums(d_endireh_vob %>% select(ends_with("on_INM_estatal")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_linea               = rowSums(d_endireh_vob %>% select(ends_with("on_linea")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_ac                  = rowSums(d_endireh_vob %>% select(ends_with("on_ac")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_justicia_mujeres    = rowSums(d_endireh_vob %>% select(ends_with("on_justicia_mujeres")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_defensoria          = rowSums(d_endireh_vob %>% select(ends_with("on_defensoria")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_clinica_publica     = rowSums(d_endireh_vob %>% select(ends_with("on_clinica_publica")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_clinica_privada     = rowSums(d_endireh_vob %>% select(ends_with("on_clinica_privada")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_dif                 = rowSums(d_endireh_vob %>% select(ends_with("on_dif")), na.rm = T)>0
  d_endireh_vob$v_vio_institucion_otro                = rowSums(d_endireh_vob %>% select(ends_with("on_otro")), na.rm = T)>0
  d_endireh_vob$v_vio_ahora_emocional                 = rowSums(d_endireh_vob %>% select(ends_with("ahora_emocional")),na.rm = T)>0
  d_endireh_vob$v_vio_ahora_fisica                    = rowSums(d_endireh_vob %>% select(ends_with("ahora_fisica")),na.rm = T)>0
  d_endireh_vob$v_vio_ahora_sexual                    = rowSums(d_endireh_vob %>% select(ends_with("ahora_sexual")),na.rm = T)>0
  d_endireh_vob$v_vio_ahora_patrimonial               = rowSums(d_endireh_vob %>% select(ends_with("ahora_patrimonial")),na.rm = T)>0
  d_endireh_vob$v_vio_ahora_discriminacion            = rowSums(d_endireh_vob %>% select(ends_with("ahora_discriminacion")),na.rm = T)>0
  d_endireh_vob$v_vio_vida_emocional                  = rowSums(d_endireh_vob %>% select(ends_with("_emocional")),na.rm = T)>0
  d_endireh_vob$v_vio_vida_fisica                     = rowSums(d_endireh_vob %>% select(ends_with("_fisica")),na.rm = T)>0
  d_endireh_vob$v_vio_vida_sexual                     = rowSums(d_endireh_vob %>% select(ends_with("_sexual")),na.rm = T)>0
  d_endireh_vob$v_vio_vida_patrimonial                = rowSums(d_endireh_vob %>% select(ends_with("_patrimonial")),na.rm = T)>0
  d_endireh_vob$v_vio_vida_discriminacion             = rowSums(d_endireh_vob %>% select(ends_with("_discriminacion")),na.rm = T)>0
  d_endireh_vob$v_vio_denuncia_en_policia             = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_policia")))>0
  d_endireh_vob$v_vio_denuncia_en_municipio           = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_municipio")))>0
  d_endireh_vob$v_vio_denuncia_en_mp                  = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_mp")))>0
  d_endireh_vob$v_vio_denuncia_en_escuela             = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_escuela")))>0
  d_endireh_vob$v_vio_denuncia_en_sindicato           = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_sindicato")))>0
  d_endireh_vob$v_vio_denuncia_en_procuraduria        = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_procuraduria")))>0
  d_endireh_vob$v_vio_denuncia_en_autoridadesLocales  = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_autoridadesLocales")))>0
  
  d_endireh_vob <- d_endireh_vob %>% 
    mutate(
      v_vio_ahora_todas = v_vio_ahora_emocional | v_vio_ahora_fisica|v_vio_ahora_sexual|v_vio_ahora_patrimonial|v_vio_ahora_discriminacion,
      v_vio_vida_todas = v_vio_vida_emocional|v_vio_vida_fisica|v_vio_vida_sexual|v_vio_vida_patrimonial|v_vio_vida_discriminacion,
    )%>% 
    glimpse()

saveRDS(d_endireh_vob, paste_out("01_endireh_2021_vob_no_filter_lolo.rds"))
