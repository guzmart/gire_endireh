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
v_carpeta <- "bd_sd_endireh2016_sitioinegi_dbf/"
v_carpeta_2 <- "bd_mujeres_endireh2016_sitioinegi_dbf/"
v_anio <- 2016
### TSDEM - Sociodemográficos ----
d_sdem <- read.dbf(paste_inp(paste0(v_carpeta, "TSDem.DBF")), as.is = T) %>% 
  janitor::clean_names() %>% 
  mutate(
    anio = v_anio,
    llave = paste0(upm, viv_sel, hogar, n_ren),
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
d_sec_iv<- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_IV.dbf")), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = v_anio, 
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
    starts_with("cruce_ingreso_otro_"), cruce_cuenta_dinero_utilizar_como_quiera_dummy = p4_11,
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  glimpse

### TSEC_VI - Ámbito escolar ----
d_sec_vi <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VI.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VI_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>%
  #head(50) %>%
  
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_vio_sex_escuela_dummy = p6_6_15,
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    ),
  ) %>% 
  mutate(across(
    c(starts_with("p6_6_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p6_7_")),
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
      c(starts_with("p6_8_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p6_9_")),
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
      c(starts_with("p6_10_")),
      .fns =  list("donde_ahora_escuela"           = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_ahora_cerca"             = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_ahora_lejos"             = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_ahora_transportepúblico" = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_ahora_particular"        = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_ahora_Otro"              = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_escuela_apoyo = case_when(p6_13_1 == "1" ~ T,T ~ F),
    v_vio_escuela_denuncia = case_when(p6_13_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p6_23_")),
      .fns =  list("que_sucedio_castigaron"         = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez" = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_recomendacion"      = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_noRatifico"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nada"               = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_nosabe"             = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p6_15_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  )

d_sec_vi$v_vio_escuela_vida_quien_Maestro     = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Maestro_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Maestra     = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Maestra_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Compañero   = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Compañero_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Compañera   = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Compañera_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Directore   = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Directore_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Trabajador  = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Trabajador_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Trabajadora = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Trabajadora_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_desconocido = rowSums(d_sec_vi %>% select(starts_with("quien_vid_desconocido_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_quien_Otra        = rowSums(d_sec_vi %>% select(starts_with("quien_vid_Otra_p6_7_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_fisica    = rowSums(d_sec_vi %>% select(paste0("freq_ahora_p6_8_",c(1,2,6))),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_emocional = rowSums(d_sec_vi %>% select(paste0("freq_ahora_p6_8_",c(4,9,13,16))),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_sexual    = rowSums(d_sec_vi %>% select(paste0("freq_ahora_p6_8_",c(3,5,7,8,10,11,12,14,15,17))),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Maestro      = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Maestro_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Maestra      = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Maestra_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Compañero    = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Compañero_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Compañera    = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Compañera_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Directore    = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Directore_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Trabajador   = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Trabajador_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Trabajadora  = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Trabajadora_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_desconocido  = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_desconocido_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_quien_Otra         = rowSums(d_sec_vi %>% select(starts_with("quien_ahora_Otra_p6_9_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_escuela             = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_escuela_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_cerca               = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_cerca_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_lejos               = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_lejos_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_transportepúblico   = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_transportepúblico_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_particular          = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_particular_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_ahora_donde_Otro                = rowSums(d_sec_vi %>% select(starts_with("donde_ahora_Otro_p6_10_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_tipo_apoyo_orientacion  = rowSums(d_sec_vi %>% select(starts_with("apoyo_inst_orientacion_p6_15_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_tipo_apoyo_legal        = rowSums(d_sec_vi %>% select(starts_with("apoyo_inst_legal_p6_15_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_tipo_apoyo_psicologico  = rowSums(d_sec_vi %>% select(starts_with("apoyo_inst_psicologico_p6_15_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_tipo_apoyo_medica       = rowSums(d_sec_vi %>% select(starts_with("apoyo_inst_medica_p6_15_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_tipo_apoyo_Otro         = rowSums(d_sec_vi %>% select(starts_with("apoyo_inst_Otro_p6_15_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_castigaron        = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_castigaron_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_consignaAnteJuez  = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_consignaAnteJuez_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_recomendacion     = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_recomendacion_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_noRatifico        = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_noRatifico_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_nada              = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_nada_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_denuncia_o_nosabe            = d_sec_vi$v_vio_escuela_denuncia & rowSums(d_sec_vi %>% select(starts_with("que_sucedio_nosabe_p6_23_")),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_fisica     = d_sec_vi$v_vio_escuela_ahora_fisica | rowSums(d_sec_vi %>% select(paste0("mod_p6_6_",c(1,2,6))),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_emocional  = d_sec_vi$v_vio_escuela_ahora_emocional | rowSums(d_sec_vi %>% select(paste0("mod_p6_6_",c(4,9,13,16))),na.rm = T)>0
d_sec_vi$v_vio_escuela_vida_sexual     = d_sec_vi$v_vio_escuela_ahora_sexual | rowSums(d_sec_vi %>% select(paste0("mod_p6_6_",c(3,5,7,8,10,11,12,14,15,17))),na.rm = T)>0

d_sec_vi <- d_sec_vi %>% 
  mutate(
    v_vio_escuela_ahora_todas    = v_vio_escuela_ahora_fisica|v_vio_escuela_ahora_emocional|v_vio_escuela_ahora_sexual,
    v_vio_escuela_vida_todas     = v_vio_escuela_ahora_todas | v_vio_escuela_vida_fisica|v_vio_escuela_vida_emocional|v_vio_escuela_vida_sexual,
    v_vio_escuela_institucion_INM              = case_when(p6_14_1 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_INM_estatal      = case_when(p6_14_2 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_linea            = case_when(p6_14_3 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_ac               = case_when(p6_14_4 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_justicia_mujeres = case_when(p6_14_5 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_defensoria       = case_when(p6_14_6 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_clinica_publica  = case_when(p6_14_7 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_clinica_privada  = case_when(p6_14_8 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_dif              = case_when(p6_14_9 == "1" ~ T,T ~ F),
    v_vio_escuela_institucion_otro             = case_when(p6_14_10 == "1" ~ T,T ~ F),
    
    v_vio_escuela_institucion_trato_INM              = p6_18_1 ,
    v_vio_escuela_institucion_trato_INM_estatal      = p6_18_2 ,
    v_vio_escuela_institucion_trato_linea            = p6_18_3 ,
    v_vio_escuela_institucion_trato_ac               = p6_18_4 ,
    v_vio_escuela_institucion_trato_justicia_mujeres = p6_18_5 ,
    v_vio_escuela_institucion_trato_defensoria       = p6_18_6 ,
    v_vio_escuela_institucion_trato_clinica_publica  = p6_18_7 ,
    v_vio_escuela_institucion_trato_clinica_privada  = p6_18_8 ,
    v_vio_escuela_institucion_trato_dif              = p6_18_9 ,
    v_vio_escuela_institucion_trato_otro             = p6_18_10,
    
    v_vio_escuela_denuncia_en_escuela          = case_when(p6_19_1 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_mp               = case_when(p6_19_2 == "1" ~T, T ~ F),
    v_vio_escuela_denuncia_en_policia          = case_when(p6_19_3 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_municipio        = case_when(p6_19_4 == "1" ~T, T ~ F),   
    
    v_vio_escuela_denuncia_que_en_escuela          = paste(p6_23_1_1,p6_23_1_2,p6_23_1_3, sep = ","),     
    v_vio_escuela_denuncia_que_en_mp               = paste(p6_23_2_1,p6_23_2_2,p6_23_2_3, sep = ","),
    v_vio_escuela_denuncia_que_en_policia          = paste(p6_23_3_1,p6_23_3_2,p6_23_3_3, sep = ","),     
    v_vio_escuela_denuncia_que_en_municipio        = paste(p6_23_4_1,p6_23_4_2,p6_23_4_3, sep = ","),    
    
    v_vio_escuela_no_denuncia_en_verguenza        = case_when(p6_24_1  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_noLeIbanACreer   = case_when(p6_24_2  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_miedo            = case_when(p6_24_3  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_familia          = case_when(p6_24_4  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_convencieron     = case_when(p6_24_5  == "1" ~T, T ~ F), 
    v_vio_escuela_no_denuncia_en_sinImportancia   = case_when(p6_24_6  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_costumbres       = case_when(p6_24_7  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_NoSabía          = case_when(p6_24_8  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_tiempo           = case_when(p6_24_9  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_desconfianza     = case_when(p6_24_10 == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_otra             = case_when(p6_24_11 == "1" ~T, T ~ F),  
  ) %>%    
  select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

### TSEC_VII - Ámbito laboral ----
d_sec_vii <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  as_tibble() %>% 
  rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>% 
  #head(50) %>%
  
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_trabajo_dummy = p7_9_13
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p7_8_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_9_")),
      .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_10_")),
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
      c(starts_with("p7_11_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_12_")),
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
      c(starts_with("p7_13_")),
      .fns =  list("donde_ahora_instalaciones"     = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_ahora_cerca"             = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_ahora_lejos"             = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_ahora_transportepúblico" = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_ahora_particular"        = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_ahora_Otro"              = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_laboral_apoyo = case_when(p7_16_1 == "1" ~ T,T ~ F),
    v_vio_laboral_denuncia = case_when(p7_16_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p7_26_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_recomendacion"           = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_despidocambio_ella_area" = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_despidocambio_el_area"   = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "6" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "7" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_18_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  ) 

d_sec_vii$v_vio_laboral_vida_quien_patron           = rowSums(d_sec_vii %>% select(starts_with("quien_vid_patron_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_supervisor       = rowSums(d_sec_vii %>% select(starts_with("quien_vid_supervisor_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_gerente          = rowSums(d_sec_vii %>% select(starts_with("quien_vid_gerente_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_compañere        = rowSums(d_sec_vii %>% select(starts_with("quien_vid_compañere_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_cliente          = rowSums(d_sec_vii %>% select(starts_with("quien_vid_cliente_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_desconocido      = rowSums(d_sec_vii %>% select(starts_with("quien_vid_desconocido_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_familiar_patron  = rowSums(d_sec_vii %>% select(starts_with("quien_vid_familiar_patron_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_quien_otro             = rowSums(d_sec_vii %>% select(starts_with("quien_vid_otro_p7_10")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_fisica         = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_11_",c(7,8,18))), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_emocional      = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_11_",c(10,11,6,10,16,17))), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_sexual         = rowSums(d_sec_vii %>% select(paste0("freq_ahora_p7_11_",c(1,2,3,4,5,9,12,13,14,15))), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_discriminacion = rowSums(d_sec_vii %>% select(paste0("mod_p7_8_",c(1:9))), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_patron          = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_patron_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_supervisor      = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_supervisor_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_gerente         = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_gerente_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_compañere       = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_compañere_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_cliente         = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_cliente_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_desconocido     = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_desconocido_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_familiar_patron = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_familiar_patron_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_quien_otro            = rowSums(d_sec_vii %>% select(starts_with("quien_ahora_otro_p7_12")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_instalaciones      = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_instalaciones_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_cerca              = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_cerca_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_lejos              = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_lejos_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_transportepúblico  = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_transportepúblico_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_particular         = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_particular_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_ahora_donde_Otro               = rowSums(d_sec_vii %>% select(starts_with("donde_ahora_Otro_p7_13_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_tipo_apoyo_orientacion = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_orientacion_p7_18_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_tipo_apoyo_legal       = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_legal_p7_18_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_tipo_apoyo_psicologico = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_psicologico_p7_18_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_tipo_apoyo_medica      = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_medica_p7_18_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_tipo_apoyo_Otro        = rowSums(d_sec_vii %>% select(starts_with("apoyo_inst_Otro_p7_18_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_castigaron               = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_castigaron_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_consignaAnteJuez         = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_consignaAnteJuez_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_recomendacion            = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_recomendacion_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_despidocambio_ella_area  = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_despidocambio_ella_area_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_despidocambio_el_area    = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_despidocambio_el_area_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_noRatifico               = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_noRatifico_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_nada                     = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_nada_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_denuncia_o_nosabe                   = d_sec_vii$v_vio_laboral_denuncia & rowSums(d_sec_vii %>% select(starts_with("que_sucedio_nosabe_p7_26_")), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_fisica         = d_sec_vii$v_vio_laboral_ahora_fisica | rowSums(d_sec_vii %>% select(paste0("mod_p7_9_",c(7,8,18))), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_emocional      = d_sec_vii$v_vio_laboral_ahora_emocional | rowSums(d_sec_vii %>% select(paste0("mod_p7_9_",c(10,11,6,10,16,17))), na.rm = T)>0
d_sec_vii$v_vio_laboral_vida_sexual         = d_sec_vii$v_vio_laboral_ahora_sexual | rowSums(d_sec_vii %>% select(paste0("mod_p7_9_",c(1,2,3,4,5,9,12,13,14,15))), na.rm = T)>0

d_sec_vii <- d_sec_vii %>% 
  mutate(
    v_vio_laboral_ahora_todas          = v_vio_laboral_ahora_fisica | v_vio_laboral_ahora_emocional |v_vio_laboral_ahora_sexual  | v_vio_laboral_ahora_discriminacion,
    
    v_vio_laboral_vida_todas          = v_vio_laboral_ahora_todas | v_vio_laboral_vida_fisica|v_vio_laboral_vida_emocional|v_vio_laboral_vida_sexual,
    v_vio_laboral_institucion_INM                 = case_when(p7_17_1  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_INM_estatal         = case_when(p7_17_2  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_linea               = case_when(p7_17_3  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_ac                  = case_when(p7_17_4  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_justicia_mujeres    = case_when(p7_17_5  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_defensoria          = case_when(p7_17_6  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_clinica_publica     = case_when(p7_17_7  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_clinica_privada     = case_when(p7_17_8  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_dif                 = case_when(p7_17_9  == "1" ~ T,T ~ F),
    v_vio_laboral_institucion_otro                = case_when(p7_17_10 == "1" ~ T,T ~ F),
    
    v_vio_laboral_institucion_trato_INM                 = p7_21_1 ,
    v_vio_laboral_institucion_trato_INM_estatal         = p7_21_2 ,
    v_vio_laboral_institucion_trato_linea               = p7_21_3 ,
    v_vio_laboral_institucion_trato_ac                  = p7_21_4 ,
    v_vio_laboral_institucion_trato_justicia_mujeres    = p7_21_5 ,
    v_vio_laboral_institucion_trato_defensoria          = p7_21_6 ,
    v_vio_laboral_institucion_trato_clinica_publica     = p7_21_7 ,
    v_vio_laboral_institucion_trato_clinica_privada     = p7_21_8 ,
    v_vio_laboral_institucion_trato_dif                 = p7_21_9 ,
    v_vio_laboral_institucion_trato_otro                = p7_21_10,
    
    v_vio_laboral_denuncia_en_sindicato           = case_when(p7_22_1 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_policia             = case_when(p7_22_2 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_municipio           = case_when(p7_22_3 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_mp                  = case_when(p7_22_4 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_procuraduria        = case_when(p7_22_5 == "1" ~T, T ~ F),  
    
    v_vio_laboral_denuncia_que_en_sindicato           = paste(p7_26_1_1,p7_26_1_2,p7_26_1_3,sep=","),
    v_vio_laboral_denuncia_que_en_policia             = paste(p7_26_2_1,p7_26_2_2,p7_26_2_3,sep=","),
    v_vio_laboral_denuncia_que_en_municipio           = paste(p7_26_3_1,p7_26_3_2,p7_26_3_3,sep=","),
    v_vio_laboral_denuncia_que_en_mp                  = paste(p7_26_4_1,p7_26_4_2,p7_26_4_3,sep=","),
    v_vio_laboral_denuncia_que_en_procuraduria        = paste(p7_26_5_1,p7_26_5_2,p7_26_5_3,sep=","),
    
    v_vio_laboral_no_denuncia_en_verguenza        = case_when(p7_27_1  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_noLeIbanACreer   = case_when(p7_27_2  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_miedo            = case_when(p7_27_3  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_familia          = case_when(p7_27_4  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_convencieron     = case_when(p7_27_5  == "1" ~T, T ~ F), 
    v_vio_laboral_no_denuncia_en_sinImportancia   = case_when(p7_27_6  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_costumbres       = case_when(p7_27_7  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_NoSabía          = case_when(p7_27_8  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_tiempo           = case_when(p7_27_9  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_desconfianza     = case_when(p7_27_10 == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_otra             = case_when(p7_27_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

### TSEC_VIII - Ámbito comunitario----
d_sec_viii <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VIII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>% 
  #head(50) %>%
  
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_comunitario_dummy = p8_1_13,
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p8_1_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_2_")),
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
      c(starts_with("p8_3_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_4_")),
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
      c(starts_with("p8_5_")),
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
    v_vio_comunitario_apoyo    = case_when(p8_8_1 == "1" ~ T,T ~ F),
    v_vio_comunitario_denuncia = case_when(p8_8_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p8_10_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_18_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    
  )  

d_sec_viii$v_vio_comunitario_vida_quien_conocido                   = rowSums(d_sec_viii %>% select(starts_with("quien_vid_conocido_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_amigo                      = rowSums(d_sec_viii %>% select(starts_with("quien_vid_amigo_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_vecino                     = rowSums(d_sec_viii %>% select(starts_with("quien_vid_vecino_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_policia                    = rowSums(d_sec_viii %>% select(starts_with("quien_vid_policia_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_militar                    = rowSums(d_sec_viii %>% select(starts_with("quien_vid_militar_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_sacerdote                  = rowSums(d_sec_viii %>% select(starts_with("quien_vid_sacerdote_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_conductor                  = rowSums(d_sec_viii %>% select(starts_with("quien_vid_conductor_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_desconodico                = rowSums(d_sec_viii %>% select(starts_with("quien_vid_desconodico_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_quien_otro                       = rowSums(d_sec_viii %>% select(starts_with("quien_vid_otro_p8_2")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_fisica                          = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_3_",c(4,6,11))), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_emocional                       = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_3_",c(2,3,8,14))), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_sexual                          = rowSums(d_sec_viii %>% select(paste0("freq_ahora_p8_3_",c(1,5,7,9,10,12,13,15))), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_conocido                  = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_conocido_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_amigo                     = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_amigo_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_vecino                    = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_vecino_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_policia                   = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_policia_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_militar                   = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_militar_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_sacerdote                 = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_sacerdote_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_conductor                 = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_conductor_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_desconodico               = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_desconodico_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_quien_otro                      = rowSums(d_sec_viii %>% select(starts_with("quien_ahora_otro_p8_4")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_calle               = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_calle_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_mercado             = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_mercado_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_autobús             = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_autobús_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_metro               = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_metro_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_metrobus            = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_metrobus_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_taxi                = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_taxi_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_iglesia             = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_iglesia_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_cantina             = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_cantina_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_feria               = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_feria_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_particular          = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_particular_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_ahora_donde_otro                = rowSums(d_sec_viii %>% select(starts_with("donde_ahora_otro_p8_5")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_tipo_apoyo_orientacion                = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_orientacion_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_tipo_apoyo_legal                      = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_legal_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_tipo_apoyo_psicologico                = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_psicologico_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_tipo_apoyo_medica                     = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_medica_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_tipo_apoyo_Otro                       = rowSums(d_sec_viii %>% select(starts_with("apoyo_inst_Otro_p8_10")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_denuncia_o_castigaron       = d_sec_viii$v_vio_comunitario_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_castigaron_p8_18")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_denuncia_o_consignaAnteJuez = d_sec_viii$v_vio_comunitario_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_consignaAnteJuez_p8_18")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_denuncia_o_noRatifico       = d_sec_viii$v_vio_comunitario_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_noRatifico_p8_18")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_denuncia_o_nada             = d_sec_viii$v_vio_comunitario_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_nada_p8_18")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_denuncia_o_nosabe           = d_sec_viii$v_vio_comunitario_denuncia & rowSums(d_sec_viii %>% select(starts_with("que_sucedio_nosabe_p8_18")), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_fisica                           = d_sec_viii$v_vio_comunitario_ahora_fisica | rowSums(d_sec_viii %>% select(paste0("mod_p8_1_",c(4,6,11))), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_emocional                        = d_sec_viii$v_vio_comunitario_ahora_emocional | rowSums(d_sec_viii %>% select(paste0("mod_p8_1_",c(2,3,8,14))), na.rm = T)>0
d_sec_viii$v_vio_comunitario_vida_sexual                           = d_sec_viii$v_vio_comunitario_ahora_sexual | rowSums(d_sec_viii %>% select(paste0("mod_p8_1_",c(1,5,7,9,10,12,13,15))), na.rm = T)>0

d_sec_viii <- d_sec_viii %>% 
  mutate(
    v_vio_comunitario_ahora_todas                           = v_vio_comunitario_ahora_fisica|v_vio_comunitario_ahora_emocional|v_vio_comunitario_ahora_sexual,
    v_vio_comunitario_vida_todas                            = v_vio_comunitario_ahora_todas|v_vio_comunitario_vida_fisica|v_vio_comunitario_vida_emocional|v_vio_comunitario_vida_sexual,
    v_vio_comunitario_institucion_INM                 = case_when(p8_9_1  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_INM_estatal         = case_when(p8_9_2  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_linea               = case_when(p8_9_3  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_ac                  = case_when(p8_9_4  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_justicia_mujeres    = case_when(p8_9_5  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_defensoria          = case_when(p8_9_6  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_clinica_publica     = case_when(p8_9_7  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_clinica_privada     = case_when(p8_9_8  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_dif                 = case_when(p8_9_9  == "1" ~ T,T ~ F),
    v_vio_comunitario_institucion_otro                = case_when(p8_9_10 == "1" ~ T,T ~ F),
    
    v_vio_comunitario_institucion_trato_INM                 = p8_13_1 ,
    v_vio_comunitario_institucion_trato_INM_estatal         = p8_13_2 ,
    v_vio_comunitario_institucion_trato_linea               = p8_13_3 ,
    v_vio_comunitario_institucion_trato_ac                  = p8_13_4 ,
    v_vio_comunitario_institucion_trato_justicia_mujeres    = p8_13_5 ,
    v_vio_comunitario_institucion_trato_defensoria          = p8_13_6 ,
    v_vio_comunitario_institucion_trato_clinica_publica     = p8_13_7 ,
    v_vio_comunitario_institucion_trato_clinica_privada     = p8_13_8 ,
    v_vio_comunitario_institucion_trato_dif                 = p8_13_9 ,
    v_vio_comunitario_institucion_trato_otro                = p8_13_10,
    
    v_vio_comunitario_denuncia_en_policia             = case_when(p8_14_1 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_municipio           = case_when(p8_14_2 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_mp                  = case_when(p8_14_3 == "1" ~T, T ~ F),   
    
    v_vio_comunitario_denuncia_que_en_policia             = paste(p8_18_1_1,p8_18_1_2,p8_18_1_3,sep=","),
    v_vio_comunitario_denuncia_que_en_municipio           = paste(p8_18_2_1,p8_18_2_2,p8_18_2_3,sep=","),
    v_vio_comunitario_denuncia_que_en_mp                  = paste(p8_18_3_1,p8_18_3_2,p8_18_3_3,sep=","),   
    
    v_vio_comunitario_no_denuncia_en_verguenza        = case_when(p8_19_1  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_noLeIbanACreer   = case_when(p8_19_2  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_miedo            = case_when(p8_19_3  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_familia          = case_when(p8_19_4  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_convencieron     = case_when(p8_19_5  == "1" ~T, T ~ F), 
    v_vio_comunitario_no_denuncia_en_sinImportancia   = case_when(p8_19_6  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_costumbres       = case_when(p8_19_7  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_NoSabía          = case_when(p8_19_8  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_tiempo           = case_when(p8_19_9  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_desconfianza     = case_when(p8_19_10 == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_otra             = case_when(p8_19_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()


### TSEC_IX - Atención obstétrica ----
d_sec_ix <- foreign::read.dbf(
  paste_inp(paste0(v_carpeta_2, "TB_SEC_IX.dbf")), as.is = T
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
    anio = v_anio, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_afiliación = case_when(
      cruce_afiliación_privada_dummy == T ~ "Servicios médicos privados",
      cruce_afiliación_pública_dummy == T ~ "Servicios médicos públicos",
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
    ),
    filtro_parto = case_when(
      as.numeric(p9_4_1) >= 1 ~ 1,
      as.numeric(p9_4_2) >= 1 ~ 1,
      T ~ 2
    )
  ) %>% 
  select(
    llave, anio, 
    filtro_embarazo = p9_2, filtro_parto,
    starts_with("cruce_afiliación"), cruce_lugar_atencion_parto = p9_7,
    starts_with("v_vob"), v_cesárea_dummy= p9_8_11, 
    v_cesárea_informaron_por_qué_dummy = p9_8_12,
    v_cesárea_autorización_dummy = p9_8_13,
    v_anio_ult_parto = p9_6
  ) %>%
  glimpse()
beepr::beep(2)

### TSEC_X - Ámbito familiar----
d_sec_x <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_X.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_X_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_familiar_dummy = case_when(
      p10_1_3 < 4 ~ "1",
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
    c(starts_with("p10_1_")),
    .fns =  list("mod" = ~ case_when(as.numeric(.) < 4 ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p10_2_")),
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
      c(starts_with("p10_3_")),
      .fns =  list("donde_casa_propia"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "donde_casa_familiar" = ~ case_when(. == "2" ~ T,T ~ F),
                   "donde_calle"         = ~ case_when(. == "3" ~ T,T ~ F),
                   "donde_mercado"       = ~ case_when(. == "4" ~ T,T ~ F),
                   "donde_lugar_publico" = ~ case_when(. == "5" ~ T,T ~ F),
                   "donde_otro"          = ~ case_when(. == "6" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    v_vio_familiar_apoyo    = case_when(p10_6_1 == "1" ~ T,T ~ F),
    v_vio_familiar_denuncia = case_when(p10_6_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p10_8_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p10_14_")),
      .fns =  list("que_sucedio_castigaron"              = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez"        = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_noRatifico"              = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_nada"                    = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_nosabe"                  = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    
  ) 

d_sec_x$v_vio_familiar_fisica                           = rowSums(d_sec_x %>% select(paste0("mod_p10_1_",c(5,10,11,17))), na.rm = T)>0
d_sec_x$v_vio_familiar_emocional                        = rowSums(d_sec_x %>% select(paste0("mod_p10_1_",c(1,7,12))), na.rm = T)>0
d_sec_x$v_vio_familiar_sexual                           = rowSums(d_sec_x %>% select(paste0("mod_p10_1_",c(2,3,4,13,18))), na.rm = T)>0
d_sec_x$v_vio_familiar_patrimonial                      = rowSums(d_sec_x %>% select(paste0("mod_p10_1_",c(6,8,9,14,15,16))), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_conocido                   = rowSums(d_sec_x %>% select(starts_with("quien_conocido_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_amigo                      = rowSums(d_sec_x %>% select(starts_with("quien_amigo_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_vecino                     = rowSums(d_sec_x %>% select(starts_with("quien_vecino_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_policia                    = rowSums(d_sec_x %>% select(starts_with("quien_policia_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_militar                    = rowSums(d_sec_x %>% select(starts_with("quien_militar_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_sacerdote                  = rowSums(d_sec_x %>% select(starts_with("quien_sacerdote_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_conductor                  = rowSums(d_sec_x %>% select(starts_with("quien_conductor_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_desconodico                = rowSums(d_sec_x %>% select(starts_with("quien_desconodico_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_quien_otro                       = rowSums(d_sec_x %>% select(starts_with("quien_otro_p10_2")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_casa_propia                = rowSums(d_sec_x %>% select(starts_with("donde_casa_propia_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_casa_familiar              = rowSums(d_sec_x %>% select(starts_with("donde_casa_familiar_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_calle                      = rowSums(d_sec_x %>% select(starts_with("donde_calle_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_mercado                    = rowSums(d_sec_x %>% select(starts_with("donde_mercado_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_lugar_publico              = rowSums(d_sec_x %>% select(starts_with("donde_lugar_publico_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_donde_otro                       = rowSums(d_sec_x %>% select(starts_with("donde_otro_p10_3")), na.rm = T)>0
d_sec_x$v_vio_familiar_tipo_apoyo_orientacion                = rowSums(d_sec_x %>% select(starts_with("apoyo_inst_orientacion_p10_8")), na.rm = T)>0
d_sec_x$v_vio_familiar_tipo_apoyo_legal                      = rowSums(d_sec_x %>% select(starts_with("apoyo_inst_legal_p10_8")), na.rm = T)>0
d_sec_x$v_vio_familiar_tipo_apoyo_psicologico                = rowSums(d_sec_x %>% select(starts_with("apoyo_inst_psicologico_p10_8")), na.rm = T)>0
d_sec_x$v_vio_familiar_tipo_apoyo_medica                     = rowSums(d_sec_x %>% select(starts_with("apoyo_inst_medica_p10_8")), na.rm = T)>0
d_sec_x$v_vio_familiar_tipo_apoyo_Otro                       = rowSums(d_sec_x %>% select(starts_with("apoyo_inst_Otro_p10_8")), na.rm = T)>0
d_sec_x$v_vio_familiar_denuncia_que_sucedio_castigaron       = d_sec_x$v_vio_familiar_denuncia & rowSums(d_sec_x %>% select(starts_with("que_sucedio_castigaron_p10_14")), na.rm = T)>0
d_sec_x$v_vio_familiar_denuncia_que_sucedio_consignaAnteJuez = d_sec_x$v_vio_familiar_denuncia & rowSums(d_sec_x %>% select(starts_with("que_sucedio_consignaAnteJuez_p10_14")), na.rm = T)>0
d_sec_x$v_vio_familiar_denuncia_que_sucedio_noRatifico       = d_sec_x$v_vio_familiar_denuncia & rowSums(d_sec_x %>% select(starts_with("que_sucedio_noRatifico_p10_14")), na.rm = T)>0
d_sec_x$v_vio_familiar_denuncia_que_sucedio_nada             = d_sec_x$v_vio_familiar_denuncia & rowSums(d_sec_x %>% select(starts_with("que_sucedio_nada_p10_14")), na.rm = T)>0
d_sec_x$v_vio_familiar_denuncia_que_sucedio_nosabe           = d_sec_x$v_vio_familiar_denuncia & rowSums(d_sec_x %>% select(starts_with("que_sucedio_nosabe_p10_14")), na.rm = T)>0

d_sec_x <- d_sec_x %>%
  mutate(
    v_vio_familiar_todas                            = v_vio_familiar_fisica|v_vio_familiar_emocional|v_vio_familiar_sexual|v_vio_familiar_patrimonial,
    v_vio_familiar_institucion_INM                 = case_when(p10_7_1  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_INM_estatal         = case_when(p10_7_2  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_linea               = case_when(p10_7_3  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_ac                  = case_when(p10_7_4  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_justicia_mujeres    = case_when(p10_7_5  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_defensoria          = case_when(p10_7_6  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_clinica_publica     = case_when(p10_7_7  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_clinica_privada     = case_when(p10_7_8  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_dif                 = case_when(p10_7_9  == "1" ~ T,T ~ F),
    v_vio_familiar_institucion_otro                = case_when(p10_7_10 == "1" ~ T,T ~ F),
    
    v_vio_familiar_institucion_trato_INM                 =p10_10_1 ,
    v_vio_familiar_institucion_trato_INM_estatal         =p10_10_2 ,
    v_vio_familiar_institucion_trato_linea               =p10_10_3 ,
    v_vio_familiar_institucion_trato_ac                  =p10_10_4 ,
    v_vio_familiar_institucion_trato_justicia_mujeres    =p10_10_5 ,
    v_vio_familiar_institucion_trato_defensoria          =p10_10_6 ,
    v_vio_familiar_institucion_trato_clinica_publica     =p10_10_7 ,
    v_vio_familiar_institucion_trato_clinica_privada     =p10_10_8 ,
    v_vio_familiar_institucion_trato_dif                 =p10_10_9 ,
    v_vio_familiar_institucion_trato_otro                =p10_10_10,
    
    v_vio_familiar_denuncia_en_policia             = case_when(p10_11_1 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_municipio           = case_when(p10_11_2 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_mp                  = case_when(p10_11_3 == "1" ~T, T ~ F),   
    
    v_vio_familiar_denuncia_en_que_policia             = paste(p10_14_1_1,p10_14_1_2,p10_14_1_3,sep=","),
    v_vio_familiar_denuncia_en_que_municipio           = paste(p10_14_2_1,p10_14_2_2,p10_14_2_3,sep=","),
    v_vio_familiar_denuncia_en_que_mp                  = paste(p10_14_3_1,p10_14_3_2,p10_14_3_3,sep=","),   
    
    v_vio_familiar_no_denuncia_en_verguenza        = case_when(p10_15_1  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_noLeIbanACreer   = case_when(p10_15_2  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_miedo            = case_when(p10_15_3  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_familia          = case_when(p10_15_4  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_convencieron     = case_when(p10_15_5  == "1" ~T, T ~ F), 
    v_vio_familiar_no_denuncia_en_sinImportancia   = case_when(p10_15_6  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_costumbres       = case_when(p10_15_7  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_NoSabía          = case_when(p10_15_8  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_tiempo           = case_when(p10_15_9  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_desconfianza     = case_when(p10_15_10 == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_otra             = case_when(p10_15_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

### TSEC_XI - Familia de origen (infancia) ----
d_sec_xi <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_XI.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_infancia_dummy = case_when(
      p11_12_5 == "1" ~ "1",
      p11_12_6 == "1" ~ "1",
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
    c(starts_with("p11_12_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p11_13_")),
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

d_sec_xi$v_vio_familiaOrigen_Otodas                            = rowSums(d_sec_xi %>% select(paste0("mod_p11_12_",c(1:6))), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_Osexual                           = rowSums(d_sec_xi %>% select(paste0("mod_p11_12_",c(1:6))), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_padre                       = rowSums(d_sec_xi %>% select(starts_with("quien_padre_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_madre                       = rowSums(d_sec_xi %>% select(starts_with("quien_madre_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_padrastro                   = rowSums(d_sec_xi %>% select(starts_with("quien_padrastro_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_abuelo                      = rowSums(d_sec_xi %>% select(starts_with("quien_abuelo_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_hermano                     = rowSums(d_sec_xi %>% select(starts_with("quien_hermano_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_tio                         = rowSums(d_sec_xi %>% select(starts_with("quien_tio_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_primo                       = rowSums(d_sec_xi %>% select(starts_with("quien_primo_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_familiar                    = rowSums(d_sec_xi %>% select(starts_with("quien_familiar_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_nofamiliar                  = rowSums(d_sec_xi %>% select(starts_with("quien_nofamiliar_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_desconocido                 = rowSums(d_sec_xi %>% select(starts_with("quien_desconocido_p11_13_")), na.rm = T)>0
d_sec_xi$v_vio_familiaOrigen_otro                        = rowSums(d_sec_xi %>% select(starts_with("quien_otro_p11_13_")), na.rm = T)>0

d_sec_xi <- d_sec_xi %>% 
  ungroup() %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
### TSEC_XIII - Relación actual o última relación ----
d_sec_xiii <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_XIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_XIII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>% 
  #head(50) %>%
  
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_vio_sex_pareja_expareja_dummy = case_when(
      p13_1_25 < 4 ~ "1",
      p13_1_26 < 4 ~ "1",
      p13_1_27 < 4 ~ "1",
      p13_1_29 < 4 ~ "1",
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
    c(starts_with("p13_1_")),
    .fns =  list("mod" = ~ case_when(
      is.na(.) ~ F,
      as.numeric(.)<4 ~ T,
      T ~ F)),
    .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p13_2_")),
      .fns =  list("gravedad_muy_grave"       = ~ case_when(. == "1" ~ T,T ~ F),
                   "gravedad_grave"           = ~ case_when(. == "2" ~ T,T ~ F),
                   "gravedad_sinimportancia"  = ~ case_when(. == "3" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p13_3_")),
      .fns =  list("freq_ahora" = ~ case_when(
        is.na(.) ~ F,
        as.numeric(.)<4 ~ T,
        T ~ F)),
      .names = "{.fn}_{.col}"),
    v_vio_pareja_apoyo = case_when(p13_7_1 == "1" ~ T,T ~ F),
    v_vio_pareja_denuncia = case_when(p13_7_2 == "1" ~ T,T ~ F),
    across(
      c(starts_with("p13_17_")),
      .fns =  list("que_sucedio_desalojo"         = ~ case_when(. == "1" ~ T,T ~ F),
                   "que_sucedio_detencion"        = ~ case_when(. == "2" ~ T,T ~ F),
                   "que_sucedio_consignaAnteJuez" = ~ case_when(. == "3" ~ T,T ~ F),
                   "que_sucedio_ordenproteccion"  = ~ case_when(. == "4" ~ T,T ~ F),
                   "que_sucedio_acuerdo"          = ~ case_when(. == "5" ~ T,T ~ F),
                   "que_sucedio_noProcedio"       = ~ case_when(. == "6" ~ T,T ~ F),
                   "que_sucedio_noRatifico"       = ~ case_when(. == "7" ~ T,T ~ F),
                   "que_sucedio_nosabe"           = ~ case_when(. == "8" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p13_9_")),
      .fns =  list("apoyo_inst_orientacion"   = ~ case_when(. == "1" ~ T,T ~ F),
                   "apoyo_inst_legal"         = ~ case_when(. == "2" ~ T,T ~ F),
                   "apoyo_inst_psicologico"   = ~ case_when(. == "3" ~ T,T ~ F),
                   "apoyo_inst_medica"        = ~ case_when(. == "4" ~ T,T ~ F),
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F)
      ),
      .names = "{.fn}_{.col}"),
  ) 

d_sec_xiii$v_vio_pareja_ahora_fisica                          = rowSums(d_sec_xiii %>% select(starts_with(paste0("freq_ahora_p13_3_",c(1:9,15,18,19)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_ahora_emocional                       = rowSums(d_sec_xiii %>% select(starts_with(paste0("freq_ahora_p13_3_",c(10,11,12,13,14,16,17,21,22,23,24)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_ahora_sexual                          = rowSums(d_sec_xiii %>% select(starts_with(paste0("freq_ahora_p13_3_",c(25,26,27,28,29)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_ahora_patrimonial                     = rowSums(d_sec_xiii %>% select(starts_with(paste0("freq_ahora_p13_3_",c(20,30,31,32,33,34,35,36)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_gravedad_muy_grave                    = rowSums(d_sec_xiii %>% select(starts_with("gravedad_muy_grave_p13_2")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_gravedad_grave                        = rowSums(d_sec_xiii %>% select(starts_with("gravedad_grave_p13_2")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_gravedad_sinimportancia               = rowSums(d_sec_xiii %>% select(starts_with("gravedad_sinimportancia_p13_2")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_tipo_apoyo_orientacion                = rowSums(d_sec_xiii %>% select(starts_with("apoyo_inst_orientacion_p13_9_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_tipo_apoyo_legal                      = rowSums(d_sec_xiii %>% select(starts_with("apoyo_inst_legal_p13_9_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_tipo_apoyo_psicologico                = rowSums(d_sec_xiii %>% select(starts_with("apoyo_inst_psicologico_p13_9_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_tipo_apoyo_medica                     = rowSums(d_sec_xiii %>% select(starts_with("apoyo_inst_medica_p13_9_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_tipo_apoyo_Otro                       = rowSums(d_sec_xiii %>% select(starts_with("apoyo_inst_Otro_p13_9_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_vida_fisica                           = d_sec_xiii$v_vio_pareja_ahora_fisica | rowSums(d_sec_xiii %>% select(starts_with(paste0("mod_p13_1_",c(1:9,15,18,19)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_vida_emocional                        = d_sec_xiii$v_vio_pareja_ahora_emocional | rowSums(d_sec_xiii %>% select(starts_with(paste0("mod_p13_1_",c(10,11,12,13,14,16,17,21,22,23,24)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_vida_sexual                           = d_sec_xiii$v_vio_pareja_ahora_sexual | rowSums(d_sec_xiii %>% select(starts_with(paste0("mod_p13_1_",c(25,26,27,28,29)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_vida_patrimonial                      = d_sec_xiii$v_vio_pareja_ahora_patrimonial | rowSums(d_sec_xiii %>% select(starts_with(paste0("mod_p13_1_",c(20,30,31,32,33,34,35,36)))), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_desalojo         = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_desalojo_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_detencion        = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_detencion_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_consignaAnteJuez = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_consignaAnteJuez_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_ordenproteccion  = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_ordenproteccion_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_acuerdo          = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_acuerdo_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_noProcedio       = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_noProcedio_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_noRatifico       = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_noRatifico_p13_17_")), na.rm = T)>0
d_sec_xiii$v_vio_pareja_denuncia_que_sucedio_nosabe           = d_sec_xiii$v_vio_pareja_denuncia & rowSums(d_sec_xiii %>% select(starts_with("que_sucedio_nosabe_p13_17_")), na.rm = T)>0
d_sec_xiii <- d_sec_xiii %>% 
  mutate(
    v_vio_pareja_ahora_todas                     = v_vio_pareja_ahora_fisica|v_vio_pareja_ahora_emocional|v_vio_pareja_ahora_sexual|v_vio_pareja_ahora_patrimonial,
    v_vio_pareja_vida_todas                      = v_vio_pareja_ahora_todas|v_vio_pareja_vida_fisica|v_vio_pareja_vida_emocional|v_vio_pareja_vida_sexual|v_vio_pareja_vida_patrimonial,
    v_vio_pareja_institucion_INM                 = case_when(p13_8_1  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_INM_estatal         = case_when(p13_8_2  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_linea               = case_when(p13_8_3  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_ac                  = case_when(p13_8_4  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_justicia_mujeres    = case_when(p13_8_5  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_defensoria          = case_when(p13_8_6  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_clinica_publica     = case_when(p13_8_7  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_clinica_privada     = case_when(p13_8_8  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_dif                 = case_when(p13_8_9  == "1" ~ T,T ~ F),
    v_vio_pareja_institucion_otro                = case_when(p13_8_10 == "1" ~ T,T ~ F),
    
    v_vio_pareja_institucion_trato_INM                 = p13_12_1,
    v_vio_pareja_institucion_trato_INM_estatal         = p13_12_2,
    v_vio_pareja_institucion_trato_linea               = p13_12_3,
    v_vio_pareja_institucion_trato_ac                  = p13_12_4,
    v_vio_pareja_institucion_trato_justicia_mujeres    = p13_12_5,
    v_vio_pareja_institucion_trato_defensoria          = p13_12_6,
    v_vio_pareja_institucion_trato_clinica_publica     = p13_12_7,
    v_vio_pareja_institucion_trato_clinica_privada     = p13_12_8,
    v_vio_pareja_institucion_trato_dif                 = p13_12_9,
    v_vio_pareja_institucion_trato_otro                = p13_12_10,
    
    v_vio_pareja_denuncia_en_policia             = case_when(p13_13_1 == "1" ~T, T ~ F),     
    v_vio_pareja_denuncia_en_municipio           = case_when(p13_13_2 == "1" ~T, T ~ F),
    v_vio_pareja_denuncia_en_autoridadesLocales  = case_when(p13_13_3 == "1" ~T, T ~ F),
    v_vio_pareja_denuncia_en_mp                  = case_when(p13_13_4 == "1" ~T, T ~ F),  
    
    v_vio_pareja_denuncia_en_que_policia             = paste(p13_17_1_1,p13_17_1_2,p13_17_1_3,sep="," ),
    v_vio_pareja_denuncia_en_que_municipio           = paste(p13_17_2_1,p13_17_2_2,p13_17_2_3,sep="," ),
    v_vio_pareja_denuncia_en_que_autoridadesLocales  = paste(p13_17_3_1,p13_17_3_2,p13_17_3_3,sep="," ),
    v_vio_pareja_denuncia_en_que_mp                  = paste(p13_17_4_1,p13_17_4_2,p13_17_4_3,sep="," ),
    
    v_vio_pareja_no_denuncia_en_verguenza        = case_when(p13_13_1  == "1" ~T, T ~ F),     
    v_vio_pareja_no_denuncia_en_noLeIbanACreer   = case_when(p13_21_2  == "1" ~T, T ~ F),
    v_vio_pareja_no_denuncia_en_miedo            = case_when(p13_21_3  == "1" ~T, T ~ F),
    v_vio_pareja_no_denuncia_en_familia          = case_when(p13_21_4  == "1" ~T, T ~ F),     
    v_vio_pareja_no_denuncia_en_convencieron     = case_when(p13_21_5  == "1" ~T, T ~ F), 
    v_vio_pareja_no_denuncia_en_sinImportancia   = case_when(p13_21_6  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_costumbres       = case_when(p13_21_7  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_NoSabía          = case_when(p13_21_8  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_tiempo           = case_when(p13_21_9  == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_desconfianza     = case_when(p13_21_10 == "1" ~T, T ~ F),  
    v_vio_pareja_no_denuncia_en_otra             = case_when(p13_21_11 == "1" ~T, T ~ F),  
    v_vio_pareja_resultado_denuncia_relacion     = case_when(
      p13_19ab == "1" ~"Separacion Temporal",
      p13_19ab == "2" ~"Separacion (Ella)",
      p13_19ab == "3" ~"Separacion (El)",
      p13_19ab == "4" ~"No Separacion",
      T ~ NA_character_),  
  ) %>%    
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
# Unir base de datos ----
d_endireh_vob <- d_sec_iii %>% 
  left_join(
    d_sdem
  ) %>% 
  left_join(
    d_sec_iv
  ) %>% 
  left_join(
    d_sec_vi
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
    d_sec_xiii
  ) %>% 
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

d_endireh_vob$v_vio_tipo_apoyo_orientacion       = rowSums(d_endireh_vob %>% select(ends_with("apoyo_orientacion")),na.rm = T)>0
d_endireh_vob$v_vio_tipo_apoyo_legal             = rowSums(d_endireh_vob %>% select(ends_with("apoyo_legal")),na.rm = T)>0
d_endireh_vob$v_vio_tipo_apoyo_psicologico       = rowSums(d_endireh_vob %>% select(ends_with("apoyo_psicologico")),na.rm = T)>0
d_endireh_vob$v_vio_tipo_apoyo_medica            = rowSums(d_endireh_vob %>% select(ends_with("apoyo_medica")),na.rm = T)>0
d_endireh_vob$v_vio_tipo_apoyo_Otro              = rowSums(d_endireh_vob %>% select(ends_with("apoyo_Otro")),na.rm = T)>0
d_endireh_vob$v_vio_institucion_INM              = rowSums(d_endireh_vob %>% select(ends_with("on_INM")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_INM_estatal      = rowSums(d_endireh_vob %>% select(ends_with("on_INM_estatal")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_linea            = rowSums(d_endireh_vob %>% select(ends_with("on_linea")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_ac               = rowSums(d_endireh_vob %>% select(ends_with("on_ac")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_justicia_mujeres = rowSums(d_endireh_vob %>% select(ends_with("on_justicia_mujeres")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_defensoria       = rowSums(d_endireh_vob %>% select(ends_with("on_defensoria")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_clinica_publica  = rowSums(d_endireh_vob %>% select(ends_with("on_clinica_publica")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_clinica_privada  = rowSums(d_endireh_vob %>% select(ends_with("on_clinica_privada")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_dif              = rowSums(d_endireh_vob %>% select(ends_with("on_dif")), na.rm = T)>0
d_endireh_vob$v_vio_institucion_otro             = rowSums(d_endireh_vob %>% select(ends_with("on_otro")), na.rm = T)>0
d_endireh_vob$v_vio_ahora_emocional              = rowSums(d_endireh_vob %>% select(ends_with("ahora_emocional")),na.rm = T)>0
d_endireh_vob$v_vio_ahora_fisica                 = rowSums(d_endireh_vob %>% select(ends_with("ahora_fisica")),na.rm = T)>0
d_endireh_vob$v_vio_ahora_sexual                 = rowSums(d_endireh_vob %>% select(ends_with("ahora_sexual")),na.rm = T)>0
d_endireh_vob$v_vio_ahora_patrimonial            = rowSums(d_endireh_vob %>% select(ends_with("ahora_patrimonial")),na.rm = T)>0
d_endireh_vob$v_vio_ahora_discriminacion         = rowSums(d_endireh_vob %>% select(ends_with("ahora_discriminacion")),na.rm = T)>0
d_endireh_vob$v_vio_vida_emocional               = rowSums(d_endireh_vob %>% select(ends_with("_emocional")),na.rm = T)>0
d_endireh_vob$v_vio_vida_fisica                  = rowSums(d_endireh_vob %>% select(ends_with("_fisica")),na.rm = T)>0
d_endireh_vob$v_vio_vida_sexual                  = rowSums(d_endireh_vob %>% select(ends_with("_sexual")),na.rm = T)>0
d_endireh_vob$v_vio_vida_patrimonial             = rowSums(d_endireh_vob %>% select(ends_with("_patrimonial")),na.rm = T)>0
d_endireh_vob$v_vio_vida_discriminacion          = rowSums(d_endireh_vob %>% select(ends_with("_discriminacion")),na.rm = T)>0
d_endireh_vob$v_vio_denuncia_en_policia          = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_policia")))>0
d_endireh_vob$v_vio_denuncia_en_municipio        = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_municipio")))>0
d_endireh_vob$v_vio_denuncia_en_mp               = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_mp")))>0
d_endireh_vob$v_vio_denuncia_en_escuela          = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_escuela")))>0
d_endireh_vob$v_vio_denuncia_en_sindicato        = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_sindicato")))>0
d_endireh_vob$v_vio_denuncia_en_procuraduria     = rowSums(d_endireh_vob %>% select(ends_with("denuncia_en_procuraduria")))>0

d_endireh_vob <- d_endireh_vob %>% 
  mutate(
    v_vio_vida_todas = v_vio_vida_emocional|v_vio_vida_fisica|v_vio_vida_sexual|v_vio_vida_patrimonial|v_vio_vida_discriminacion,
    v_vio_ahora_todas = v_vio_ahora_emocional | v_vio_ahora_fisica|v_vio_ahora_sexual|v_vio_ahora_patrimonial|v_vio_ahora_discriminacion,
  ) %>% 
  glimpse

saveRDS(d_endireh_vob, paste_out("01_endireh_2016_vob_no_filter_lolo.rds"))
