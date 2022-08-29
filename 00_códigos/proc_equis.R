#------------------------------------------------------------------------------#
# Proyecto:                   ANÁLISIS DE ENDIREH PARA Equis
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


Sys.setlocale("LC_ALL", "es_ES.UTF-8")
options(scipen=999)

pacman::p_load(lubridate,hot.deck,zoo,stringi,gridExtra,ggthemes,tidyverse,
               hrbrthemes,magick,scales,RColorBrewer,foreign,srvyr,openxlsx)


# Directorios ----
paste_inp       <- function(x){paste0("01_datos_crudos/" , x)}
paste_out       <- function(x){paste0("02_datos_limpios/", x)}
paste_plot      <- function(x){paste0("03_graficas/", x)}
`%+%` <- paste0
source("00_códigos/00_funciones.R")

# Datos ----

### TSDEM - Sociodemográficos ----
d_sdem <- read.dbf(paste_inp("bd_sd_endireh2016_sitioinegi_dbf/TSDem.DBF"), as.is = T) %>% 
  janitor::clean_names() %>% 
  mutate(
    anio = 2016,
    llave = paste0(upm, viv_sel, hogar, n_ren),
    cruce_tipo_loc = dominio,
    cruce_auto_indig_dummy = ifelse(p2_10 < 3, "1", "2"),
    cruce_g_edad = recode_g_edad(as.numeric(edad)),
    cruce_escolaridad = recode_escolaridad(niv)
  ) %>% 
  select(
    llave, anio, cruce_cve_ent = cve_ent, edad,
    #keep_edad_num,
    cruce_g_edad,
    cruce_escolaridad, keep_escolaridad_num = gra, 
    cruce_alfabet_dummy = p2_8, cruce_auto_indig_dummy, 
    cruce_lengua_indig_dummy = p2_11, cruce_ocupada_dummy = p2_13, 
    cruce_pnea_pea = p2_14, cruce_pos_ocu = p2_15,
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
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_III.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = 2016,
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
      .fns =  list("freq_ahora_" = ~ case_when(
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
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F),
      ),
      .names = "{.fn}_{.col}"),
  )   

d_sec_vi <- d_sec_vi %>% 
  rowwise() %>%   
  mutate(
    v_vio_escuela_vida_todas     = any(c_across(paste0("mod_p6_6_",c(1:17))), na.rm = T),
    v_vio_escuela_vida_fisica    = any(c_across(paste0("mod_p6_6_",c(1,2,6))), na.rm = T),
    v_vio_escuela_vida_emocional = any(c_across(paste0("mod_p6_6_",c(4,9,13,16))), na.rm = T),
    v_vio_escuela_vida_sexual    = any(c_across(paste0("mod_p6_6_",c(3,5,7,8,10,11,12,14,15,17))), na.rm = T),
    
    v_vio_escuela_vida_quien_Maestro     = any(c_across(starts_with("quien_vid_Maestro_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Maestra     = any(c_across(starts_with("quien_vid_Maestra_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Compañero   = any(c_across(starts_with("quien_vid_Compañero_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Compañera   = any(c_across(starts_with("quien_vid_Compañera_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Directore   = any(c_across(starts_with("quien_vid_Directore_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Trabajador  = any(c_across(starts_with("quien_vid_Trabajador_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Trabajadora = any(c_across(starts_with("quien_vid_Trabajadora_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_desconocido = any(c_across(starts_with("quien_vid_desconocido_p6_7_")), na.rm = T),
    v_vio_escuela_vida_quien_Otra        = any(c_across(starts_with("quien_vid_Otra_p6_7_")), na.rm = T),
    
    v_vio_escuela_ahora_todas    = v_vio_escuela_vida_todas     & any(c_across(paste0("freq_ahora_p6_8_",c(1:17))), na.rm = T),
    v_vio_escuela_ahora_fisica   = v_vio_escuela_vida_fisica    & any(c_across(paste0("freq_ahora_p6_8_",c(1,2,6))), na.rm = T),
    v_vio_escuela_ahora_emocional= v_vio_escuela_vida_emocional & any(c_across(paste0("freq_ahora_p6_8_",c(4,9,13,16))), na.rm = T),
    v_vio_escuela_ahora_sexual   = v_vio_escuela_vida_sexual    & any(c_across(paste0("freq_ahora_p6_8_",c(3,5,7,8,10,11,12,14,15,17))), na.rm = T),
    
    v_vio_escuela_ahora_quien_Maestro     = any(c_across(starts_with("quien_ahora_Maestro_p6_9_")), na.rm = T),
    v_vio_escuela_ahora_quien_Maestra     = any(c_across(starts_with("quien_ahora_Maestra_p6_9_")), na.rm = T),    
    v_vio_escuela_ahora_quien_Compañero   = any(c_across(starts_with("quien_ahora_Compañero_p6_9_")), na.rm = T),      
    v_vio_escuela_ahora_quien_Compañera   = any(c_across(starts_with("quien_ahora_Compañera_p6_9_")), na.rm = T),      
    v_vio_escuela_ahora_quien_Directore   = any(c_across(starts_with("quien_ahora_Directore_p6_9_")), na.rm = T),      
    v_vio_escuela_ahora_quien_Trabajador  = any(c_across(starts_with("quien_ahora_Trabajador_p6_9_")), na.rm = T),       
    v_vio_escuela_ahora_quien_Trabajadora = any(c_across(starts_with("quien_ahora_Trabajadora_p6_9_")), na.rm = T),        
    v_vio_escuela_ahora_quien_desconocido = any(c_across(starts_with("quien_ahora_desconocido_p6_9_")), na.rm = T),        
    v_vio_escuela_ahora_quien_Otra        = any(c_across(starts_with("quien_ahora_Otra_p6_9_")), na.rm = T), 
    
    v_vio_escuela_ahora_donde_escuela            = any(c_across(starts_with("donde_ahora_escuela_p6_10_")), na.rm = T),
    v_vio_escuela_ahora_donde_cerca              = any(c_across(starts_with("donde_ahora_cerca_p6_10_")), na.rm = T),
    v_vio_escuela_ahora_donde_lejos              = any(c_across(starts_with("donde_ahora_lejos_p6_10_")), na.rm = T),
    v_vio_escuela_ahora_donde_transportepúblico  = any(c_across(starts_with("donde_ahora_transportepúblico_p6_10_")), na.rm = T),            
    v_vio_escuela_ahora_donde_particular         = any(c_across(starts_with("donde_ahora_particular_p6_10_")), na.rm = T),     
    v_vio_escuela_ahora_donde_Otro               = any(c_across(starts_with("donde_ahora_Otro_p6_10_")), na.rm = T),
    
    v_vio_escuela_denuncia_que_sucedio_castigaron       = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_castigaron_p6_23_")), na.rm = T),
    v_vio_escuela_denuncia_que_sucedio_consignaAnteJuez = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_consignaAnteJuez_p6_23_")), na.rm = T),
    v_vio_escuela_denuncia_que_sucedio_recomendacion    = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_recomendacion_p6_23_")), na.rm = T),
    v_vio_escuela_denuncia_que_sucedio_noRatifico       = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_noRatifico_p6_23_")), na.rm = T),            
    v_vio_escuela_denuncia_que_sucedio_nada             = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_nada_p6_23_")), na.rm = T),     
    v_vio_escuela_denuncia_que_sucedio_nosabe           = v_vio_escuela_denuncia & any(c_across(starts_with("que_sucedio_nosabe_p6_23_")), na.rm = T),
    
    v_vio_escuela_tipo_apoyo_orientacion = any(c_across(starts_with("apoyo_inst_orientacion_p6_15_")), na.rm = T),
    v_vio_escuela_tipo_apoyo_legal       = any(c_across(starts_with("apoyo_inst_legal_p6_15_")), na.rm = T),
    v_vio_escuela_tipo_apoyo_psicologico = any(c_across(starts_with("apoyo_inst_psicologico_p6_15_")), na.rm = T),
    v_vio_escuela_tipo_apoyo_medica      = any(c_across(starts_with("apoyo_inst_medica_p6_15_")), na.rm = T),            
    v_vio_escuela_tipo_apoyo_Otro        = any(c_across(starts_with("apoyo_inst_Otro_p6_15_")), na.rm = T)
    ) %>% 
  ungroup() %>% 
  mutate(
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
    
    v_vio_escuela_denuncia_en_escuela          = case-when(p6_19_1 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_mp               = case-when(p6_19_2 == "1" ~T, T ~ F),
    v_vio_escuela_denuncia_en_policia          = case-when(p6_19_3 == "1" ~T, T ~ F),     
    v_vio_escuela_denuncia_en_municipio        = case-when(p6_19_4 == "1" ~T, T ~ F),      
    
    v_vio_escuela_no_denuncia_en_verguenza        = case-when(p6_24_1  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_noLeIbanACreer   = case-when(p6_24_2  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_miedo            = case-when(p6_24_3  == "1" ~T, T ~ F),
    v_vio_escuela_no_denuncia_en_familia          = case-when(p6_24_4  == "1" ~T, T ~ F),     
    v_vio_escuela_no_denuncia_en_convencieron     = case-when(p6_24_5  == "1" ~T, T ~ F), 
    v_vio_escuela_no_denuncia_en_sinImportancia   = case-when(p6_24_6  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_costumbres       = case-when(p6_24_7  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_NoSabía          = case-when(p6_24_8  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_tiempo           = case-when(p6_24_9  == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_desconfianza     = case-when(p6_24_10 == "1" ~T, T ~ F),  
    v_vio_escuela_no_denuncia_en_otra             = case-when(p6_24_11 == "1" ~T, T ~ F),  
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
  left_join(
    foreign::read.dbf(
      paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VII_2.dbf"), as.is = T
    ) %>%
      janitor::clean_names() %>% 
      rename(n_ren = ren_m_ele)
  ) %>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_trabajo_dummy = p7_9_13
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
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
                   "quien_vid_otro"            = ~ case_when(. == "8" ~ T,T ~ F),
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p7_11_")),
      .fns =  list("freq_ahora_" = ~ case_when(
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
                   "quien_ahora_otro"             = ~ case_when(. == "8" ~ T,T ~ F),
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
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F),
      ),
      .names = "{.fn}_{.col}"),
  )  %>% 
  rowwise() %>%   
  mutate(
    v_vio_laboral_vida_todas          = any(c_across(paste0("mod_p7_9_",c(1:18))), na.rm = T),
    v_vio_laboral_vida_fisica         = any(c_across(paste0("mod_p7_9_",c(7,8,18))), na.rm = T),
    v_vio_laboral_vida_emocional      = any(c_across(paste0("mod_p7_9_",c(10,11))), na.rm = T),
    v_vio_laboral_vida_sexual         = any(c_across(paste0("mod_p7_9_",c(1,2,3,4,5,9,12,13,14,15))), na.rm = T),
    v_vio_laboral_vida_discriminacion = any(c_across(paste0("mod_p7_9_",c(6,10,16,17))), na.rm = T),
    v_vio_laboral_vida_quien_patron           = any(c_across(starts_with("quien_vid_patron_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_supervisor       = any(c_across(starts_with("quien_vid_supervisor_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_gerente          = any(c_across(starts_with("quien_vid_gerente_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_compañere        = any(c_across(starts_with("quien_vid_compañere_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_cliente          = any(c_across(starts_with("quien_vid_cliente_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_desconocido      = any(c_across(starts_with("quien_vid_desconocido_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_familiar_patron  = any(c_across(starts_with("quien_vid_familiar_patron_p7_10")), na.rm = T),
    v_vio_laboral_vida_quien_otro             = any(c_across(starts_with("quien_vid_otro_p7_10")), na.rm = T),
    v_vio_laboral_ahora_todas          = v_vio_laboral_vida_todas     & any(c_across(paste0("freq_ahora_p7_11",c(1:18))), na.rm = T),
    v_vio_laboral_ahora_fisica         = v_vio_laboral_vida_fisica    & any(c_across(paste0("freq_ahora_p7_11",c(7,8,18))), na.rm = T),
    v_vio_laboral_ahora_emocional      = v_vio_laboral_vida_emocional & any(c_across(paste0("freq_ahora_p7_11",c(10,11))), na.rm = T),
    v_vio_laboral_ahora_sexual         = v_vio_laboral_vida_sexual    & any(c_across(paste0("freq_ahora_p7_11",c(1,2,3,4,5,9,12,13,14,15))), na.rm = T),
    v_vio_laboral_ahora_discriminacion = v_vio_laboral_vida_discriminacion    & any(c_across(paste0("freq_ahora_p7_11",c(6,10,16,17))), na.rm = T),
    v_vio_laboral_ahora_quien_patron          = any(c_across(starts_with("quien_ahora_patron_p7_12")), na.rm = T),
    v_vio_laboral_ahora_quien_supervisor      = any(c_across(starts_with("quien_ahora_supervisor_p7_12")), na.rm = T),    
    v_vio_laboral_ahora_quien_gerente         = any(c_across(starts_with("quien_ahora_gerente_p7_12")), na.rm = T),      
    v_vio_laboral_ahora_quien_compañere       = any(c_across(starts_with("quien_ahora_compañere_p7_12")), na.rm = T),      
    v_vio_laboral_ahora_quien_cliente         = any(c_across(starts_with("quien_ahora_cliente_p7_12")), na.rm = T),      
    v_vio_laboral_ahora_quien_desconocido     = any(c_across(starts_with("quien_ahora_desconocido_p7_12")), na.rm = T),       
    v_vio_laboral_ahora_quien_familiar_patron = any(c_across(starts_with("quien_ahora_familiar_patron_p7_12")), na.rm = T),        
    v_vio_laboral_ahora_quien_otro            = any(c_across(starts_with("quien_ahora_otro_p7_12")), na.rm = T),        
    v_vio_laboral_ahora_donde_instalaciones      = any(c_across(starts_with("donde_ahora_instalaciones_p7_13_")), na.rm = T),
    v_vio_laboral_ahora_donde_cerca              = any(c_across(starts_with("donde_ahora_cerca_p7_13_")), na.rm = T),
    v_vio_laboral_ahora_donde_lejos              = any(c_across(starts_with("donde_ahora_lejos_p7_13_")), na.rm = T),
    v_vio_laboral_ahora_donde_transportepúblico  = any(c_across(starts_with("donde_ahora_transportepúblico_p7_13_")), na.rm = T),            
    v_vio_laboral_ahora_donde_particular         = any(c_across(starts_with("donde_ahora_particular_p7_13_")), na.rm = T),     
    v_vio_laboral_ahora_donde_Otro               = any(c_across(starts_with("donde_ahora_Otro_p7_13_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_castigaron               = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_castigaron_p7_14_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_consignaAnteJuez         = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_consignaAnteJuez_p7_14_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_recomendacion            = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_recomendacion_p7_14_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_despidocambio_ella_area  = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_despidocambio_ella_area_p7_14_")), na.rm = T),            
    v_vio_laboral_denuncia_que_sucedio_despidocambio_el_area    = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_despidocambio_el_area_p7_14_")), na.rm = T),     
    v_vio_laboral_denuncia_que_sucedio_noRatifico               = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_noRatifico_p7_14_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_nada                     = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_nada_p7_14_")), na.rm = T),
    v_vio_laboral_denuncia_que_sucedio_nosabe                   = v_vio_laboral_denuncia & any(c_across(starts_with("que_sucedio_nosabe_p7_14_")), na.rm = T),
    v_vio_laboral_tipo_apoyo_orientacion = any(c_across(starts_with("apoyo_inst_orientacion_p7_18_")), na.rm = T),
    v_vio_laboral_tipo_apoyo_legal       = any(c_across(starts_with("apoyo_inst_legal_p7_18_")), na.rm = T),
    v_vio_laboral_tipo_apoyo_psicologico = any(c_across(starts_with("apoyo_inst_psicologico_p7_18_")), na.rm = T),
    v_vio_laboral_tipo_apoyo_medica      = any(c_across(starts_with("apoyo_inst_medica_p7_18_")), na.rm = T),            
    v_vio_laboral_tipo_apoyo_Otro        = any(c_across(starts_with("apoyo_inst_Otro_p7_18_")), na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
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
    v_vio_laboral_denuncia_en_sindicato           = case-when(p7_22_1 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_policia             = case-when(p7_22_2 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_municipio           = case-when(p7_22_3 == "1" ~T, T ~ F),
    v_vio_laboral_denuncia_en_mp                  = case-when(p7_22_4 == "1" ~T, T ~ F),     
    v_vio_laboral_denuncia_en_procuraduria        = case-when(p7_22_5 == "1" ~T, T ~ F),       
    v_vio_laboral_no_denuncia_en_verguenza        = case-when(p7_27_1  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_noLeIbanACreer   = case-when(p7_27_2  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_miedo            = case-when(p7_27_3  == "1" ~T, T ~ F),
    v_vio_laboral_no_denuncia_en_familia          = case-when(p7_27_4  == "1" ~T, T ~ F),     
    v_vio_laboral_no_denuncia_en_convencieron     = case-when(p7_27_5  == "1" ~T, T ~ F), 
    v_vio_laboral_no_denuncia_en_sinImportancia   = case-when(p7_27_6  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_costumbres       = case-when(p7_27_7  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_NoSabía          = case-when(p7_27_8  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_tiempo           = case-when(p7_27_9  == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_desconfianza     = case-when(p7_27_10 == "1" ~T, T ~ F),  
    v_vio_laboral_no_denuncia_en_otra             = case-when(p7_27_11 == "1" ~T, T ~ F),  

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
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  select(
    llave, anio, cruce_vio_sex_comunitario_dummy = p8_1_13
  ) %>% 
  mutate(
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
                   "quien_vid_otro"        = ~ case_when(. == "9" ~ T,T ~ F),
      ),
      .names = "{.fn}_{.col}"),
    across(
      c(starts_with("p8_3_")),
      .fns =  list("freq_ahora_" = ~ case_when(
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
                   "apoyo_inst_Otro"          = ~ case_when(. == "5" ~ T,T ~ F),
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

  )  %>% 
  rowwise() %>%   
  mutate(
    v_vio_comunitario_vida_todas                            = any(c_across(paste0("mod_p8_1_",c(1:15))), na.rm = T),
    v_vio_comunitario_vida_fisica                           = any(c_across(paste0("mod_p8_1_",c(4,6,11))), na.rm = T),
    v_vio_comunitario_vida_emocional                        = any(c_across(paste0("mod_p8_1_",c(2,3,8,14))), na.rm = T),
    v_vio_comunitario_vida_sexual                           = any(c_across(paste0("mod_p8_1_",c(1,5,7,9,10,12,13,15))), na.rm = T),
    v_vio_comunitario_vida_quien_conocido                   = any(c_across(starts_with("quien_vid_conocido_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_amigo                      = any(c_across(starts_with("quien_vid_amigo_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_vecino                     = any(c_across(starts_with("quien_vid_vecino_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_policia                    = any(c_across(starts_with("quien_vid_policia_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_militar                    = any(c_across(starts_with("quien_vid_militar_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_sacerdote                  = any(c_across(starts_with("quien_vid_sacerdote_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_conductor                  = any(c_across(starts_with("quien_vid_conductor_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_desconodico                = any(c_across(starts_with("quien_vid_desconodico_p8_2")), na.rm = T),
    v_vio_comunitario_vida_quien_otro                       = any(c_across(starts_with("quien_vid_otro_p8_2")), na.rm = T),
    v_vio_comunitario_ahora_todas                           = v_vio_comunitario_vida_todas          & any(c_across(paste0("freq_ahora_p8_3",c(1:15))), na.rm = T),
    v_vio_comunitario_ahora_fisica                          = v_vio_comunitario_vida_fisica         & any(c_across(paste0("freq_ahora_p8_3",c(4,6,11))), na.rm = T),
    v_vio_comunitario_ahora_emocional                       = v_vio_comunitario_vida_emocional      & any(c_across(paste0("freq_ahora_p8_3",c(2,3,8,14))), na.rm = T),
    v_vio_comunitario_ahora_sexual                          = v_vio_comunitario_vida_sexual         & any(c_across(paste0("freq_ahora_p8_3",c(1,5,7,9,10,12,13,15))), na.rm = T),
    v_vio_comunitario_ahora_quien_conocido                  = any(c_across(starts_with("quien_ahora_conocido_p8_4")), na.rm = T),
    v_vio_comunitario_ahora_quien_amigo                     = any(c_across(starts_with("quien_ahora_amigo_p8_4")), na.rm = T),    
    v_vio_comunitario_ahora_quien_vecino                    = any(c_across(starts_with("quien_ahora_vecino_p8_4")), na.rm = T),      
    v_vio_comunitario_ahora_quien_policia                   = any(c_across(starts_with("quien_ahora_policia_p8_4")), na.rm = T),      
    v_vio_comunitario_ahora_quien_militar                   = any(c_across(starts_with("quien_ahora_militar_p8_4")), na.rm = T),      
    v_vio_comunitario_ahora_quien_sacerdote                 = any(c_across(starts_with("quien_ahora_sacerdote_p8_4")), na.rm = T),       
    v_vio_comunitario_ahora_quien_conductor                 = any(c_across(starts_with("quien_ahora_conductor_p8_4")), na.rm = T),        
    v_vio_comunitario_ahora_quien_desconodico               = any(c_across(starts_with("quien_ahora_desconodico_p8_4")), na.rm = T),        
    v_vio_comunitario_ahora_quien_otro                      = any(c_across(starts_with("quien_ahora_otro_p8_4")), na.rm = T),        
    v_vio_comunitario_ahora_donde_calle               = any(c_across(starts_with("donde_ahora_calle_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_mercado             = any(c_across(starts_with("donde_ahora_mercado_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_autobús             = any(c_across(starts_with("donde_ahora_autobús_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_metro               = any(c_across(starts_with("donde_ahora_metro_p8_5")), na.rm = T),            
    v_vio_comunitario_ahora_donde_metrobus            = any(c_across(starts_with("donde_ahora_metrobus_p8_5")), na.rm = T),     
    v_vio_comunitario_ahora_donde_taxi                = any(c_across(starts_with("donde_ahora_taxi_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_iglesia             = any(c_across(starts_with("donde_ahora_iglesia_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_cantina             = any(c_across(starts_with("donde_ahora_cantina_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_feria               = any(c_across(starts_with("donde_ahora_feria_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_particular          = any(c_across(starts_with("donde_ahora_particular_p8_5")), na.rm = T),
    v_vio_comunitario_ahora_donde_otro                = any(c_across(starts_with("donde_ahora_otro_p8_5")), na.rm = T),
    v_vio_comunitario_denuncia_que_sucedio_castigaron       = v_vio_comunitario_denuncia & any(c_across(starts_with("que_sucedio_castigaron_p8_18")), na.rm = T),
    v_vio_comunitario_denuncia_que_sucedio_consignaAnteJuez = v_vio_comunitario_denuncia & any(c_across(starts_with("que_sucedio_consignaAnteJuez_p8_18")), na.rm = T),
    v_vio_comunitario_denuncia_que_sucedio_noRatifico       = v_vio_comunitario_denuncia & any(c_across(starts_with("que_sucedio_noRatifico_p8_18")), na.rm = T),
    v_vio_comunitario_denuncia_que_sucedio_nada             = v_vio_comunitario_denuncia & any(c_across(starts_with("que_sucedio_nada_p8_18")), na.rm = T),
    v_vio_comunitario_denuncia_que_sucedio_nosabe           = v_vio_comunitario_denuncia & any(c_across(starts_with("que_sucedio_nosabe_p8_18")), na.rm = T),
    v_vio_comunitario_tipo_apoyo_orientacion                = any(c_across(starts_with("apoyo_inst_orientacion_p8_10")), na.rm = T),
    v_vio_comunitario_tipo_apoyo_legal                      = any(c_across(starts_with("apoyo_inst_legal_p8_10")), na.rm = T),
    v_vio_comunitario_tipo_apoyo_psicologico                = any(c_across(starts_with("apoyo_inst_psicologico_p8_10")), na.rm = T),
    v_vio_comunitario_tipo_apoyo_medica                     = any(c_across(starts_with("apoyo_inst_medica_p8_10")), na.rm = T),            
    v_vio_comunitario_tipo_apoyo_Otro                       = any(c_across(starts_with("apoyo_inst_Otro_p8_10")), na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
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
    v_vio_comunitario_denuncia_en_policia             = case-when(p8_14_1 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_municipio           = case-when(p8_14_2 == "1" ~T, T ~ F),
    v_vio_comunitario_denuncia_en_mp                  = case-when(p8_14_3 == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_verguenza        = case-when(p8_19_1  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_noLeIbanACreer   = case-when(p8_19_2  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_miedo            = case-when(p8_19_3  == "1" ~T, T ~ F),
    v_vio_comunitario_no_denuncia_en_familia          = case-when(p8_19_4  == "1" ~T, T ~ F),     
    v_vio_comunitario_no_denuncia_en_convencieron     = case-when(p8_19_5  == "1" ~T, T ~ F), 
    v_vio_comunitario_no_denuncia_en_sinImportancia   = case-when(p8_19_6  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_costumbres       = case-when(p8_19_7  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_NoSabía          = case-when(p8_19_8  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_tiempo           = case-when(p8_19_9  == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_desconfianza     = case-when(p8_19_10 == "1" ~T, T ~ F),  
    v_vio_comunitario_no_denuncia_en_otra             = case-when(p8_19_11 == "1" ~T, T ~ F),  
    
  ) %>%    
  #select(-starts_with(c("mod_", "quien_", "donde_", "freq_"))) %>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()


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
  select(
    llave, anio, cruce_vio_sex_familiar_dummy
  ) %>% 
  mutate(
    across(
      ends_with("_dummy"),
      ~ case_when(. == "1" ~ T, T ~ F)
    )
  ) %>% 
  mutate(across(
    c(starts_with("p10_1_")),
    .fns =  list("mod" = ~ case_when(. == "1" ~ T,T ~ F)),
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
    
  )  %>% 
  rowwise() %>%   
  mutate(
    v_vio_familiar_todas                            = any(c_across(paste0("mod_p10_1_",c(1:18))), na.rm = T),
    v_vio_familiar_fisica                           = any(c_across(paste0("mod_p10_1_",c(5,10,11,17))), na.rm = T),
    v_vio_familiar_emocional                        = any(c_across(paste0("mod_p10_1_",c(1,7,12))), na.rm = T),
    v_vio_familiar_sexual                           = any(c_across(paste0("mod_p10_1_",c(2,3,4,13,18))), na.rm = T),
    v_vio_familiar_patrimonial                      = any(c_across(paste0("mod_p10_1_",c(6,8,9,14,15,16))), na.rm = T),
    
    v_vio_familiar_quien_conocido                   = any(c_across(starts_with("quien_conocido_p10_2")), na.rm = T),
    v_vio_familiar_quien_amigo                      = any(c_across(starts_with("quien_amigo_p10_2")), na.rm = T),
    v_vio_familiar_quien_vecino                     = any(c_across(starts_with("quien_vecino_p10_2")), na.rm = T),
    v_vio_familiar_quien_policia                    = any(c_across(starts_with("quien_policia_p10_2")), na.rm = T),
    v_vio_familiar_quien_militar                    = any(c_across(starts_with("quien_militar_p10_2")), na.rm = T),
    v_vio_familiar_quien_sacerdote                  = any(c_across(starts_with("quien_sacerdote_p10_2")), na.rm = T),
    v_vio_familiar_quien_conductor                  = any(c_across(starts_with("quien_conductor_p10_2")), na.rm = T),
    v_vio_familiar_quien_desconodico                = any(c_across(starts_with("quien_desconodico_p10_2")), na.rm = T),
    v_vio_familiar_quien_otro                       = any(c_across(starts_with("quien_otro_p10_2")), na.rm = T),
    
    v_vio_familiar_donde_casa_propia                = any(c_across(starts_with("donde_casa_propia_p10_3")), na.rm = T),
    v_vio_familiar_donde_casa_familiar              = any(c_across(starts_with("donde_casa_familiar_p10_3")), na.rm = T),
    v_vio_familiar_donde_calle                      = any(c_across(starts_with("donde_calle_p10_3")), na.rm = T),
    v_vio_familiar_donde_mercado                    = any(c_across(starts_with("donde_mercado_p10_3")), na.rm = T),            
    v_vio_familiar_donde_lugar_publico              = any(c_across(starts_with("donde_lugar_publico_p10_3")), na.rm = T),     
    v_vio_familiar_donde_otro                       = any(c_across(starts_with("donde_otro_p10_3")), na.rm = T),
    
    v_vio_familiar_denuncia_que_sucedio_castigaron       = v_vio_familiar_denuncia & any(c_across(starts_with("que_sucedio_castigaron_p10_14")), na.rm = T),
    v_vio_familiar_denuncia_que_sucedio_consignaAnteJuez = v_vio_familiar_denuncia & any(c_across(starts_with("que_sucedio_consignaAnteJuez_p10_14")), na.rm = T),
    v_vio_familiar_denuncia_que_sucedio_noRatifico       = v_vio_familiar_denuncia & any(c_across(starts_with("que_sucedio_noRatifico_p10_14")), na.rm = T),
    v_vio_familiar_denuncia_que_sucedio_nada             = v_vio_familiar_denuncia & any(c_across(starts_with("que_sucedio_nada_p10_14")), na.rm = T),
    v_vio_familiar_denuncia_que_sucedio_nosabe           = v_vio_familiar_denuncia & any(c_across(starts_with("que_sucedio_nosabe_p10_14")), na.rm = T),
    v_vio_familiar_tipo_apoyo_orientacion                = any(c_across(starts_with("apoyo_inst_orientacion_p10_8")), na.rm = T),
    v_vio_familiar_tipo_apoyo_legal                      = any(c_across(starts_with("apoyo_inst_legal_p10_8")), na.rm = T),
    v_vio_familiar_tipo_apoyo_psicologico                = any(c_across(starts_with("apoyo_inst_psicologico_p10_8")), na.rm = T),
    v_vio_familiar_tipo_apoyo_medica                     = any(c_across(starts_with("apoyo_inst_medica_p10_8")), na.rm = T),            
    v_vio_familiar_tipo_apoyo_Otro                       = any(c_across(starts_with("apoyo_inst_Otro_p10_8")), na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
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
    
    v_vio_familiar_denuncia_en_policia             = case-when(p10_11_1 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_municipio           = case-when(p10_11_2 == "1" ~T, T ~ F),
    v_vio_familiar_denuncia_en_mp                  = case-when(p10_11_3 == "1" ~T, T ~ F),   
    
    v_vio_familiar_no_denuncia_en_verguenza        = case-when(p10_15_1  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_noLeIbanACreer   = case-when(p10_15_2  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_miedo            = case-when(p10_15_3  == "1" ~T, T ~ F),
    v_vio_familiar_no_denuncia_en_familia          = case-when(p10_15_4  == "1" ~T, T ~ F),     
    v_vio_familiar_no_denuncia_en_convencieron     = case-when(p10_15_5  == "1" ~T, T ~ F), 
    v_vio_familiar_no_denuncia_en_sinImportancia   = case-when(p10_15_6  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_costumbres       = case-when(p10_15_7  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_NoSabía          = case-when(p10_15_8  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_tiempo           = case-when(p10_15_9  == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_desconfianza     = case-when(p10_15_10 == "1" ~T, T ~ F),  
    v_vio_familiar_no_denuncia_en_otra             = case-when(p10_15_11 == "1" ~T, T ~ F),  
    
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

### TSEC_XIII - Relación actual o última relación ----
d_sec_xiii <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_XIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele) %>% 
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
  #left_join(
  #  d_sec_ix
  #) %>% 
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
  #filter(filtro_embarazo == "1") %>% 
  select(
    llave, anio, edad,
    starts_with("cruce_"), starts_with("v_"),
    starts_with("sucio"), fac_viv:upm_dis
  ) %>% 
  glimpse




group_and_wponder_by(.tabla = d_endireh_vob,
                     .variable_a_pond = cruce_escolaridad, 
                     .ponderador = fac_viv, 
                     .strata = estrato,cruce_g_edad)
    
