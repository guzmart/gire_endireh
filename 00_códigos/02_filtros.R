# 2021 ----
data_filtro_escolar <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_VII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_filtro_escolaridad = p7_1 == "1",
    )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

data_filtro_pareja <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_XIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_filtro_pareja = ifelse(is.na(p13_c_1),T,p13_c_1 != "3"),
  )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()
  
data_filtro_trabajo <- foreign::read.dbf(
  paste_inp("bd_endireh_2021_dbf/TB_SEC_VIII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  mutate(
    anio = 2021, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>%
  mutate(
    cruce_filtro_trabajo = p8_1 == "1",
  )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

filtros_2021 <- data_filtro_escolar %>% 
  left_join(data_filtro_trabajo) %>% 
  left_join(data_filtro_pareja)

# 2016 ----
data_filtro_escolar <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VI.dbf"), as.is = T
) %>%
  janitor::clean_names()%>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_filtro_escolaridad = p6_1 == "1",
  )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

data_filtro_pareja <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_XII.dbf"), as.is = T
) %>%
  janitor::clean_names()%>% 
  rename(n_ren = ren_m_ele) %>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>% 
  mutate(
    cruce_filtro_pareja = ifelse(is.na(p12_c_1),T,p12_c_1 != "3"),
  )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

data_filtro_trabajo <- foreign::read.dbf(
  paste_inp("bd_mujeres_endireh2016_sitioinegi_dbf/TB_SEC_VII.dbf"), as.is = T
) %>%
  janitor::clean_names() %>% 
  rename(n_ren = ren_m_ele)%>% 
  mutate(
    anio = 2016, 
    llave = paste0(upm, viv_sel, hogar, n_ren)
  ) %>%
  mutate(
    cruce_filtro_trabajo = p7_1 == "1",
  )%>% 
  select(
    llave, anio, starts_with(c("v_vio", "cruce_")) 
  ) %>% 
  glimpse()

filtros_2016 <- data_filtro_escolar %>% 
  left_join(data_filtro_trabajo) %>% 
  left_join(data_filtro_pareja)

d_endireh_equis_2016 <- readRDS(paste_out("01_endireh_2016_vob_no_filter_lolo.rds")) %>% 
  mutate(v_vio_denuncia_en_autoridadesLocales = v_vio_pareja_denuncia_en_autoridadesLocales) %>% 
  left_join(filtros_2016)
d_endireh_equis_2016 %>% saveRDS(paste_out("01_endireh_2016_vob_no_filter_lolo.rds"))
d_endireh_equis_2022 <- readRDS(paste_out("01_endireh_2021_vob_no_filter_lolo.rds"))%>% 
  left_join(filtros_2021)
d_endireh_equis_2022 %>% saveRDS(paste_out("01_endireh_2021_vob_no_filter_lolo.rds"))

d_endireh_equis <- bind_rows(d_endireh_equis_2016,d_endireh_equis_2022) %>% 
  mutate(v_vio_laboral_vida_discriminacion = v_vio_laboral_ahora_discriminacion) 
  
