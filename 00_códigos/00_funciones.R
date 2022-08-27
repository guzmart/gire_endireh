# Renombrar específicas ----
# Renombrar preguntas que contienen información de ingresos que recibe la 
# entrevistada y no dependen de la pareja o expareja.
rename_p4_8 <- function(x){
  x = case_when(
    str_ends(x, "_1") ~ "cruce_ingreso_otro_jubilación_pensión_dummy",
    str_ends(x, "_2") ~ "cruce_ingreso_otro_familiar_eeuu_dummy",
    str_ends(x, "_3") ~ "cruce_ingreso_otro_familiar_mx_dummy",
    str_ends(x, "_4") ~ "cruce_ingreso_otro_becas_escolares_hijes_dummy",
    str_ends(x, "_5") ~ "cruce_ingreso_otro_becas_escolares_ud_dummy",
    str_ends(x, "_6") ~ "cruce_ingreso_otro_programa_prospera_dummy",
    str_ends(x, "_7") ~ "cruce_ingreso_otro_programa_social_dummy",
    str_ends(x, "_8") ~ "cruce_ingreso_otro_otro_dummy",
  )
}


rename_p4_9 <- function(x){
  x = case_when(
    str_ends(x, "_1") ~ "cruce_ingreso_otro_jubilación_pensión_mensual",
    str_ends(x, "_2") ~ "cruce_ingreso_otro_familiar_eeuu_mensual",
    str_ends(x, "_3") ~ "cruce_ingreso_otro_familiar_mx_mensual",
    str_ends(x, "_4") ~ "cruce_ingreso_otro_becas_escolares_hijes_mensual",
    str_ends(x, "_5") ~ "cruce_ingreso_otro_becas_escolares_ud_mensual",
    str_ends(x, "_6") ~ "cruce_ingreso_otro_programa_prospera_mensual",
    str_ends(x, "_7") ~ "cruce_ingreso_otro_programa_social_mensual",
    str_ends(x, "_8") ~ "cruce_ingreso_otro_otro_mensual",
  )
}


# Recodificar dummy ----
recode_dummy <- function(.data, name, ...){
  groupvars <- enquos(...)
  mutate(.data, across(
    c(!!!groupvars),
    .fns =  list(
      "mod" = ~ case_when(
        . == "1" ~ T,
        T ~ F
      )
    ),
    .names = "{.fn}_{.col}"
  )) %>%    rowwise() %>%    mutate(
    !!name := any(c_across(starts_with("mod_")), na.rm = T)
  ) %>%    select(-starts_with("mod_"))
}



# Recodificaciones específicas ----
# Estado civil
recode_cruce_edo_civil <- function(x){
  x = case_when(
    x == "1" ~ "Unión libre",
    x == "2" ~ "Separada",
    x == "3" ~ "Divorciada",
    x == "4" ~ "Viuda",
    x == "5" ~ "Casada",
    x == "6" ~ "Soltera",
    T ~ NA_character_
  )
}

recode_cruce_edo_civil_2 <- function(x){
  x = case_when(
    x == "A1" ~ "Mujer casada o unida con pareja residente",
    x == "A2" ~ "Mujer casada o unida con pareja ausente temporal",
    x == "B1" ~ "Mujer separada o divorciada",
    x == "B2" ~ "Mujer viuda",
    x == "C1" ~ "Mujer soltera con novio o pareja o ex-novio o ex-pareja",
    x == "C2" ~ "Mujer soltera que nunca ha tenido pareja",
    T ~ NA_character_
  )
}

# Grupos de edad
recode_g_edad <- function(x){
  x = case_when(
    x > 96 ~ NA_character_,
    x < 20 ~ "15 a 19 años",
    x >= 20 & x < 29 ~ "20 a 29 años",
    x >= 30 & x < 39 ~ "30 a 39 años",
    x >= 40 & x < 50 ~ "40 a 99 años"
  )
}

# Escolaridad 
recode_escolaridad <- function(x){
  x = case_when(
    x == 99 ~ NA_character_,
    x == 0 ~ "Ninguno",
    x == 1 ~ "Ninguno",
    x == 2 ~ "Primaria",
    x == 3 ~ "Secundaria",
    x == 4 ~ "Preparatoria",
    x == 5 ~ "Primaria",
    x == 6 ~ "Secundaria",
    x == 7 ~ "Preparatoria",
    x == 8 ~ "Secundaria",
    x >= 9 ~ "Licenciatura o más"
  )
}

# cruce_pnea_pea, cruce_pos_ocu, cruce_tipo_loc
