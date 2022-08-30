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

# Recodificar PEA y PNEA
recode_pea_pnea <- function(x){
  x = case_when(
    x == 1 ~ "PEA - Ocupada", # hizo o vendió algún producto?
    x == 2 ~ "PEA - Ocupada", # ayudó en algún negocio? (familiar o de otra persona)
    x == 3 ~ "PEA - Ocupada", # crió animales o cultivó algo? (en el terreno o en casa, para autoconsumo o venta)
    x == 4 ~ "PEA - Ocupada", # ofreció algún servicio por un pago? (cargó bolsas, lavó autos, cuidó niñas(os), etc.)
    x == 5 ~ "PEA - Ocupada", # atendió su propio negocio?
    x == 6 ~ "PEA - Ocupada", # tenía trabajo, pero no trabajó? (por licencia, incapacidad o vacaciones)
    x == 7 ~ "PEA - Desocupada", # buscó trabajo?
    x == 8 ~ "PNEA - No disponible", # ¿Es estudiante?
    x == 9 ~ "PNEA - No disponible", # ¿Es jubilada(o) o pensionada(o)?
    x == 10 ~ "PNEA - No disponible", # ¿Se dedica a los quehaceres de su hogar?
    x == 11 ~ "PNEA - No disponible", # ¿Tiene alguna limitación física o mental que le impide trabajar?
    x == 12 ~ "PNEA - Disponible" # ¿No trabajó?
  )
}

# Recodificar posición de la ocupación 
recode_pos_ocu <- function(x){
  x = case_when(
    x == 1 ~ "Trabajadores subordinados y remunerados", # empleado(a)? 
    x == 2 ~ "Trabajadores subordinados y remunerados", # obrero(a)?
    x == 3 ~ "Trabajadores por cuenta propia",# jornalero(a) o peón?
    x == 4 ~ "Trabajadores por cuenta propia", # trabajador(a) por su cuenta (no contrata trabajadores/as)?
    x == 5 ~ "Empleadores", # patrón(a) o empleador(a) (contrata trabajadores/as)?
    x == 6 ~ "Trabajadores sin pago", # trabajador(a) sin pago?
  )
}

# Recodificación de tipo de localidad
recode_tipo_loc <- function(x){
  x = case_when(
    x == "C" ~ "Urbana",
    x == "R" ~ "Rural",
    x == "U" ~ "Urbana",
    T ~ NA_character_
  )
}

# Agrupar y ponderar ----
group_and_wponder_by <- function(.tabla, .variable_a_pond, .ponderador, .strata,.ids,...){
  variable_a_pond <- enquo(.variable_a_pond)
  ponderador <- enquo(.ponderador)
  estrato <- enquo(.strata)
  ids1 <- enquo(.ids)
  group_vars <- enquos(...)
  .tabla%>%
    ungroup() %>%
    filter(if_all(c(!!!group_vars,!!variable_a_pond), ~!is.na(.))) %>%
    #filter(across (starts_with("f_"), ~ifelse(is.na(.),T,. == "Sí")))  %>%
    as_survey_design(weights = !!ponderador,strata=!!estrato, ids = !!ids1) %>%
    group_by(!!!group_vars,!!variable_a_pond, .drop = T) %>%
    summarise(v_tot = survey_total(na.rm = T, vartype = "se", levels = 0.95),
              v_obs = n(),
              v_prop = survey_mean(na.rm = T, vartype = "se", levels = 0.95)) %>%
    mutate(num_pregunta = rlang::as_string(enexpr(.variable_a_pond)),
           fac = rlang::as_string(enexpr(.ponderador))) %>%
    rename(preferencia = !!variable_a_pond) %>%
    select(!!!group_vars,num_pregunta,fac, preferencia, contains(c("v_prop","v_obs", "v_tot")))
}
