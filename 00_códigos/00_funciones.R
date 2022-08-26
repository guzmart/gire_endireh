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


# Recodificar dummy
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



mutate(across(ends_with("_dummy"), ~case_when(.=="1"~T,T~F)))