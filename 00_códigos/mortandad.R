mortalidad <- read.dbf("01_datos_crudos/defunciones_base_datos_2020_dbf/defun20.dbf")
table(!is.na(mortalidad$RAZON_M), useNA = "n")
table(!is.na(mortalidad$MATERNAS),!is.na(mortalidad$RAZON_M), useNA = "n")
catalogo_enf <- read.dbf("01_datos_crudos/defunciones_base_datos_2020_dbf/CATMINDE.dbf",as.is = T)
catalogo_enf_mex <- read.dbf("01_datos_crudos/defunciones_base_datos_2020_dbf/LISTAMEX.dbf",as.is = T)

Encoding(catalogo_enf$DESCRIP) <-"latin1"
enc2native(catalogo_enf$DESCRIP)


(38173-18000)/2+(59487.98)/2
