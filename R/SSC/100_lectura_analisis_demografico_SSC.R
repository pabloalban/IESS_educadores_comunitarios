message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo analisis demografico SSC del IESS' )

# Carga de datos
file <- paste0( parametros$Data_seg, 'IESS_SSC_analisis_demografico.xlsx' )

# Jefes de familia ---------------------------------------------------------------------------------
message( '\tLeyendo jefes de familia del  SSC' )
col_nom <- c( 'Anio', 'Male', 'Female','Afiliados_activos', 'Porcentaje_incremento')
col_tip <- c( 'character', 'numeric', 'numeric')

afi_hist_ssc <-read_excel( file,
                           sheet="Afi_historico",
                           col_names=TRUE,
                           guess_max = 24000, range = "A1:E11" )
afi_hist_ssc <- as.data.table( afi_hist_ssc)[1:10,]
setnames(afi_hist_ssc, col_nom )

col_nom <- c( 'Anio', 'Male', 'Female','Afiliados_inactivos', 'Porcentaje_incremento')

afi_inac_hist_ssc <-read_excel( file,
                           sheet="Afi_historico",
                           col_names=TRUE,
                           guess_max = 24000, range = "G1:K11" )
afi_inac_hist_ssc <- as.data.table( afi_inac_hist_ssc)[1:10,]
setnames(afi_inac_hist_ssc, col_nom )

# Jefes de familia por edad y sexo ---------------------------------------------------------------
message( '\tLeyendo Jefes de familia por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n')
afi_edad_sexo_ssc <-read_excel( file,
                           sheet="Afi_edad_sexo",
                           col_names=TRUE,
                           guess_max = 24000 )
afi_edad_sexo_ssc <- as.data.table( afi_edad_sexo_ssc)
setnames(afi_edad_sexo_ssc, col_nom )


# Ingresos de los Jefes de familia ----------------------------------------------------------------
message( '\tLeyendo Ingresos de los Jefes de familia' )
col_nom <- c( 'anio', 'Male_A', 'Female_A','masa_anual', 'Male_D', 'Female_D','masa_diciembre', 'cre_anual', 'por')
masa_ssc <-read_excel( file,
                                sheet="Masa_Salarial",
                                col_names=TRUE,
                                guess_max = 24000 )
masa_ssc <- as.data.table( masa_ssc )
setnames( masa_ssc, col_nom )

# Jubilacion por vejez---------------------------------------------------
message( '\tLeyendo jubilacion vejez SSC del IESS' )
col_nom <- c( 'tipo','Anio', 'Jubilados_vejez', 'Tasa_crecimiento_pob',
              'Beneficio_pagado_anual','Tasa_crecimiento_ben', 'pen_prom', 'por_cre')

jub_vejez_ssc <- read_excel(file,
                            sheet="Jub_Vejez",
                            col_names=TRUE, guess_max = 24000)
jub_vejez_ssc <- as.data.table( jub_vejez_ssc)
setnames( jub_vejez_ssc, col_nom )

# Pensionistas de Vejez del SSC por edad y sexo ---------------------------------------------------------------
message( '\tLeyendo Pensionistas de Vejez del SSC por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n')
jub_vejez_edad_sexo_ssc <-read_excel( file,
                                sheet="Jub_vejez_edad_sexo",
                                col_names=TRUE,
                                guess_max = 24000 )
jub_vejez_edad_sexo_ssc <- as.data.table( jub_vejez_edad_sexo_ssc )
setnames(jub_vejez_edad_sexo_ssc, col_nom )

# Crecimiento de la población jubilada por invalidez----------------------
message( '\tLeyendo jubilacion por invalidez SSC del IESS' )
col_nom <- c( 'tipo','Anio', 'Jubilados_invalidez', 'Tasa_crecimiento_pob',
              'Beneficio_pagado_anual','Tasa_crecimiento_ben', 'pen_prom', 'por_cre')

jub_invalidez_ssc <-read_excel( file, sheet="Jub_invalidez",
                                col_names=TRUE,guess_max = 24000)
jub_invalidez_ssc <- as.data.table( jub_invalidez_ssc)
setnames(jub_invalidez_ssc, col_nom )

#Leyendo Pensionistas de invalidez del SSC por edad y sexo ---------------------------------------
message( '\tLeyendo Pensionistas de invalidez del SSC por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n' )
col_tip <- c( 'character', 'numeric', 'numeric' )

jub_invalidez_edad_sexo_ssc <-read_excel( file,
                                      sheet="Jub_invalidez_edad_sexo",
                                      col_names=TRUE,guess_max = 24000)

jub_invalidez_edad_sexo_ssc <- as.data.table( jub_invalidez_edad_sexo_ssc )
setnames( jub_invalidez_edad_sexo_ssc, col_nom )

# Leyendo dependientes del SSC ---------------------------------------------------------------------
message( '\tLeyendo dependientes del SSC' )
col_nom <- c( 'tipo','Anio', 'hijos', 'conyuge', 'otros', 'Dependientes', 'Porcentaje_de_crecimiento')

dependientes_ssc <-read_excel( file,
                               sheet="Dependientes",
                               col_names=TRUE,guess_max = 24000)
dependientes_ssc <- as.data.table( dependientes_ssc)[1:27,]
setnames(dependientes_ssc, col_nom )

#Leyendo dependientes del SSC  por edad y sexo -----------------------------------------------------
message( '\tLeyendo dependientes del SSC  por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n' )

dep_edad_sexo <-read_excel( file,
                            sheet="Dep_Edad_Sexo",
                            col_names=TRUE,guess_max = 24000)
dep_edad_sexo <- as.data.table( dep_edad_sexo )
setnames( dep_edad_sexo, col_nom )

#Leyendo dependientes hijos del SSC  por edad y sexo -----------------------------------------------
message( '\tLeyendo dependientes hijos del SSC  por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n' )

dep_edad_sexo_hijos <-read_excel( file,
                            sheet="Dep_Edad_Sexo_hijos",
                            col_names=TRUE,guess_max = 24000)
dep_edad_sexo_hijos <- as.data.table( dep_edad_sexo_hijos )
setnames( dep_edad_sexo_hijos, col_nom )

#Leyendo dependientes conyuges del SSC  por edad y sexo --------------------------------------------
message( '\tLeyendo dependientes conyuges del SSC  por edad y sexo' )
col_nom <- c( 'sexo', 'edad', 'n' )

dep_edad_sexo_cony <-read_excel( file,
                                  sheet="Dep_Edad_Sexo_conyuge",
                                  col_names=TRUE,guess_max = 24000)
dep_edad_sexo_cony <- as.data.table( dep_edad_sexo_cony )
setnames( dep_edad_sexo_cony, col_nom )

# Leyendo dependientes del SSC por parentesco-------------------------------------------------------
message( '\tLeyendo dependientes del SSC por parentesco' )
col_nom <- c( 'Parentesco', 'Numero', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric')

parentesco_ssc <-read_excel( file,
                             sheet="Parentesco",
                             col_names=TRUE,guess_max = 24000)
parentesco_ssc <- as.data.table(parentesco_ssc)[1:18,]
setnames(parentesco_ssc, col_nom )

message( '\tLeyendo dependientes del SSC por zona del IESS' )
col_nom <- c( 'Zona', 'Numero', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric')

zona_ssc <-read_excel(file,
                      sheet="Zona",
                      col_names=TRUE,guess_max = 24000)
zona_ssc <- as.data.table(zona_ssc)[1:8,]
setnames(zona_ssc, col_nom )

#Leyendo dependientes del SSC por provincia del IESS -----------------------------------------------
message( '\tLeyendo dependientes del SSC por provincia del IESS' )
col_nom <- c( 'Provincia', 'Numero', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric')

prov_ssc <-read_excel( file,
                       sheet="Provincia_dep",
                       col_names=TRUE,guess_max = 24000)
prov_ssc  <- as.data.table(prov_ssc )
setnames(prov_ssc , col_nom )

#Leyendo jefes del SSC por organización del IESS ---------------------------------------------------
message( '\tLeyendo jefes del SSC por organización del IESS' )
col_nom <- c( 'Provincia', 'Numero', 'Porcentaje')
col_tip <- c( 'character', 'numeric', 'numeric')

prov_jef_ssc <-read_excel( file,
                       sheet="Provincia_jefes",
                       col_names=TRUE,guess_max = 24000)
prov_jef_ssc  <- as.data.table( prov_jef_ssc )
setnames( prov_jef_ssc , col_nom )

#Leyendo jefes de familia activos e inactivos ------------------------------------------------------
message( '\tLeyendo jefes de familia activos e inactivos' )
col_nom <- c('Des','Descp',	'Total', 'Por')

prop_jef_act_inac <-read_excel( file,
                                sheet="Por_act_inac",
                                col_names=TRUE,
                                guess_max = 24000)
prop_jef_act_inac <- as.data.table( prop_jef_act_inac )
setnames( prop_jef_act_inac, col_nom )

#Leyendo porporción de jefes de familia masculino por tipo de riesgos ------------------------------
message( '\tLeyendo porporción de jefes de familia masculino por tipo de riesgos' )
col_nom <- c('riesgo', 'n_jef', 'por')

past_jef_male_tipo_riesgo <- read_excel( file,
                                sheet = "Jefes_male_tipo_riesgo",
                                col_names=TRUE,
                                guess_max = 24000, range = "I1:K5")
past_jef_male_tipo_riesgo <- as.data.table( past_jef_male_tipo_riesgo )
setnames( past_jef_male_tipo_riesgo, col_nom )

#Leyendo jefes de familia masculino por edad, años de imposiciones y tipo de riesgos ---------------
message( '\tLeyendo jefes de familia masculino por edad, años de imposiciones y tipo de riesgos' )
col_nom <- c('sexo', 'edad', 'impo_anios', 'impo', 'n_afi', 'riesgo1', 'riesgo3')

jef_male_tipo_riesgo <- read_excel( file,
                                         sheet = "Jefes_male_tipo_riesgo",
                                         col_names=TRUE,
                                         guess_max = 24000, range = "A1:G49435")
jef_male_tipo_riesgo <- as.data.table( jef_male_tipo_riesgo )
setnames( jef_male_tipo_riesgo, col_nom )

#Leyendo jefes de familia masculino por número de cotizaciones -------------------------------------
message( '\tLeyendo jefes de familia masculino por número de cotizaciones' )
col_nom <- c('sexo','cot', 'num')

Jefes_male_num_cotiz <- read_excel( file,
                                    sheet = "Jefes_male_num_cotiz",
                                    col_names=TRUE,
                                    guess_max = 24000)
Jefes_male_num_cotiz <- as.data.table( Jefes_male_num_cotiz )
setnames( Jefes_male_num_cotiz, col_nom )

#Leyendo porporción de jefes de familia femenino por tipo de riesgos -------------------------------
message( '\tLeyendo porporción de jefes de familia femenino por tipo de riesgos' )
col_nom <- c('riesgo', 'n_jef', 'por')

past_jef_female_tipo_riesgo <- read_excel( file,
                                         sheet = "Jefes_female_tipo_riesgo",
                                         col_names=TRUE,
                                         guess_max = 24000, range = "I1:K5")
past_jef_female_tipo_riesgo <- as.data.table( past_jef_female_tipo_riesgo )
setnames( past_jef_female_tipo_riesgo, col_nom )

#Leyendo jefes de familia femenino por edad, años de imposiciones y tipo de riesgos ----------------
message( '\tLeyendo jefes de familia femenino por edad, años de imposiciones y tipo de riesgos' )
col_nom <- c('sexo', 'edad', 'impo_anios', 'impo', 'n_afi', 'riesgo1', 'riesgo3')

jef_female_tipo_riesgo <- read_excel( file,
                                    sheet = "Jefes_female_tipo_riesgo",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "A1:G46981")
jef_female_tipo_riesgo <- as.data.table( jef_female_tipo_riesgo )
setnames( jef_female_tipo_riesgo, col_nom )

#Leyendo jefes de familia femenino por número de cotizaciones -------------------------------------
message( '\tLeyendo jefes de familia femenino por número de cotizaciones' )
col_nom <- c( 'sexo','cot', 'num')

Jefes_female_num_cotiz <- read_excel( file,
                                    sheet = "Jefes_female_num_cotiz",
                                    col_names=TRUE,
                                    guess_max = 24000)
Jefes_female_num_cotiz <- as.data.table( Jefes_female_num_cotiz )
setnames( Jefes_female_num_cotiz, col_nom )

#INACTIVOS
#Leyendo porporción de jefes de familia masculino inactivos por tipo de riesgos --------------------
message( '\tLeyendo porporción de jefes de familia masculino inactivos por tipo de riesgos' )
col_nom <- c('riesgo', 'n_jef', 'por')

past_jef_male_tipo_riesgo_inac <- read_excel( file,
                                         sheet = "Jefes_male_tipo_riesgo_inac",
                                         col_names=TRUE,
                                         guess_max = 24000, range = "I1:K5")
past_jef_male_tipo_riesgo_inac <- as.data.table( past_jef_male_tipo_riesgo_inac )
setnames( past_jef_male_tipo_riesgo_inac, col_nom )

#Leyendo jefes de familia masculino inactivos por edad, años de imposiciones y tipo de riesgos -----
message( '\tLeyendo jefes de familia masculino inactivos por edad, años de imposiciones y tipo de riesgos' )
col_nom <- c('sexo', 'edad', 'impo_anios', 'impo', 'n_afi', 'riesgo1', 'riesgo3')

jef_male_tipo_riesgo_inac <- read_excel( file,
                                    sheet = "Jefes_male_tipo_riesgo_inac",
                                    col_names=TRUE,
                                    guess_max = 24000, range = "A1:G45218")
jef_male_tipo_riesgo_inac <- as.data.table( jef_male_tipo_riesgo_inac )
setnames( jef_male_tipo_riesgo_inac, col_nom )

#Leyendo jefes de familia masculino inactivos por número de cotizaciones ---------------------------
message( '\tLeyendo jefes de familia masculino inactivos por número de cotizaciones' )
col_nom <- c( 'sexo','cot', 'num')

Jefes_male_num_cotiz_inac <- read_excel( file,
                                    sheet = "Jefes_male_num_cotiz_inac",
                                    col_names=TRUE,
                                    guess_max = 24000)
Jefes_male_num_cotiz_inac <- as.data.table( Jefes_male_num_cotiz_inac )
setnames( Jefes_male_num_cotiz_inac, col_nom )

#Leyendo porporción de jefes de familia femenino inactivos por tipo de riesgos ---------------------
message( '\tLeyendo porporción de jefes de familia femenino inactivos por tipo de riesgos' )
col_nom <- c('riesgo', 'n_jef', 'por')

past_jef_female_tipo_riesgo_inac <- read_excel( file,
                                           sheet = "Jefes_female_tipo_riesgo_inac",
                                           col_names=TRUE,
                                           guess_max = 24000, range = "I1:K5")
past_jef_female_tipo_riesgo_inac <- as.data.table( past_jef_female_tipo_riesgo_inac )
setnames( past_jef_female_tipo_riesgo_inac, col_nom )

#Leyendo jefes de familia femenino inactivos  por edad, años de imposiciones y tipo de riesgos -----
message( '\tLeyendo jefes de familia femenino inactivos por edad, años de imposiciones y tipo de riesgos' )
col_nom <- c('sexo', 'edad', 'impo_anios', 'impo', 'n_afi', 'riesgo1', 'riesgo3')

jef_female_tipo_riesgo_inac <- read_excel( file,
                                      sheet = "Jefes_female_tipo_riesgo_inac",
                                      col_names=TRUE,
                                      guess_max = 24000, range = "A1:G42017")
jef_female_tipo_riesgo_inac <- as.data.table( jef_female_tipo_riesgo_inac )
setnames( jef_female_tipo_riesgo_inac, col_nom )

#Leyendo jefes de familia femenino inactivos  por número de cotizaciones ---------------------------
message( '\tLeyendo jefes de familia femenino inactivos por número de cotizaciones' )
col_nom <- c('sexo','cot', 'num')

Jefes_female_num_cotiz_inac <- read_excel( file,
                                      sheet = "Jefes_female_num_cotiz_inac",
                                      col_names=TRUE,
                                      guess_max = 24000)
Jefes_female_num_cotiz_inac <- as.data.table( Jefes_female_num_cotiz_inac )
setnames( Jefes_female_num_cotiz_inac, col_nom ) 

#Leyendo edad promedio de pensionistas del SSC -----------------------------------------------------
message( '\tLeyendo edad promedio de pensionistas del SSC' )
col_nom <- c('tipo', 'male', 'female', 'total')

edad_prom_pen <- read_excel( file,
                             sheet = "edad_prom_pens",
                             col_names= TRUE,
                             guess_max = 24000)
edad_prom_pen <- as.data.table( edad_prom_pen )
setnames( edad_prom_pen, col_nom ) 

# Leyendo tiempo de aportación de jefes de familia del SSC -----------------------------------------
message( '\tLeyendo tiempo de aportación de jefes de familia del SSC' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')


jef_tiempo_aportacion <- read_excel( file,
                                     sheet="tiempo_aportacion_ssc_total",
                                    col_names=TRUE, guess_max = 24000)

jef_tiempo_aportacion <- as.data.table( jef_tiempo_aportacion )[2:31,]
setnames( jef_tiempo_aportacion, col_nom )

# Leyendo tiempo de aportación de jefes de familia hombres del SSC ---------------------------------
message( '\tLeyendo tiempo de aportación de jefes de familia hombres del SSC' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')

jef_tiempo_aportacion_male <- read_excel( file,
                                     sheet="tiempo_aportacion_ssc_male",
                                     col_names=TRUE, guess_max = 24000)

jef_tiempo_aportacion_male <- as.data.table( jef_tiempo_aportacion_male )[2:31,]
setnames( jef_tiempo_aportacion_male, col_nom )

# Leyendo tiempo de aportación de jefes de familia mujeres del SSC ---------------------------------
message( '\tLeyendo tiempo de aportación de jefes de familia mujeres del SSC' )
col_nom <- c( 'edad', '[0,5]', '[5,10]', '[10,15]', '[15,20]', '[20,25]', '[25,30]', '[30,35]', '[35,40]', '[40,45]', '[45,50]', '>50','Total')

jef_tiempo_aportacion_female <- read_excel( file,
                                          sheet="tiempo_aportacion_ssc_female",
                                          col_names=TRUE, guess_max = 24000)

jef_tiempo_aportacion_female <- as.data.table( jef_tiempo_aportacion_female )[2:31,]
setnames( jef_tiempo_aportacion_male, col_nom )

# Leyendo nuevos pensionistas del SSC --------------------------------------------------------------
message( '\tLeyendo nuevos pensionistas del SSC ' )
col_nom <- c( 't', 'tipo', 'sexo', 'num')

new_pens <- read_excel( file,
                        sheet="nuevos_pensionistas_ssc",
                        col_names=TRUE, 
                        guess_max = 24000)

new_pens <- as.data.table( new_pens)
setnames( new_pens, col_nom )

# Leyendo atenciones médicas del SSC --------------------------------------------------------------
message( '\tLeyendo atenciones médicas del SSC' )
col_nom <- c( 'mes', 'A2016', 'A2017', 'A2018', 'A2019', 'A2020')

aten_med_mes <- read_excel( file,
                        sheet="aten_medicas_mes",
                        col_names=TRUE, 
                        guess_max = 24000)

aten_med_mes <- as.data.table( aten_med_mes)
setnames( aten_med_mes, col_nom )

# Leyendo atenciones médicas por año y día de la semana del SSC ------------------------------------
message( '\tLeyendo atenciones médicas por año y día de la semana del SSC ' )
col_nom <- c( 'anio', 'mes', 'lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado', 'domingo',
              'total')

aten_med_dia <- read_excel( file,
                            sheet="aten_medicas_dia",
                            col_names=TRUE, 
                            guess_max = 24000)

aten_med_dia <- as.data.table( aten_med_dia )
setnames( aten_med_dia, col_nom )

# Leyendo atenciones médicas por año, edad y sexo del SSC ----- ------------------------------------
message( '\tLeyendo atenciones médicas por año, edad y sexo del SSC' )
col_nom <- c( 'anio', 'sexo', 'edad', 'n_pac')

aten_med_edad_sexo <- read_excel( file,
                            sheet="aten_medicas_edad_sexo",
                            col_names=TRUE, 
                            guess_max = 24000)

aten_med_edad_sexo <- as.data.table( aten_med_edad_sexo)
setnames( aten_med_edad_sexo, col_nom )

# Leyendo atenciones médicas por año y edad simple -------------------------------------------------
message( '\tLeyendo atenciones médicas por año y edad simple' )
col_nom <- c( 'edad', 'A2016', 'A2017', 'A2018', 'A2019', 'A2020')

aten_med_edad <- read_excel( file,
                            sheet="aten_medicas_edad",
                            col_names=TRUE, 
                            guess_max = 24000)

aten_med_edad <- as.data.table( aten_med_edad )
setnames( aten_med_edad, col_nom )

# Leyendo atenciones médicas por año y provincia - -------------------------------------------------
message( '\tLeyendo atenciones médicas por año y provincia' )
col_nom <- c( 'prov', 'A2016freq', 'A2016P', 'A2017freq', 'A2017P', 'A2018freq', 'A2018P', 'A2019freq', 'A2019P',
              'A2020freq', 'A2020P')

aten_med_prov <- read_excel( file,
                             sheet="aten_medicas_prov",
                             col_names=TRUE, 
                             guess_max = 24000)

aten_med_prov <- as.data.table( aten_med_prov )
setnames( aten_med_prov, col_nom )
aten_med_prov  <- aten_med_prov[ -1 ]

# Leyendo atenciones médicas de los dispensarios por zonas------------------------------------------
message( '\tLeyendo atenciones médicas de los dispensarios por zonas' )
col_nom <- c( 'zona', 'n' )

aten_med_zonas <- read_excel( file,
                             sheet="dispensarios_zonas",
                             col_names=TRUE, 
                             guess_max = 24000)

aten_med_zonas <- as.data.table( aten_med_zonas )
setnames( aten_med_zonas, col_nom )

# Leyendo atenciones médicas de los dispensarios por año y sexo-------------------------------------
message( '\tLeyendo atenciones médicas de los dispensarios por año y sexo' )
col_nom <- c( 'zona', 'H16', 'M16', 'ND16', 'T16', 'H17', 'M17', 'ND17','T17', 'H18', 'M18', 'ND18','T18',
              'H19', 'M19', 'ND19','T19', 'H20', 'M20', 'ND20','T20')

aten_med_zonas_sexo <- read_excel( file,
                              sheet="aten_medicas_zonas",
                              col_names=TRUE, 
                              guess_max = 24000)

aten_med_zonas_sexo <- as.data.table( aten_med_zonas_sexo )
setnames( aten_med_zonas_sexo, col_nom )
aten_med_zonas_sexo <- aten_med_zonas_sexo[ -1 ]

# Leyendo atenciones médicas de los dispensarios por año y CIE10------------------------------------
message( '\tLeyendo atenciones médicas de los dispensarios por año y CIE-10' )
col_nom <- c( 'cap', 'cod', 'cie', 'A2016', 'P2016', 'A2017', 'P2017', 'A2018', 'P2018', 'A2019', 'P2019',
              'A2020', 'P2020')

aten_med_cie <- read_excel( file,
                                   sheet="aten_medicas_cie10",
                                   col_names=TRUE, 
                                   guess_max = 24000)

aten_med_cie <- as.data.table( aten_med_cie )
setnames( aten_med_cie, col_nom )

# Leyendo auxilio de funerales del SSSC-------------------------------------------------------------
message( '\tLeyendo auxilio de funerales del SSSC' )

col_nom <- c( 't', 'N', 'tipo'  )
muertes <-read_excel( file,
                     sheet = "Funerales",
                     col_names = TRUE,
                     guess_max = 24000
                    )
muertos <- as.data.table(muertes)

setnames( muertos, col_nom )
muertos <- muertos[ t >= 2011]

fune <- merge( muertos[ tipo=="afi" | tipo=="pen", list( NAP = sum(N) ), by=t],
               muertos[ tipo=="aux", list(Naux=N), by=t], by=c('t') )

fune <- fune[, list( var_est= Naux / NAP), by=t]

tab_var_proj <- data.table( t = seq( 2012, 2020 + parametros$horizonte, 1) )

var_smooth_model <- lm( var_est ~ bs( t, df = 1, degree = 1 ,  Boundary.knots = range( t )
                                     ),
                        data = fune[ t >= 2013 & t <= 2018] )
# summary(var_smooth_model)
tab_var_proj[ , var := predict( object = var_smooth_model,
                                newdata = tab_var_proj ) ]
plot(fune$t, fune$var_est);points(tab_var_proj$t,tab_var_proj$var, col="red")
plot(tab_var_proj$t,tab_var_proj$var, col="red")

# Guardando ---------------------------------------------------------------

lista <- c('afi_hist_ssc', 'afi_edad_sexo_ssc', 'masa_ssc', 'jub_vejez_ssc', 'jub_vejez_edad_sexo_ssc',
           'jub_invalidez_ssc', 'jub_invalidez_edad_sexo_ssc', 'dependientes_ssc', 'dep_edad_sexo',
           'dep_edad_sexo_hijos', 'dep_edad_sexo_cony','parentesco_ssc','zona_ssc', 'prov_ssc', 'prov_jef_ssc',
           'afi_inac_hist_ssc', 'prop_jef_act_inac', 'past_jef_male_tipo_riesgo', 'jef_male_tipo_riesgo',
           'Jefes_male_num_cotiz', 'past_jef_female_tipo_riesgo', 'jef_female_tipo_riesgo',
           'Jefes_female_num_cotiz',
           'past_jef_male_tipo_riesgo_inac', 'jef_male_tipo_riesgo_inac','Jefes_male_num_cotiz_inac',
           'past_jef_female_tipo_riesgo_inac', 'jef_female_tipo_riesgo_inac','Jefes_female_num_cotiz_inac',
           'edad_prom_pen',
           'jef_tiempo_aportacion', 'jef_tiempo_aportacion_male', 'jef_tiempo_aportacion_female', 
           'new_pens',
           #Perfil epidemiológico
           'aten_med_mes', 'aten_med_dia', 'aten_med_edad_sexo','aten_med_edad', 'aten_med_prov',
           'aten_med_zonas', 'aten_med_zonas_sexo', 'aten_med_cie', 'muertes', 'tab_var_proj')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) )


###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

