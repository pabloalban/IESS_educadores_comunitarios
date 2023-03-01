message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura afiliados activos 2005-2020' )

# Notas --------------------------------------------------------------------------------------------
# Se ha cambiado los nombres "incremento" a "variación" pues no siempre crece, a veces baja. 
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_demografico.RData' ) ) 
# Tabla afiliados activos 2005-2020 a diciembre-----------------------------------------------------
aux <- copy( pob_afi_ini )

aux[ , Tasa:=100*Tasa]
aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pob_afi_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla masa salarial 2005-2020 --------------------------------------------------------------------
message( '\tTabla masa salarial inicial' )

aux <- copy( masa_salarial_ini )
aux[ ,Tasa:=100*Tasa]
aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
        print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla pensionistas de vejez 2012-2020------------------------------------------------------------
message( '\tPensionistas de vejez inicial' )

aux <- copy( jub_vejez )
aux <- aux[, .(anio,jub_vjz,creci,benef,creci_porcen,`Salario Promedio`,tasa_variacion)]

aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , creci:=100*creci]
aux[ , anio := as.character( anio ) ]
baux <- aux[ , list( anio, jub_vjz, creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_vejez_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de vejez hombres 2012-2020------------------------------------------------------------
message( '\tPensionistas de vejez inicial hombres' )

aux <- copy( jub_vejez_h )
aux <- aux[, .(anio,jub_vjz,creci,benef,creci_porcen, `Salario Promedio`,tasa_variacion)]

aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , creci:=100*creci]
aux[ , anio := as.character( anio ) ]
baux_h <- aux[ , list( jub_vjz_h = jub_vjz, creci_h = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_vejez_ini_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de vejez mujeres 2012-2020------------------------------------------------------------
message( '\tPensionistas de vejez inicial mujeres' )

aux <- copy( jub_vejez_m )
aux <- aux[, .(anio,jub_vjz,creci,benef,creci_porcen,`Salario Promedio`,tasa_variacion)]

aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , creci:=100*creci]
aux[ , anio := as.character( anio ) ]
baux_m <- aux[ , list( jub_vjz_m = jub_vjz, creci_m = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_vejez_ini_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Unificaion pensionistas de vejez 
aux_jv <- cbind( baux, baux_h, baux_m )
aux_xtable <- xtable( aux_jv, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_vejez_unificado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla pensionistas de invalidez 2012-2020 --------------------------------------------------------
message( '\tPensionistas de invalidez inicial' )

aux <- copy( jub_inv )
#aux <- aux[, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci :=100*creci ]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_ji <- aux[ , list( anio, jub_invlz, creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_invalidez_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla pensionistas de invalidez hombres 2012-2020 --------------------------------------------------------
message( '\tPensionistas de invalidez inicial' )

aux <- copy( jub_inv_h )
#aux <- aux[, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci :=100*creci ]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_ji_h <- aux[ , list( jub_invlz_h = jub_invlz, creci_h = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_invalidez_ini_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla pensionistas de invalidez mujeres 2012-2020 --------------------------------------------------------
message( '\tPensionistas de invalidez inicial' )

aux <- copy( jub_inv_m )
#aux <- aux[, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci :=100*creci ]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_ji_m <- aux[ , list( jub_invlz_m = jub_invlz, creci_m = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_invalidez_ini_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Jubilados invalidez unificado
aux_ji_unificado <- cbind( aux_ji, aux_ji_h, aux_ji_m )
aux_xtable <- xtable( aux_ji_unificado, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_invalidez_unificado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla pensionistas de discapacidad 2014-2020-----------------------------------------------------
message( '\tPensionistas de discapacidad inicial' )

aux <- copy( jub_vjz_especial )
#aux <- aux[Tipo=="P.Discapacidad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_jd <- aux[ , list( anio, jub_dsc, creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_discapacidad_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de discapacidad hombres 2014-2020-----------------------------------------------------
message( '\tPensionistas de discapacidad inicial' )

aux <- copy( jub_vjz_especial_h )
#aux <- aux[Tipo=="P.Discapacidad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_jd_h <- aux[ , list( jub_dsc_h = jub_dsc, creci_h = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_discapacidad_ini_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla pensionistas de discapacidad mujeres 2014-2020-----------------------------------------------------
message( '\tPensionistas de discapacidad inicial' )

aux <- copy( jub_vjz_especial_m )
#aux <- aux[Tipo=="P.Discapacidad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_jd_m <- aux[ , list( jub_dsc_m = jub_dsc, creci_m = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_discapacidad_ini_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Jubilados Discapacidad unificado
aux_jd_unificado <- cbind( aux_jd, aux_jd_h, aux_jd_m)
aux_xtable <- xtable( aux_jd_unificado, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_discapacidad_unificado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla pensionistas de viudedad 2012-2020---------------------------------------------------------
message( '\tPensionistas por viudedad inicial' )

aux <- copy( pen_viud )
#aux <- aux[Tipo=="P.Viudedad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_viu <- aux[ , list( anio, pension, creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_viudedad_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de viudedad hombre 2012-2020---------------------------------------------------------
message( '\tPensionistas por viudedad inicial' )

aux <- copy( pen_viud_h )
#aux <- aux[Tipo=="P.Viudedad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_viu_h <- aux[ , list( pension_h = pension, creci_h = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_viudedad_ini_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de viudedad mujer 2012-2020---------------------------------------------------------
message( '\tPensionistas por viudedad inicial' )

aux <- copy( pen_viud_m )
#aux <- aux[Tipo=="P.Viudedad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_viu_m <- aux[ , list( pension_m = pension, creci_m = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_viudedad_ini_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Pensionistas de viuded unificado
aux_viu_unificado <- cbind( aux_viu, aux_viu_h, aux_viu_m )
aux_xtable <- xtable( aux_viu_unificado, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_viudedad_unificado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla pensionistas de orfandad 2012-2020---------------------------------------------------------
message( '\tPensionistas por orfandad inicial' )

aux <- copy( pen_orf )
#aux <- aux[Tipo=="P. Orfandad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_or <- aux[ , list( anio, pension, creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_orfandad_ini', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de orfandad hombres 2012-2020---------------------------------------------------------
message( '\tPensionistas por orfandad inicial' )

aux <- copy( pen_orf_h )
#aux <- aux[Tipo=="P. Orfandad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_or_h <- aux[ , list(  pension_h = pension, creci_h = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_orfandad_ini_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )
# Tabla pensionistas de orfandad mujeres 2012-2020---------------------------------------------------------
message( '\tPensionistas por orfandad inicial' )

aux <- copy( pen_orf_m )
#aux <- aux[Tipo=="P. Orfandad"][, .(anio,Numero,Tasa_n,Beneficio,Tasa_beneficio)]

aux[ , creci:=100*creci]
aux[ , creci_porcen:=100*creci_porcen]
aux[ , tasa_variacion:=100*tasa_variacion]
aux[ , anio := as.character( anio ) ]
aux_or_m <- aux[ , list(  pension_m = pension, creci_m = creci ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_orfandad_ini_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Pensionistas orfandad unificado
aux_or_unificado <- cbind( aux_or, aux_or_h, aux_or_m )
aux_xtable <- xtable( aux_or_unificado, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pen_orfandad_unificado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

#Si la data no necesariamente caracter

# message( '\tTiempo de aportación de afiliados' )
# # Tabla tiempo de aportación de afiliados ------------------------------------------------------------------------------------
# aux <-copy(afi_tiempo_aportacion)
# mdat<-matrix(rep(c(rep(0,14),0,rep(2,13)),18), nrow = 36, ncol=14, byrow=TRUE)
# 
# aux_xtable <- xtable( aux, digits = mdat)
# 
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = nrow(aux_xtable)-2,
#        sanitize.text.function = identity )


message( '\tTiempo de aportación de afiliados' )
# Tabla tiempo de aportación de afiliados ------------------------------------------------------------------------------------
aux <-copy(afi_tiempo_aportacion)

aux_xtable <- xtable( aux)
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

message( '\tTiempo de aportación de afiliados hombres' )
# Tabla tiempo de aportación de afiliados ------------------------------------------------------------------------------------
aux <-copy(afi_tiempo_aportacion_h)

aux_xtable <- xtable( aux)
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_h', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

message( '\tTiempo de aportación de afiliados mujeres' )
# Tabla tiempo de aportación de afiliados ------------------------------------------------------------------------------------
aux <-copy(afi_tiempo_aportacion_m)

aux_xtable <- xtable( aux)

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_tiempo_aportacion_m', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = nrow(aux_xtable)-2,
       sanitize.text.function = identity )

# message( '\tPensiones mínimas' )
# 
# # Carga de datos -----------------------------------------------------------------------------------
# load( file = paste0( parametros$RData_seg, 'IESS_IVM_pensiones_max_min.RData' ) )
# 
# # Tabla pensionistas mínimas 2012-2018 -------------------------------------------------------------
# aux <- copy( pen_min )
# aux[ , sbu:=100*sbu]
# aux[ , rango:=as.character(rango)]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_pen_min', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )
# 
# message( '\tPensiones máximas' )
# # 
# # # Tabla pensiones máximas 2012-2018 ------------------------------------------------------------------------------------
# aux <- copy( pen_max )
# aux[ , sbu:=100*sbu]
# aux[ , rango:=as.character(rango)]
# aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
# print( aux_xtable,
#        file = paste0( parametros$resultado_tablas, 'iess_pen_max', '.tex' ),
#        type = 'latex',
#        include.colnames = FALSE,
#        include.rownames = FALSE,
#        format.args = list( decimal.mark = ',', big.mark = '.' ),
#        only.contents = TRUE,
#        hline.after = NULL,
#        sanitize.text.function = identity )


# Tabla de hijos del SGO
aux <-copy( pob_hijos_18 )
aux$anio <- as.character( aux$anio )
aux[ , creci :=100*creci ]
aux[ , creci_h :=100*creci_h ]
aux[ , creci_m :=100*creci_m ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2, 0, 2, 0, 2 ) )
# aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0 ) )

print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_hijos_sgo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )



# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

