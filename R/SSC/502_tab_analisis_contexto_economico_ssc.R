message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación Tablas del Capítulo de Analisis de Contexto Económico' )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_contexto_economico.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_aportes.RData' ) ) 

# Tabla de aporte solidario del SGO-----------------------------------------------------------------
message( '\tTabla de aporte solidario del SGO' )

aux <- copy( aporte_sgo )
aux[ , tasa:=100*tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aporte_solidario_sgo', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de aportes de jefes de familia--------------------------------------------------------------
message( '\tTabla de aportes de jefes de familia' )

aux <- copy( aporte_jefes )
aux[ , tasa:=100*tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aporte_jefes', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de aporte solidario del ISSFA e ISSPOL------------------------------------------------------
message( '\tTabla de aporte solidario del ISSFA e ISSPOL' )

aux <- copy( aporte_issfa_isspol )
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aporte_solidario_issfa_isspol', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de aporte solidario de Seguros Privados-----------------------------------------------------
message( '\tTabla de aporte solidario de Seguros Privados' )

aux <- copy( aporte_sp )
aux[ , tasa:=100*tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aporte_solidario_seguros_privados', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla de aporte del 0,3% del Estado --------------------------------------------------------------
message( '\tTabla de aporte del 0,3% del Estado' )

aux <- copy( aporte_est )
aux[ , tasa:=100*tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_aporte_solidario_estado', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla del crecimiento de las pensiones del SSC----------------------------------------------------
message( '\tTabla del crecimiento de las pensiones del SSC' )

aux <- copy( cre_pen )
aux[ , tasa:=100*tasa]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_crecimiento_pensiones_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de tasas de los aportes solidarios ---------------------------------------------------------
message( '\tTabla de tasas de los aportes solidarios' )

aux <- copy( tasas_aporte )
aux[ , sgo := 100*sgo]
aux[ , jefes := 100*jefes]
aux[ , sp := 100*sp]
aux[ , estado := 100*estado]
aux[ , anio:=as.character(anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tasas_aporte_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de proyección de aporte solidario del SGO al SSC--------------------------------------------
message( '\tTabla de proyección de aporte solidario del SGO al SSC' )

aux <- copy( masa_proy )
aux[, t:= t + 2020 ]
aux <- aux[ , list( sgo = sum( 1.051 * 0.007 * M, na.rm=TRUE)), by = list( t) ]
aux[ , t := as.character( t )]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tab_proy_aporte_afiliados_sgo_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de proyección de aporte solidario del 0,3% del Estado al SSC--------------------------------
message( '\tTabla de proyección de aporte solidario del 0,3% del Estado al SSC' )

aux <- copy( masa_proy )
aux[, t:= t + 2020 ]
aux <- aux[ , list( md = sum( 1.0024 * 1.155 * 0.4933 * 0.003 * MD, na.rm=TRUE)), by = list( t) ]
aux[ , t := as.character( t )]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tab_proy_aporte_estado_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )


# Tabla de proyección de aporte de los seguros públicos ISSFA e ISSPOL------------------------------
message( '\tTabla de proyección de aporte de los seguros públicos ISSFA e ISSPOL' )

aux <- copy( aportes )
aux <- aux[ , list( isspol = sum( A_isspol, na.rm=TRUE),
                    issfa_RT = sum( A_issfa_RT, na.rm=TRUE),
                    issfa_NS = sum( A_issfa_NS, na.rm=TRUE),
                    issfa_T = sum( A_issfa_Total, na.rm=TRUE)), by = list( t) ]
aux[ , t := as.character( t + parametros$anio )]
aux[ , A_70:= issfa_NS * 0.007 ]
aux[ , A_10:= issfa_T * 0.001 ]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tab_proy_aporte_issfa_isspol_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de proyección de aporte de los seguros privados --------------------------------------------
message( '\tTabla de proyección de aporte de los seguros privados' )
aux <- copy( aportes )
aux <- aux[ , list( A_sp = sum( A_sp, na.rm=TRUE)), by = list( t) ]
aux[ , t := as.character( t + parametros$anio)]

aux_xtable <- xtable( aux, digits = c( 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_tab_proy_aporte_seguros_privados_ssc', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()