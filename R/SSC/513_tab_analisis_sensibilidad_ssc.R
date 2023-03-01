message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación tablas del análisis de sensibilidad' )

source( 'R/502_tab_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad.RData' ) ) 

# Sensbilidad de la tasa de descuento---------------------------------------------------------------
message( '\tTabla de Afiliados activos SGO' )

aux <- copy( sensi[ , -c('escenarios_part')] )
aux[ , bal_shif:= shift(balance, type='lag')]
aux[ , var:= ( balance/bal_shif - 1)*100 ]
aux[ , bal_shif:=NULL ]
aux <- aux[ , c('tasa', 'balance', 'var', 'raiz', 'ultimo')]

xtb_pri <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 0 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_bal_analisis_sensibilidad.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )


# Tabla de sensibilidad médica ---------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad_medica.RData' ) ) 

aux <- copy( sensi[ , -c('escenarios_part')] )
aux[ , bal_shif:= shift(balance, type='lag')]
aux[ , var:= ( balance/bal_shif - 1)*100 ]
aux[ , bal_shif:=NULL ]
aux <- aux[ , c('tasa', 'balance', 'var', 'raiz', 'ultimo')]

xtb_pri <- xtable( aux, digits = c( 0, 2, 2, 2, 2, 0 ) )
print( xtb_pri,
       file = paste0( parametros$resultado_tablas, 'iess_bal_analisis_sensibilidad_med.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()