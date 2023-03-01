message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura afiliados activos TNRH 2015-2020' )

# Notas --------------------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_TNRH_analisis_demografico.RData' )) 

# Tabla afiliados activos 2015-2020 a diciembre-----------------------------------------------------
aux <- copy( afi_acti_tnrh )
aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 0, 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_acti_tnrh', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla afiliados activos (2) ---------------------------------------------------------------------
aux <- copy( afi_acti2_tnrh )
aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_afi_acti2_tnrh', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla de población afiliada por sexo y edad -----------------------------------------------------
aux <- copy( pob_afi_edad_sexo_tnrh )
aux[, sexo := as.character( sexo ) ]
aux[, edad := as.character( edad ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 2 ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_pob_afi_edad_sexo_tnrh', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

# Tabla masa salarial 2015-2020 --------------------------------------------------------------------
message( '\tTabla masa salarial inicial' )

aux <- copy( masa_salarial_tnrh )
aux[, anio := as.character( anio ) ]
aux_xtable <- xtable( aux, digits = c( 0, 0, 0, 0, 2, 2  ) )
print( aux_xtable,
       file = paste0( parametros$resultado_tablas, 'iess_masa_salarial_tnrh', '.tex' ),
       type = 'latex',
       include.colnames = FALSE,
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = NULL,
       sanitize.text.function = identity )

