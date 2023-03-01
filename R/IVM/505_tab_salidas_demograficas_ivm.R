message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCreación Edad promedio de pensionistas por tipo de pensión y sexo' ) 

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_salidas_demograficos.RData')) 

# Edad promedio ---------------------------------------------------------------------------

aux <- copy( edad_promedio)
aux <- setnames( aux, # set column names
                 c('Pensión',
                   'Masculino',
                   'Femenino',	
                   'Total'))
# View(aux)

aux_xtable <- xtable( aux, digits = c( 0, 2, 2, 2, 2 ) )
print( aux_xtable, 
       file = paste0( parametros$resultado_tablas, 'iess_edad_promedio_ivm', '.tex' ),
       type = 'latex',
       include.colnames = FALSE, 
       include.rownames = FALSE,
       format.args = list( decimal.mark = ',', big.mark = '.' ),
       only.contents = TRUE,
       hline.after = 4L,
       sanitize.text.function = identity )
