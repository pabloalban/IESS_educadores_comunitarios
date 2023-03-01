message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de agregado demográfico' )

escenarios_lista <- paste0( 'escenario_', 1:8 )

# -------------------------------------------------------------------------------------------------- 
#Escenario Base

for(i in 1:length( escenarios_lista )) #i=1
{
  escenario <- escenarios_lista[i]
  
  load(paste0( parametros$RData_seg, 'IESS_IVM_agregado_demografico_', escenario, '.RData' ))
  copy(agregado_demografico_F)
  
  #Agregado Demográfico ------------------------------------------------------------------------
  aux_f <- agregado_demografico_F[ , list(anio,fuerza_laboral_empleada,activos_contribuyentes,retiro1,discapacidad1,viudedad,orfandad,total1)]
  aux_f[, anio := as.character( anio )]
  xtb_aux <- xtable( aux_f, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_agregado_demografico_f_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux_m <- agregado_demografico_M[ , list(anio,fuerza_laboral_empleada,activos_contribuyentes,retiro1,discapacidad1,viudedad,orfandad,total1)]
  aux_m[, anio := as.character( anio )]
  xtb_aux <- xtable( aux_m, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_agregado_demografico_m_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  

  
  
  }

