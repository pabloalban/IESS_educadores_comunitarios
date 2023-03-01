message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance corriente' )

escenarios_lista <- paste0( 'escenario_', 1:8 )


# -------------------------------------------------------------------------------------------------- 
#Escenario Base

for(i in 1:length( escenarios_lista )) #i=3
{
escenario <- escenarios_lista[i]
load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) )

# Balance corriente ------------------------------------------------------------------------
aux <- agregado_financiero[ , list(anio, contribuciones,Otros,beneficios_vejez, beneficios_invalidez, beneficios_orfandad,
                                                beneficios_viudedad,gastos_otros,gastos_administrativos,reserva ) ]
aux[, anio := as.character( anio )]
aux<- aux[2:41,] #seleccionar rango de filas
xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2, 2,2 ) )
print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )
}


  