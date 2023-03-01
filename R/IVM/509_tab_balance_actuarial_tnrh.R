message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

escenarios_lista <- paste0( 'escenario_', 1:3 ) #i=3
for ( i in 1:length(escenarios_lista))
{
  escenario <- escenarios_lista[i]
  load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
  aux <- agregado_financiero[ t == max(t), 
                              list( V0, apo_activos_vap, apo_estatal_vap, aporte_tot_vap,activo_vap,
                                    beneficios_vejez_vap,beneficios_invalidez_vap,beneficios_orfandad_vap,
                                    beneficios_viudedad_vap,beneficios_aux_fun_vap,B_tot_vap,gastos_administrativos_vap,pasivo_vap,V)] 
  
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux))
  aux2 <- data.table( V0 = 'Reserva inicial', 
                      activo_vap = 'Aportes activos', 
                      apo_estatal_vap = 'Aporte estatal', 
                      aporte_tot_vap = 'Aportes totales', 
                      activo_vap = 'Activo actuarial', 
                      beneficios_vejez_vap = 'Beneficios pensionistas vejez', 
                      beneficios_invalidez_vap = 'Beneficios pensionistas invalidez', 
                      beneficios_orfandad_vap = 'Beneficios pensionistas orfandad', 
                      beneficios_viudedad_vap = 'Beneficios pensionistas viudedad',
                      beneficios_aux_vap = 'Beneficios auxilio funerales', 
                      B_tot_vap = 'Beneficios totales', 
                      gastos_administrativos_vap = 'Gastos administrativos',
                      pasivo_vap = 'Pasivo actuarial',
                      V_cap = 'Balance actuarial' )
  
  aux<-cbind(t(aux2),aux1)
  setnames(aux, c('item', 'descripcion', 'valor'))
  xtb_aux <- xtable( aux[ , list(item, valor)], digits = c( 0,0,2) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_tnrh_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c(1, 3, 4, 5,10, 13, 11, 12),sanitize.text.function = identity )
}

