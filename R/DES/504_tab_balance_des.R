message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

escenarios_lista <- paste0( 'escenario_', 1: 1)

for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  # escenario <- escenarios_lista[1]
  load( paste0( parametros$RData_seg, 'IESS_DES_balances_', escenario, '.RData' ) )
  
  # Balance corriente ------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A, B, G, V_cor, V_cap ) ]
  aux<-aux[t>2018]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux),
         sanitize.text.function = identity )
  
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, B1, B2, B3, B4, B5, B ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2 ,2) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux), 
         sanitize.text.function = identity )
  
  # Balance dinámico  ------------------------------------------------------------------------
  # Balance dinámico (actuarial) -------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A_vap, B_vap, G_vap, V0, V ) ]
  aux<-aux[anio>2018]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux),
         sanitize.text.function = identity )
  
  
  # Balance dinámico (beneficios) ------------------------------------------------------------
  
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t,B1_vap,B2_vap,B3_vap,B4_vap,B5_vap,B_vap ) ]
  aux<-aux[anio>2018]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = nrow(aux),
         sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------
  aux <- balance_anual[ t == max(t), 
                        list( V0, 
                              A_vap, 
                              activo = V0 + A_vap,
                              B1_vap, B2_vap, B3_vap, B4_vap, B5_vap, 
                              B_vap,
                              G_vap, 
                              pasivo = B_vap + G_vap, 
                              V ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( V0 = 'Reserva inicial', 
                      A_vap = 'Aportes afiliados', 
                      activo = 'Total activo actuarial', 
                      B1_vap = 'Beneficios del pago 1',
                      B2_vap = 'Beneficios del pago 2',
                      B3_vap = 'Beneficios del pago 3',
                      B4_vap = 'Beneficios del pago 4',
                      B5_vap = 'Beneficios del pago 5',
                      B_vap = 'Beneficios totales', 
                      G_vap = 'Gastos administrativos',
                      pasivo = 'Total pasivo actuarial',
                      V = 'Super\\\'{a}vit actuarial' )
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames(aux, c('item', 'descripcion', 'valor'))
  xtb_aux <- xtable( aux[ , list(descripcion, valor)], digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         sanitize.text.function = identity,
         hline.after = c(2, 3, 8, 9, 10, 11) ,
         add.to.row = list(pos = list(3),
                           command = c(paste(" \n \\hline  \\multicolumn{2}{c}{\\textbf{Pasivo actuarial}}  \\\\ \n"))))
  
  rm( balance, balance_anual )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
