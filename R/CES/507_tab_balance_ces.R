message( paste( rep( '-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

# --------------------------------------------------------------------------------------------------
escenarios_lista <- paste0( 'escenario_', 1:2 )

for ( i in 1:length( escenarios_lista ) ) {
  escenario <- escenarios_lista[i]
  # escenario <- escenarios_lista[1]
  load( paste0( parametros$RData_seg, 'IESS_CES_balances_', escenario, '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_CES_configuracion_', escenario, '.RData' ) )
  # Balance corriente ------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, A2_per, A2_pat, B, G, V_cor, V_cap, C ) ]
  aux[ , t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_corriente_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Beneficios -------------------------------------------------------------------------------------
  aux <- balance_anual[ , list( t = t + parametros$anio_ini, B9, B10, B11, B12, B13, B14, B15, B16, B17, B18, B ) ]
  aux[ , t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (actuarial) -------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, A2_per_vap, A2_pat_vap, C0, V0, B_vap, G_vap, C_vap,
                                V ) ]
  aux[ , anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_actuarial_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  

  # Balance dinámico (beneficios) ------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio_ini, t, B9_vap, B10_vap, B11_vap, B12_vap,
                                B13_vap, B14_vap, B15_vap, B16_vap,  B17_vap, B18_vap, B_vap ) ]
  aux[ , anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_',
                        escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------------
  aux <- balance_anual[ t == max(t), 
                        list( V0, S0 = esc$S0, A2_vap, Act_vap = V0 + esc$S0 + A2_per_vap + A2_pat_vap, A2_per_vap, A2_pat_vap,
                              B9_vap, B10_vap, B11_vap, B12_vap, B13_vap, B14_vap, B15_vap,
                              B16_vap, B17_vap, B18_vap,
                              B_vap, G_vap, Pas_vap = B_vap + G_vap + C_vap, C_vap,
                              V ) ]
  aux1 <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux2 <- data.table( V0 = 'Patrimonio inicial', 
                      S0 = 'Saldo inicial de las cuentas individuales',
                      A2_per_vap = 'Aportes personales', 
                      A2_pat_vap = 'Aportes patronales', 
                      Act_vap = 'Total activo actuarial', 
                      B9_vap = 'Retiro de la cesantía del afiliado cesante', 
                      B10_vap = 'Retiro de la cesantía del jubilado',
                      B11_vap = 'Débito automático por ejecución de las garantías',
                      B12_vap = 'Parte variable del Seguro de Desempleo',
                      B13_vap = 'Retiro de la cesantía del afiliado voluntario',
                      B14_vap = 'Derechohabientes de la Prestación de Cesantía',
                      B15_vap = 'Cruce de Fondos de Cesantía con Obligaciones patronales',
                      B16_vap = 'Retiro de la cesantía del afiliado de la industria azucarera',
                      B17_vap = 'Retiro de la cesantía por licencia de maternidad o paternidad',
                      B18_vap = 'Reliquidación de fondos de Cesantía por aportes extemporáneos',
                      B_vap = 'Beneficios totales', 
                      G_vap = 'Gastos administrativos',
                      C_vap = 'Saldo de las cuentas individuales por pagar al 2060',
                      Pas_vap = 'Total pasivo actuarial',
                      V = 'Superávit actuarial' )
  aux2 <- melt.data.table( aux2, measure.vars = 1:ncol(aux2) )
  aux <- merge( aux2, aux1, by = 'variable', all.x = TRUE )
  setnames( aux, c('item', 'descripcion', 'valor') )
  xtb_aux <- xtable( aux[ , list(descripcion, valor) ], digits = c( 0, 0, 2 ) )
  xtb_aux <- tildes_a_latex(xtb_aux)
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_bal_act_vap_', escenario, '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = c( 4, 5, 15, 18, 19 ),
         sanitize.text.function = identity,
         add.to.row = 
           list( pos = list(0, 5, 19),
                 command = c(paste(" \n \\multicolumn{2}{c}{\\textbf{Activo actuarial}} \\\\ \n \\hline \n"), 
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Pasivo actuarial}} \\\\ \n"),
                             paste(" \n \\hline \\multicolumn{2}{c}{\\textbf{Balance actuarial}} \\\\ \n") ) 
           ) )
  
  rm( balance, balance_anual )
}

# --------------------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
