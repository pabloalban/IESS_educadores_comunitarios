message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tabla de balance total' )

escenarios_lista <- paste0( 'escenario_', 1:5 )
nombres_flujos <- data.table( order = 1:31, 
                              variable = c( 'V0', 
                                            'A2_vap', 'A_sgo_vap', 'A_afi_vap', 
                                            
                                            'A_est_pen_vap', 'A_est_rel_dep_vap', 'A_est_cat_vap', 'A_est_pen_sal_vap', 
                                            'A_est_fij_vap', 'A_est_vap', 
                                            
                                            'A_issfa_vap', 'A_isspol_vap', 'A_seg_pri_vap', 'A_otr_vap',
                                            
                                            'A_vap', 'Activo', 
                                            
                                            'B3_vap', 'B4_vap', 'B8_vap', 'B9_vap', 'B_aux_vap', 'B_pen_aux_vap', 
                                            
                                            'B_cot_dep_sal_vap', 'B_pen_sal_vap',  
                                            'B_cot_dep_sal_cat_vap', 'B_pen_sal_cat_vap', 'B_sal_cat_total_vap',
                                            
                                            'B_vap', 'G_vap', 'Pasivo', 'V' ),
                              
                              descripcion = c( '\\textbf{Reserva inicial}',
                                               'Aportes cotizantes del SSC',
                                               'Aportes cotizantes del SGO',
                                               '\\rowcolor{gray!10}\\textbf{Aportes de los cotizantes}',
                                               
                                               'Aporte Estatal para pensiones',
                                               'Aporte Estatal por afiliados en relaci\\\'{o}n de dependencia',
                                               'Aporte Estatal para enfermedades catastr\\\'{o}ficas',
                                               'Aporte Estatal para salud de pensionistas',
                                               'Aporte Estatal fijo',
                                               '\\rowcolor{gray!10}\\textbf{Aporte estatal total}',
                                               
                                               'Aporte del ISSFA',
                                               'Aporte del ISSPOL',
                                               'Aporte de seguros privados',
                                               '\\rowcolor{gray!10}\\textbf{Aporte otros total}',
                                               
                                               '\\rowcolor{gray!10}\\textbf{Aporte total}',
                                               '\\rowcolor{gray!30}\\textbf{Activo}\\protect\\footnotemark',
                                               
                                               'Beneficio por pensiones de vejez',
                                               'Beneficio por pensiones de invalidez',
                                               'Beneficio por pensiones de montep\\\'{i}o por orfandad',
                                               'Beneficio por pensiones de montep\\\'{i}o por viudedad',
                                               'Beneficio por auxilio de funerales',
                                               '\\rowcolor{gray!10}\\textbf{Beneficio por IVM}',
                                               
                                               'Beneficio por salud de cotizantes y dependientes',
                                               'Beneficio por salud de pensionistas',
                                               'Beneficio por enfermedades catastr\\\'{o}ficas de cotizantes y dependientes',
                                               'Beneficio por enfermedades catastr\\\'{o}ficas de pensionistas',
                                               '\\rowcolor{gray!10}\\textbf{Beneficio por salud}',
                                               '\\rowcolor{gray!10}\\textbf{Beneficio prestacional total}',
                                               'Gastos administrativos',
                                               '\\rowcolor{gray!30}\\textbf{Pasivo}',
                                               '\\rowcolor{iess_blue!10}\\, \\, \\, \\, \\, \\, \\, \\, \\, \\,\\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\, \\,\\textbf{Balance actuarial}' ) )

for ( i in 1:length( escenarios_lista ) ) { # i<-1
  escenario <- escenarios_lista[i]
  load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario, '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
  
  # Balance corriente ------------------------------------------------------------------------------
  # Balance corriente total
  aux <- balance_anual[ , list( t = t + parametros$anio, A, interes, B, G, V_cor, V_cap ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2, 2, 2, 2, 2, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_corriente_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance corriente aportes 
  aux <- balance_anual[ , list( t = t + parametros$anio,
                                A2, A_sgo, A_afi, A_est_rel_dep, A_est_pen, A_est_cat, A_est_pen_sal, 
                                A_est_fij, A_est, A_afi_est, A_otr, A ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 12 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_aportes_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance corriente aportes otros
  aux <- balance_anual[ , list( t = t + parametros$anio,
                                A_issfa, A_isspol, A_seg_pri, A_otr ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_aportes_otros_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance corriente beneficios pensiones
  aux <- balance_anual[ , list( t = t + parametros$anio, 
                                B3, B4, B8, B9, B_pen,
                                B2_5, B3_5, B4_5, B6_5, B7_5, B8_5, B9_5, B_aux ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 13 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_beneficios_pensiones_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance corriente beneficios salud 
  aux <- balance_anual[ , list( t = t + parametros$anio, 
                                B3_sal, B4_sal, B8_sal, B9_sal,  
                                B2_sal, B6_sal, B7_sal, B_sal  ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_beneficios_salud_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance corriente beneficios salud catastróficas
  aux <- balance_anual[ , list( t = t + parametros$anio,
                                B3_sal_cat, B4_sal_cat, B8_sal_cat, B9_sal_cat,
                                B2_sal_cat, B6_sal_cat, B7_sal_cat, B_sal_cat ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_beneficios_salud_catastrofico_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico -------------------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio, t, 
                                A_vap, Int_vap,  B_vap, G_vap, V0, V ) ]
  aux[, anio := as.character( anio ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, 0, rep( 2, 6 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_actuarial_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (aportes) ---------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio,
                                A2_vap, A_sgo_vap, A_afi_vap,
                                A_est_rel_dep_vap, A_est_pen_vap, A_est_cat_vap,
                                A_est_pen_sal_vap, A_est_fij_vap, A_est_vap, A_afi_est_vap, 
                                A_otr_vap, A_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 12 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_aportes_vap_', escenario, '_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico aportes otros
  aux <- balance_anual[ , list( t = t + parametros$anio,
                                A_issfa_vap, A_isspol_vap, A_seg_pri_vap, A_otr_vap ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 4 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_aportes_otros_vap_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (beneficios) ------------------------------------------------------------------
  aux <- balance_anual[ , list( anio = t + parametros$anio,
                                B3_vap, B4_vap, B8_vap, B9_vap, B_pen_vap,
                                B2_5_vap, B3_5_vap, B4_5_vap, B6_5_vap, B7_5_vap, B8_5_vap, B9_5_vap, B_aux_vap ) ]
  aux[, anio := as.character( anio )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 13 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_balance_beneficios_vap_', escenario, '_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámica beneficios salud 
  aux <- balance_anual[ , list( t = t + parametros$anio, 
                                B3_sal_vap, B4_sal_vap, B8_sal_vap, B9_sal_vap, 
                                B2_sal_vap, B6_sal_vap, B7_sal_vap, B_sal_vap  ) ]
  aux[, t := as.character( t )]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_beneficios_salud_vap_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico beneficios salud catastróficas
  aux <- balance_anual[ , list( t = t + parametros$anio,
                                B3_sal_cat_vap, B4_sal_cat_vap, B8_sal_cat_vap, B9_sal_cat_vap,
                                B2_sal_cat_vap, B6_sal_cat_vap, B7_sal_cat_vap, B_sal_cat_vap ) ]
  aux[, t := as.character( t ) ]
  xtb_aux <- xtable( aux, digits = c( 0, 0, rep( 2, 8 ) ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_balance_beneficios_salud_catastrofico_vap_', escenario,'_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE,
         hline.after = NULL, sanitize.text.function = identity )
  
  # Balance dinámico (resumen) ---------------------------------------------------------------------
  aux <- balance_anual[ t == max( t ),
                        list( V0,
                              A2_vap, A_sgo_vap, 
                              A_afi_vap, 
                              
                              A_est_pen_vap, 
                              A_est_rel_dep_vap, 
                              A_est_cat_vap, 
                              A_est_pen_sal_vap, 
                              A_est_fij_vap, 
                              A_est_vap, 
                              
                              A_issfa_vap, 
                              A_isspol_vap, 
                              A_seg_pri_vap,
                              A_otr_vap,
                              
                              A_vap,
                              Activo,
                              
                              B3_vap, B4_vap, B8_vap, B9_vap, B_aux_vap, B_pen_aux_vap,
                              
                              B_cot_dep_sal_vap, B_pen_sal_vap, 
                              B_cot_dep_sal_cat_vap, B_pen_sal_cat_vap, 
                              B_sal_cat_total_vap,
                              
                              B_vap,
                              G_vap, 
                              Pasivo,
                              
                              V ) ]
  
  aux <- melt.data.table( aux, measure.vars = 1:ncol(aux) )
  aux <- merge( nombres_flujos, aux, by = 'variable' )
  setorder( aux, order )
  aux[ , variable := NULL ]
  aux[ , order := NULL ]
  
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 
                        'iess_bal_act_vap_', escenario, '_ssc', '.tex' ),
         type = 'latex',
         include.colnames = FALSE, include.rownames = FALSE,
         format.args = list( decimal.mark = ',', big.mark = '.' ),
         only.contents = TRUE,
         hline.after = c( 1, 3, 4, 9, 10, 13, 14, 15, 16, 21, 22, 26, 27, 28, 29, 30), 
         sanitize.text.function = identity )
  
  rm( balance, balance_anual )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()