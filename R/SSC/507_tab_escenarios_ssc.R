message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )

escenario <- paste0( 'escenario_', 1:5 )

nombres <- c( 'Base', 'Pesimista', 'Legal', 'Aternativo 1', 'Alternativo 2'  )

var_nom <- data.table( orden = 1:14,
                       
                       variable = c( 'i_a', 'i_r', 'i_s', 'i_p', 'i_f', 
                                     'apo_cot', 'apo_sgo', 
                                     'apo_est', 'apo_est_rel_dep', 'apo_est_fij', 
                                     'apo_est_cat', 'apo_est_pen_sal', 
                                     'por_gas_masa', 'por_gas_apor' ),
                       
                       nombre = c( '$i_a$', '$i_r$', '$i_s$', '$i_p$', '$i_f$', 
                                   '$\\alpha_2$', '$\\alpha_{sgo}$',
                                   '$\\alpha_{1,est}$', '$\\alpha_{2,est}$', '$\\alpha_{3,est}$', 
                                   '$\\alpha_{4,est}$', '$\\alpha_{5,est}$', 
                                   '$\\gamma_M$', '$\\gamma_A$' ),
                       
                       descripcion = c( 'Tasa actuarial',
                                        'Tasa crecimiento salarios',
                                        'Tasa crecimiento salario b\\\'{a}sico unificado',
                                        'Tasa crecimiento pensiones',
                                        'Tasa crecimiento auxilios de funerales',
                                        
                                        'Prima de cotizantes',
                                        'Porcentaje de afiliados del SGO',
                                        
                                        'Porcentaje de aporte estatal para pensiones',
                                        'Porcentaje de aporte estatal en relación de dependencia',
                                        'Porcentaje de aporte estatal fijo',
                                        'Porcentaje de aporte estatal para enfermedades catastróficas',
                                        'Porcentaje de aporte estatal para salud de pensionistas',
                                        
                                        'Porcentaje gasto administrativo sobre la materia gravada',
                                        'Porcentaje gasto administrativo sobre aportes de las familias' ) )

aux_tot_tas <- NULL
aux_tot_apo <- NULL
for( i in 1:length( escenario ) ){ # i <- 1
  load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario[i], '.RData' ) )
  
  aux <- esc$hip_esc[ , list( t = as.character( t + parametros$anio ),
                              i_a = 100 * i_a, 
                              i_r = 100 * i_r, 
                              i_s = 100 * i_s, 
                              i_p = 100 * i_p, 
                              i_f = 100 * i_f,
                              apo_cot = 100 * ( apo_ind + apo_inv ), 
                              apo_sgo = 100 * apo_sgo,
                              apo_est = 100 * apo_est, 
                              apo_est_rel_dep = 100 * apo_est_rel_dep, 
                              apo_est_fij, 
                              apo_est_cat = 100 * apo_est_cat, 
                              apo_est_pen_sal = 100 * apo_est_pen_sal,
                              por_gas_masa = 100 * por_gas_masa, 
                              por_gas_apor = 100 * por_gas_apor ) ]
  
  aux_tas <- aux[ , list( t, i_a, i_r, i_s, i_p, i_f ) ]
  aux_apo <- aux[ , list( t, apo_cot, apo_sgo, apo_est, apo_est_rel_dep, apo_est_fij, apo_est_cat, 
                          apo_est_pen_sal, por_gas_masa, por_gas_apor ) ]
  
  setorder( aux_tas, t )
  setorder( aux_apo, t )
  
  xtb_aux_tas <- xtable( aux_tas, digits = c( 0, 0, rep( 2, 5 ) ) )
  xtb_aux_apo <- xtable( aux_apo, digits = c( 0, 0, rep( 2, 9 ) ) )
  
  print( xtb_aux_tas,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_1_', escenario[i], '_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  print( xtb_aux_apo,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_2_', escenario[i], '_ssc', '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
  
  aux_tas <- cbind( data.table( nom = nombres[ i ] ), aux_tas )
  aux_tot_tas <- rbind( aux_tot_tas, aux_tas )
  
  aux_apo <- cbind( data.table( nom = nombres[ i ] ), aux_apo )
  aux_tot_apo <- rbind( aux_tot_apo, aux_apo )
}

xtb_aux_tot_tas <- xtable( aux_tot_tas, digits = c( 0, 0, 0, rep( 2, 5 ) ) )
xtb_aux_tot_apo <- xtable( aux_tot_apo, digits = c( 0, 0, 0, rep( 2, 9 ) ) )

print( xtb_aux_tot_tas,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_1_escenarios_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = c( 21, 42, 63, 84 ), sanitize.text.function = identity )

print( xtb_aux_tot_apo,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_2_escenarios_ssc.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = c( 21, 42, 63, 84 ), sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
