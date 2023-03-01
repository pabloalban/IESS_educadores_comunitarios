message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:5 )
for ( escenario in escenarios ) { # escenario <- escenarios[1]
  message( '\tCálculo de la prima para el ', escenario )
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', escenario , '.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', escenario , '.RData' ) )
  
  prima <- balance_anual[ t > 0, list( t, v, M, MD, MS,
                                       A2, A_sgo, A_est_pen, A_est_rel_dep, A_est_cat, A_est_pen_sal, A_afi,
                                       A_est_fij, A_issfa, A_isspol, A_seg_pri, A_otr, A_est, A_afi_est, A, interes,
                                       
                                       B3, B4, B8, B9, B_pen_mon, B_pen, 
                                       B2_5, B3_5, B4_5, B6_5, B7_5, B8_5, B9_5, B_aux, 
                                       B2_sal, B3_sal, B4_sal, B6_sal, B7_sal, B8_sal, B9_sal, B_sal, B_pen_sal, 
                                       B2_sal_cat, B3_sal_cat, B4_sal_cat, B6_sal_cat, B7_sal_cat, B8_sal_cat, B9_sal_cat, B_sal_cat, B_pen_sal_cat,
                                       B,
                                       G,
                                       
                                       M_vap, MD_vap, MS_vap,
                                       A2_vap, A_sgo_vap, A_est_pen_vap, A_est_rel_dep_vap, A_est_cat_vap, A_est_pen_sal_vap, A_est_vap, 
                                       A_afi_vap, A_afi_est_vap, A_est_fij_vap, A_issfa_vap, A_isspol_vap, A_seg_pri_vap, A_otr_vap, A_vap, Int_vap,
                                       
                                       B3_vap, B4_vap, B8_vap, B9_vap, B_pen_mon_vap, B_pen_vap, 
                                       B2_5_vap, B3_5_vap, B4_5_vap, B6_5_vap, B7_5_vap, B8_5_vap, B9_5_vap, B_aux_vap, 
                                       B2_sal_vap, B3_sal_vap, B4_sal_vap, B6_sal_vap, B7_sal_vap, B8_sal_vap, B9_sal_vap, B_sal_vap, B_pen_sal_vap, 
                                       B2_sal_cat_vap, B3_sal_cat_vap, B4_sal_cat_vap, B6_sal_cat_vap, B7_sal_cat_vap, B8_sal_cat_vap, B9_sal_cat_vap,
                                       B_sal_cat_vap, B_pen_sal_cat_vap, 
                                       
                                       B_vap,
                                       G_vap,
                                       
                                       V0,
                                       V ) ]
  
  prima <- merge.data.table( prima,
                             esc$hip_esc[ , list( t, bas_ref ) ],
                             by = c( 't' ) )
  
  # Prima de reparto puro ----------------------------------------------------------------------------
  # Prima pura sin aportes
  prima[ , pri_rep_pur := ( B + G ) / (  MS ) ]
  
  # Prima reglamentaria
  prima[ , pri_rep_pur_reg := ( B + G - A_est - A_sgo - A_otr ) / ( MS ) ] 
  
  # Prima reglamentaria sin aporte del Estado por relación de dependencia
  prima[ , pri_rep_pur_reg_nrel := ( B + G - ( A_est - A_est_rel_dep ) - A_sgo - A_otr ) / (  MS ) ] 
  
  # Prima pura sobre el SGO
  prima[ , pri_rep_pur_sgo := ( B + G - A_est - A2 - A_otr ) / M ]
  
  # Prima nivelada en cada horizonte -----------------------------------------------------------------
  # Prima media nivelada sin aportes
  prima[ , pri_med_niv := ( B_vap + G_vap - V0 ) / (  MS_vap ) ]
  
  # Prima media nivelada con aportes reglamentaria
  prima[ , pri_med_niv_reg := ( B_vap + G_vap - A_est_vap - A_sgo_vap - A_otr_vap - V0 ) / (  MS_vap ) ]
  
  # Prima media nivelada con aportes reglamentaria, sin aporte del Estado por relación de dependencia
  prima[ , pri_med_niv_reg_nrel := ( B_vap + G_vap - ( A_est_vap - A_est_rel_dep_vap ) - A_sgo_vap - A_otr_vap - V0 ) / ( MS_vap ) ]
  
  # Prima media nivelada sobre el SGO
  prima[ , pri_med_niv_sgo := ( B_vap + G_vap - A_est_vap - A2_vap - A_otr_vap - V0 ) / M_vap ]
  
  
  save( prima, 
        file = paste0( parametros$RData_seg, 'IESS_SSC_primas_', escenario, '.RData' ) )  
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
