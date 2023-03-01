message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:4 )

for ( escenario in escenarios ) { # escenario<-'escenario_1'
  message( '\tCálculo de la prima para el ', escenario )
  load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', escenario, '.RData' ) )
  
  prima <- balance_anual[ t > 0, list( t, v, M, A, A3, A4, A6, A_est, B, B7, B7_cat, B7_ncat, G,
                                       M_vap, A_vap, A3_vap, A4_vap, A6_vap, A_est_vap,
                                       B3_vap, B4_vap, B6_vap, P_sd_vap, P6_sd_vap, 
                                       A8_vap,
                                       #M8_vap, P_sd_cony, P6_sd_vap, 
                                       B7_vap, B8_vap, B_vap, G_vap, B_cat_vap, B2_vap, B9_vap,
                                       B2_cat_vap,B7_cat_vap, B8_cat_vap,
                                       B_ncat_vap, B6_ncat_vap, B7_ncat_vap, B2_ncat_vap,
                                       B8_ncat_vap, B3_ncat_vap, B4_ncat_vap, B9_vap,
                                       B3_cat_vap, B4_cat_vap, B6_cat_vap,
                                       V0, V #P6_sd_cal_vap, P_sd_cony_vap, B_est_cat_vap 
                                       ) ]
  
  # Prima de reparto puro ----------------------------------------------------------------------------
  # prima[ , pri_rep_pur := ( B + G ) /  M ] # sin aporte estatal
  # prima[ , pri_rep_pur_apo_est := ( B + G - A_est ) /  M ] # con aporte estatal AE
  # prima[ , pri_rep_pur_7 := B7 /  M ]
  
  # Prima nivelada en cada horizonte -----------------------------------------------------------------
  # prima[ , pri_med_niv := ( B_vap + G_vap - V0 ) /  M_vap ] # sin aporte estatal
  prima[ , pri_med_niv_apo_est     := ( B2_ncat_vap  + B9_vap - V0 + (B3_ncat_vap + B4_ncat_vap+B3_cat_vap + B4_cat_vap) ) /  M_vap ] # con aporte estatal AE
  prima[ , pri_med_niv_7           :=  B7_ncat_vap /  M_vap ] # Menores de 18 años
  prima[ , pri_med_niv_8           :=  B8_ncat_vap / M_vap ] # Conyuges
  prima[ , pri_med_niv_6           :=  B6_ncat_vap /  P6_sd_vap ] # Viudas
  prima[ , pri_med_niv_3_4         := ( B3_ncat_vap + B4_ncat_vap ) / M_vap ] # Jubilados
  
  prima[ , pri_med_niv_3_4_cat     := ( B3_cat_vap + B4_cat_vap ) / M_vap ] # Catastróficas: B3+B4
  prima[ , pri_med_niv_6_cat       := ( B6_cat_vap ) / P6_sd_vap ] # Catastróficas: B6
  prima[ , pri_med_niv_apo_est_cat :=  B2_cat_vap /  M_vap ] #Catastróficas: B2
  prima[ , pri_med_niv_7_cat       :=  B7_cat_vap /  M_vap ] #Catastróficas: B7
  prima[ , pri_med_niv_cat         :=  B_cat_vap / M_vap ]   #Catastróficas: B2+B7+B8
  prima[ , pri_med_niv_8_cat       :=  B8_cat_vap / M_vap ]  #Catastróficas: B8
  
  # Primas suficientes
  prima[ , pri_med_niv_apo_est_suf     := ( B2_vap + B9_vap + B6_vap + B3_vap + B4_vap - V0 ) /  M_vap ] # con aporte estatal AE
  prima[ , pri_med_niv_7_suf           :=  B7_vap /  M_vap ] # Menores de 18 años
  prima[ , pri_med_niv_8_suf           :=  B8_vap / M_vap ] # Conyuges
  
  # Prima definida por Mauricio ------------------------------------------------
  prima[ , pri_med_niv_2_6_8      := ( B2_ncat_vap + B6_ncat_vap + B8_ncat_vap + B9_vap - V0 ) /  M_vap ]# Afiliados, conyuges. viudas
  prima[ , pri_med_niv_3_4_t      := ( B3_vap + B4_vap ) /  M_vap ]# Jubilados
  prima[ , pri_med_niv_7_t        :=  B7_ncat_vap /  M_vap ] # Hijos
  prima[ , pri_med_niv_2_6_7_8_cat:= ( B2_cat_vap + B6_cat_vap + B8_cat_vap )/  M_vap ] #Catastróficas: B2,B6,B8
  prima[ , pri_med_niv_total      := ( B_vap - V0 ) /  M_vap ] # Todos
  prima[ , pri_med_niv_total_legal:= ( B_vap - V0 -(B3_vap + B4_vap + B2_cat_vap + B6_cat_vap + B7_cat_vap + B8_cat_vap) ) /  M_vap ] # Todos Legal
  # Primas Cristian Z
  prima[ , pri_3_4_ncat :=  ( B3_ncat_vap + B4_ncat_vap ) /  M_vap ]
  prima[ , pri_3_4_cat  :=  ( B3_cat_vap + B4_cat_vap )   /  M_vap ]
  prima[ , pri_7_ncat   :=  ( B7_ncat_vap ) /  M_vap ]
  prima[ , pri_7_cat    :=  ( B7_cat_vap )  /  M_vap ]
  prima[ , pri_2_cat    :=  ( B2_cat_vap )  /  M_vap ]
  prima[ , pri_2_ncat   :=  ( B2_ncat_vap ) /  M_vap ]
  prima[ , pri_vo_cat   :=  ( B6_cat_vap )  /  M_vap ]
  prima[ , pri_vo_ncat  :=  ( B6_ncat_vap ) /  M_vap ]
  prima[ , pri_ex_cat   :=  ( B8_cat_vap  ) /  M_vap ]
  prima[ , pri_ex_ncat  :=  ( B8_ncat_vap ) /  M_vap ]
  # prima <- prima[, list( pri_3_4_ncat, pri_3_4_cat, pri_7_ncat , pri_7_cat,
  #                        pri_2_cat, pri_2_ncat, pri_vo_cat, pri_vo_ncat,
  #                        pri_vo_cat, pri_vo_ncat, pri_ex_cat, pri_ex_ncat,
  #                        pri_7_ncat, pri_7_cat ) ]
  
  # prima <- prima[, list( M, B7, B7_cat, B7_ncat,
  #                        pri_7_ncat, pri_7_cat ) ]
  # write_csv(prima,'C:/Users/jendry.toapanta/Downloads/hijos_sal.csv')
  
  save( prima,
        file = paste0( parametros$RData_seg, 'IESS_SAL_primas_', escenario, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
