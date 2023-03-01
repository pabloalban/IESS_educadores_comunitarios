message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# Escenario 1 --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_CES_configuracion_', 'escenario_1', '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_CES_balances_', 'escenario_1', '.RData' ) )

REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$S_vap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$C_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$i_a_esc_1 <- format( esc$hip_esc$i_a[2] * 100,
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$i_q_esc_1 <- format( esc$hip_esc$i_q[2] * 100,
                              digits = 2, nsmall = 5, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

REP$i_r_esc_1 <- format( esc$hip_esc$i_r[2] * 100,
                         digits = 2, nsmall = 5, big.mark = '.',
                         decimal.mark = ',', format = 'f' )

REP$aporte_esc_1 <- format( (esc$hip_esc$apo_per[2]+esc$hip_esc$apo_pat[2])*100,
                         digits = 2, nsmall = 2, big.mark = '.',
                         decimal.mark = ',', format = 'f' )

REP$apo_per_esc_1 <- format( (esc$hip_esc$apo_per[2])*100,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )


REP$apo_pat_esc_1 <- format( (esc$hip_esc$apo_pat[2])*100,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


REP$por_gas_esc_1 <- format( esc$hip_esc$por_gas[2] * 100,
                         digits = 2, nsmall = 2, big.mark = '.',
                         decimal.mark = ',', format = 'f' )


REP$Act_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte ]$V0 +
                              esc$S0  + 
                              balance_anual[ t == parametros$horizonte ]$A2_per_vap +
                              balance_anual[ t == parametros$horizonte ]$A2_pat_vap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$A2_vap_esc_1   <- format( balance_anual[ t == parametros$horizonte ]$A2_vap  ,
                        digits = 2, nsmall = 2, big.mark = '.',
                        decimal.mark = ',', format = 'f' )

REP$A_fin_vap_esc_1   <- format( balance_anual[ t == parametros$horizonte ]$A_fin_vap  ,
                        digits = 2, nsmall = 2, big.mark = '.',
                        decimal.mark = ',', format = 'f' )

REP$Pas_vap_esc_1  <- format( balance_anual[ t == parametros$horizonte ]$B_vap +
                              balance_anual[ t == parametros$horizonte ]$G_vap +
                              balance_anual[ t == parametros$horizonte ]$C_vap,
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$B_vap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$B_vap,
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$G_vap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$G_vap,
                     digits = 2, nsmall = 2, big.mark = '.',
                     decimal.mark = ',', format = 'f' )

REP$cap_ini_esc_1 <- format( esc$V0,
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )

REP$saldo_ini_esc_1 <- format( esc$S0,
                       digits = 2, nsmall = 2, big.mark = '.',
                       decimal.mark = ',', format = 'f' )


# Escenario 2 --------------------------------------------------------------------------------------
escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_CES_configuracion_', 'escenario_2', '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_CES_balances_', 'escenario_2', '.RData' ) )

REP$bal_act_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$bal_cap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V_cap,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$S_vap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$C_vap,
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$i_a_esc_2 <- format( esc$hip_esc$i_a[2] * 100,
                         digits = 2, nsmall = 2, big.mark = '.',
                         decimal.mark = ',', format = 'f' )

REP$i_q_esc_2 <- format( esc$hip_esc$i_q[2] * 100,
                         digits = 2, nsmall = 5, big.mark = '.',
                         decimal.mark = ',', format = 'f' )

REP$i_r_esc_2 <- format( esc$hip_esc$i_r[2] * 100,
                         digits = 2, nsmall = 5, big.mark = '.',
                         decimal.mark = ',', format = 'f' )

REP$aporte_esc_2 <- format( (esc$hip_esc$apo_per[2]+esc$hip_esc$apo_pat[2])*100,
                            digits = 2, nsmall = 2, big.mark = '.',
                            decimal.mark = ',', format = 'f' )

REP$apo_per_esc_2 <- format( (esc$hip_esc$apo_per[2])*100,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )


REP$apo_pat_esc_2 <- format( (esc$hip_esc$apo_pat[2])*100,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$por_gas_esc_2 <- format( esc$hip_esc$por_gas[2] * 100,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$Act_vap_esc_2  <- format( balance_anual[ t == parametros$horizonte ]$V0 +
                                esc$S0  + 
                                balance_anual[ t == parametros$horizonte ]$A2_per_vap +
                                balance_anual[ t == parametros$horizonte ]$A2_pat_vap ,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

REP$A2_vap_esc_2   <- format( balance_anual[ t == parametros$horizonte ]$A2_vap  ,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

REP$A_fin_vap_esc_2   <- format( balance_anual[ t == parametros$horizonte ]$A_fin_vap  ,
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

REP$Pas_vap_esc_2  <- format( balance_anual[ t == parametros$horizonte ]$B_vap +
                                balance_anual[ t == parametros$horizonte ]$G_vap +
                                balance_anual[ t == parametros$horizonte ]$C_vap,
                              digits = 2, nsmall = 2, big.mark = '.',
                              decimal.mark = ',', format = 'f' )

REP$B_vap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$B_vap,
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$G_vap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$G_vap,
                           digits = 2, nsmall = 2, big.mark = '.',
                           decimal.mark = ',', format = 'f' )

REP$cap_ini_esc_2 <- format( esc$V0,
                             digits = 2, nsmall = 2, big.mark = '.',
                             decimal.mark = ',', format = 'f' )

REP$saldo_ini_esc_2 <- format( esc$S0,
                               digits = 2, nsmall = 2, big.mark = '.',
                               decimal.mark = ',', format = 'f' )