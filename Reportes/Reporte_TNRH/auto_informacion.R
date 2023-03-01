message( '\tEstableciendo información para la configuración del reporte' )

REP <- new.env()

# # Escenario 1 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_1'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_1<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_1 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

##COMENTADO
#REP$pri_med_niv_tnrh_esc_1<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
#                                    digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_1<-format( 100 * esc$apo_act$por_apo_est[1],
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_1<-format( 100 * esc$apo_act$i_a[1],
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# # Escenario 2 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_2'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_2<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_2 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )


#COEMNTADO
##REP$pri_med_niv_tnrh_esc_2<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
##                                    digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_2<-format( 100 * esc$apo_act$por_apo_est[1],
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_2<-format( 100 * esc$apo_act$i_a[1],
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )

# # Escenario 3 TNRH --------------------------------------------------------------------------------------
escenario <- 'escenario_3'
load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_IVM_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
REP$duracion_tnrh_esc_3<- max( which( agregado_financiero$reserva > 0 ) ) + parametros$anio_ini - 1
REP$balance_tnrh_esc_3 <- format( agregado_financiero$V[41],digits = 2, nsmall = 2, big.mark = '.', 
                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_apo_est_pen,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )

##COMENTADO
#REP$pri_med_niv_tnrh_esc_3<-format( 100*prima[ t == parametros$horizonte ]$pri_med_niv,
#                                    digits = 2, nsmall = 2, decimal.mark = ',', format = 'f' )

REP$apo_est_tnrh_esc_3<-format( 100 * esc$apo_act$por_apo_est[1],
                                digits = 2, nsmall = 2, big.mark = '.',
                                decimal.mark = ',', format = 'f' )

REP$tasa_act_tnrh_esc_3<-format( 100 * esc$apo_act$i_a[1],
                                 digits = 2, nsmall = 2, big.mark = '.',
                                 decimal.mark = ',', format = 'f' )
#################################
#LO DE ARRIBA ES DEL IVM (ANEXO)#
#################################



# # Escenario 1 --------------------------------------------------------------------------------------
# escenario <- 'escenario_1'
# load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$opcion_esc_1 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")
# 
# REP$bal_act_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_1 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_1 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1
# 
# # REP$sup_apo_esc_1 <- min( balance_anual[ A_est >= A & t > 0 ]$t ) + parametros$anio_ini
# 
# REP$cap_ini <- format( esc$V0, 
#                        digits = 2, nsmall = 2, big.mark = '.', 
#                        decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_ini_esc_1 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_1 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_1 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_1 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# # REP$factor_aporte_familias_esc_1 <- format( esc$factor, 
# #                                             digits = 2, nsmall = 2, big.mark = '.', 
# #                                             decimal.mark = ',', format = 'f' )
# 
# # REP$aporte_familias_esc_1 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)], 
# #                                      digits = 2, nsmall = 2, big.mark = '.', 
# #                                      decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_est_otr_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
#                                             digits = 2, nsmall = 2, big.mark = '.', 
#                                             decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_sgo_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
#                                              digits = 2, nsmall = 2, big.mark = '.', 
#                                              decimal.mark = ',', format = 'f' )
# 
# # REP$pri_med_niv_sgo_apo_est_pen_esc_1 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo_apo_est_pen,
# #                                                  digits = 2, nsmall = 2, big.mark = '.', 
# #                                                  decimal.mark = ',', format = 'f' )
# # 
# REP$porce_masa_dep_1 <- format(  esc$hip_esc$por_mas_rel_dep[1], digits = 4, nsmall = 2, big.mark = '.', 
#                                   decimal.mark = ',', format = 'f' )
#  
# # REP$apo_est_esc_1<-format( 100 * esc$aporte_estado,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_esc_1<-format( 100 * esc$aporte_estado / 0.4,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_dep_esc_1<-format( 100 * esc$aporte_estado_dep/0.003,
# #                                digits = 2, nsmall = 2, big.mark = '.', 
# #                                decimal.mark = ',', format = 'f' )
# # 
# # REP$valor_est_fijo_esc_1 <- format( esc$aporte_estado_fijo,
# #                                digits = 2, nsmall = 2, big.mark = '.', 
# #                                decimal.mark = ',', format = 'f' )
# # 
# # REP$porce_valor_est_fijo_esc_1 <- format( 100 * esc$por_aporte_estado_fijo,
# #                                     digits = 2, nsmall = 2, big.mark = '.', 
# #                                     decimal.mark = ',', format = 'f' )
# # 
# # REP$tasa_act_esc_1<-format( 100 * esc$i_a,
# #                             digits = 2, nsmall = 2, big.mark = '.', 
# #                             decimal.mark = ',', format = 'f' )
# 
# # Escenario 2 --------------------------------------------------------------------------------------
# escenario <- 'escenario_2'
# load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$opcion_esc_2 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")
# 
# REP$bal_act_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_2 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_2 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini -1
# 
# REP$dep_tas_ini_esc_2 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_2 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_2 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_2 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# # REP$factor_aporte_familias_esc_2 <- format( esc$factor, 
# #                                       digits = 2, nsmall = 2, big.mark = '.', 
# #                                       decimal.mark = ',', format = 'f' )
# # 
# # REP$aporte_familias_esc_2 <- format( esc$factor * esc$apo_act_TNRH * balance[ t==0, unique(sbu)], 
# #                                      digits = 2, nsmall = 2, big.mark = '.', 
# #                                      decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_est_otr_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
#                                          digits = 2, nsmall = 2, big.mark = '.', 
#                                          decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_sgo_esc_2 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
#                                      digits = 2, nsmall = 2, big.mark = '.', 
#                                      decimal.mark = ',', format = 'f' )
# 
# # REP$apo_est_esc_2 <- format( 100 * esc$aporte_estado,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_esc_2 <- format( 100 * esc$aporte_estado/0.4,
# #                                   digits = 2, nsmall = 2, big.mark = '.', 
# #                                   decimal.mark = ',', format = 'f' )
# # 
# # REP$apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep,
# #                                digits = 2, nsmall = 2, big.mark = '.', 
# #                                decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_dep_esc_2 <- format( 100 * esc$aporte_estado_dep/0.003,
# #                                       digits = 2, nsmall = 2, big.mark = '.', 
# #                                       decimal.mark = ',', format = 'f' )
# # 
# # REP$valor_est_fijo_esc_2 <- format( esc$aporte_estado_fijo,
# #                                     digits = 2, nsmall = 2, big.mark = '.', 
# #                                     decimal.mark = ',', format = 'f' )
# # 
# # REP$porce_valor_est_fijo_esc_2 <- format( 100 * esc$por_aporte_estado_fijo,
# #                                           digits = 2, nsmall = 2, big.mark = '.', 
# #                                           decimal.mark = ',', format = 'f' )
# # 
# # REP$tasa_act_esc_2 <- format( 100 * esc$i_a,
# #                             digits = 2, nsmall = 2, big.mark = '.', 
# #                             decimal.mark = ',', format = 'f' )
# 
# # Escenario 3 --------------------------------------------------------------------------------------
# escenario <- 'escenario_3'
# load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_primas_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', esc$nombre, '.RData' ) )
# load( paste0( parametros$RData_seg, 'IESS_TNRH_analisis_ratios_', esc$nombre, '.RData' ) )
# 
# REP$opcion_esc_3 <- ifelse( balance_anual[ t == parametros$horizonte ]$V < 0, "déficit", "superávit")
# 
# REP$bal_act_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$bal_cap_esc_3 <- format( balance_anual[ t == parametros$horizonte ]$V_cap, 
#                              digits = 2, nsmall = 2, big.mark = '.', 
#                              decimal.mark = ',', format = 'f' )
# 
# REP$duracion_esc_3 <- max( which( balance_anual$V_cap > 0 ) ) + parametros$anio_ini - 1
# 
# REP$dep_tas_ini_esc_3 <- format( ratios[ t == 1 ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$dep_tas_fin_esc_3 <- format( ratios[ t == parametros$horizonte ]$dep_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_ini_esc_3 <- format( 100 * ratios[ t == 1 ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# REP$sus_tas_fin_esc_3 <- format( 100 * ratios[ t == parametros$horizonte ]$rem_tasa,
#                                  digits = 2, nsmall = 2, big.mark = '.', 
#                                  decimal.mark = ',', format = 'f' )
# 
# # REP$factor_aporte_familias_esc_3 <- format( esc$factor, 
# #                                             digits = 2, nsmall = 2, big.mark = '.', 
# #                                             decimal.mark = ',', format = 'f' )
# # 
# # REP$aporte_familias_esc_3 <- format( esc$factor * esc$apo_act_ssc * balance[ t==0, unique(sbu)], 
# #                                             digits = 2, nsmall = 2, big.mark = '.', 
# #                                             decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_est_otr_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_reg,
#                                          digits = 2, nsmall = 2, big.mark = '.', 
#                                          decimal.mark = ',', format = 'f' )
# 
# REP$pri_med_niv_sgo_esc_3 <- format( 100 * prima[ t == parametros$horizonte ]$pri_med_niv_sgo,
#                                      digits = 2, nsmall = 2, big.mark = '.', 
#                                      decimal.mark = ',', format = 'f' )
# 
# # REP$apo_est_esc_3 <- format( 100 * esc$aporte_estado,
# #                            digits = 2, nsmall = 2, big.mark = '.', 
# #                            decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_esc_3 <- format( 100 * esc$aporte_estado/0.4,
# #                                   digits = 2, nsmall = 2, big.mark = '.', 
# #                                   decimal.mark = ',', format = 'f' )
# # 
# # REP$apo_est_dep_esc_3 <- format( 100 * esc$aporte_estado_dep,
# #                                digits = 2, nsmall = 2, big.mark = '.', 
# #                                decimal.mark = ',', format = 'f' )
# # 
# # REP$porcen_apo_est_dep_esc_3 <- format( 100 * esc$aporte_estado_dep/0.003,
# #                                       digits = 2, nsmall = 2, big.mark = '.', 
# #                                       decimal.mark = ',', format = 'f' )
# # 
# # REP$valor_est_fijo_esc_3 <- format( esc$aporte_estado_fijo,
# #                                     digits = 2, nsmall = 2, big.mark = '.', 
# #                                     decimal.mark = ',', format = 'f' )
# # 
# # REP$porce_valor_est_fijo_esc_3 <- format( 100 * esc$por_aporte_estado_fijo,
# #                                           digits = 2, nsmall = 2, big.mark = '.', 
# #                                           decimal.mark = ',', format = 'f' )
# # REP$tasa_act_esc_3 <- format( 100 * esc$i_a,
# #                             digits = 2, nsmall = 2, big.mark = '.', 
# #                             decimal.mark = ',', format = 'f' )
