message( paste( rep('-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------
# Datos globales
load( paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_aportes.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) )
load( paste0( parametros$RData, 'SAL/', 'IESS_SAL_estimacion.RData' ) ) # ben_est_fre, ben_est_sev

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros', 'proy', 'aportes', 'tasas_macro_pred',
                                 'tab_var_proj', 'ben_est_fre', 'ben_est_sev') ) ] )

# Funcion de negacion ------------------------------------------------------------------------------
"%notin%" <- Negate("%in%")

tasas_macro_pred <- as.data.table( tasas_macro_pred )
tasas_macro_pred[ , t := anio - parametros$anio ]

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Balance corriente --------------------------------------------------------------------------------
message( '\tGenerando balance corriente' )
balance <- copy( esc$hip_esc )
balance <- merge.data.table( balance, tasas_macro_pred[ , list( t , sbu )], by=c('t'), all.x=T )
balance <- merge.data.table( expand.grid( t = 0:parametros$horizonte, x = 0:105, sexo = c('Female', 'Male') ),
                             balance, by = c('t'),all.x = TRUE)

proy[ is.na( proy )] <- 0

balance <- merge.data.table( balance, proy[, list( t, sexo, x, l2, l3, l4, l6, l7, l8, l9, B3, B4, B8, B9, 
                                                   l25, l35, l45, l65, l75, l85, l95 ) ], 
                             by=c('t', 'x', 'sexo'), all.x = T )

setorder( balance, t, sexo, x)
balance <- as.data.table( merge( balance, aportes, by = c('t', 'sexo', 'x' ), all.x = T ) )
balance[ is.na(balance)] <- 0

# Beneficios de renta vitalicia --------------------------------------------------------------------
message( '\tProyectando beneficios por pensiones' )
# Beneficio por pensiones de vejez
balance[ , B3:= cal_pen_vej * B3 ]

# Beneficio por pensiones de invalidez
balance[ , B4:= cal_pen_inv * B4 ]

# Beneficios de montepios
# Orfandad
balance[ , B8 := inc_montepio * cal_pen_hue * B8  ]

# Viudedad
balance[ , B9 := inc_montepio * cal_pen_viu * B9  ]

# Todo montepio
balance[ , B_pen_mon := B8 + B9  ]

# Beneficios por pensiones
balance[ , B_pen := B3 + B4 + B8 + B9 ]
# balance[, list( B3=sum(B3, na.rm=T), B4=sum(B4, na.rm=T), B8=sum(B8, na.rm=T), B9=sum(B9, na.rm=T)), by=list(t, sexo)]

# Beneficios por transición a muerto ---------------------------------------------------------------
message( '\tProyectando beneficios por auxilio de funerales' )
aux <- copy( tab_var_proj[ t >= parametros$anio_ini & t <= parametros$horizonte + parametros$anio_ini, 
                           list( t, por_aux = var ) ] )
aux[ , t := t - parametros$anio ]
balance <- merge( balance, aux, by =  c( 't' ), all.x = TRUE )
balance[ , B2_5 := (ben_fun * bas_ref * sbu ) * por_aux * l25 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l25 ]
balance[ , B3_5 := (ben_fun * bas_ref * sbu ) * por_aux * l35 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l35 ]   
balance[ , B4_5 := (ben_fun * bas_ref * sbu ) * por_aux * l45 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l45 ]
balance[ , B6_5 := (ben_fun * bas_ref * sbu ) * por_aux * l65 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l65 ]
balance[ , B7_5 := (ben_fun * bas_ref * sbu ) * por_aux * l75 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l75 ]
balance[ , B8_5 := (ben_fun * bas_ref * sbu ) * por_aux * l85 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l85 ]
balance[ , B9_5 := (ben_fun * bas_ref * sbu ) * por_aux * l95 + inc_fun * (ben_fun * bas_ref * sbu ) * ( 1 - por_aux ) * l95 ]

balance[ , B2_5 := cal_aux_fun * B2_5 ]
balance[ , B3_5 := cal_aux_fun * B3_5 ]
balance[ , B4_5 := cal_aux_fun * B4_5 ]
balance[ , B6_5 := cal_aux_fun * B6_5 ]
balance[ , B7_5 := cal_aux_fun * B7_5 ]
balance[ , B8_5 := inc_montepio * cal_aux_fun * B8_5 ]
balance[ , B9_5 := inc_montepio * cal_aux_fun * B9_5 ]

# Beneficios por auxilio de funerales
balance[ , B_aux := B2_5 + B3_5 + B4_5 + B6_5 + B7_5 + B8_5 + B9_5 ]

# Beneficios por pensiones y auxilio de funerales
balance[ , B_pen_aux := B_pen + B_aux ]

# Beneficios de salud ------------------------------------------------------------------------------
ben_sal <- balance[ , list( t, sexo, x, l2, l3, l4, l6, l7, l8, l9, M ) ]
ben_sal[ ,l6_m18 := l6]
ben_sal[ x >= 18 , l6_m18 := 0]
ben_sal[ sexo == 'Female', sexo := 'F' ]
ben_sal[ sexo == 'Male', sexo := 'M' ]

# Estimación de beneficios de salud
ben_sal[ , u := as.character( cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                   include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
aux <- copy( ben_sal )
aux[ , u := as.character( cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                               include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
ben_sal <- rbind( ben_sal, aux )


ben_sal <- merge.data.table( ben_sal,
                             esc$hip_esc[ , list( t, u_m, inc_montepio, cal_ben_sal, cal_l2_2021 ) ], 
                             by = c( 't' ),
                             all.x = TRUE )

ben_sal <- merge.data.table( ben_sal, 
                             ben_est_sev[ , list( sexo, u, enf, ser, cap, icd, ED, EX, q_p, q_s, q_c ) ],
                             by = c( 'sexo', 'u' ), 
                             all.x = TRUE, 
                             allow.cartesian = TRUE )

ben_sal <- merge.data.table( ben_sal, 
                             ben_est_fre[ t == t_max, list( sexo, u, enf, lambda ) ],
                             by = c( 'sexo', 'u', 'enf' ), 
                             all.x = TRUE, 
                             allow.cartesian = TRUE )

ben_sal[ is.na( q_p ), q_p := 0 ]
ben_sal[ is.na( q_s ), q_s := 0 ]
ben_sal[ is.na( q_c ), q_c := 0 ]
ben_sal[ is.na( lambda ), lambda := 0 ]
ben_sal[ is.na( ED ), ED := 0 ]
ben_sal[ is.na( EX ), EX := 0 ]

# Inflación médica
ben_sal[ , EX := u_m * EX ]

# Población total cubierta
ben_sal[ , l := l2 + l3 + l4 + l6_m18 + l7 + l8 ] # Considero que debe ser a todos los hijos menores de 18 años

# Probabilidad de enfermar
q_e <- 1.0

# Proyección de beneficios por grupo de asegurados
ben_sal[ ser != 'HO', B2_sal := cal_l2_2021 * cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l2 ]
ben_sal[ ser == 'HO', B2_sal := cal_l2_2021 * cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l2 ]
ben_sal[ ser != 'HO', B3_sal := cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l3 ]
ben_sal[ ser == 'HO', B3_sal := cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l3 ]
ben_sal[ ser != 'HO', B4_sal := cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l4 ]
ben_sal[ ser == 'HO', B4_sal := cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l4 ]
ben_sal[ , B6_sal := 0 ]
ben_sal[ ser != 'HO' & x < 18, B6_sal := cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l6 ] 
ben_sal[ ser == 'HO' & x < 18, B6_sal := cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l6 ]
ben_sal[ ser != 'HO', B7_sal := cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l7 ]
ben_sal[ ser == 'HO', B7_sal := cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l7 ]
ben_sal[ , B8_sal := 0 ]
ben_sal[ ser != 'HO' & x < 18, B8_sal := inc_montepio * cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l8 ] 
ben_sal[ ser == 'HO' & x < 18, B8_sal := inc_montepio * cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l8 ]
ben_sal[ ser != 'HO', B9_sal := inc_montepio * cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l9 ]
ben_sal[ ser == 'HO', B9_sal := inc_montepio * cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l9 ]

# Beneficios totales
ben_sal[ , B_pen_sal := B3_sal + B4_sal + B8_sal + B9_sal ]
ben_sal[ , B_cot_dep_sal := B2_sal + B6_sal + B7_sal ]
ben_sal[ , B_sal := B2_sal + B3_sal + B4_sal + B6_sal + B7_sal + B8_sal + B9_sal ]

ben_sal_tot <- ben_sal[ enf == 'E', list( B_sal = sum( B_sal, na.rm = TRUE ),
                                          B_pen_sal = sum( B_pen_sal, na.rm = TRUE ),
                                          B_cot_dep_sal = sum( B_cot_dep_sal, na.rm = TRUE ),
                                          B2_sal = sum( B2_sal, na.rm = TRUE ),
                                          B3_sal = sum( B3_sal, na.rm = TRUE ), 
                                          B4_sal = sum( B4_sal, na.rm = TRUE ),
                                          B6_sal = sum( B6_sal, na.rm = TRUE ),
                                          B7_sal = sum( B7_sal, na.rm = TRUE ),
                                          B8_sal = sum( B8_sal, na.rm = TRUE ),
                                          B9_sal = sum( B9_sal, na.rm = TRUE ) ),
                        by = list( t, sexo, x ) ]

ben_sal_tot <- merge.data.table( ben_sal_tot,
                                 ben_sal[ enf == 'C', list( B_sal_cat = sum( B_sal, na.rm = TRUE ),
                                                            B_pen_sal_cat = sum( B_pen_sal, na.rm = TRUE ),
                                                            B_cot_dep_sal_cat = sum( B_cot_dep_sal, na.rm = TRUE ),
                                                            B2_sal_cat = sum( B2_sal, na.rm = TRUE ),
                                                            B3_sal_cat = sum( B3_sal, na.rm = TRUE ), 
                                                            B4_sal_cat = sum( B4_sal, na.rm = TRUE ),
                                                            B6_sal_cat = sum( B6_sal, na.rm = TRUE ),
                                                            B7_sal_cat = sum( B7_sal, na.rm = TRUE ),
                                                            B8_sal_cat = sum( B8_sal, na.rm = TRUE ),
                                                            B9_sal_cat = sum( B9_sal, na.rm = TRUE ) ),
                                          by = list( t, sexo, x ) ],
                                 by = c( 't', 'sexo', 'x' ), 
                                 all.x = TRUE )

ben_sal_tot[ sexo=='F', sexo := 'Female']
ben_sal_tot[ sexo=='M', sexo := 'Male']

#Proyectando aportes -------------------------------------------------------------------------------
message( '\tProyectando aportes' )
# Incluyendo beneficios por asistencia médica
balance <- merge.data.table( balance,
                             ben_sal_tot, 
                             by = c( 't', 'sexo', 'x' ),
                             all.x = TRUE )

# balance[ , MS2 := 12 * 0.225 * sbu * l2 ]
#balance[, list( MS2=sum(MS2, na.rm=T), MS=sum(MS, na.rm=T)), by=list(t, sexo)]

# Aportes de activos del SSC
balance[ , A2 := cal_apo_ssc * ( apo_ind + apo_inv ) * MS ]
balance[ t == 0 , A2 := 0 ]
# balance[ , A22 := cal_apo_ssc * ( apo_ind + apo_inv ) * MS2]
#balance[, list( A2=sum(A2, na.rm=T), A22=sum(A22, na.rm=T)), by=list(t, sexo)]

# Aportes de activos del sgo
balance[ , A_sgo := cal_apo_sgo * apo_sgo * M ]
balance[ t == 0 , A_sgo := 0 ]

# Aporte estado ------------------------------------------------------------------------------------
# Aporte del Estado 40%
balance[ , A_est_pen := cal_apo_est * apo_est * B_pen ]
balance[ t == 0 , A_est_pen := 0 ]

# Aportes del Estado 0.3%
balance[ , MD := cal_masa_dep * MD ]
balance[ , A_est_rel_dep := cal_apo_est_rel_dep * apo_est_rel_dep * MD ]
balance[ t == 0 , A_est_rel_dep := 0 ]

# Aportes del Estado para enfermedades catastróficas y salud de jubilados
balance[ , A_est_cat := cal_apo_est_cat * apo_est_cat * B_sal_cat ]
balance[ t == 0, A_est_cat := 0 ]

# Aporte del Estado para salud de pensionistas
balance[ , A_est_pen_sal := cal_apo_est_pen_sal * apo_est_pen_sal * B_pen_sal ]
balance[ t == 0 , A_est_pen_sal := 0 ]

# Agregación de aportes ----------------------------------------------------------------------------
# Aportes no estatales
balance[ , A_afi := A2 + A_sgo ]
balance[ t == 0 , A_afi := 0 ]

# Gasto administrativo
balance[ , G := cal_gast_adm * ( ( por_gas_masa * M) + por_gas_apor * A2 )  ]
balance[ t == 0 , G := 0 ]

# Excedente Gasto administrativo
balance[ , Exc_G := G * cal_exc_gast_adm ]
balance[ t == 0 , Exc_G := 0]
balance[ , G := G * cal_prom_gast_adm ]

# Aporte otros -------------------------------------------------------------------------------------
t_max <- parametros$horizonte
apo_otr <- copy( esc$hip_esc[ , list( t, cal_apo_est_fij, apo_est_fij, paga_issfa, apo_issfa,
                                       paga_isspol, apo_isspol, por_sp  ) ] )

# Aporte del Estado fijo 
apo_otr[ , A_est_fij  := cal_apo_est_fij * apo_est_fij ]
apo_otr[ t == 0 , A_est_fij := 0 ]

# Aportes del ISSFA  
issfa <- copy( aportes[ , list(  A_issfa_RT = sum( A_issfa_RT, na.rm = T ), 
                                 A_issfa_NS = sum( A_issfa_NS , na.rm = T ),
                                 A_issfa_Total = sum( A_issfa_Total, na.rm=T), 
                                 A_isspol = sum(A_isspol, na.rm = T ),
                                 A_sp = sum( A_sp, na.rm=T ) ), by = t  ] )
apo_otr <- merge( apo_otr, issfa, by = 't', all.x = TRUE )
apo_otr[ , A_issfa:= paga_issfa * apo_issfa * A_issfa_NS ]
apo_otr[ t == 0, A_issfa := 0 ]

# Aportes del ISSPOL
apo_otr[ , A_isspol:= paga_isspol * apo_isspol * A_isspol ]
apo_otr[ t == 0, A_isspol := 0 ]

# Aportes de Seguros Privados y Medicina Prepagada
apo_otr[ , A_seg_pri:= por_sp * A_sp]
apo_otr[ t == 0, A_seg_pri := 0 ]

# Balance corriente anual --------------------------------------------------------------------------
balance_anual <- balance[ , list( M = sum( M, na.rm = TRUE ),
                                  MD = sum( MD, na.rm = TRUE ),
                                  MS = sum( MS, na.rm = TRUE ),
                                  
                                  A2 = sum( A2, na.rm = TRUE ),
                                  A_sgo = sum( A_sgo, na.rm = TRUE ),
                                  A_est_pen = sum( A_est_pen, na.rm = TRUE ),
                                  A_est_rel_dep = sum( A_est_rel_dep, na.rm = TRUE ),
                                  A_est_cat = sum( A_est_cat, na.rm = TRUE ),
                                  A_est_pen_sal = sum( A_est_pen_sal, na.rm = TRUE ),
                                  #A_est = sum( A_est, na.rm = TRUE ),
                                  A_afi = sum( A_afi, na.rm = TRUE ),
                                  #A_afi_est = sum( A_afi_est, na.rm = TRUE ),
                                  
                                  B3 = sum( B3, na.rm = TRUE ),
                                  B4 = sum( B4, na.rm = TRUE ),
                                  B8 = sum( B8, na.rm = TRUE ),
                                  B9 = sum( B9, na.rm = TRUE ),
                                  B_pen_mon = sum( B_pen_mon, na.rm = TRUE ),
                                  B_pen = sum( B_pen, na.rm = TRUE ),
                                  
                                  B2_5 = sum( B2_5, na.rm = TRUE ),
                                  B3_5 = sum( B3_5, na.rm = TRUE ),
                                  B4_5 = sum( B4_5, na.rm = TRUE ),
                                  B6_5 = sum( B6_5, na.rm = TRUE ),
                                  B7_5 = sum( B7_5, na.rm = TRUE ),
                                  B8_5 = sum( B8_5, na.rm = TRUE ),
                                  B9_5 = sum( B9_5, na.rm = TRUE ),
                                  B_aux = sum( B_aux, na.rm = TRUE ),
                                  
                                  B_pen_aux = sum( B_pen_aux, na.rm = TRUE ),
                                  
                                  B2_sal = sum( B2_sal, na.rm = TRUE ),
                                  B3_sal = sum( B3_sal, na.rm = TRUE ),
                                  B4_sal = sum( B4_sal, na.rm = TRUE ),
                                  B6_sal = sum( B6_sal, na.rm = TRUE ),
                                  B7_sal = sum( B7_sal, na.rm = TRUE ),
                                  B8_sal = sum( B8_sal, na.rm = TRUE ),
                                  B9_sal = sum( B9_sal, na.rm = TRUE ),
                                  B_sal = sum( B_sal, na.rm = TRUE ),
                                  B_pen_sal = sum( B_pen_sal, na.rm = TRUE ),
                                  B_cot_dep_sal = sum( B_cot_dep_sal, na.rm = TRUE ),
                                  
                                  B2_sal_cat = sum( B2_sal_cat, na.rm = TRUE ),
                                  B3_sal_cat = sum( B3_sal_cat, na.rm = TRUE ),
                                  B4_sal_cat = sum( B4_sal_cat, na.rm = TRUE ),
                                  B6_sal_cat = sum( B6_sal_cat, na.rm = TRUE ),
                                  B7_sal_cat = sum( B7_sal_cat, na.rm = TRUE ),
                                  B8_sal_cat = sum( B8_sal_cat, na.rm = TRUE ),
                                  B9_sal_cat = sum( B9_sal_cat, na.rm = TRUE ),
                                  B_sal_cat = sum( B_sal_cat, na.rm = TRUE ),
                                  B_pen_sal_cat = sum( B_pen_sal_cat, na.rm = TRUE ),
                                  B_cot_dep_sal_cat = sum( B_cot_dep_sal_cat, na.rm = TRUE ),
                                  
                                  B_sal_cat_total = sum( B_sal, na.rm = TRUE ) + sum( B_sal_cat, na.rm = TRUE ),
                                  
                                  G = sum( G, na.rm = TRUE ),
                                  Exc_G = sum( Exc_G, na.rm = TRUE )), 
                          by = list( t ) ]

balance_anual <- merge.data.table( balance_anual, 
                                   apo_otr,
                                   by = c( 't' ) )

balance_anual <- merge.data.table( esc$hip_esc[ , list( t, u, v ) ],
                                   balance_anual, 
                                   by = c( 't' ) )

balance_anual[ , A_otr :=  A_issfa + A_isspol + A_seg_pri ]

# Contribución total del estado 
balance_anual[ , A_est := A_est_pen + A_est_rel_dep + A_est_cat + A_est_pen_sal + A_est_fij ]
balance_anual[ t == 0 , A_est := 0 ]

# Aportes totales
balance_anual[ , A_afi_est := A_afi + A_est ]

# Flujos de activos y pasivos
balance_anual[ , A := A_afi_est + A_otr ]
balance_anual[ t == 0 , A := 0 ]
balance_anual[ , B := B_pen + B_aux + B_sal + B_sal_cat ]
balance_anual[ , interes := 0 ]
balance_anual[ , i_d := esc$hip_esc$i_d ]
balance_anual[ , AA := A ]
balance_anual[ t == 0, RES := esc$V0 ]
balance_anual[ t == 0, Resul := 0 ]
balance_anual[ t == 0, Exc_G := esc$exc_gast_adm ]

balance_anual$interes[1] <- esc$interes_reserva
reserva_ini <- esc$V0

for ( i in 2:dim(balance_anual)[1] ) { # i <- 3
  balance_anual$AA[i] <- balance_anual$AA[i] + balance_anual$interes[i-1]
  balance_anual$interes[i] <- reserva_ini * balance_anual$i_d[ i ] + ( ( 1 + balance_anual$i_d[ i ] )^(1/2) - 1 ) *
                              ( balance_anual$AA[i] - balance_anual$B[i] - balance_anual$G[i] ) 
  balance_anual$Resul[i] <- balance_anual$AA[i] - balance_anual$B[i] - balance_anual$G[i]
  balance_anual$RES[i] <- reserva_ini  + balance_anual$Resul[i] + balance_anual$Exc_G[i-1]
  reserva_ini <- balance_anual$RES[i] 
  
  
  }
balance_anual[ t==0, interes:=0]
balance_anual[ , V_cor := Resul ]

# balance_anual[ t == 0, `:=`(  M = 0, MD = 0, MS = 0,
#                               
#                               A2 = 0, A_sgo = 0, A_est_pen = 0, A_est_rel_dep = 0, A_est_cat = 0, 
#                               A_est_pen_sal = 0, A_est = 0, A_afi = 0, A_afi_est = 0, 
#                               
#                               A_est_fij = 0, A_issfa = 0, A_isspol = 0, A_seg_pri = 0, A_otr = 0, 
#                               A = 0,
#                               
#                               B3_ant = 0, B3_nue = 0, B3 = 0, 
#                               B4_ant = 0, B4_nue = 0, B4 = 0, 
#                               B8 = 0, B9 = 0, B_pen_mon = 0, B_pen = 0,
#                               
#                               B2_5 = 0, B3_5 = 0, B4_5 = 0, B6_5 = 0, B7_5 = 0, B8_5 = 0, B9_5 = 0, 
#                               B_aux = 0, 
#                               
#                               B2_sal = 0, B3_sal = 0, B4_sal = 0, B6_sal = 0, B7_sal = 0, 
#                               B8_sal = 0, B9_sal = 0, B_sal = 0, B_pen_sal = 0, B_cot_dep_sal = 0,
#                               
#                               B2_sal_cat = 0, B3_sal_cat = 0, B4_sal_cat = 0, B6_sal_cat = 0, 
#                               B7_sal_cat = 0, B8_sal_cat = 0, B9_sal_cat = 0, B_sal_cat = 0, 
#                               B_pen_sal_cat = 0, B_cot_dep_sal_cat = 0, B = 0,
#                               
#                               G = 0,
#                               
#                               V_cor = 0 ) ]

balance_anual[ , V_cap := RES ]

# Balance actuarial
balance_anual[ , `:=`(  M_vap = cumsum( v * M ),
                        MD_vap = cumsum( v * MD ),
                        MS_vap = cumsum( v * MS ),
                        
                        A2_vap = cumsum( v * A2 ),
                        A_sgo_vap = cumsum( v * A_sgo ),
                        A_est_pen_vap = cumsum( v * A_est_pen ),
                        A_est_rel_dep_vap = cumsum( v * A_est_rel_dep ),
                        A_est_cat_vap = cumsum( v * A_est_cat ),
                        A_est_pen_sal_vap = cumsum( v * A_est_pen_sal ),
                        A_est_vap = cumsum( v * A_est ),
                        A_afi_vap = cumsum( v * A_afi ),
                        A_afi_est_vap = cumsum( v * A_afi_est ),
                        
                        A_est_fij_vap = cumsum( v * A_est_fij ),
                        A_issfa_vap = cumsum( v * A_issfa ),
                        A_isspol_vap = cumsum( v * A_isspol ),
                        A_seg_pri_vap = cumsum( v * A_seg_pri ),
                        A_otr_vap = cumsum( v * A_otr ),
                        
                        B3_vap = cumsum( v * B3 ),
                        B4_vap = cumsum( v * B4 ),
                        B8_vap = cumsum( v * B8 ),
                        B9_vap = cumsum( v * B9 ),
                        B_pen_mon_vap = cumsum( v * B_pen_mon ),
                        B_pen_vap = cumsum( v * B_pen ),
                        
                        B2_5_vap = cumsum( v * B2_5 ),
                        B3_5_vap = cumsum( v * B3_5 ),
                        B4_5_vap = cumsum( v * B4_5 ),
                        B6_5_vap = cumsum( v * B6_5 ),
                        B7_5_vap = cumsum( v * B7_5 ),
                        B8_5_vap = cumsum( v * B8_5 ),
                        B9_5_vap = cumsum( v * B9_5 ),
                        B_aux_vap = cumsum( v * B_aux ),
                        
                        B_pen_aux_vap = cumsum( v * B_pen_aux ),
                        
                        B2_sal_vap = cumsum( v * B2_sal ),
                        B3_sal_vap = cumsum( v * B3_sal ),
                        B4_sal_vap = cumsum( v * B4_sal ),
                        B6_sal_vap = cumsum( v * B6_sal ),
                        B7_sal_vap = cumsum( v * B7_sal ),
                        B8_sal_vap = cumsum( v * B8_sal ),
                        B9_sal_vap = cumsum( v * B9_sal ),
                        B_sal_vap = cumsum( v * B_sal ),
                        B_pen_sal_vap = cumsum( v * B_pen_sal ),
                        B_cot_dep_sal_vap = cumsum( v * B_cot_dep_sal ),
                        
                        B2_sal_cat_vap = cumsum( v * B2_sal_cat ),
                        B3_sal_cat_vap = cumsum( v * B3_sal_cat ),
                        B4_sal_cat_vap = cumsum( v * B4_sal_cat ),
                        B6_sal_cat_vap = cumsum( v * B6_sal_cat ),
                        B7_sal_cat_vap = cumsum( v * B7_sal_cat ),
                        B8_sal_cat_vap = cumsum( v * B8_sal_cat ),
                        B9_sal_cat_vap = cumsum( v * B9_sal_cat ),
                        B_sal_cat_vap = cumsum( v * B_sal_cat ),
                        B_pen_sal_cat_vap = cumsum( v * B_pen_sal_cat ),
                        B_cot_dep_sal_cat_vap = cumsum( v * B_cot_dep_sal_cat ),
                        
                        B_sal_cat_total_vap = cumsum( v * B_sal_cat_total ),
                        
                        G_vap = cumsum( v * G ),
                        Exc_G_vap = cumsum( v * Exc_G ),
                        
                        A_vap = cumsum( v * A ),
                        Int_vap = cumsum( v * interes ),
                        AI_vap = cumsum( v * AA ),
                        
                        B_vap = cumsum( v * B ) ) ]

balance_anual[ , V0 := esc$V0 ]
balance_anual[ , Activo := V0 + AI_vap]
balance_anual[ , Pasivo := B_vap + G_vap ]
balance_anual[ , V := cumsum( v * V_cor  ) + V0 ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )

if( esc$nombre%notin%c('escenario_1_sens', 'escenario_1_sens_med') ){

save( balance, balance_anual,
      file = paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '.RData' ) )
} 

if( esc$nombre=='escenario_1_sens' | esc$nombre=='escenario_1_sens_med' ){
  
  save( balance, balance_anual,
        file = paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '_', esc$k,'.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()
