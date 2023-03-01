# PARAMETROS DE ESCENARIO ----
message( '\tCalculando escenarios del balance para SSC' )

# Carga
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'Hipotesis' ) ) ] )

# Escenario 1
esc <- new.env()

esc$nombre <- 'escenario_1'
# message( '\t\t\t', esc$nombre )

# # Patrimonio inicial
# esc$V0 <- 1354474852.90

# Hipótesis
esc$hip_esc <- data.table( t = (1:parametros$horizonte) + 2020,
                           
                           # # Tasas
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_s = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 5, 2 ],
                           # i_p = Hipotesis[ 5, 2 ],
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * 0.0049, #Hipotesis[ 7, 2 ], Se toma la inclación proyectada del 2021 del IVM
                           
                           # # Gasto administrativo
                           # por_gas_masa = 0.0005,
                           # por_gas_apor = 0.03,
                           
                           # # Aporte Estado
                           # apo_est = 0.69 * 0.40,
                           # apo_est_rel_dep = 0.372 * 0.003,
                           # apo_est_fij = 0,
                           # apo_est_cat = 0.0,
                           # apo_est_pen_sal = 0.0,
                           # 
                           # # Aportación
                           # apo_sgo = 0.007,
                           # por_mas_rel_dep = 0.92444791488,
                           # 
                           # # Montepios configuración
                           inc_montepio = 0,
                           # por_pen_orfa = 0.20,
                           # por_pen_cony = 0.40,
                           
                           # # Auxilio de Funerales
                           # inc_fun = 0,
                           # ben_fun = 0.25,
                           # 
                           # # Aporte afiliados
                           # apo_ind = 0.025,
                           # apo_inv = 0.001,
                           # 
                           # bas_ref = 0.225,
                           # reg_pen = 0.75,
                           # pen_min = 100, 
                           # 
                           # # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           # por_pag_sal = 0,
                           # por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           # cal_mas = 1.0,
                           # cal_apo = 0.9419157968,
                           # cal_apo_sgo = 1.0,
                           # 
                           # cal_aux_fun = 0.85,
                           # cal_pen_vej = 1.0258663694,
                           # cal_pen_inv = 0.8145432191,
                           # cal_pen_viu = 0.5,
                           # cal_pen_hue = 0.5,
                           # 
                           # cal_apo_est = 1.15,
                           # cal_apo_est_rel_dep = 1.11,
                           # cal_apo_est_fij = 1.0,
                           # cal_apo_est_cat = 1.0,
                           # cal_apo_est_pen_sal = 1.0,
                           
                           cal_ben_sal = 0.8,
                           cal_l2_2021 = (1+0.193)
                           )

# esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]
# 
esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]
# 
# esc$hip_esc[ , u_s := i_s ]
# esc$hip_esc[ t == 0, u_s := 0 ]
# esc$hip_esc[ , u_s := 1 + u_s ]
# esc$hip_esc[ , u_s := cumprod( u_s ) ]
# esc$hip_esc[ , v_s := 1 / u_s  ]
# # 
# esc$hip_esc[ , u_p := i_p ]
# esc$hip_esc[ t == 0, u_p := 0 ]
# esc$hip_esc[ , u_p := 1 + u_p ]
# esc$hip_esc[ , u_p := cumprod( u_p ) ]
# esc$hip_esc[ , v_p := 1 / u_p  ]
# 
# esc$hip_esc[ , u_f := i_f ]
# esc$hip_esc[ t == 0, u_f := 0 ]
# esc$hip_esc[ , u_f := 1 + u_f ]
# esc$hip_esc[ , u_f := cumprod( u_f ) ]
# esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

# esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

# FLUJOS ------------------------------------------------------------------ ----
message( paste( rep('-', 100 ), collapse = '' ) )

# Carga información --------------------------------------------------------------------------------

# Modelo 2020
load( paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) ) # acum_dem_salud

# Datos globales
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion.RData' ) ) # pob_proy
load( paste0( parametros$RData, 'IESS_proyeccion_salarios_', 'escenario_1', '.RData' ) ) # sal_int, sal_proy, sbu_proy
load( paste0( parametros$RData_seg, 'IESS_SAL_estimacion.RData' ) ) # ben_est_fre, ben_est_sev
pob_proy_sgo <- copy( pob_proy )

# Datos del seguro
#load( paste0( parametros$RData_seg, 'IESS_SSC_estimacion_issfa_isspol_demografia.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_SSC_estructura_familiar.RData' ) ) # tab_var_proj
load( paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_poblacion.RData' ) ) # pob_proy
load( paste0( parametros$RData_seg, 'IESS_SSC_proyeccion_beneficios_', 'escenario_1', '.RData' ) ) # ben_proy


# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista,
                                 'parametros', 'pob_proy_sgo', 'ben_proy', 'sal_proy', 
                                 'pen_proy', 'pob_proy' #, 'issfa_proy', 'isspol_proy', 'seguros_proy'
                                 , 'tab_var_proj' #, 'balance_anual_sal'
                                 , 'pob_proy_fami', 'ben_est_fre', 'ben_est_sev'
                                 , 'acum_dem_salud' ) ) ] )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Balance corriente 
message( '\tGenerando balance corriente' )
# Comento ingresos por otros ----
# balance <- copy( esc$hip_esc )
# 
# balance <- merge.data.table( balance,
#                              pob_proy_sgo[ , list( t, sexo, x, l2_cot_sgo = l2_cot ) ], 
#                              by = c( 't' ), 
#                              all.x = TRUE )
# 
# balance <- merge.data.table( balance,
#                              pob_proy, 
#                              by = c( 't', 'sexo', 'x' ), 
#                              all.y = TRUE )
# 
# balance <- merge.data.table( balance, 
#                              sal_proy[ , list( t, sexo, x, sal ) ], 
#                              by = c( 't', 'sexo', 'x' ),
#                              all.x = TRUE )
# 
# balance[ is.na( sal ), sal := 0 ]
# 
# balance <- merge.data.table( balance, 
#                              ben_proy[, list( t, sexo, x, sbu,
#                                               pen_ant_vej, pen_nue_vej,
#                                               pen_ant_inv, pen_nue_inv,
#                                               pen_orf, pen_cony,
#                                               ben_2_5, ben_3_5, ben_4_5, ben_6_5, ben_7_5, ben_8_5, 
#                                               ben_9_5 ) ], 
#                              by = c( 't', 'sexo', 'x' ),
#                              all.x = TRUE ) 
# balance[ is.na( sbu ), sbu := 0 ]
# balance[ is.na( pen_ant_vej ), pen_ant_vej := 0 ]
# balance[ is.na( pen_nue_vej ), pen_nue_vej := 0 ]
# balance[ is.na( pen_ant_inv ), pen_ant_inv := 0 ]
# balance[ is.na( pen_nue_inv ), pen_nue_inv := 0 ]
# balance[ is.na( pen_orf ), pen_orf := 0 ]
# balance[ is.na( pen_cony ), pen_cony := 0 ]
# balance[ is.na( ben_2_5 ), ben_2_5 := 0 ]
# balance[ is.na( ben_3_5 ), ben_3_5 := 0 ]
# balance[ is.na( ben_4_5 ), ben_4_5 := 0 ]
# balance[ is.na( ben_6_5 ), ben_6_5 := 0 ]
# balance[ is.na( ben_7_5 ), ben_7_5 := 0 ]
# balance[ is.na( ben_8_5 ), ben_8_5 := 0 ]
# balance[ is.na( ben_9_5 ), ben_9_5 := 0 ]
# 
# setorder( balance, t, sexo, x )
# 
# message( '\tProyectando masa salarial del SGO' )
# balance[ , M := cal_mas * sal * l2_cot_sgo ]
# 
# message( '\tProyectando masa salarial del SSC' )
# balance[ , MS := 12 * sbu * l2_cot ]
# 
# message( '\tProyectando masa salarial del SGO bajo relación de dependencia' )
# balance[ , MD := por_mas_rel_dep * M ]
# 
# # Beneficios de renta vitalicia
# message( '\tProyectando beneficios por pensiones' )
# 
# # Beneficio por pensiones de vejez
# balance[ t < 10 & t > 0, B3_ant := cal_pen_vej * pen_ant_vej * l3_3 ]
# balance[ t >= 10, B3_ant := cal_pen_vej * pen_nue_vej * l3_3 ]
# balance[ t > 0, B3_nue := cal_pen_vej * pen_nue_vej * l2_3 ]
# balance[ t == 0, B3 := 0 ]
# balance[ t > 0, B3 := B3_ant + B3_nue ]
# 
# # Beneficio por pensiones de invalidez
# balance[ t < 10 & t > 0, B4_ant := cal_pen_inv * pen_ant_inv * l4_4 ]
# balance[ t >= 10, B4_ant := cal_pen_inv * pen_nue_inv * l4_4 ]
# balance[ t > 0, B4_nue := cal_pen_inv * pen_nue_inv * l2_4 ]
# balance[ t == 0, B4 := 0 ]
# balance[ t > 0, B4 := B4_ant + B4_nue ]
# 
# # Beneficios de montepios
# # Orfandad
# balance[ t == 0, B8 := 0 ]
# balance[ t > 0, B8 := inc_montepio * cal_pen_hue * pen_orf * l8_m18 ]
# 
# # Viudedad
# balance[ t == 0, B9 := 0 ]
# balance[ t > 0, B9 := inc_montepio * cal_pen_viu * pen_cony * l9 ]
# 
# # Todo montepio
# balance[ t == 0, B_pen_mon := 0 ]
# balance[ t > 0, B_pen_mon := B8 + B9  ]
# 
# # Beneficios por pensiones
# balance[ t == 0, B_pen := 0 ]
# balance[ , B_pen := B3 + B4 + B8 + B9 ]
# 
# # Beneficios por transición a muerto 
# message( '\tProyectando beneficios por auxilio de funerales' )
# 
# aux <- copy( tab_var_proj[ t > parametros$anio_ini & t <= parametros$horizonte + parametros$anio_ini, 
#                            list( t = t - parametros$anio_ini, por_aux = var ) ] )
# balance <- merge( balance, aux, by =  c( 't' ), all.x = TRUE )
# 
# balance[ , B2_5 := ben_2_5 * por_aux * l2_5 + inc_fun * ben_2_5 * ( 1 - por_aux ) * l2_5 ]
# balance[ , B3_5 := ben_3_5 * por_aux * l3_5 + inc_fun * ben_3_5 * ( 1 - por_aux ) * l3_5 ]   
# balance[ , B4_5 := ben_4_5 * por_aux * l4_5 + inc_fun * ben_4_5 * ( 1 - por_aux ) * l4_5 ]
# balance[ , B6_5 := ben_6_5 * por_aux * l6_5 + inc_fun * ben_6_5 * ( 1 - por_aux ) * l6_5 ]
# balance[ , B7_5 := ben_7_5 * por_aux * l7_5 + inc_fun * ben_7_5 * ( 1 - por_aux ) * l7_5 ]
# balance[ , B8_5 := ben_8_5 * por_aux * l8_5 + inc_fun * ben_8_5 * ( 1 - por_aux ) * l8_5 ]
# balance[ , B9_5 := ben_9_5 * por_aux * l9_5 + inc_fun * ben_9_5 * ( 1 - por_aux ) * l9_5 ]
# 
# balance[ , B2_5 := cal_aux_fun * B2_5 ]
# balance[ , B3_5 := cal_aux_fun * B3_5 ]
# balance[ , B4_5 := cal_aux_fun * B4_5 ]
# balance[ , B6_5 := cal_aux_fun * B6_5 ]
# balance[ , B7_5 := cal_aux_fun * B7_5 ]
# balance[ , B8_5 := inc_montepio * cal_aux_fun * B8_5 ]
# balance[ , B9_5 := inc_montepio * cal_aux_fun * B9_5 ]
# 
# # Beneficios por auxilio de funerales
# balance[ , B_aux := B2_5 + B3_5 + B4_5 + B6_5 + B7_5 + B8_5 + B9_5 ]


# DESCRIPCION DE VARIABLES ----
# l2 := afiliados
# l3 := pensionistas de vejez
# l4 := pensionistas de invalidez
# l5 := muertos
# l2_cot := afiliados cotizantes
# l6 := Dependiente Hijos
# l6_m18 := Dependiente Hijos Menor 18 anios
# l7 := Dependiente Conyuge
# l8 := Montepio Orfandad
# l8_m18 := Montepio Orfandad Menor 18 anios
# l9 := Montepio Viudedad

# B3_sal := salud Vejez (l3)
# B4_sal := salud Invalidez (l4)
# B8_sal := salud Montepio Orfandad (l8_m18)
# B9_sal := salud Montepio Viudedad (l9)
# B_pen_sal := salud Total Pensionistas
# B2_sal := salud cotizantes (l2_cot)
# B6_sal := Dependientes Hijos (l6)
# B7_sal := Dependientes Conyuges (l7)
# B_sal := Total Salud SSC
aux_pob_poy <- pob_proy[ , list( l2 = sum( l2, na.rm = TRUE ), 
                                 l3 = sum( l3, na.rm = TRUE ), 
                                 l4 = sum( l4, na.rm = TRUE ), 
                                 l5 = sum( l5, na.rm = TRUE ), 
                                 l2_cot = sum( l2_cot, na.rm = TRUE ), 
                                 l6 = sum( l6, na.rm = TRUE ), 
                                 #l6_m18 = sum( l6_m18, na.rm = TRUE ),
                                 #l7 = sum( l7, na.rm = TRUE ),
                                 l8 = sum( l8, na.rm = TRUE ), 
                                 l8_m18 = 0,
                                 l9 = sum( l9, na.rm = TRUE ) ),
                        by = list( t )]
aux_acum_dem_salud <- acum_dem_salud[ , list( l2 = sum( l2, na.rm = TRUE ), 
                                 l3 = sum( l3, na.rm = TRUE ), 
                                 l4 = sum( l4, na.rm = TRUE ), 
                                 #l5 = sum( l5, na.rm = TRUE ), 
                                 l2_cot = sum( l2, na.rm = TRUE ), 
                                 l6 = sum( l6, na.rm = TRUE ), 
                                 #l6_m18 = sum( l6_m18, na.rm = TRUE ),
                                 l7 = sum( l7, na.rm = TRUE ),
                                 l8 = sum( l8, na.rm = TRUE ), 
                                 l8_m18 = 0,
                                 l9 = sum( l9, na.rm = TRUE )
                                 ),
                         by = list( t )]
ax_2021 <- acum_dem_salud[t==2021, list(t, sexo, x, l2)]
ax_2021[sexo=='Female', sexo:= 'F']
ax_2021[sexo=='Male', sexo:= 'M']
ax_2018 <- pob_proy[t==0, list(t, sexo, x, l2)]
ax_21_18 <- merge(ax_2018, ax_2021[, list(sexo, x, l2_2021=l2)], all.x = TRUE,  by = c( 'sexo', 'x' ))
# Beneficios de salud ------------------------------------------------ ----
ben_sal <- acum_dem_salud[ , list( t, sexo, x, l2, l3, l4, l2_cot = l2, l6, 
                             l6_m18 = 0, l7, l8
                             , l8_m18 = 0, l9 ) ]
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
ben_sal[ , l := l2_cot + l3 + l4 + l6_m18 + l7 + l8_m18 ]

# Probabilidad de enfermar
q_e <- 1.0

# Proyección de beneficios por grupo de asegurados
ben_sal[ ser != 'HO', B2_sal := cal_l2_2021 * cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l2_cot ]
ben_sal[ ser == 'HO', B2_sal := cal_l2_2021 * cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l2_cot ]
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
ben_sal[ ser != 'HO' & x < 18, B8_sal := inc_montepio * cal_ben_sal * lambda * EX * q_p * q_s * q_c * q_e * l8_m18 ] 
ben_sal[ ser == 'HO' & x < 18, B8_sal := inc_montepio * cal_ben_sal * lambda * ED * EX * q_p * q_s * q_c * q_e * l8_m18 ]
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

# # Aportes ----
# message( '\tProyectando aportes' )
# 
# # Incluyendo beneficios por asistencia médica
# balance <- merge.data.table( balance,
#                              ben_sal_tot, 
#                              by = c( 't', 'sexo', 'x' ),
#                              all.x = TRUE )
# 
# # Aportes de activos del ssc
# balance[ , A2 := cal_apo * apo_cot * 12 * sbu * l2_cot ]
# balance[ t == 0 , A2 := 0 ]
# 
# # Aportes de activos del sgo
# balance[ , A_sgo := cal_apo_sgo * apo_sgo * M ]
# balance[ t == 0 , A_sgo := 0 ]
# 
# # Aporte estado 
# # Aporte del Estado 40%
# balance[ , A_est_pen := cal_apo_est * apo_est * B_pen ]
# balance[ t == 0 , A_est_pen := 0 ]
# 
# # Aportes del Estado 0.3%
# balance[ , A_est_rel_dep := cal_apo_est_rel_dep * apo_est_rel_dep * MD ]
# balance[ t == 0 , A_est_rel_dep := 0 ]
# 
# # Aportes del Estado para enfermedades catastróficas y salud de jubilados
# balance[ , A_est_cat := cal_apo_est_cat * apo_est_cat * B_sal_cat ]
# balance[ t == 0, A_est_cat := 0 ]
# 
# # Aporte del Estado para salud de pensionistas
# balance[ , A_est_pen_sal := cal_apo_est_pen_sal * apo_est_pen_sal * B_pen_sal ]
# balance[ t == 0 , A_est_pen_sal := 0 ]
# 
# # Agregación de aportes 
# 
# # Aportes no estatales
# balance[ , A_afi := A2 + A_sgo ]
# balance[ t == 0 , A_afi := 0 ]
# 
# # Gasto administrativo
# balance[ , G := por_gas_masa * M + por_gas_apor * A2 ]
# balance[ t == 0 , G := 0 ]
# 
# # Aporte otros 
# t_max <- parametros$horizonte
# apo_otr <- copy( esc$hip_esc[ , list( t, cal_apo_est_fij, apo_est_fij ) ] )
# 
# # Aporte del Estado fijo 
# apo_otr[ , A_est_fij  := cal_apo_est_fij * apo_est_fij ]
# apo_otr[ t == 0 , A_est_fij := 0 ]
# 

# 
# # Balance corriente anual 
# # cat( paste0( names( balance )[101:137], ' = sum( ', names( balance )[101:137], ' ),\n' ) )

# ----
balance_anual <- ben_sal_tot[ , list( 
  # M = sum( M, na.rm = TRUE ),
#                                   MD = sum( MD, na.rm = TRUE ),
#                                   MS = sum( MS, na.rm = TRUE ),
#                                   
#                                   A2 = sum( A2, na.rm = TRUE ),
#                                   A_sgo = sum( A_sgo, na.rm = TRUE ),
#                                   A_est_pen = sum( A_est_pen, na.rm = TRUE ),
#                                   A_est_rel_dep = sum( A_est_rel_dep, na.rm = TRUE ),
#                                   A_est_cat = sum( A_est_cat, na.rm = TRUE ),
#                                   A_est_pen_sal = sum( A_est_pen_sal, na.rm = TRUE ),
#                                   #A_est = sum( A_est, na.rm = TRUE ),
#                                   A_afi = sum( A_afi, na.rm = TRUE ),
#                                   #A_afi_est = sum( A_afi_est, na.rm = TRUE ),
#                                   
#                                   B3_ant = sum( B3_ant ),
#                                   B3_nue = sum( B3_nue ),
#                                   B3 = sum( B3 ),
#                                   B4_ant = sum( B4_ant ),
#                                   B4_nue = sum( B4_nue ),
#                                   B4 = sum( B4 ),
#                                   B8 = sum( B8 ),
#                                   B9 = sum( B9 ),
#                                   B_pen_mon = sum( B_pen_mon ),
#                                   B_pen = sum( B_pen ),
#                                   
#                                   B2_5 = sum( B2_5 ),
#                                   B3_5 = sum( B3_5 ),
#                                   B4_5 = sum( B4_5 ),
#                                   B6_5 = sum( B6_5 ),
#                                   B7_5 = sum( B7_5 ),
#                                   B8_5 = sum( B8_5 ),
#                                   B9_5 = sum( B9_5 ),
#                                   B_aux = sum( B_aux ),
                                  
                                  B2_sal = sum( B2_sal ),
                                  B3_sal = sum( B3_sal ),
                                  B4_sal = sum( B4_sal ),
                                  B6_sal = sum( B6_sal ),
                                  B7_sal = sum( B7_sal ),
                                  B8_sal = sum( B8_sal ),
                                  B9_sal = sum( B9_sal ),
                                  B_sal = sum( B_sal ),
                                  B_pen_sal = sum( B_pen_sal ),
                                  B_cot_dep_sal = sum( B_cot_dep_sal ),
                                  
                                  B2_sal_cat = sum( B2_sal_cat ),
                                  B3_sal_cat = sum( B3_sal_cat ),
                                  B4_sal_cat = sum( B4_sal_cat ),
                                  B6_sal_cat = sum( B6_sal_cat ),
                                  B7_sal_cat = sum( B7_sal_cat ),
                                  B8_sal_cat = sum( B8_sal_cat ),
                                  B9_sal_cat = sum( B9_sal_cat ),
                                  B_sal_cat = sum( B_sal_cat ),
                                  B_pen_sal_cat = sum( B_pen_sal_cat ),
                                  B_cot_dep_sal_cat = sum( B_cot_dep_sal_cat )
                                  
                                  # G = sum( G ) 
                                  ), 
                          by = list( t ) ]

# balance_anual <- merge.data.table( balance_anual, 
#                                    apo_otr,
#                                    by = c( 't' ) )

balance_anual <- merge.data.table( esc$hip_esc[ , list( t, u, v ) ],
                                   balance_anual, 
                                   by = c( 't' ) )
# ----
# balance_anual[ , A_otr :=  A_issfa + A_isspol + A_seg_pri ]

# Contribución total del estado 
# balance_anual[ , A_est := A_est_pen + A_est_rel_dep + A_est_cat + A_est_pen_sal + A_est_fij ]
# balance_anual[ t == 0 , A_est := 0 ]
# 
# # Aportes totales
# balance_anual[ , A_afi_est := A_afi + A_est ]
# balance_anual[ t == 0 , A := 0 ]
# 
# # Flujos de activos y pasivos
# balance_anual[ , A := A_afi_est + A_otr ]
# balance_anual[ , B := B_pen + B_aux + B_sal + B_sal_cat ]
# balance_anual[ , V_cor := A - B - G ]

# cat( paste0( names( balance_anual ), ' = 0,' ) )
# ----
balance_anual[ t == 0, `:=`(  
  # M = 0, MD = 0, MS = 0,
  #                             
  #                             A2 = 0, A_sgo = 0, A_est_pen = 0, A_est_rel_dep = 0, A_est_cat = 0, 
  #                             A_est_pen_sal = 0, A_est = 0, A_afi = 0, A_afi_est = 0, 
  #                             
  #                             A_est_fij = 0, A_issfa = 0, A_isspol = 0, A_seg_pri = 0, A_otr = 0, 
  #                             A = 0,
  #                             
  #                             B3_ant = 0, B3_nue = 0, B3 = 0, 
  #                             B4_ant = 0, B4_nue = 0, B4 = 0, 
  #                             B8 = 0, B9 = 0, B_pen_mon = 0, B_pen = 0,
  #                             
  #                             B2_5 = 0, B3_5 = 0, B4_5 = 0, B6_5 = 0, B7_5 = 0, B8_5 = 0, B9_5 = 0, 
  #                             B_aux = 0, 
                              
                              B2_sal = 0, B3_sal = 0, B4_sal = 0, B6_sal = 0, B7_sal = 0, 
                              B8_sal = 0, B9_sal = 0, B_sal = 0, B_pen_sal = 0, B_cot_dep_sal = 0,
                              
                              B2_sal_cat = 0, B3_sal_cat = 0, B4_sal_cat = 0, B6_sal_cat = 0, 
                              B7_sal_cat = 0, B8_sal_cat = 0, B9_sal_cat = 0, B_sal_cat = 0, 
                              B_pen_sal_cat = 0, B_cot_dep_sal_cat = 0, B = 0
                              # 
                              # G = 0,
                              # 
                              # V_cor = 0 
                              ) ]

# balance_anual[ , V_cap := V_cor ]
# balance_anual[ t == 0, V_cap := esc$V0 ]
# balance_anual[ , V_cap := u * cumsum( v * V_cap ) ]

# Balance actuarial
# cat( paste0( names( balance_anual ), '_vap = sum( v * ', names( balance_anual ), ' ),\n' ) )
balance_anual[ , `:=`(  
  # M_vap = cumsum( v * M ),
  #                       MD_vap = cumsum( v * MD ),
  #                       MS_vap = cumsum( v * MS ),
  #                       
  #                       A2_vap = cumsum( v * A2 ),
  #                       A_sgo_vap = cumsum( v * A_sgo ),
  #                       A_est_pen_vap = cumsum( v * A_est_pen ),
  #                       A_est_rel_dep_vap = cumsum( v * A_est_rel_dep ),
  #                       A_est_cat_vap = cumsum( v * A_est_cat ),
  #                       A_est_pen_sal_vap = cumsum( v * A_est_pen_sal ),
  #                       A_est_vap = cumsum( v * A_est ),
  #                       A_afi_vap = cumsum( v * A_afi ),
  #                       A_afi_est_vap = cumsum( v * A_afi_est ),
  #                       
  #                       A_est_fij_vap = cumsum( v * A_est_fij ),
  #                       A_issfa_vap = cumsum( v * A_issfa ),
  #                       A_isspol_vap = cumsum( v * A_isspol ),
  #                       A_seg_pri_vap = cumsum( v * A_seg_pri ),
  #                       A_otr_vap = cumsum( v * A_otr ),
  #                       
  #                       B3_ant_vap = cumsum( v * B3_ant ),
  #                       B3_nue_vap = cumsum( v * B3_nue ),
  #                       B3_vap = cumsum( v * B3 ),
  #                       B4_ant_vap = cumsum( v * B4_ant ),
  #                       B4_nue_vap = cumsum( v * B4_nue ),
  #                       B4_vap = cumsum( v * B4 ),
  #                       B8_vap = cumsum( v * B8 ),
  #                       B9_vap = cumsum( v * B9 ),
  #                       B_pen_mon_vap = cumsum( v * B_pen_mon ),
  #                       B_pen_vap = cumsum( v * B_pen ),
  #                       
  #                       B2_5_vap = cumsum( v * B2_5 ),
  #                       B3_5_vap = cumsum( v * B3_5 ),
  #                       B4_5_vap = cumsum( v * B4_5 ),
  #                       B6_5_vap = cumsum( v * B6_5 ),
  #                       B7_5_vap = cumsum( v * B7_5 ),
  #                       B8_5_vap = cumsum( v * B8_5 ),
  #                       B9_5_vap = cumsum( v * B9_5 ),
  #                       B_aux_vap = cumsum( v * B_aux ),
                        
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
                        B_cot_dep_sal_cat_vap = cumsum( v * B_cot_dep_sal_cat )
                        
                        # G_vap = cumsum( v * G ),
                        # 
                        # A_vap = cumsum( v * A ),
                        # B_vap = cumsum( v * B ) 
                        ) ]

# balance_anual[ , V0 := esc$V0 ]
# balance_anual[ , Activo := V0 + A_vap ]
# balance_anual[ , Pasivo := B_vap + G_vap ]
# balance_anual[ , V := v * V_cap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( balance, balance_anual,
      file = paste0( parametros$RData_seg, 'IESS_SSC_balances_salud_', esc$nombre, '.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()

# # EXPORTO
# library("xlsx")
# write.xlsx2( ax_21_18, 'C:/Users/jendry.toapanta/Downloads/Poblaciones _SSC_2018_2021.xlsx'
#              , sheetName="Sheet1",
#             col.names=TRUE, row.names=TRUE, append=FALSE)
