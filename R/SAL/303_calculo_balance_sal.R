#---------------------------------------------------------------------------------------------------
# Cargando información
load( paste0( parametros$RData_seg, 'IESS_SAL_estimacion.RData' ) )
load( paste0( parametros$RData, 'IESS_proyeccion_poblacion_salud.RData' ) )

# load( paste0( parametros$RData, 'IESS_proyeccion_salarios_escenario_1.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_proyeccion_salarios_escenario_1_ivm.RData' ) )

# load( paste0( parametros$RData, 'ivm/IESS_IVM_proyeccion_beneficios_escenario_1.RData' ) )
# load( paste0( parametros$RData, 'ivm/IESS_IVM_proyeccion_calibrada_beneficios_escenario_1.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_proyeccion_beneficios_escenario_1_ivm.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista, 
                                 'pob_proy', 'ben_proy', 'sal_proy', #'pen_proy', 
                                 'gast_proy',
                                 'ben_est_fre', 'ben_est_sev', 'Masa_ILO' ) ) ] )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# Balance corriente --------------------------------------------------------------------------------
message( '\tGenerando balance corriente' )

Masa_ILO[ sexo == 'Female', sexo := 'F' ]
Masa_ILO[ sexo == 'Male', sexo := 'M' ]

aportes <- merge( pob_proy, 
                  # sal_proy[ , list( t, sexo, x, sal ) ],
                  Masa_ILO[ , list( t, sexo, x, masa ) ], 
                  by = c( 't', 'sexo', 'x' ),
                  all.x = TRUE, 
                  all.y = TRUE
                  )
# aportes <- merge( aportes, 
#                   pen_proy[ , list( t, sexo, x, pen_ant_3, pen_nue_3, pen_ant_4, pen_nue_4 ) ], 
#                   by = c( 't', 'sexo', 'x' ) )
aportes <- merge( aportes, 
                  ben_proy,
                  by = c( 't', 'sexo', 'x' ),
                  all.x = TRUE, 
                  all.y = TRUE
                  )
setorder( aportes, t, sexo, x )
aportes[ is.na( aportes ) ] <- 0

aportes <- aportes[ t <= parametros$horizonte ]

# Proyectando masa salarial ----
message( '\tProyectando masa salarial' )
# aportes[ , M := sal * l2 ]
aportes[ , M := masa ]

# Beneficios de renta vitalicia ----
message( '\tProyectando beneficios por pensiones' )

# Beneficio por pensiones de vejez
# cal_pen_vej <- esc$calibra_pen_vej
# 
aportes[ , P3 := P3 * l3 ]
# aportes[ t > 0, P3_nue := cal_pen_vej * pen_nue_3 * l23 ]

# aportes[ t == 0, P3 := cal_pen_vej * pen_ant_3 * l3 ]
# aportes[ t > 0, P3 := P3_ant + P3_nue ]
# aportes[ , P3_sbu := sbu * l3 ]
# aportes[ , P3_dec := ( P3 - P3_sbu ) / 13 + P3_sbu ]
# aportes[ , P3_nodec := 12 * ( P3 - P3_sbu ) / 13 ]
# 
# Beneficio por pensiones de invalidez
# cal_pen_inv <- esc$calibra_pen_inv

aportes[ , P4 := P4 * l4  ]
# aportes[ t > 0, P4_nue := cal_pen_inv * pen_nue_4 * l24 ]
# 
# aportes[ t == 0, P4 := cal_pen_inv * pen_ant_4 * l4 ]
# aportes[ t > 0, P4 := P4_ant + P4_nue ]
# aportes[ , P4_sbu := sbu * l4 ]
# aportes[ , P4_dec := ( P4 - P4_sbu ) / 13 + P4_sbu ]
# aportes[ , P4_nodec := 12 * ( P4 - P4_sbu ) / 13 ]
# 
# Beneficios de montepios
aportes[ , P6 :=  P6 * l6 ]
# aportes[ , P6_sbu := P6 - sbu * parametros$mont_prop_afi * ( l3 + l4 ) ]
# aportes[ , P6_dec := ( P6 - P6_sbu ) / 13 + P6_sbu ]
# aportes[ , P6_nodec := 12 * ( P6 - P6_sbu ) / 13 ]
# 
# # Beneficios por pensiones
# aportes[ , P_pen := P3 + P4 + P6 ]
# aportes[ , P_sbu := P3_sbu + P4_sbu + P6_sbu ]
# aportes[ , P_dec := P3_dec + P4_dec + P6_dec ]
# aportes[ , P_nodec := P3_nodec + P4_nodec + P6_nodec ]
# 
# aportes[ , P := P_pen ]

# Proyectando beneficios por pensiones sin decimas ----
message( '\tProyectando beneficios por pensiones sin decimas' )
# Masa pensional sin décimos
aportes[ , P3_sd := ( P3 - P3 / 12 - sbu ) ]
aportes[ , P4_sd := ( P4 - P4 / 12 - sbu ) ]
aportes[ , P6_sd := ( P6 - P6 / 12 - sbu ) ]

aportes[ P3_sd <= 0, P3_sd := 0 ]
aportes[ P4_sd <= 0, P4_sd := 0 ]
aportes[ P6_sd <= 0, P6_sd := 0 ]

aportes[ , P_sd := P3_sd + P4_sd ]

# Proyectando aportes ----------------------------------------------------------
message( '\tProyectando aportes' )

# Conyuges de afiliados con extension de cobertura
pob_ext_cob <- 1/10 # valor 2018
aportes[ , l8 := pob_ext_cob * l8 ]

# Aportes de activos
aportes <- merge( aportes, esc$apo_act, by = 't', all.x = TRUE )
aportes[ t == 0 , A2 := 0 ]
aportes[ , A2 := por_apo * M ]

aportes[ , A8_2 := 0 ]
aportes[ l2 + l3 + l4 + l6 > 0, A8_2 := por_apo_ext_cot * M * l8 / ( l2 + l3 + l4 + l6 ) ] 
aportes[ t == 0 , A8_2 := 0 ]

# Aportes de pensionistas de vejez sin décimos
apo_pen <- esc$aporte_pen
aportes[ , A3 := apo_pen * ( P3 - P3 / 12 - sbu ) ] 
aportes[ t == 0 , A3 := 0 ]

aportes[ , A8_3 := 0 ]
aportes[ l2 + l3 + l4 + l6 > 0, A8_3 := por_apo_ext_pen * ( P3 - P3 / 12 - sbu ) * l8 / ( l2 + l3 + l4 + l6 ) ] 
aportes[ t == 0 , A8_3 := 0 ]

# Aportes de pensionistas de invalidez sin décimos
aportes[ , A4 := apo_pen * ( P4 - P4 / 12 - sbu ) ]
aportes[ t == 0 , A4 := 0 ]

aportes[ , A8_4 := 0 ]
aportes[ l2 + l3 + l4 + l6 > 0, A8_4 := por_apo_ext_pen * ( P4 - P4 / 12 - sbu ) * l8 / ( l2 + l3 + l4 + l6 ) ] 
aportes[ t == 0 , A8_4 := 0 ]

# Aportes de montepios sin décimos
aportes[ , A6 := apo_pen * ( P6 - P6 / 12 - sbu ) ]
aportes[ t == 0 , A6 := 0 ]

aportes[ , A8_6 := 0 ]
aportes[ l2 + l3 + l4 + l6 > 0, A8_6 := por_apo_ext_pen * ( P6 - P6 / 12 - sbu ) * l8 / ( l2 + l3 + l4 + l6 ) ] 
aportes[ t == 0 , A8_6 := 0 ]

# Aportes de cotizantes para menores de 18
aportes[ t == 0 , A7 := 0 ]
aportes[ , A7 := por_apo_men_18 * M ]

# Aportes afiliados
aportes[ , A_afi := A2 + A3 + A4 + A6 + A7 ]

# Aportes ext. conyu. todos
aportes[ , A8 := A8_2 + A8_3 + A8_4 + A8_6  ]

# Aportes totales
aportes[ , A := A_afi + A8 ]

# Gasto administrativo
aportes[ , G := por_apo_gas * M ]
aportes[ t == 0 , G := 0 ]

aportes_anual <- aportes[ , list( M = sum( M ),
                                  
                                  # P = sum( P ),
                                  # P_dec = sum( P_dec ), 
                                  # P_nodec = sum( P_nodec ),
                                  P3 = sum( P3 ),
                                  # P3_dec = sum( P3_dec ),
                                  # P3_nodec = sum( P3_nodec ),
                                  P4 = sum( P4 ),
                                  # P4_dec = sum( P4_dec ),
                                  # P4_nodec = sum( P4_nodec ),
                                  P6 = sum( P6 ),
                                  # P6_dec = sum( P6_dec ),
                                  # P6_nodec = sum( P6_nodec ), 
                                  P3_sd = sum( P3_sd ),
                                  P4_sd = sum( P4_sd ),
                                  P6_sd = sum( P6_sd ),
                                  P_sd = sum( P_sd ),
                                  A = sum( A ),
                                  A_afi = sum( A_afi ),
                                  A2 = sum( A2 ),
                                  A3 = sum( A3 ),
                                  A4 = sum( A4 ),
                                  A6 = sum( A6 ),
                                  A7 = sum( A7 ),
                                  A8 = sum( A8 ),
                                  A8_2 = sum( A8_2 ),
                                  A8_3 = sum( A8_3 ),
                                  A8_4 = sum( A8_4 ),
                                  A8_6 = sum( A8_6 ),
                                  G = sum( G ) ), 
                          by = list( t ) ]

# Estimación de beneficios -------------------------------------------------------------------------
beneficios <- aportes[ , list( t, sexo, x, l2, l3, l4, l6, l7, l8 = pob_ext_cob * l8, M ) ]

# Estimación de beneficios de salud
beneficios[ , u := as.character( cut( x, breaks = c( 0, 1, seq( 5, 60, 5 ), seq( 70, 80, 10 ), 110 ), 
                                      include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
aux <- copy( beneficios )
aux[ , u := as.character( cut( x, breaks = c( 0, 5, seq( 20, 60, 20 ), 110 ), 
                               include.lowest = TRUE, right = FALSE, ordered_result = TRUE ) ) ]
beneficios <- rbind( beneficios, aux )

beneficios <- merge( beneficios, 
                     ben_est_sev[ , list( sexo, u, enf, ser, cap, icd, ED, EX, q_p, q_s, q_c ) ],
                     by = c( 'sexo', 'u' ), 
                     all.x = TRUE, 
                     allow.cartesian = TRUE )

beneficios <- merge( beneficios, 
                     ben_est_fre[ t == t_max, list( sexo, u, enf, lambda ) ],
                     by = c( 'sexo', 'u', 'enf' ), 
                     all.x = TRUE, 
                     allow.cartesian = TRUE )

beneficios[ , m := ( 1 + esc$i_m )^(t) ]
beneficios[ is.na( q_p ), q_p := 0 ]
beneficios[ is.na( q_s ), q_s := 0 ]
beneficios[ is.na( q_c ), q_c := 0 ]
beneficios[ is.na( lambda ), lambda := 0 ]
beneficios[ is.na( ED ), ED := 0 ]
beneficios[ is.na( EX ), EX := 0 ]

# Inflación médica
beneficios[ , EX := m * EX ]

# Población total cubierta
beneficios[ , l := l2 + l3 + l4 + l6 + l7 + l8 ]

# Probabilidad de enfermar
q_e <- 1.0

beneficios <- merge.data.table( esc$apo_act[ , list( t, 
                                                     cal_ben_2, cal_ben_cat_2,
                                                     cal_ben_3, cal_ben_cat_3,
                                                     cal_ben_4, cal_ben_cat_4,
                                                     cal_ben_6, cal_ben_cat_6,
                                                     cal_ben_7, cal_ben_cat_7,
                                                     cal_ben_8, cal_ben_cat_8,
                                                     cal_apo_est_3,
                                                     cal_apo_est_4,
                                                     cal_apo_est_6,
                                                     cal_apo_est_cat ) ],
                                beneficios, 
                                by = c( 't' ) )

# Proyección de beneficios por grupo de asegurados
# Cotizantes
beneficios[ enf == 'E' & ser != 'HO', B2 := cal_ben_2 * lambda * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'E' & ser == 'HO', B2 := cal_ben_2 * lambda * ED * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'C' & ser != 'HO', B2 := cal_ben_cat_2 * lambda * EX * q_p * q_s * q_c * q_e * l2 ]
beneficios[ enf == 'C' & ser == 'HO', B2 := cal_ben_cat_2 * lambda * ED * EX * q_p * q_s * q_c * q_e * l2 ]

# Pensionistas de vejez
beneficios[ enf == 'E' & ser != 'HO', B3 := cal_ben_3 * lambda * EX * q_p * q_s * q_c * q_e * l3 ]
beneficios[ enf == 'E' & ser == 'HO', B3 := cal_ben_3 * lambda * ED * EX * q_p * q_s * q_c * q_e * l3 ]
beneficios[ enf == 'C' & ser != 'HO', B3 := cal_ben_cat_3 * lambda * EX * q_p * q_s * q_c * q_e * l3 ]
beneficios[ enf == 'C' & ser == 'HO', B3 := cal_ben_cat_3 * lambda * ED * EX * q_p * q_s * q_c * q_e * l3 ]

# Pensionistas de invalidez
beneficios[ enf == 'E' & ser != 'HO', B4 := cal_ben_4 * lambda * EX * q_p * q_s * q_c * q_e * l4 ]
beneficios[ enf == 'E' & ser == 'HO', B4 := cal_ben_4 * lambda * ED * EX * q_p * q_s * q_c * q_e * l4 ]
beneficios[ enf == 'C' & ser != 'HO', B4 := cal_ben_cat_4 * lambda * EX * q_p * q_s * q_c * q_e * l4 ]
beneficios[ enf == 'C' & ser == 'HO', B4 := cal_ben_cat_4 * lambda * ED * EX * q_p * q_s * q_c * q_e * l4 ]

# Pensionistas de montepio
beneficios[ enf == 'E' & ser != 'HO', B6 := cal_ben_6 * lambda * EX * q_p * q_s * q_c * q_e * l6 ]
beneficios[ enf == 'E' & ser == 'HO', B6 := cal_ben_6 * lambda * ED * EX * q_p * q_s * q_c * q_e * l6 ]
beneficios[ enf == 'C' & ser != 'HO', B6 := cal_ben_cat_6 * lambda * EX * q_p * q_s * q_c * q_e * l6 ]
beneficios[ enf == 'C' & ser == 'HO', B6 := cal_ben_cat_6 * lambda * ED * EX * q_p * q_s * q_c * q_e * l6 ]

# Dependientes hijos
beneficios[ , B7 := 0 ]
beneficios[ enf == 'E' & ser != 'HO' & x < 18, B7 := 6 * cal_ben_7 * lambda * EX * q_p * q_s * q_c * q_e * l7 ] 
beneficios[ enf == 'E' & ser == 'HO' & x < 18, B7 := 6 * cal_ben_7 * lambda * ED * EX * q_p * q_s * q_c * q_e * l7 ]
beneficios[ enf == 'C' & ser != 'HO' & x < 18, B7 := 6 * cal_ben_cat_7 * lambda * EX * q_p * q_s * q_c * q_e * l7 ] 
beneficios[ enf == 'C' & ser == 'HO' & x < 18, B7 := 6 * cal_ben_cat_7 * lambda * ED * EX * q_p * q_s * q_c * q_e * l7 ]

# Dependientes cónyuges
beneficios[ enf == 'E' & ser != 'HO', B8 := cal_ben_8 * lambda * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'E' & ser == 'HO', B8 := cal_ben_8 * lambda * ED * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'C' & ser != 'HO', B8 := cal_ben_cat_8 * lambda * EX * q_p * q_s * q_c * q_e * l8 ]
beneficios[ enf == 'C' & ser == 'HO', B8 := cal_ben_cat_8 * lambda * ED * EX * q_p * q_s * q_c * q_e * l8 ]

# Beneficio por subsidios
cal_sub <- 0.021040
por_mas <- 70 / 365
beneficios[ , B9 := 0 ]
beneficios[ enf == 'E', B9 := cal_sub * q_p * q_s * q_c * q_e * por_mas * M ]

# Beneficios totales
beneficios[ , B_pen := B3 + B4 + B6 ] # Pensionistas incluye catastroficas
beneficios[ , B_dep := B7 ] # Dependientes solo hijos

beneficios[ , `:=`( B2_cat = B2, 
                    B3_cat = B3, 
                    B4_cat = B4, 
                    B6_cat = B6, 
                    B7_cat = B7,
                    B8_cat = B8 ) ]

beneficios[ enf == 'E',`:=`( B2_cat = 0, 
                             B3_cat = 0, 
                             B4_cat = 0, 
                             B6_cat = 0, 
                             B7_cat = 0,
                             B8_cat = 0 ) ]

beneficios[ , `:=`( B2_ncat = B2, 
                    B3_ncat = B3, 
                    B4_ncat = B4, 
                    B6_ncat = B6, 
                    B7_ncat = B7,
                    B8_ncat = B8 ) ]

beneficios[ enf == 'C',`:=`( B2_ncat = 0, 
                             B3_ncat = 0, 
                             B4_ncat = 0, 
                             B6_ncat = 0, 
                             B7_ncat = 0,
                             B8_ncat = 0 ) ]

#Catastroficas solo considera afiliados, dependientes hijos menores de edad y extension conyuges
beneficios[ , B_cat := B2_cat + B7_cat + B8_cat ]#[ , B_cat := B2_cat + B3_cat + B4_cat + B6_cat + B7_cat + B8_cat ]
beneficios[ , B_ncat := B2_ncat + B7_ncat + B8_ncat ]
beneficios[ , B := B2 + B3 + B4 + B6 + B7 + B8 + B9 ]

# Sin asegurados no hay beneficios
beneficios[ l == 0, `:=`( B = 0, B2 = 0, B3 = 0, B4 = 0, B6 = 0, B7 = 0, B8 = 0, B9 = 0,
                          B_cat = 0, B2_cat = 0, B3_cat = 0, B4_cat = 0, B6_cat = 0, B7_cat = 0, B8_cat = 0, 
                          B_ncat = 0, B2_ncat = 0, B3_ncat = 0, B4_ncat = 0, B6_ncat = 0, B7_ncat = 0, B8_ncat = 0 ) ]

beneficios[ , A_est_cat := 0 ]
beneficios[ enf == 'C', A_est_cat := esc$aporte_estado * cal_apo_est_cat * ( B2 + B7 + B8 ) ]
beneficios[ , A_est_3 := esc$aporte_estado * cal_apo_est_3 * B3 ]
beneficios[ , A_est_4 := esc$aporte_estado * cal_apo_est_4 * B4 ]
beneficios[ , A_est_6 := esc$aporte_estado * cal_apo_est_6 * B6 ]
beneficios[ , A_est_pen := A_est_3 + A_est_4 + A_est_6 ]
beneficios[ , A_est := A_est_cat + A_est_pen ]

beneficios_anual <- beneficios[ , list( B = sum( B ),
                                        B_pen = sum( B_pen ),
                                        B_dep = sum( B_dep ),
                                        
                                        B2 = sum( B2 ),
                                        B3 = sum( B3 ),
                                        B4 = sum( B4 ),
                                        B6 = sum( B6 ),
                                        B7 = sum( B7 ),
                                        B8 = sum( B8 ),
                                        B9 = sum( B9 ),
                                        
                                        B_cat = sum( B_cat ),
                                        B2_cat = sum( B2_cat ),
                                        B3_cat = sum( B3_cat ),
                                        B4_cat = sum( B4_cat ),
                                        B6_cat = sum( B6_cat ),
                                        B7_cat = sum( B7_cat ),
                                        B8_cat = sum( B8_cat ),
                                        
                                        B_ncat = sum( B_ncat ),
                                        B2_ncat = sum( B2_ncat ),
                                        B3_ncat = sum( B3_ncat ),
                                        B4_ncat = sum( B4_ncat ),
                                        B6_ncat = sum( B6_ncat ),
                                        B7_ncat = sum( B7_ncat ),
                                        B8_ncat = sum( B8_ncat ),
                                        
                                        A_est_3 = sum( A_est_3 ),
                                        A_est_4 = sum( A_est_4 ),
                                        A_est_6 = sum( A_est_6 ),
                                        A_est_pen = sum( A_est_pen ),
                                        A_est_cat = sum( A_est_cat ),
                                        A_est = sum( A_est ) ),
                                by = list( t ) ]

setorder( beneficios_anual, t )

balance_anual <- merge( aportes_anual,
                        beneficios_anual, by = 't' )
balance_anual[ , A := A + A_est ]
balance_anual[ , i_a := esc$i_a ]
balance_anual[ , r := ( 1 + i_a )^(t) ]
balance_anual[ , v := ( 1 + i_a )^(-t) ]
balance_anual[ , V_cor := A - B - G ]
balance_anual[ t == 0, `:=`( M = 0, 
                             P = 0, P_dec = 0, P_nodec = 0,
                             P3 = 0, P3_dec = 0, P3_nodec = 0,
                             P4 = 0, P4_dec = 0, P4_nodec = 0,
                             P6 = 0, P6_dec = 0, P6_nodec = 0,
                             
                             A = 0, A_afi = 0, A2 = 0, A3 = 0, A4 = 0, A6 = 0, A7 = 0, A8 = 0, 
                             A8_2 = 0, A8_3 = 0, A8_4 = 0, A8_6 = 0, 
                             
                             P_sd = 0, P3_sd = 0,
                             P4_sd = 0, P6_sd = 0,
                             
                             G = 0, B = 0, B_pen = 0, B_dep = 0, 
                             B2 = 0, B3 = 0, B4 = 0, B6 = 0, B7 = 0, B8 = 0, B9 = 0, 
                             B_cat = 0, B2_cat = 0, B3_cat = 0, B4_cat = 0, B6_cat = 0, B7_cat = 0, B8_cat = 0, 
                             B_ncat = 0, B2_ncat = 0, B3_ncat = 0, B4_ncat = 0, B6_ncat = 0, B7_ncat = 0, B8_ncat = 0, 
                             
                             A_est_3 = 0, A_est_4 = 0, A_est_6 = 0, A_est_pen = 0, A_est_cat = 0, A_est = 0 ) ]
balance_anual[ , V_cap := V_cor ]
balance_anual[ t == 0, V_cap := esc$V0 ]
balance_anual[ , V_cap := r * cumsum( v * V_cap ) ]

# Balance actuarial
balance_anual[ , M_vap := cumsum( v * M ) ]

balance_anual[ , P_vap := cumsum( v * P ) ]
balance_anual[ , P_dec_vap := cumsum( v * P_dec ) ]
balance_anual[ , P_nodec_vap := cumsum( v * P_nodec ) ]
balance_anual[ , P3_vap := cumsum( v * P3 ) ]
balance_anual[ , P3_dec_vap := cumsum( v * P3_dec ) ]
balance_anual[ , P3_nodec_vap := cumsum( v * P3_nodec ) ]
balance_anual[ , P4_vap := cumsum( v * P4 ) ]
balance_anual[ , P4_dec_vap := cumsum( v * P4_dec ) ]
balance_anual[ , P4_nodec_vap := cumsum( v * P4_nodec ) ]
balance_anual[ , P6_vap := cumsum( v * P6 ) ]
balance_anual[ , P6_dec_vap := cumsum( v * P6_dec ) ]
balance_anual[ , P6_nodec_vap := cumsum( v * P6_nodec ) ]

balance_anual[ , A_vap := cumsum( v * A ) ]
balance_anual[ , A_afi_vap := cumsum( v * A_afi ) ]
balance_anual[ , A2_vap := cumsum( v * A2 ) ]
balance_anual[ , A3_vap := cumsum( v * A3 ) ]
balance_anual[ , A4_vap := cumsum( v * A4 ) ]
balance_anual[ , A6_vap := cumsum( v * A6 ) ]
balance_anual[ , A7_vap := cumsum( v * A7 ) ]
balance_anual[ , A8_vap := cumsum( v * A8 ) ]
balance_anual[ , A8_2_vap := cumsum( v * A8_2 ) ]
balance_anual[ , A8_3_vap := cumsum( v * A8_3 ) ]
balance_anual[ , A8_4_vap := cumsum( v * A8_4 ) ]
balance_anual[ , A8_6_vap := cumsum( v * A8_6 ) ]
balance_anual[ , A_est_3_vap := cumsum( v * A_est_3 ) ]
balance_anual[ , A_est_4_vap := cumsum( v * A_est_4 ) ]
balance_anual[ , A_est_6_vap := cumsum( v * A_est_6 ) ]
balance_anual[ , A_est_cat_vap := cumsum( v * A_est_cat ) ]
balance_anual[ , A_est_pen_vap := cumsum( v * A_est_pen ) ]
balance_anual[ , A_est_vap := cumsum( v * A_est ) ]

balance_anual[ , P_sd_vap := cumsum( v * P_sd ) ]
balance_anual[ , P3_sd_vap := cumsum( v * P3_sd ) ]
balance_anual[ , P4_sd_vap := cumsum( v * P4_sd ) ]
balance_anual[ , P6_sd_vap := cumsum( v * P6_sd ) ]

balance_anual[ , B_vap := cumsum( v * B ) ]
balance_anual[ , B_pen_vap := cumsum( v * B_pen ) ]
balance_anual[ , B_dep_vap := cumsum( v * B_dep ) ]
balance_anual[ , B2_vap := cumsum( v * B2 ) ]
balance_anual[ , B3_vap := cumsum( v * B3 ) ]
balance_anual[ , B4_vap := cumsum( v * B4 ) ]
balance_anual[ , B6_vap := cumsum( v * B6 ) ]
balance_anual[ , B7_vap := cumsum( v * B7 ) ]
balance_anual[ , B8_vap := cumsum( v * B8 ) ]
balance_anual[ , B9_vap := cumsum( v * B9 ) ]

balance_anual[ , B_cat_vap := cumsum( v * B_cat ) ]
balance_anual[ , B2_cat_vap := cumsum( v * B2_cat ) ]
balance_anual[ , B3_cat_vap := cumsum( v * B3_cat ) ]
balance_anual[ , B4_cat_vap := cumsum( v * B4_cat ) ]
balance_anual[ , B6_cat_vap := cumsum( v * B6_cat ) ]
balance_anual[ , B7_cat_vap := cumsum( v * B7_cat ) ]
balance_anual[ , B8_cat_vap := cumsum( v * B8_cat ) ]

balance_anual[ , B_ncat_vap := cumsum( v * B_ncat ) ]
balance_anual[ , B2_ncat_vap := cumsum( v * B2_ncat ) ]
balance_anual[ , B3_ncat_vap := cumsum( v * B3_ncat ) ]
balance_anual[ , B4_ncat_vap := cumsum( v * B4_ncat ) ]
balance_anual[ , B6_ncat_vap := cumsum( v * B6_ncat ) ]
balance_anual[ , B7_ncat_vap := cumsum( v * B7_ncat ) ]
balance_anual[ , B8_ncat_vap := cumsum( v * B8_ncat ) ]

balance_anual[ , G_vap := cumsum( v * G ) ]
balance_anual[ , V := v * V_cap ]
balance_anual[ , V0 := esc$V0 ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( aportes, beneficios, balance_anual,
      file = paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )

rm( list = ls()[ !( ls() %in% c( parametros_lista ) ) ] )
gc()
