message( paste( rep( '-', 100 ), collapse = '' ) )

load( paste0( parametros$RData_seg, "IESS_CES_DES_proyeccion_salarios.RData"))
load( paste0( parametros$RData_seg, 'IESS_CES_Fondo_disponible_inicial_int.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_CES_f_i.RData' ) )
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, 'IESS_CES_proyeccion_poblacion.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros', 'pob_proy_edad_sexo', 'sal_proy', 
                                 'sbu_proy', 'f_i_int', 'S_F_edad_sexo', 'densidad_cotizacion_int',
                                 'pob_proy') ) ] )

message( '\tBalance seguro ', parametros$seguro, ' calculando escenario: ', esc$nombre )

# 1. Proyectando retiros de cesantía por cada causal------------------------------------------------
message( '\tProyectando retiros de cesantía por cada causal' )
source( 'R/ces/303_proyeccion_obligaciones_ces.R', encoding = 'UTF-8', echo = FALSE )

# 2. Juntando hipótesis y proyecciones demograficas-------------------------------------------------
message( '\tProyectando ingresos por aportes' )

balance <- merge( pob_proy, 
                  sal_proy[ , list( t, sexo, x, sal_mes, sal ) ], 
                  by = c( 't', 'sexo', 'x' ), all.x = TRUE )
balance[ is.na( sal ), sal := 0 ]
balance[ is.na( sal_mes ), sal_mes := 0 ]

balance <- merge( balance, 
                  esc$hip_esc, 
                  by = c( 't' ), all.x = TRUE )

setorder( balance, t, sexo, x )

# 3. Proyectando masa salarial----------------------------------------------------------------------
message( '\tProyectando masa salarial' )
balance[ , M := cal_mas * sal * l2_cotc ] # se utiliza la masa salarial del SGO ajustada al 2018 por un factor

# 4. Añadiendo las prestaciones por cada causal-----------------------------------------------------
message( '\tAñadiendo las prestaciones por cada causal' )
balance <- merge( balance, 
                  proy_fondo, 
                  by = c( 'sexo', 'x', 't' ), all.x = TRUE )
balance[ is.na( balance ), ] <- 0

# 5. Proyectando ingresos por aportes --------------------------------------------------------------
message( '\tProyectando aportes' )
balance[ , A2_per := apo_per * M ]
balance[ , A2_pat := apo_pat * M ]
balance[ , A2 := A2_per + A2_pat ]
balance[ , A := A2 ]
balance[ t == 0 , A2_pat := 0 ]
balance[ t == 0 , A2_per := 0 ]
balance[ t == 0 , A2 := 0 ]
balance[ t == 0 , A := 0 ]

# 5. 1.  Proyectando gastos administrativos---------------------------------------------------------
message( '\tProyectando gastos administrativos' )
balance[ , G := por_gas * A2 ]
balance[ t == 0 , G := 0 ]

# 5. 2. Proyectando Prestaciones totales------------------------------------------------------------
message( '\tProyectando gastos prestacionales totales' )
balance[ , B := B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 +B18 ]

# 5. 3. Proyectando ingresos y egresos--------------------------------------------------------------
message( '\tProyectando ingresos y egresos' )
balance[ , Act := A  ] 
balance[ , Pas := B + G  ] 

# 6. Calculo Balance corriente ---------------------------------------------------------------------
balance_anual <- balance[ , list( M = sum( M , na.rm = TRUE ),
                                  
                                  Act = sum( Act, na.rm = TRUE ),
                                  Pas = sum( Pas , na.rm = TRUE ),
                                  
                                  A = sum( A, na.rm = TRUE ),
                                  A2 = sum( A2, na.rm = TRUE ),
                                  A2_per = sum( A2_per, na.rm = TRUE ),
                                  A2_pat = sum( A2_pat, na.rm = TRUE ),

                                  B = sum( B, na.rm = TRUE ),
                                  B9 = sum( B9, na.rm = TRUE ),
                                  B10 = sum( B10, na.rm = TRUE ),
                                  B11 = sum( B11, na.rm = TRUE ),
                                  B12 = sum( B12, na.rm = TRUE ),
                                  B13 = sum( B13, na.rm = TRUE ),
                                  B14 = sum( B14, na.rm = TRUE ),
                                  B15 = sum( B15, na.rm = TRUE ),
                                  B16 = sum( B16, na.rm = TRUE ),
                                  B17 = sum( B17, na.rm = TRUE ),
                                  B18 = sum( B18, na.rm = TRUE ),
                                  S = sum( S, na.rm = TRUE ),

                                  G = sum( G, na.rm = TRUE ) ), 
                          by = list( t ) ]

balance_anual[ , V_cor := A - B - G ]

# cat( paste0( names( balance_anual ), ' = 0,' ) )
balance_anual[ t == 0, `:=`( M = 0, 
                             Act = 0, Pas = 0, A = 0, A2 = 0, A2_per = 0, A2_pat = 0, 
                             B = 0, B9 = 0, B10 = 0, B11 = 0, B12 = 0, B13 = 0, B14 = 0, B15 = 0, 
                             B16 = 0, B17 = 0, B18 = 0, S = 0, G = 0, V_cor = 0 ) ]

balance_anual <- merge( balance_anual, esc$hip_esc[ , list( t, u, v, r, vq, c, V0, C0 = S0 ) ], by = 't' )
setorder( balance_anual, t )

#Calculo del balance corriente----------------------------------------------------------------------
balance_anual[ , A_cap := u * cumsum( v * A ) ]
balance_anual[ , A2_cap := u * cumsum( v * A2 ) ]
balance_anual[ , A2_per_cap := u * cumsum( v * A2_per ) ]
balance_anual[ , A2_pat_cap := u * cumsum( v * A2_pat ) ]
balance_anual[ , G_cap := u * cumsum( v * G ) ]
balance_anual[ , B_cap := u * cumsum( v * B ) ]

#balance_anual[ , S := r * ( cumsum( v * A2 - v * G - v * B ) + esc$S0 ) ] # Saldos acumulados al corte después de ser capitalizados
balance_anual[ t == 0, Act := 0 ]
balance_anual[ t == 0, Pas := 0 ] 
balance_anual[ , Act := u * cumsum( v * Act ) ] 
balance_anual[ , Pas := u * cumsum( v * Pas ) ] 

balance_anual[ , C := r * C0 + r * cumsum( vq * ( A - B ) ) ]

balance_anual[ , V_cap := u * C0 + u * V0 + Act - Pas ] # exceso de rendimientos financieros (se paga a i_q, pero se invierte a i_a)
balance_anual[ t == 0, V_cap := V0 + C0 ]

# 7. Balance actuarial -----------------------------------------------------------------------------
balance_anual[ , M_vap := cumsum( v * M ) ]
balance_anual[ , Act_vap := v * Act ]
balance_anual[ , A_vap := v * A_cap ]
balance_anual[ , A2_vap := v * A2_cap ]
balance_anual[ , A2_per_vap := v * A2_per_cap ]
balance_anual[ , A2_pat_vap := v * A2_pat_cap ]
balance_anual[ , Act_vap := v * Act ] 
balance_anual[ , Pas_vap := v * Pas ]
balance_anual[ , G_vap := cumsum( v * G ) ]
balance_anual[ , B_vap := cumsum( v * B ) ]
balance_anual[ , B9_vap := cumsum( v * B9 ) ]
balance_anual[ , B10_vap := cumsum( v * B10 ) ]
balance_anual[ , B11_vap := cumsum( v * B11 ) ]
balance_anual[ , B12_vap := cumsum( v * B12 ) ]
balance_anual[ , B13_vap := cumsum( v * B13 ) ]
balance_anual[ , B14_vap := cumsum( v * B14 ) ]
balance_anual[ , B15_vap := cumsum( v * B15 ) ]
balance_anual[ , B16_vap := cumsum( v * B16 ) ]
balance_anual[ , B17_vap := cumsum( v * B17 ) ]
balance_anual[ , B18_vap := cumsum( v * B18 ) ]

balance_anual[ , C_vap :=  v * C  ]
balance_anual[ , V := v * V_cap - C_vap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( balance, balance_anual,
      file = paste0( parametros$RData_seg, 'IESS_CES_balances_', esc$nombre, '.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()
