message( paste( rep( '-', 100 ), collapse = '' ) )

#Script diseñado para ejecutarse dentro del 305_calculo_balance_ces.R
#Calcula los fondos disponibles antes de egresos y saldos anuales de las cuentas individuales
#administradas por el IESS.

# 1. Proyectando depositos por aportes--------------------------------------------------------------
proy_fondo <- merge( pob_proy, 
                     sal_proy[ , list( t, sexo, x, sal_mes, sal ) ], 
                     by = c( 't', 'sexo', 'x' ), all.x = TRUE )
proy_fondo[ is.na( sal ), sal := 0 ]
proy_fondo[ is.na( sal_mes ), sal_mes := 0 ]

proy_fondo <- merge( proy_fondo, 
                     esc$hip_esc, 
                     by = c( 't' ), all.x = TRUE )

setorder( proy_fondo, t, sexo, x )

proy_fondo[ , M := cal_mas * sal * l2_cotc ]


proy_fondo[ , A2_per := apo_per * M ]
proy_fondo[ , A2_pat := apo_pat * M ]
proy_fondo[ , A2 := A2_per + A2_pat ]
proy_fondo[ t == 0 , A2_pat := 0 ]
proy_fondo[ t == 0 , A2_per := 0 ]
proy_fondo[ t == 0 , A2 := 0 ]


# Gasto administrativo
proy_fondo[ , G := por_gas * A2 ]
proy_fondo[ t == 0 , G := 0 ]

# Deposito neto
proy_fondo[ , D := A2 - G ]


# 2. Proyectando Fondos disponibles, Saldos de cuentas y prestaciones-------------------------------
S_F <- as.data.table(S_F_edad_sexo)
S_F <- S_F[ , list( sexo, x, S = S_int, F =  F_int)]
S_F[,t:=0]

proy_fondo <- merge( proy_fondo, 
                     S_F, 
                     by = c( 't', 'sexo', 'x' ), all.x = TRUE )
proy_fondo[ is.na( F ), F := 0 ]
proy_fondo[ is.na( S ), S := 0 ]

f_i_int <- as.data.table(f_i_int)
f_i <- f_i_int[ , f_i := exp(log_f_i_int) ]

f_i <- as.data.table(dcast(f_i, sexo + x ~ i, value.var = "f_i"))
colnames(f_i) <- c('sexo','x',paste0(rep('f_',10),c(10:18,9)))
f_i[ is.na(f_i), ] <- 0


#Cruze de proy_fondo con f_i_int--------------------------------------------------------------------
proy_fondo <- merge( proy_fondo, 
                     f_i, 
                     by = c( 'sexo', 'x' ), all.x = TRUE )
proy_fondo[ is.na( proy_fondo ), ] <- 0

setorder( proy_fondo, t, sexo, x )

#inicializando para t=0-----------------------------------------------------------------------------
message( '\tProyectando gastos prestacionales' )
proy_fondo <- proy_fondo[ x >= 15, ]
#proy_fondo[ l2_cotc == 0,  l2_cotc := 1 ]
proy_fondo[ l2_cotc > 0 , B9 := esc$hip_esc$cal_B9[1] * f_9 * ( F / l2_cotc ) * l9 ]
proy_fondo[ l2_cotc == 0 , B9 := 0 ]
proy_fondo[ l2_cotc > 0 , B10 := esc$hip_esc$cal_B10[1] * f_10 * ( F / l2_cotc  ) * l10]
proy_fondo[ l2_cotc == 0 , B10 := 0 ]
proy_fondo[ l2_cotc > 0 , B11 := esc$hip_esc$cal_B11[1] * f_11 * ( F / l2_cotc ) * l11]
proy_fondo[ l2_cotc == 0 , B11 := 0 ]
proy_fondo[ l2_cotc > 0 , B12 := esc$hip_esc$cal_B12[1] * f_12 * ( F / l2_cotc ) * l12]
proy_fondo[ l2_cotc == 0 , B12 := 0 ]
proy_fondo[ l2_cotc > 0 , B13 := esc$hip_esc$cal_B13[1] * f_13 * ( F / l2_cotc ) * l13]
proy_fondo[ l2_cotc == 0 , B13 := 0 ]
proy_fondo[ l2_cotc > 0 , B14 := esc$hip_esc$cal_B14[1] * f_14 * ( F / l2_cotc ) * l14]
proy_fondo[ l2_cotc == 0 , B14 := 0 ]
proy_fondo[ , Numerador :=  B9 +  B11 + B12 + B13 + B14 ]
proy_fondo[ , Denominador := sum( proy_fondo$B9, na.rm = TRUE ) +
              sum( proy_fondo$B11, na.rm = TRUE) +
              sum( proy_fondo$B12, na.rm = TRUE) +
              sum( proy_fondo$B13, na.rm = TRUE) +
              sum( proy_fondo$B14, na.rm = TRUE) ]
proy_fondo[ l2_cotc > 0 , B15 := Numerador * 657613.86 / Denominador ]
proy_fondo[ l2_cotc == 0 , B15 := 0 ]
proy_fondo[ l2_cotc > 0 , B16 := Numerador * 340149.24 / Denominador ]
proy_fondo[ l2_cotc == 0 , B16 := 0 ]
proy_fondo[ l2_cotc > 0 , B17 := Numerador * 101833.90 / Denominador ]
proy_fondo[ l2_cotc == 0 , B17 := 0 ] 
proy_fondo[ l2_cotc > 0 , B18 := Numerador * 44464.26 / Denominador ]
proy_fondo[ l2_cotc == 0 , B18 := 0 ]
proy_fondo[ is.na(proy_fondo) ] <- 0 
proy_fondo[ ,B := B9 + B10 + B11 + B13 + B14 + B15 + B16 + B17 + B18 ]
proy_fondo[ sexo=='M' ,cod_sexo:= 2]
proy_fondo_m <- proy_fondo[ sexo=='M', ] 
proy_fondo_m[ ,sexo:=NULL]
proy_fondo[ sexo=='F' ,cod_sexo:= 1]
proy_fondo_f <- proy_fondo[ sexo=='F', ]
proy_fondo_f[ ,sexo:=NULL]

# Horizonte de proyección
t_horiz <- parametros$horizonte

# Año inicial de proyección
fec_ini <- parametros$anio_ini

# Año final de proyección
fec_fin <- fec_ini + t_horiz

# Tiempo
t <- 0:t_horiz

# Edades
x_max <- parametros$edad_max
x <- 15 : x_max

N <- length( t )
M <- length( x ) 

# Arrays para beneficios
B_f <- simplify2array(by(proy_fondo_f[ , list(t, x, B,
                                              B9, B10, B11, B12,
                                              B13, B14, B15, B16,
                                              B17, B18, Denominador, Numerador)], proy_fondo_f$x, as.matrix))
B_m <- simplify2array(by(proy_fondo_m[ , list(t,x, B,
                                              B9, B10, B11, B12,
                                              B13, B14, B15, B16,
                                              B17, B18, Denominador, Numerador)], proy_fondo_m$x, as.matrix))

# Arrays para tasas
f_f <- simplify2array(by(proy_fondo_f[ , list(t, x,
                                              f_9, f_10, f_11, f_12,
                                              f_13, f_14, f_15, f_16,
                                              f_17, f_18)], proy_fondo_f$x, as.matrix))
f_m <- simplify2array(by(proy_fondo_m[ , list(t,x,
                                              f_9, f_10, f_11, f_12,
                                              f_13, f_14, f_15, f_16,
                                              f_17, f_18)], proy_fondo_m$x, as.matrix))

# Arrays para población
l_f <- simplify2array(by(proy_fondo_f[ , list(t, x, l2_cotc,
                                              l9, l10, l11, l12,
                                              l13, l14, l15, l16,
                                              l17, l18)], proy_fondo_f$x, as.matrix))
l_m <- simplify2array(by(proy_fondo_m[ , list(t,x, l2_cotc,
                                              l9, l10, l11, l12,
                                              l13, l14, l15, l16,
                                              l17, l18)], proy_fondo_m$x, as.matrix))

# Arrays para depositos
D_f <- simplify2array(by(proy_fondo_m[ , list(t, x, D)], proy_fondo_m$x, as.matrix))
D_m <- simplify2array(by(proy_fondo_f[ , list(t, x, D)], proy_fondo_f$x, as.matrix))

# Arrays para saldos de fondos por cohorte 
S_F_f <- simplify2array(by(proy_fondo_m[ , list(t, x, i_q, S, F, A2)], proy_fondo_m$x, as.matrix))
S_F_m <- simplify2array(by(proy_fondo_f[ , list(t, x, i_q, S, F, A2)], proy_fondo_f$x, as.matrix))

# Proyección población -----------------------------------------------------------------------------
message( '\tProyectando beneficios' )
for ( n in 1 : (N-1) ) {
  for ( k in 1 : (M-1) ) {
    #Mujeres
    S_F_f[ n + 1, 'F', 1 ] <- 0
    S_F_f[ n + 1, 'F', k + 1 ] <-  D_f[ n + 1 , 'D', k + 1 ] + (1 +  S_F_f[ n + 1 , 'i_q', k + 1 ]) * S_F_f[ n , 'S', k ]
    if (l_f[ n + 1, 'l2_cotc', k + 1  ] > 0) {
      B_f[ n + 1, 'B9', k + 1  ] <-  esc$hip_esc$cal_B9[1] * f_f[ n + 1, 'f_9', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l9', k + 1 ]
      B_f[ n + 1, 'B10', k + 1  ] <-  esc$hip_esc$cal_B10[1] * f_f[ n + 1, 'f_10', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l10', k + 1 ]
      B_f[ n + 1, 'B11', k + 1  ] <-  esc$hip_esc$cal_B11[1] * f_f[ n + 1, 'f_11', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l11', k + 1 ]
      B_f[ n + 1, 'B12', k + 1  ] <-  esc$hip_esc$cal_B12[1] * f_f[ n + 1, 'f_12', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l12', k + 1 ]
      B_f[ n + 1, 'B13', k + 1  ] <-  esc$hip_esc$cal_B13[1] * f_f[ n + 1, 'f_13', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l13', k + 1 ]
      B_f[ n + 1, 'B14', k + 1  ] <-  esc$hip_esc$cal_B14[1] * f_f[ n + 1, 'f_14', k + 1  ] * ( S_F_f[ n + 1, 'F', k + 1 ] / l_f[ n + 1, 'l2_cotc', k + 1  ] ) * l_f[ n + 1, 'l14', k + 1 ]
      B_f[ n + 1, 'Numerador', k + 1  ] <-  ( B_f[ n + 1, 'B9', k + 1  ] +
                                          B_f[ n + 1, 'B11', k + 1  ] + 
                                          B_f[ n + 1, 'B12', k + 1  ] +
                                          B_f[ n + 1, 'B13', k + 1  ] +
                                          B_f[ n + 1, 'B14', k + 1  ] )
      B_f[ n + 1, 'B15', k + 1  ] <-  B_f[ n + 1, 'Numerador', k + 1  ] * 657613.86 / B_f[ n + 1, 'Denominador', k + 1  ]
      B_f[ n + 1, 'B16', k + 1  ] <-  B_f[ n + 1, 'Numerador', k + 1  ] * 340149.24 / B_f[ n + 1, 'Denominador', k + 1 ]
      B_f[ n + 1, 'B17', k + 1  ] <-  B_f[ n + 1, 'Numerador', k + 1  ] * 101833.90  / B_f[ n + 1, 'Denominador', k + 1 ]
      B_f[ n + 1, 'B18', k + 1  ] <-  B_f[ n + 1, 'Numerador', k + 1  ] * 44464.26 / B_f[ n + 1, 'Denominador', k + 1 ]
      }
    
    B_f[ n + 1, 'B', k + 1  ] <- B_f[ n + 1, 'B9', k + 1  ] +
      B_f[ n + 1, 'B10', k + 1  ] +
      B_f[ n + 1, 'B11', k + 1  ] +
      B_f[ n + 1, 'B12', k + 1  ] +
      B_f[ n + 1, 'B13', k + 1  ] +
      B_f[ n + 1, 'B14', k + 1  ] +
      B_f[ n + 1, 'B15', k + 1  ] +
      B_f[ n + 1, 'B16', k + 1  ] +
      B_f[ n + 1, 'B17', k + 1  ] +
      B_f[ n + 1, 'B18', k + 1  ]
    S_F_f[ n + 1, 'S', k + 1 ] <- S_F_f[ n + 1, 'F', k + 1 ] - B_f[ n + 1, 'B', k + 1  ]
    
    #Hombres
    if (l_m[ n + 1, 'l2_cotc', k + 1  ] > 0) {
     # B_m[ n + 1, 'Denominador', k + 1  ] <- B_f[ n + 1, 'Denominador', k + 1  ] 
      S_F_m[ n + 1, 'F', 1 ] <- 0
      S_F_m[ n + 1, 'F', k + 1 ] <-  D_m[ n + 1 , 'D', k + 1 ] + (1 +  S_F_m[ n + 1 , 'i_q', k + 1 ]) * S_F_m[ n , 'S', k ]
      B_m[ n + 1, 'B9', k + 1  ] <-  esc$hip_esc$cal_B9[1] * f_m[ n + 1, 'f_9', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l9', k + 1 ]
      B_m[ n + 1, 'B10', k + 1  ] <-  esc$hip_esc$cal_B10[1] * f_m[ n + 1, 'f_10', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l10', k + 1 ]
      B_m[ n + 1, 'B11', k + 1  ] <-  esc$hip_esc$cal_B11[1] * f_m[ n + 1, 'f_11', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l11', k + 1 ]
      B_m[ n + 1, 'B12', k + 1  ] <-  esc$hip_esc$cal_B12[1] * f_m[ n + 1, 'f_12', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l12', k + 1 ]
      B_m[ n + 1, 'B13', k + 1  ] <-  esc$hip_esc$cal_B13[1] * f_m[ n + 1, 'f_13', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l13', k + 1 ]
      B_m[ n + 1, 'B14', k + 1  ] <-  esc$hip_esc$cal_B14[1] * f_m[ n + 1, 'f_14', k + 1  ] * ( S_F_m[ n + 1, 'F', k + 1 ] / l_m[ n + 1, 'l2_cotc', k + 1  ] ) * l_m[ n + 1, 'l14', k + 1 ]
      B_m[ n + 1, 'Numerador', k + 1  ] <-  ( B_m[ n + 1, 'B9', k + 1  ] +
                                                B_m[ n + 1, 'B11', k + 1  ] + 
                                                B_m[ n + 1, 'B12', k + 1  ] +
                                                B_m[ n + 1, 'B13', k + 1  ] +
                                                B_m[ n + 1, 'B14', k + 1  ] )
      B_m[ n + 1, 'B15', k + 1  ] <-  B_m[ n + 1, 'Numerador', k + 1  ] * 657613.86 / B_m[ n + 1, 'Denominador', k + 1 ] 
      B_m[ n + 1, 'B16', k + 1  ] <-  B_m[ n + 1, 'Numerador', k + 1  ] * 340149.24 / B_m[ n + 1, 'Denominador', k + 1 ]
      B_m[ n + 1, 'B17', k + 1  ] <-  B_m[ n + 1, 'Numerador', k + 1  ] * 101833.90  / B_m[ n + 1, 'Denominador', k + 1 ]
      B_m[ n + 1, 'B18', k + 1  ] <-  B_m[ n + 1, 'Numerador', k + 1  ] * 44464.26 / B_m[ n + 1, 'Denominador', k + 1 ]
      
        B_m[ n + 1, 'B', k + 1  ] <- B_m[ n + 1, 'B9', k + 1  ] +
        B_m[ n + 1, 'B10', k + 1  ] +
        B_m[ n + 1, 'B11', k + 1  ] +
        B_m[ n + 1, 'B12', k + 1  ] +
        B_m[ n + 1, 'B13', k + 1  ] +
        B_m[ n + 1, 'B14', k + 1  ] +
        B_m[ n + 1, 'B15', k + 1  ] +
        B_m[ n + 1, 'B16', k + 1  ] +
        B_m[ n + 1, 'B17', k + 1  ] +
        B_m[ n + 1, 'B18', k + 1  ]
      S_F_m[ n + 1, 'S', k + 1 ] <- S_F_m[ n + 1, 'F', k + 1 ] - B_m[ n + 1, 'B', k + 1  ]
    }
  }
  }

# Transformación proyección a data.table -----------------------------------------------------------
message( '\tCreando data.table con gastos prestacionales' )
B_ini <- NULL
S_F_ini <- NULL
for ( n in 1:N ) {
  B <- rbind( t(B_f[ n, , ]), t(B_m[n , , ]) )
  S_F <- rbind( t(S_F_f[ n, , ]), t(S_F_m[ n, , ] ) )
  
  B <- as.data.table( B )
  B<-B[, sexo := rep( c( 'F', 'M' ), each = M )]
  
  S_F <- as.data.table( S_F )
  S_F<-S_F[, sexo := rep( c( 'F', 'M' ), each = M )]
  
  B <- rbind( B, B_ini )
  B_ini <- B
  
  S_F <- rbind( S_F, S_F_ini )
  S_F_ini <- S_F
}


proy_fondo <- merge( B, S_F, by = c('x', 't', 'sexo'), all = TRUE)
proy_fondo[mapply(is.infinite, proy_fondo)] <- 0

proy_fondo_anual <- proy_fondo[ , list( A2 = sum( A2 , na.rm = TRUE ),
                                        B = sum( B , na.rm = TRUE ),
                                        B9 = sum( B9 , na.rm = TRUE ),
                                        B10 = sum( B10 , na.rm = TRUE ),
                                        B11 = sum( B11 , na.rm = TRUE ),
                                        B12 = sum( B12 , na.rm = TRUE ),
                                        B13 = sum( B13 , na.rm = TRUE ),
                                        B14 = sum( B14 , na.rm = TRUE ),
                                        B15 = sum( B15 , na.rm = TRUE ),
                                        B16 = sum( B16 , na.rm = TRUE ),
                                        B17 = sum( B17 , na.rm = TRUE ),
                                        B18 = sum( B18 , na.rm = TRUE ),
                                        S= sum(S,na.rm = TRUE),
                                        F = sum(F,na.rm = TRUE)),
                          by = list( t ) ]

# proy_fondo <- B
# proy_fondo[mapply(is.infinite, proy_fondo)] <- 0