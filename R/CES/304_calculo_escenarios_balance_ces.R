message( '\tCalculando escenarios del balance para CES' )

# Carga de hipótesis macro 
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'Hipotesis' ) ) ] )

# 1. Escenario 1 -----------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

# 1.1. Hipótesis -----------------------------------------------------------------------------------
esc$V0 <- 664846613.08
esc$S0 <- 7417200150.63 + 715043240.00

esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           i_a = 0.0625,
                           i_r = Hipotesis[ 4, 2 ],
                           i_q = Hipotesis[ 2, 2 ],
                           
                           # Tasas de aportación y gasto
                           por_gas = 0.02,
                           apo_per = 0.02,
                           apo_pat = 0.00,
                           
                           # Factores de calibración
                           cal_mas = 1,
                           cal_B9 =  0.992711784739206,
                           cal_B10 = 1.07066763646142,
                           cal_B11 = 1.00304510856172,
                           cal_B12 = 1.03757362038561,
                           cal_B13 = 1.13562711869205,
                           cal_B14 = 1.11845140820745,
                           cal_B15 = 1,
                           cal_B16 = 1,
                           cal_B17 = 1,
                           cal_B18 = 1 )

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ , r := i_q ] # rendimiento de las cuentas
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ t == 0, r := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , r := 1 + r ]
esc$hip_esc[ , r := cumprod( r ) ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , c := ( r / u ) ]
esc$hip_esc[ , v := 1 / u ]
esc$hip_esc[ , vq := 1 / r ]
esc$hip_esc[ , V0 := esc$V0 ]
esc$hip_esc[ , S0 := esc$S0 ]

# 1.2. Calculos necesarios para el modelo actuarial ------------------------------------------------
parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/ces/305_calculo_balance_ces.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_CES_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Limpiando Ram-------------------------------------------------------------------------------------
message( paste( rep( '-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
