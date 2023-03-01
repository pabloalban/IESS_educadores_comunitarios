message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tCalculando escenarios del balance para SAL' )

# Carga --------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'Hipotesis' ) ) ] )

# Escenario 1 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_1'
message( '\n\t\t\t', esc$nombre )
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$i_f <- Hipotesis[ 7, 2 ]
esc$i_p <- Hipotesis[ 7, 2 ]
esc$i_m <- 1.2 * Hipotesis[ 7, 2 ] # Definido 2018
esc$V0 <- parametros$reserva_ini 
esc$aporte_estado <- 0.00
esc$aporte_pen <- 0
# esc$calibra_aux_fun <- 0.275 
# esc$calibra_apo <- 1 #0.916559 # correccion para igualar la masa de IVM-ILO PENS
# esc$calibra_pen_vej <- 1.105
# esc$calibra_pen_inv <- 0.70
# esc$mont_prof_afi <- 0.1275
esc$gadmin <- 0.00 # porcentaje sobre el ingreso presupuestario que en la práctica corresponde a los aportes anuales

esc$apo_act <- data.table( t = 0:parametros$horizonte, 
                           por_apo = c( 0.0516, rep( 0.0516, 10 ) ),# + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
                           por_apo_ext_cot = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_ext_pen = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_men_18 = 0, # prima para financiar beneficios de menores a 18
                           por_apo_gas = esc$gadmin * c( 0.0516, rep( 0.0516, 10 ) ), # límite legal del gasto administrativo en porcentaje de los aportes
                           
                           # Calibración de beneficios
                           cal_ben_2     = 1,
                           cal_ben_cat_2 = 1,
                           cal_ben_3     = 1.44,
                           cal_ben_cat_3 = 1.44,
                           cal_ben_4     = 1.44,
                           cal_ben_cat_4 = 1.44,
                           cal_ben_6     = 1,
                           cal_ben_cat_6 = 1,
                           cal_ben_7     = 0.47,
                           cal_ben_cat_7 = 0.47,
                           cal_ben_8     = 50.,
                           cal_ben_cat_8 = 50.,
                           
                           cal_apo_est_3 = 0.00,
                           cal_apo_est_4 = 0.00,
                           cal_apo_est_6 = 0.00,
                           cal_apo_est_cat = 0.00
)

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Escenario 2 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_2'
message( '\n\t\t\t', esc$nombre )
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$i_f <- Hipotesis[ 7, 2 ]
esc$i_p <- Hipotesis[ 7, 2 ]
esc$i_m <- 1.2 * Hipotesis[ 7, 2 ] # Definido 2018
esc$V0 <- parametros$reserva_ini
esc$aporte_estado <- 1.0
esc$aporte_pen <- 0
# esc$calibra_aux_fun <- 0.275
# esc$calibra_apo <- 1 #0.916559 # correccion para igualar la masa de IVM-ILO PENS
# esc$calibra_pen_vej <- 1.105
# esc$calibra_pen_inv <- 0.70
# esc$mont_prof_afi <- 0.1275
esc$gadmin <- 0.00 # porcentaje sobre el ingreso presupuestario que en la práctica corresponde a los aportes anuales

esc$apo_act <- data.table( t = 0:parametros$horizonte,
                           por_apo = c( 0.0516, rep( 0.0516, 10 ) ),# + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
                           por_apo_ext_cot = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_ext_pen = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_men_18 = 0, # prima para financiar beneficios de menores a 18
                           por_apo_gas = esc$gadmin * c( 0.0516, rep( 0.0516, 10 ) ), # límite legal del gasto administrativo en porcentaje de los aportes
                           
                           # Calibración de beneficios
                           cal_ben_2     = 1,
                           cal_ben_cat_2 = 1,
                           cal_ben_3     = 1.44,
                           cal_ben_cat_3 = 1.44,
                           cal_ben_4     = 1.44,
                           cal_ben_cat_4 = 1.44,
                           cal_ben_6     = 1,
                           cal_ben_cat_6 = 1,
                           cal_ben_7     = 0.47,
                           cal_ben_cat_7 = 0.47,
                           cal_ben_8     = 50.,
                           cal_ben_cat_8 = 50.,
                           
                           cal_apo_est_3 = 1.00,
                           cal_apo_est_4 = 1.00,
                           cal_apo_est_6 = 1.00,
                           cal_apo_est_cat = 1.00
)

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


# Escenario 3 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_3'
message( '\n\t\t\t', esc$nombre )
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$i_f <- Hipotesis[ 7, 2 ]
esc$i_p <- Hipotesis[ 7, 2 ]
esc$i_m <- 1.2 * Hipotesis[ 7, 2 ] # Definido 2018
esc$V0 <- 489002157.62 #2403773950 #parametros$reserva_ini
esc$aporte_estado <- 0.0
esc$aporte_pen <- 0
# esc$calibra_aux_fun <- 0.275
# esc$calibra_apo <- 1 #0.916559 # correccion para igualar la masa de IVM-ILO PENS
# esc$calibra_pen_vej <- 1.105
# esc$calibra_pen_inv <- 0.70
# esc$mont_prof_afi <- 0.1275
esc$gadmin <- 0.00 # porcentaje sobre el ingreso presupuestario que en la práctica corresponde a los aportes anuales

esc$apo_act <- data.table( t = 0:parametros$horizonte,
                           por_apo = c( 0.0516, rep( 0.0516, 10 ) ),# + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
                           por_apo_ext_cot = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_ext_pen = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_men_18 = 0, # prima para financiar beneficios de menores a 18
                           por_apo_gas = esc$gadmin * c( 0.0516, rep( 0.0516, 10 ) ), # límite legal del gasto administrativo en porcentaje de los aportes
                           
                           # Calibración de beneficios
                           cal_ben_2     = 1,
                           cal_ben_cat_2 = 1,
                           cal_ben_3     = 1.44,
                           cal_ben_cat_3 = 1.44,
                           cal_ben_4     = 1.44,
                           cal_ben_cat_4 = 1.44,
                           cal_ben_6     = 1,
                           cal_ben_cat_6 = 1,
                           cal_ben_7     = 0.47,
                           cal_ben_cat_7 = 0.47,
                           cal_ben_8     = 50.,
                           cal_ben_cat_8 = 50.,
                           
                           cal_apo_est_3 = 1.00,
                           cal_apo_est_4 = 1.00,
                           cal_apo_est_6 = 1.00,
                           cal_apo_est_cat = 1.00
                           )

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Escenario 4 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_4'
message( '\n\t\t\t', esc$nombre )
esc$i_a <- 0.0625
esc$i_r <- Hipotesis[ 4, 2 ]
esc$i_sbu <- Hipotesis[ 5, 2 ]
esc$i_f <- Hipotesis[ 7, 2 ]
esc$i_p <- Hipotesis[ 7, 2 ]
esc$i_m <- 1.2 * Hipotesis[ 7, 2 ] # Definido 2018
esc$V0 <- 489002157.62 #2403773950 #parametros$reserva_ini
esc$aporte_estado <- 1.0
esc$aporte_pen <- 0
# esc$calibra_aux_fun <- 0.275
# esc$calibra_apo <- 1 #0.916559 # correccion para igualar la masa de IVM-ILO PENS
# esc$calibra_pen_vej <- 1.105
# esc$calibra_pen_inv <- 0.70
# esc$mont_prof_afi <- 0.1275
esc$gadmin <- 0.00 # porcentaje sobre el ingreso presupuestario que en la práctica corresponde a los aportes anuales

esc$apo_act <- data.table( t = 0:parametros$horizonte,
                           por_apo = c( 0.0516, rep( 0.0516, 10 ) ),# + 0.1 * 0.008, # prima para financiar el gasto administrativo, en porcentaje de los aportes
                           por_apo_ext_cot = 0.0341, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_ext_pen = 0.0415, # prima para financiar beneficios de cónyuges por extensión de cobertura
                           por_apo_men_18 = 0, # prima para financiar beneficios de menores a 18
                           por_apo_gas = esc$gadmin * c( 0.0516, rep( 0.0516, 10 ) ), # límite legal del gasto administrativo en porcentaje de los aportes
                           
                           # Calibración de beneficios
                           cal_ben_2     = 1,
                           cal_ben_cat_2 = 1,
                           cal_ben_3     = 1.44,
                           cal_ben_cat_3 = 1.44,
                           cal_ben_4     = 1.44,
                           cal_ben_cat_4 = 1.44,
                           cal_ben_6     = 1,
                           cal_ben_cat_6 = 1,
                           cal_ben_7     = 0.47,
                           cal_ben_cat_7 = 0.47,
                           cal_ben_8     = 50.,
                           cal_ben_cat_8 = 50.,
                           
                           cal_apo_est_3 = 1.00,
                           cal_apo_est_4 = 1.00,
                           cal_apo_est_6 = 1.00,
                           cal_apo_est_cat = 1.00
)

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )
source( 'R/sal/303_calculo_balance_sal.R', encoding = 'UTF-8', echo = FALSE )
save( esc, file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Cálculo de primas y análisis de ratios para todos los escenarios ---------------------------------
message( '\n\tCalculando primas y análisis de ratios' )
source( 'R/SAL/305_calculo_prima_sal.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SAL/306_analisis_ratios_sal.R', encoding = 'UTF-8', echo = FALSE )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
