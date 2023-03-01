message( '\tCalculando escenarios del balance para SSC' )

# Carga --------------------------------------------------------------------------------------------
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'hip_macro_resumen' ) ) ] )

Hipotesis <- copy( hip_macro_resumen )

# Escenario 1 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

# Patrimonio inicial
esc$V0 <- 1776430561
# esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = 0.0625,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 0.3339 * 0.40,
                           apo_est_rel_dep = 0.4933 * 0.003,
                           apo_est_fij = 0.5833 * 288000,
                           apo_est_cat = 0.0,
                           apo_est_pen_sal = 0.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 0.93,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,    # 0.915,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1.0,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262 , 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Escenario 2 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_2'
message( '\t\t\t', esc$nombre )

# Patrimonio inicial
esc$V0 <- 1776430561
# esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = 0.0625,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 0 * 0.40,
                           apo_est_rel_dep = 0 * 0.003,
                           apo_est_fij = 0 * 288000,
                           apo_est_cat = 0.0,
                           apo_est_pen_sal = 0.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1.0, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 0.93,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1.0,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262, 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Escenario 3 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_3'
message( '\t\t\t', esc$nombre )

# Patrimonio inicial
esc$V0 <- 1776430561
# esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = 0.0625,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 1.0 * 0.40,
                           apo_est_rel_dep = 1.0 * 0.003,
                           apo_est_fij = 1.0 * 288000,
                           apo_est_cat = 1.0,
                           apo_est_pen_sal = 1.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 1.0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 1.0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 1.0,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1.0,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262, 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


# Escenario Alternativo 1 --------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_4'
message( '\t\t\t', esc$nombre )

# Patrimonio inicial
esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = 0.0625,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 0.3339 * 0.40,
                           apo_est_rel_dep = 0.4933 * 0.003,
                           apo_est_fij = 0.5833 * 288000,
                           apo_est_cat = 0.0,
                           apo_est_pen_sal = 0.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 0.93,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,    # 0.915,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1.0,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262 , 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Escenario Alternativo 2 --------------------------------------------------------------------------------------
esc <- new.env()

esc$nombre <- 'escenario_5'
message( '\t\t\t', esc$nombre )

# Patrimonio inicial
esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = 0.0625,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 0 * 0.40,
                           apo_est_rel_dep = 0 * 0.003,
                           apo_est_fij = 0 * 288000,
                           apo_est_cat = 0.0,
                           apo_est_pen_sal = 0.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1.0, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 0.93,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1.0,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262, 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

# Sensibilidad al Escenario 1 ----------------------------------------------------------------------
#Modificando la tasa de descuento actuarial
esc <- new.env()

do.call(file.remove, list( list.files( parametros$RData_seg, full.names = TRUE, pattern = "1_sens_")))
esc$nombre <- 'escenario_1_sens'
message( '\t\t\t', esc$nombre )

esc$part_tasas_des <- seq( 0.02, 0.085, by = 0.0025) 
#esc$part_tasas_des <- c( 0, 0.04, 0.0625 )

for ( k in 1:length( esc$part_tasas_des )) { # k <- 1
message( '\t\t\t\t', 'parte ', k )

# Patrimonio inicial
esc$V0 <- 1776430561
# esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS

# Excedente Gasto administrativo inicial
esc$exc_gast_adm <- 8261272.56

# Rendimiento inicial de la reserva
esc$interes_reserva <- 101021033.56

# Hipótesis
esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                           
                           #Tasas
                           # i_a = 0.0625,
                           i_r = Hipotesis[ 2,2 ]/100,
                           i_s = Hipotesis[ 3,2 ]/100,
                           i_f = Hipotesis[ 3,2 ]/100,
                           i_p = Hipotesis[ 3,2 ]/100,
                           i_d = esc$part_tasas_des[k],
                           i_a = ( esc$part_tasas_des[k] + 1 )/( 1 + Hipotesis[ 6, 2 ]/100 ) - 1,
                           
                           # Tasa gastos médicos
                           i_m = 1.2 * Hipotesis[ 6, 2 ]/100,
                           
                           # Gasto administrativo
                           por_gas_masa = 0.0005,
                           por_gas_apor = 0.03,
                           
                           # Aporte Estado
                           apo_est = 0.3339 * 0.40,
                           apo_est_rel_dep = 0.4933 * 0.003,
                           apo_est_fij = 0.5833 * 288000,
                           apo_est_cat = 0.0,
                           apo_est_pen_sal = 0.0,
                           
                           # Aportación SGO
                           apo_sgo = 0.007,
                           
                           #Aportación ISSFA e ISSPOL
                           apo_issfa = 0.007,
                           paga_issfa = 0,
                           apo_isspol = 1, # Las proyecciones corresponden al aporte al SSC
                           paga_isspol = 0,
                           
                           #Aportación de los seguros privado
                           por_sp = 1,
                           
                           # Montepios configuración
                           inc_montepio = 0,
                           por_pen_orfa = 0.20,
                           por_pen_cony = 0.40,
                           
                           # Auxilio de Funerales
                           inc_fun = 0,
                           ben_fun = 0.25,
                           
                           # Aporte jefes de familia
                           apo_ind = 0.025,
                           apo_inv = 0.001,
                           
                           bas_ref = 0.225,
                           reg_pen = 0.75,
                           pen_min = 100, 
                           
                           # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                           por_pag_sal = 0,
                           por_pag_sal_cat = 0,
                           
                           # Factores calibración
                           cal_masa_dep = 1.0024,
                           cal_apo_ssc = 0.965,
                           cal_apo_sgo = 1.051,
                           cal_aux_fun = 0.93,
                           cal_pen_vej = 1.0,
                           cal_pen_inv = 1.0,
                           cal_pen_viu = 1.0,
                           cal_pen_hue = 1,
                           
                           cal_apo_est = 1.0,
                           cal_apo_est_rel_dep = 1.155,
                           cal_apo_est_fij = 1.0,
                           cal_apo_est_cat = 1.0,
                           cal_apo_est_pen_sal = 1.0,
                           
                           cal_gast_adm = 1.00,
                           cal_prom_gast_adm = 0.391923031602262,
                           cal_exc_gast_adm = 1 - 0.391923031602262, 
                           
                           cal_ben_sal = 1.0,
                           cal_l2_2021 = 1.0 )

esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]

esc$hip_esc[ , u := i_a ]
esc$hip_esc[ t == 0, u := 0 ]
esc$hip_esc[ , u := 1 + u ]
esc$hip_esc[ , u := cumprod( u ) ]
esc$hip_esc[ , v := 1 / u  ]

esc$hip_esc[ , u_s := i_s ]
esc$hip_esc[ t == 0, u_s := 0 ]
esc$hip_esc[ , u_s := 1 + u_s ]
esc$hip_esc[ , u_s := cumprod( u_s ) ]
esc$hip_esc[ , v_s := 1 / u_s  ]

esc$hip_esc[ , u_p := i_p ]
esc$hip_esc[ t == 0, u_p := 0 ]
esc$hip_esc[ , u_p := 1 + u_p ]
esc$hip_esc[ , u_p := cumprod( u_p ) ]
esc$hip_esc[ , v_p := 1 / u_p  ]

esc$hip_esc[ , u_f := i_f ]
esc$hip_esc[ t == 0, u_f := 0 ]
esc$hip_esc[ , u_f := 1 + u_f ]
esc$hip_esc[ , u_f := cumprod( u_f ) ]
esc$hip_esc[ , v_f := 1 / u_f  ]

esc$hip_esc[ , u_m := i_m ]
esc$hip_esc[ t == 0, u_m := 0 ]
esc$hip_esc[ , u_m := 1 + u_m ]
esc$hip_esc[ , u_m := cumprod( u_m ) ]
esc$hip_esc[ , v_m := 1 / u_m  ]

esc$hip_esc[ , pen_min := pen_min * u_s ]

parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )

esc$k <- k
#source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )

}

source( 'R/SSC/314_analisis_sensibilidad_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Modificando la inflación médica -------------------------------------------------------------------
esc <- new.env()
do.call(file.remove, list( list.files( parametros$RData_seg, full.names = TRUE, pattern = "1_sens_med")))
esc$nombre <- 'escenario_1_sens_med'
message( '\t\t\t', esc$nombre )

esc$part_tasas_infl <- seq( 0.01, 0.045, by = 0.0025) 

for ( k in 1:length( esc$part_tasas_infl )) { # k <- 1
  message( '\t\t\t\t', 'parte ', k )
  
  # Patrimonio inicial
  esc$V0 <- 1776430561
  # esc$V0 <- 1066142805.89  # Fondo administrado por el  BIESS
  
  # Excedente Gasto administrativo inicial
  esc$exc_gast_adm <- 8261272.56
  
  # Rendimiento inicial de la reserva
  esc$interes_reserva <- 101021033.56
  
  # Hipótesis
  esc$hip_esc <- data.table( t = 0:parametros$horizonte,
                             
                             #Tasas
                             i_a = 0.0625,
                             i_r = Hipotesis[ 2,2 ]/100,
                             i_s = Hipotesis[ 3,2 ]/100,
                             i_f = Hipotesis[ 3,2 ]/100,
                             i_p = Hipotesis[ 3,2 ]/100,
                             i_d = 0.0625,
                             
                             # Tasa gastos médicos
                             i_m = esc$part_tasas_infl[ k ],
                             
                             # Gasto administrativo
                             por_gas_masa = 0.0005,
                             por_gas_apor = 0.03,
                             
                             # Aporte Estado
                             apo_est = 0.3339 * 0.40,
                             apo_est_rel_dep = 0.4933 * 0.003,
                             apo_est_fij = 0.5833 * 288000,
                             apo_est_cat = 0.0,
                             apo_est_pen_sal = 0.0,
                             
                             # Aportación SGO
                             apo_sgo = 0.007,
                             
                             #Aportación ISSFA e ISSPOL
                             apo_issfa = 0.007,
                             paga_issfa = 0,
                             apo_isspol = 1, # Las proyecciones corresponden al aporte al SSC
                             paga_isspol = 0,
                             
                             #Aportación de los seguros privado
                             por_sp = 1,
                             
                             # Montepios configuración
                             inc_montepio = 0,
                             por_pen_orfa = 0.20,
                             por_pen_cony = 0.40,
                             
                             # Auxilio de Funerales
                             inc_fun = 0,
                             ben_fun = 0.25,
                             
                             # Aporte jefes de familia
                             apo_ind = 0.025,
                             apo_inv = 0.001,
                             
                             bas_ref = 0.225,
                             reg_pen = 0.75,
                             pen_min = 100, 
                             
                             # Reconocimiento pagos de salud # estas variables al momento no se utilizan
                             por_pag_sal = 0,
                             por_pag_sal_cat = 0,
                             
                             # Factores calibración
                             cal_masa_dep = 1.0024,
                             cal_apo_ssc = 0.965,
                             cal_apo_sgo = 1.051,
                             cal_aux_fun = 0.93,
                             cal_pen_vej = 1.0,
                             cal_pen_inv = 1.0,
                             cal_pen_viu = 1.0,
                             cal_pen_hue = 1.0,
                             
                             cal_apo_est = 1.0,
                             cal_apo_est_rel_dep = 1.155,
                             cal_apo_est_fij = 1.0,
                             cal_apo_est_cat = 1.0,
                             cal_apo_est_pen_sal = 1.0,
                             
                             cal_gast_adm = 1.00,
                             cal_prom_gast_adm = 0.391923031602262,
                             cal_exc_gast_adm = 1 - 0.391923031602262, 
                             
                             cal_ben_sal = 1.0,
                             cal_l2_2021 = 1.0 )
  
  esc$hip_esc[ , apo_cot := ( apo_ind + apo_inv ) * bas_ref ]
  
  esc$hip_esc[ , u := i_a ]
  esc$hip_esc[ t == 0, u := 0 ]
  esc$hip_esc[ , u := 1 + u ]
  esc$hip_esc[ , u := cumprod( u ) ]
  esc$hip_esc[ , v := 1 / u  ]
  
  esc$hip_esc[ , u_s := i_s ]
  esc$hip_esc[ t == 0, u_s := 0 ]
  esc$hip_esc[ , u_s := 1 + u_s ]
  esc$hip_esc[ , u_s := cumprod( u_s ) ]
  esc$hip_esc[ , v_s := 1 / u_s  ]
  
  esc$hip_esc[ , u_p := i_p ]
  esc$hip_esc[ t == 0, u_p := 0 ]
  esc$hip_esc[ , u_p := 1 + u_p ]
  esc$hip_esc[ , u_p := cumprod( u_p ) ]
  esc$hip_esc[ , v_p := 1 / u_p  ]
  
  esc$hip_esc[ , u_f := i_f ]
  esc$hip_esc[ t == 0, u_f := 0 ]
  esc$hip_esc[ , u_f := 1 + u_f ]
  esc$hip_esc[ , u_f := cumprod( u_f ) ]
  esc$hip_esc[ , v_f := 1 / u_f  ]
  
  esc$hip_esc[ , u_m := i_m ]
  esc$hip_esc[ t == 0, u_m := 0 ]
  esc$hip_esc[ , u_m := 1 + u_m ]
  esc$hip_esc[ , u_m := cumprod( u_m ) ]
  esc$hip_esc[ , v_m := 1 / u_m  ]
  
  esc$hip_esc[ , pen_min := pen_min * u_s ]
  
  parametros_lista <- c( 'parametros', 'parametros_lista', 'esc', 'Hipotesis' )
  
  esc$k <- k
  #source( 'R/ssc/310_proyeccion_beneficios_ssc.R', encoding = 'UTF-8', echo = FALSE )
  source( 'R/SSC/310_calculo_balance_ssc.R', encoding = 'UTF-8', echo = FALSE )
  
}

source( 'R/SSC/315_analisis_sensibilidad_medica_ssc.R', encoding = 'UTF-8', echo = FALSE )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SSC_configuracion_', esc$nombre, '.RData' ) )
rm( esc )



# Cálculo de primas y análisis de ratios para todos los escenarios ---------------------------------
message( '\tCalculando primas y análisis de ratios' )
source( 'R/SSC/311_calculo_prima_ssc.R', encoding = 'UTF-8', echo = FALSE )
# source( 'R/ssc/313_analisis_ratios_ssc.R', encoding = 'UTF-8', echo = FALSE )

rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()


