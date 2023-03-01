message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tBalance actuarial' )

#Parámetros Escenario base-------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
                           #por_gast = 0.04,
                           # cal_aux_fun = 0.275,
                           # cal_mas = 1.0,
                           #cal_pen_vej = 1.105,
                           # cal_pen_inv = 0.70,
                           # cal_gast = 1.0 )


save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario pesimista-------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_2'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.0)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario alternativo-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_3'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.3133)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario reforma 1-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_4'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario reforma 2-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_5'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario reforma 3-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_6'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )


#Parámetros Escenario reforma 4-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_7'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#Parámetros Escenario reforma 5-------------------------------------------------------------------------------------

esc <- new.env()
esc$nombre <- 'escenario_8'
message( '\t\t\t', esc$nombre )

esc$V0 <- 7695229302.77
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis<-NULL

esc$apo_act <- data.table( t = 0:40,
                           i_a = 0.0625,
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           por_apo_est = 0.40)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', esc$nombre, '.RData' ) )
rm( esc )

#--------------------------------------------------------------------------------------------------------------------------
#Balances actuariales
escenarios_lista <- paste0('escenario_', 1:8 )

for( i in 1:length(escenarios_lista)) #i=2
{
escenario <- escenarios_lista[i]
load( file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_financiero_',escenario,'.RData' ) )
load( paste0( parametros$RData_seg, 'IESS_IVM_configuracion_', escenario, '.RData' ) )

agregado_financiero<- copy( agregado_financiero )

agregado_financiero[, ':='(t =esc$apo_act$t ,i_a = esc$apo_act$i_a )]
agregado_financiero[ , r := i_a ]
agregado_financiero[ t == 0, r := 0 ]
agregado_financiero[ t == 0, anio:=2020 ]
agregado_financiero[ , r := 1 + r ]
agregado_financiero[ , r := cumprod( r ) ]
agregado_financiero[ , v := 1 / r  ]
agregado_financiero[  ,V0:= esc$V0 ]
agregado_financiero[ t == 0, reserva := esc$V0 ]
#aportes activos 
agregado_financiero[ , apo_activos:= contribuciones]
#aporte estatal
agregado_financiero[ , apo_estatal:= Otros]
#aportes total
agregado_financiero[ , A := apo_activos+apo_estatal]
#aportes activos actuarial
agregado_financiero[ , apo_activos_vap:= cumsum( v * apo_activos)]
#aporte estatal actuarial
agregado_financiero[ ,apo_estatal_vap := cumsum( v * Otros) ]
#aporte total actuarial
agregado_financiero[t==40,aporte_tot_vap := apo_activos_vap + apo_estatal_vap  ]
#activo actuarial
agregado_financiero[t==40 ,activo_vap := aporte_tot_vap+esc$V0 ]
#----masa salarial valor actual-----
agregado_financiero[ ,masa_salarial_vap := cumsum( v * masa_salarial)]


#-------------------------------------------------------------------------
#beneficios actuarial
agregado_financiero[ ,beneficios_vejez_vap := cumsum( v * beneficios_vejez) ]
agregado_financiero[ ,beneficios_invalidez_vap := cumsum( v * beneficios_invalidez) ]
agregado_financiero[ ,beneficios_viudedad_vap := cumsum( v * beneficios_viudedad) ]
agregado_financiero[ ,beneficios_orfandad_vap := cumsum( v * beneficios_orfandad ) ]
agregado_financiero[ ,beneficios_aux_fun_vap := cumsum( v * gastos_otros  ) ] #auxilio de funerales
#beneficios totales
agregado_financiero[ , B := total2+gastos_otros]
#gastos administrativos actuarial
agregado_financiero[ ,gastos_administrativos_vap := cumsum( v * gastos_administrativos) ]
#beneficios totales actuarial
agregado_financiero[ , B_tot_vap :=cumsum( v * B)]

#pasivo actuarial
agregado_financiero[t==40 ,pasivo_vap := B_tot_vap+gastos_administrativos_vap ]
#balance_actuarial
agregado_financiero[t==40 , V := activo_vap-pasivo_vap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( agregado_financiero,file = paste0( parametros$RData_seg, 'IESS_IVM_balances_', esc$nombre, '.RData' ) )
}

#Escenarios de simulación de la sensibilidad para el escenario base --------------------------------
#Carga
load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros', 'hip_macro_resumen' ) ) ] )

Hipotesis <- copy( hip_macro_resumen )

esc <- new.env()
do.call(file.remove, list( list.files( parametros$RData_seg, full.names = TRUE, pattern = "1_sens_")))

esc$nombre <- 'escenario_1_sens'
message( '\t\t\t', esc$nombre )
escenario <- paste0('escenario_', 1 )

esc$part_tasas_des <- seq( 0.02, 0.085, by = 0.0025) 

for ( k in 1:length( esc$part_tasas_des ) ) { # k <- 1
message( '\t\t\t\t', 'parte ', k )

esc$V0 <- 7695229302.77

# esc$apo_act <- data.table( t = 0:60,
#                            i_d = esc$part_tasas_des[k],
#                            i_a = ( esc$part_tasas_des[k] + 1 )/( 1 + Hipotesis[ 6, 2 ]/100 ) - 1,
#                            por_apo_est = 0.40 )

load( file = paste0( parametros$RData_seg, 'IESS_IVM_agregado_financiero_',escenario,'.RData' ) )

agregado_financiero <- copy( agregado_financiero )
agregado_financiero[ , t:= seq( 0,40,1) ]
agregado_financiero[ , i_d := esc$part_tasas_des[k]]
agregado_financiero[ , i_a:= ( esc$part_tasas_des[k] + 1 )/( 1 + Hipotesis[ 6, 2 ]/100 ) - 1 ]
agregado_financiero[ , r := i_a ]
agregado_financiero[ t == 0, r := 0 ]
agregado_financiero[ t == 0, anio:=2020 ]
agregado_financiero[ , r := 1 + r ]
agregado_financiero[ , r := cumprod( r ) ]
agregado_financiero[ , v := 1 / r  ]
agregado_financiero[  ,V0:= esc$V0 ]
agregado_financiero[ t == 0, reserva := esc$V0 ]
#aportes activos 
agregado_financiero[ , apo_activos:= contribuciones]
#aporte estatal
agregado_financiero[ , apo_estatal:= Otros]
#aportes total
agregado_financiero[ , A := apo_activos+apo_estatal]
#aportes activos actuarial
agregado_financiero[ , apo_activos_vap:= cumsum( v * apo_activos)]
#aporte estatal actuarial
agregado_financiero[ ,apo_estatal_vap := cumsum( v * Otros) ]
#aporte total actuarial
agregado_financiero[ ,aporte_tot_vap := apo_activos_vap + apo_estatal_vap  ]
#activo actuarial
agregado_financiero[ ,activo_vap := aporte_tot_vap+esc$V0 ]
#----masa salarial valor actual-----
agregado_financiero[ ,masa_salarial_vap := cumsum( v * masa_salarial)]


#-------------------------------------------------------------------------
#beneficios actuarial
agregado_financiero[ ,beneficios_vejez_vap := cumsum( v * beneficios_vejez) ]
agregado_financiero[ ,beneficios_invalidez_vap := cumsum( v * beneficios_invalidez) ]
agregado_financiero[ ,beneficios_viudedad_vap := cumsum( v * beneficios_viudedad) ]
agregado_financiero[ ,beneficios_orfandad_vap := cumsum( v * beneficios_orfandad ) ]
agregado_financiero[ ,beneficios_aux_fun_vap := cumsum( v * gastos_otros  ) ] #auxilio de funerales
#beneficios totales
agregado_financiero[ , B := total2+gastos_otros]
#gastos administrativos actuarial
agregado_financiero[ ,gastos_administrativos_vap := cumsum( v * gastos_administrativos) ]
#beneficios totales actuarial
agregado_financiero[ , B_tot_vap :=cumsum( v * B)]

#pasivo actuarial
agregado_financiero[ , pasivo_vap := B_tot_vap+gastos_administrativos_vap ]
#balance_actuarial
agregado_financiero[ , V := activo_vap-pasivo_vap ]

# Guardando balances -------------------------------------------------------------------------------
message( '\tGuardando balances' )
save( agregado_financiero,file = paste0( parametros$RData_seg, 'IESS_IVM_balances_', esc$nombre, '_', k, '.RData' ) )
}

source( 'R/IVM/307_analisis_sensibilidad_ivm.R', encoding = 'UTF-8', echo = FALSE )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
