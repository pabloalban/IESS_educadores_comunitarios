message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tBalance actuarial' )

#Parámetros Escenario 1-------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

esc$V0 <- 114831905.00
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
                           por_apo_est = 0.00)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )


save( esc, file = paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', esc$nombre, '.RData' ) )
rm( esc )
#--------------------------------------------------------------------------------------------------------------------------
#Parámetros Escenario 2-------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_2'
message( '\t\t\t', esc$nombre )

esc$V0 <- 114831905.00
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
                           por_apo_est = 0.31)
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )


save( esc, file = paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', esc$nombre, '.RData' ) )
rm( esc )
#--------------------------------------------------------------------------------------------------------------------------
#Parámetros Escenario 3-------------------------------------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_3'
message( '\t\t\t', esc$nombre )

esc$V0 <- 114831905.00
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


save( esc, file = paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', esc$nombre, '.RData' ) )
rm( esc )









#Balances actuariales
escenarios_lista <- paste0('escenario_', 1:3 )

for( i in 1:length(escenarios_lista)) #i=2
{
  escenario <- escenarios_lista[i]
  load( file = paste0( parametros$RData_seg, 'IESS_TNRH_agregado_financiero_',escenario,'.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_TNRH_configuracion_', escenario, '.RData' ) )
  
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
  save( agregado_financiero,file = paste0( parametros$RData_seg, 'IESS_TNRH_balances_', esc$nombre, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
#rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()
