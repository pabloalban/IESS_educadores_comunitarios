message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tBalance actuarial' )

#Parámetros Escenario base------------------------------------------------------
esc <- new.env()
esc$nombre <- 'escenario_1'
message( '\t\t\t', esc$nombre )

esc$V0 <- parametros$reserva_ini
esc$mont_prop_afi <- parametros$mont_prop_afi
esc$use_arit_mean <- FALSE
Hipotesis <- NULL

esc$apo_act <- data.table( t = 1:10,
                           i_a = 0.0625
                           # i_r = Hipotesis[ 4, 2 ],
                           # i_sbu = Hipotesis[ 5, 2 ],
                           # i_f = Hipotesis[ 7, 2 ],
                           # i_p = Hipotesis[ 7, 2 ],
                           # por_apo = c( 0.0766, 0.0886, 0.0986, rep( 0.1046, 38 ) ) + 0.001,
                           # por_apo_pen_vej = 0.0276,
                           # por_apo_pen_inv = 0.0276,
                           # por_apo_pen_mon = 0.0276,
                           # por_apo_est = 0.40
                           )
#por_gast = 0.04,
# cal_aux_fun = 0.275,
# cal_mas = 1.0,
#cal_pen_vej = 1.105,
# cal_pen_inv = 0.70,
# cal_gast = 1.0 )

parametros_lista <- c( 'parametros_lista', 'esc', 'Hipotesis' )

save( esc, file = paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', esc$nombre, '.RData' ) )
rm( esc )
# OTRAS CONFIGURACIONES PARA OTROS ESCENARIOS
#-------------------------------------------------------------------------------
# Balances actuariales
escenarios_lista <- paste0('escenario_', 1:1 )

for( i in 1:length(escenarios_lista)) # i=1
{
  escenario <- escenarios_lista[i]
  load( file = paste0( parametros$RData_seg, 'IESS_SAL_nominal_',escenario,'.RData' ) )
  load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario, '.RData' ) )
  
  agregado_financiero <- copy( agregado_financiero )
  
  agregado_financiero[, ':='(t = esc$apo_act$t ,i_a = esc$apo_act$i_a )]
  agregado_financiero[ , r := i_a ]
  agregado_financiero[ t == 0, r := 0 ]
  agregado_financiero[ t == 0, anio:=2020 ]
  agregado_financiero[ , r := 1 + r ]
  agregado_financiero[ , r := cumprod( r ) ]
  agregado_financiero[ , v := 1 / r  ]
  agregado_financiero[  , V0 := esc$V0 ]
  agregado_financiero[ t == 0, reserva := esc$V0 ]
  
  # Aportes activos 
  agregado_financiero[ , apo_activos := contributions ]
  # Aporte estatal
  agregado_financiero[ , apo_estatal := goverment_trans ]
  # Aporte para menores de 18
  agregado_financiero[ , apo_menores := aporte_menores_18 ]
  # Aporte extension cobertura
  agregado_financiero[ , apo_exten_cob := aporte_extension_cob ]
  # Aporte afiliados
  agregado_financiero[ , apo_afi :=  apo_activos + apo_menores + apo_exten_cob ]
  # Aportes total
  agregado_financiero[ , A := total ]
  
  # Aportes activos actuarial
  agregado_financiero[ , apo_activos_vap:= cumsum( v * apo_activos)]
  # Aporte estatal actuarial
  agregado_financiero[ , apo_estatal_vap := cumsum( v * apo_estatal ) ]
  # Aporte menores de 18 actuarial
  agregado_financiero[ , apo_menores_18_vap := cumsum( v * apo_menores ) ]
  # Aporte extension cobertura actuarial
  agregado_financiero[ , apo_exten_cob_vap := cumsum( v * apo_exten_cob ) ]
  # Aporte extension cobertura actuarial
  agregado_financiero[ , apo_afi_vap := cumsum( v * apo_afi ) ]
  
  # Aporte total actuarial (Activo actuarial)
  agregado_financiero[ , aporte_tot_vap := cumsum( v * A ) ]
  
  # Activo actuarial
  agregado_financiero[ , activo_vap := cumsum( v * A ) ]
  # Masa salarial valor actual
  agregado_financiero[ , masa_salarial_vap := cumsum( v * salary_mass ) ]
  
  
  #-------------------------------------------------------------------------
  # Beneficios actuarial
  agregado_financiero[ ,beneficios_medicos := cumsum( v * health ) ]
  agregado_financiero[ ,beneficios_pensionistas := cumsum( v * beneficio_pensionistas ) ]
  agregado_financiero[ ,beneficios_menores_18 := cumsum( v * beneficio_menores_18 ) ]
  agregado_financiero[ ,beneficios_exten_cob := cumsum( v * beneficio_extension_cob ) ]
  agregado_financiero[ ,beneficios_subsidios := cumsum( v * cash  ) ]
  # Beneficios totales
  agregado_financiero[ , B := total_b ]
  # Beneficios totales actuarial
  agregado_financiero[ , B_tot_vap :=cumsum( v * B) ]
  # Gastos administrativos actuarial
  agregado_financiero[ ,gastos_administrativos_vap := cumsum( v * admin ) ]
  # Gastos otro actuarial
  agregado_financiero[ ,other_vap := cumsum( v * other ) ]
  
  # Pasivo actuarial
  agregado_financiero[ , pasivo_vap := cumsum( v * total_t ) ]
  
  # Balance_actuarial
  agregado_financiero[ , V := cumsum( v * begin_year_reserve ) ]
  
  # Guardando balances ---------------------------------------------------------
  message( '\tGuardando balances' )
  save( agregado_financiero, 
        file = paste0( parametros$RData_seg, 'IESS_SAL_balances_', esc$nombre, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( parametros_lista, 'parametros' ) ) ] )
gc()
