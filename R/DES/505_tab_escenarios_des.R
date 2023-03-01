message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )


#Tabla de las hipotesis utilizadas en cada escenario------------------------------------------------ 
escenario <- paste0( 'escenario_',1:1 )
nombres <- c( 'Escenario 1' )
var_nom <- c( 'Tasa actuarial $i_a$',
              'Aporte de afiliados $\\pi^{2,\\;cot\\;des}$',
              'Tasa crecimiento salarios $i_r$',
              'Tasa crecimiento salario b\\\'{a}sico unificado $i_s$',
              'Porcentaje gasto administrativo',
              'Acepta aportes extempor\\\'{a}neos',
              'Tabla de siniestralidad')

aux_tot <- NULL
for( i in 1:length( escenario ) ){
  load( paste0( parametros$RData_seg, 'IESS_DES_configuracion_', escenario[i], '.RData' ) )
  
  aux <- data.table( nom = var_nom,
                     val = c( paste0(formatC(esc$i_a*100,decimal.mark = ",", format='f',digits = 2),"\\%"),
                              paste0(formatC(as.numeric(esc$apo_act[1,2])*100,decimal.mark = ",", format='f',digits = 2),"\\%"),
                              paste0(formatC(esc$i_r*100,decimal.mark = ",", format='f',digits = 4),"\\%"),
                              paste0(formatC(esc$i_sbu*100,decimal.mark = ",", format='f',digits = 4),"\\%"),
                              paste0(formatC(esc$porcentaje_gasto *100,decimal.mark = ",", format='f',digits = 2),"\\%"),
                              ifelse(i==1,'No',ifelse(i==2,'Si','No')),
                              ifelse(i==1,'Tabla 9.1',ifelse(i==2,'Tabla 9.2','Tabla 9.1')) 
                              ))
  aux_tot <- cbind( aux_tot, aux[ , list( val ) ] )
  xtb_aux <- xtable( aux, digits = c( 0, 0, 4 ) )
  print( xtb_aux,
         file = paste0( parametros$resultado_tablas, 'iess_tab_conf_', escenario[i], '.tex' ),
         type = 'latex', 
         include.colnames = FALSE, include.rownames = FALSE, 
         format.args = list( decimal.mark = ',', big.mark = '.' ), 
         only.contents = TRUE, 
         hline.after = NULL, sanitize.text.function = identity )
}

setnames( aux_tot, escenario )
aux_tot[ , nom := var_nom ]
aux_tot <- aux_tot[ , c( 2, 1), with = FALSE ]
xtb_aux_tot <- xtable( aux_tot, digits = c( 0, 0, 4 ) )

print( xtb_aux_tot,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_escenarios.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = nrow(aux),
       sanitize.text.function = identity )

#Tabla de las hipótesis utilizadas------------------------------------------------------------------
# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
message( '\tGenerando tablas de la evolución de las hipótesis macroeconómicas' )
# Tabla resumen de hipótesis macro -----------------------------------------------------------------
var_nom <- c( 'Tasa activa referencial', 
              'Tasa pasiva referencial', 
              'Tasa actuarial',
              'Tasa variaci\\\'{o}n salarial', 
              'Tasa variaci\\\'{o}n SBU',
              'Tasa variaci\\\'{o}n PIB', 
              'Tasa inflaci\\\'{o}n')
aux <- data.table( nom = var_nom,
                   val = c( paste0(formatC(Hipotesis$Proyeccion[1]*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(Hipotesis$Proyeccion[2]*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(esc$i_a*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(Hipotesis$Proyeccion[4]*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(Hipotesis$Proyeccion[5]*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(Hipotesis$Proyeccion[6]*100,decimal.mark = ",", format='f',digits = 3),"\\%"),
                            paste0(formatC(Hipotesis$Proyeccion[7]*100,decimal.mark = ",", format='f',digits = 3),"\\%") 
                   ))
xtb_aux <- xtable( aux, digits = c( 0, 0, 3 ) )


print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_hipotesis_macro.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )
#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
