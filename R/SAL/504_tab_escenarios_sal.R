message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de parámetros escenarios' )

#Función de tildes a latex--------------------------------------------------------------------------
source( 'R/500_tildes_a_latex.R', encoding = 'UTF-8', echo = FALSE )

n <- 4
# if ( exists( 'RData_rev', envir = parametros ) ) {
#   n <- 5
# }
escenario <- paste0( 'escenario_',1:n )
nombres <- c( 'Base', 'Pesimista', 'Legal', 'Alternativo' )
var_nom <- c( 'Tasa actuarial $i_a$',
              'Tasa crecimiento salarios $i_r$',
              'Tasa crecimiento salario b\\\'{a}sico unificado $i_s$',
              'Tasa de inflación general $d$',
              'Tasa de inflación de costos de salud $i_m$',
              'Porcentaje aporte estatal $\\alpha_{est}$',
              # 'Porcentaje máximo gasto administrativo',
              'Aporte para menores de 18 años')

aux_tot <- NULL
for( i in 1:length( escenario ) ) {
  load( paste0( parametros$RData_seg, 'IESS_SAL_configuracion_', escenario[i], '.RData' ) )
  
  aux <- data.table( nom = var_nom,
                     val = c( esc$i_a, esc$i_r,
                              esc$i_sbu,
                              esc$i_f, 
                              esc$i_m, 
                              esc$aporte_estado, 
                              # esc$gadmin, 
                              esc$apo_act$por_apo_men_18[1] ) )
  aux[ , val := 100 * val ]
  
  aux_tot <- cbind( aux_tot, aux[ , list( val ) ] )        
  
  xtb_aux <- xtable( aux, digits = c( 0, 0, 2 ) )
  xtb_aux <- tildes_a_latex(xtb_aux)
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
if ( exists( 'RData_rev', envir = parametros ) ) {
  aux_tot[, escenario_5 := NULL ]
}
aux_tot <- aux_tot[ , c( 3, 1, 2 ), with = FALSE ]
xtb_aux_tot <- xtable( aux_tot, digits = c( 0, 0, 2, 2 ) )
xtb_aux_tot <- tildes_a_latex(xtb_aux_tot)
print( xtb_aux_tot,
       file = paste0( parametros$resultado_tablas, 'iess_tab_conf_escenarios.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL, sanitize.text.function = identity )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
