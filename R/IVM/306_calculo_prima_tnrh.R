message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

#N <- 1:3
#if ( parametros$cal_add_esc )
#{
#  N <- 1:11
#}

escenarios_lista <- paste0('escenario_', 1:3)

for (i in 1:length(escenarios_lista)) #i=2
{
  message( '\tCálculo de la prima para el ',escenarios_lista[i] )
  load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenarios_lista[i], '.RData' ) )
  
  prima <- agregado_financiero[ t > 0, list( t, v, masa_salarial_vap, pasivo_vap,V0, apo_estatal_vap ) ]
  
  # Porcentaje de contribución por décimos
  #  delta <- 1.0 # delta = 100% de las décimas no está a cargo de los cotizantes
  
  # Prima de reparto puro ----------------------------------------------------------------------------
  #  prima[ , pri_rep_pur := ( B + G ) /  M ] # sin aporte estatal
  #  prima[ , pri_rep_pur_apo_est := ( B + G - A_est ) /  M ] # con aporte estatal AE
  #  prima[ , pri_rep_pur_delta := ( B + G - delta * B_dec - A_est ) / M ] 
  #  prima[ , pri_rep_pur_delta_pen := delta * B_dec / M ]
  
  # Prima nivelada en cada horizonte -----------------------------------------------------------------
  #  prima[ , pri_med_niv := ( B_vap + G_vap - V0 ) /  M_vap ] # sin aporte estatal
  prima[t==40 , pri_med_niv := ( pasivo_vap-apo_estatal_vap-V0 ) /  masa_salarial_vap ] # sin aporte estatal  
  #  prima[ , pri_med_niv_apo_est_pen := ( B_vap + G_vap - A_est_vap - V0 ) /  M_vap ] # con aporte estatal AE
  #  prima[ , pri_med_niv_delta := ( B_vap + G_vap - delta * B_dec_vap - A_est_vap - V0 ) / M_vap ] # esta prima financia las pensiones sin décimas
  #  prima[ , pri_med_niv_delta_pen := delta * B_dec_vap / M_vap ]
  
  save( prima, 
        file = paste0( parametros$RData_seg, 'IESS_TNRH_primas_', escenarios_lista[i], '.RData' ) )  
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

#---------------------------------------------------------------------------------------------------
# cargar resultados
# escenario <- 'escenario_7' 
# load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', escenario, '.RData' ) ) 
# load( paste0( parametros$RData_seg, 'IESS_IVM_primas_', escenario, '.RData' ) )
