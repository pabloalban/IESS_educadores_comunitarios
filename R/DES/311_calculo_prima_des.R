message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCálculo de la prima' )

escenarios <- paste0( 'escenario_', 1:1 )

for ( escenario in escenarios ) {
  message( '\tCálculo de la prima para el ', escenario )
  load( paste0( parametros$RData_seg, 'IESS_DES_balances_', escenario, '.RData' ) )
  
  prima <- balance_anual[ t > 0, list( t, M, A, A_vap, B, B_vap, G, 
                                       M_vap, G_vap, V, V0 ) ]
  
  # Prima de reparto puro --------------------------------------------------------------------------
  prima[ , pri_rep_pur_apo := ( B + G - esc$gamma * V0 ) /  M ]
  
  # Prima nivelada en cada horizonte ---------------------------------------------------------------
  prima[ , pri_med_niv_apo := ( B_vap + G_vap - esc$gamma * V0 ) / M_vap ] 
  
  
  save( prima, 
        file = paste0( parametros$RData_seg, 'IESS_DES_primas_', escenario, '.RData' ) )  
}

# Limpiando memoria RAM-----------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
