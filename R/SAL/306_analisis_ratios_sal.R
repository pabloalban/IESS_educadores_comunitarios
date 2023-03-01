message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tAnálsis de ratios para SAL' )

escenarios <- paste0( 'escenario_', 1:4 )

for ( escenario in escenarios ) { # escenario <- escenarios[1]
  message( '\tAnálisis ratios para el ', escenario )
  
  load( paste0( parametros$RData_seg, 'IESS_SAL_balances_', escenario, '.RData' ) )
  
  ratios <- aportes[ t > 0, list( l2 = sum( l2 ),
                                  l3 = sum( l3 ),
                                  l4 = sum( l4 ),
                                  l6 = sum( l6 ),
                                  l7 = sum( l7 ),
                                  l8 = sum( l8 ) ), by = list( t ) ]
  ratios <- merge( ratios, 
                   balance_anual[ t > 0, list( t, M, A, A_est, B2, B3, B4, B6, B7, B8 ) ],
                   by = c( 't' ) )
  
  ratios[ , sal_mean := M / ( l2 * 12 )  ]
  ratios[ , dep_tasa := l2 / ( l3 + l4 + l6 + l7 + l8 ) ]
  ratios[ , dep_ben_tasa := B7 / M ]
  
  save( ratios, 
        file = paste0( parametros$RData_seg, 'IESS_SAL_analisis_ratios_', escenario, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
