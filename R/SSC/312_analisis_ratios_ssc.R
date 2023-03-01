message( paste( rep('-', 100 ), collapse = '' ) )

load( paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )

# Borrando variables, solo quedan variables a ser utilizadas
rm( list = ls()[ !( ls() %in% c( 'parametros', 'balance', 'balance_anual', 'crecimiento_pib_anual',
                                 'sbu_proy') ) ] )

message( '\tAnálsis de ratios para SSC' )

pib <- as.data.table( crecimiento_pib_anual )
pib <- pib[ , list( t = anio, pib_tasa = crec_pib ) ]
pib <- pib[ t > parametros$anio_ini ]
pib[ , t := t - parametros$anio_ini ]
setorder( pib, t )
pib[ , pib := cumprod( 1 + pib_tasa ) * 108398e6 ]


escenarios <- paste0( 'escenario_', 1:4 )  #Poner más escenarios en 304 para  ssc

for ( escenario in escenarios ) {
  message( '\tAnálisis ratios para el ', escenario )
  
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', escenario, '.RData' ) )
  load( paste0( parametros$RData, 'IESS_tasas_macro_predicciones.RData' ) )
  
  ratios <- balance[ t > 2020, list( l2 = sum( l2 ), 
                                  l_pen = sum( l3 + l4 ),
                                  l3 = sum( l3 ),
                                  l4 = sum( l4 ),
                                  l6 = parametros$mont_prop_afi * sum( l3 + l4 ) ), by = list( t ) ]
  ratios <- merge( ratios, 
                   balance_anual[ t > 0, list( t, M, A_est, B_pen, B3, B4, B6 ) ],
                   by = c( 't' ) )
  
  ratios <- merge( ratios, 
                   tasas_macro_pred[ , list( t, sbu )],
                   by = c( 't' ) )
  
  ratios <- merge( ratios, pib, by = c( 't' ) )
  ratios[ , sal_mean := M / ( l2_cot * 12 )  ]
  ratios[ , pen_mean := ( ( B3 + B4 + B6 ) / ( l3 + l4 + l6 ) - sbu ) / 13 ]
  ratios[ , dep_tasa := l2_cot / ( l3 + l4 + l6 ) ]
  ratios[ , rem_tasa := pen_mean / sal_mean ]
  ratios[ , pen_tasa := shift( pen_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , pen_tasa := ( pen_mean - pen_tasa ) / pen_tasa ]
  ratios[ t == 1, pen_tasa := 0 ]
  ratios[ , sal_tasa := shift( sal_mean, 1, type = 'lag', fill = 0 ) ]
  ratios[ , sal_tasa := ( sal_mean - sal_tasa ) / sal_tasa ]
  ratios[ t == 1, sal_tasa := 0 ]
  ratios[ , mas_tasa := shift( M, 1, type = 'lag', fill = 0 ) ]
  ratios[ , mas_tasa := ( M - mas_tasa ) / mas_tasa ]
  ratios[ t == 1, mas_tasa := 0 ]
  ratios[ , ae_pib_tasa := A_est / pib ]
  
  
  save( ratios, 
        file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_ratios_', escenario, '.RData' ) )
}

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
