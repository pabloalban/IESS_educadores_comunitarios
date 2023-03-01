message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando resultados del análisis de sensibilidad para el escenario 1' )

escenarios_part <- esc$part_tasas_des 
resu <- NULL
val_root <- NULL
bal_cum <- data.frame( t = 0:parametros$horizonte )
bal_cum_sua <- data.frame( t = 0:parametros$horizonte )
col_nams <- c('t')
for ( i in 1:length( escenarios_part ) ) { # i<-1
  load( paste0( parametros$RData_seg, 'IESS_IVM_balances_', esc$nombre, '_', i ,'.RData' ) )
  
  resu <- rbind( resu, 
                 agregado_financiero[ t == 60, 
                                list( balance = V,
                                      ultimo = ifelse( (max( which( agregado_financiero$V > 0 ) ) + parametros$anio - 1)!=2060,
                                                       max( which( agregado_financiero$V > 0 ) ) + parametros$anio - 1, 
                                                       paste0("superior a ", (max( which( agregado_financiero$V > 0 ) ) + parametros$anio - 1))  ) ) ] )
  aux <- agregado_financiero[ , list( t, V )]
  bal_cum <- cbind( bal_cum, aux$V)
  col_nams <- c( col_nams, as.character( escenarios_part[i ] ) )

  
}  

colnames( bal_cum ) <- col_nams

sensi <- resu
sensi <- cbind(sensi, escenarios_part)

lista <- c('bal_cum')
save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_sensibilidad.RData' ) )

