message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando resultados del análisis de sensibilidad para el escenario 1' )

escenarios_part <- esc$part_tasas_des 
resu <- NULL
val_root <- NULL
bal_cum <- data.frame( t = 0:parametros$horizonte )
bal_cum_sua <- data.frame( t = 0:parametros$horizonte )
col_nams <- c('t')
for ( i in 1:length( escenarios_part ) ) { # i<-1
  load( paste0( parametros$RData_seg, 'IESS_SSC_balances_', esc$nombre, '_', i ,'.RData' ) )
  
  resu <- rbind( resu, 
                 balance_anual[ t == parametros$horizonte, 
                                list( balance = V,
                                      ultimo = ifelse( (max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1)!=parametros$anio_fin,
                                                        max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1, 
                                                        paste0("superior a ", (max( which( balance_anual$V_cap > 0 ) ) + parametros$anio - 1))  ) ) ] )
  aux <- balance_anual[ , list( t, V, V_cap )]
  bal_cum <- cbind( bal_cum, aux$V)
  col_nams <- c( col_nams, as.character( escenarios_part[i ] ) )
  
  fit3 <- lm ( V_cap ~ poly( t, 3, raw = T), data = aux )
  est_3 <- function( x ){
    fit3$coefficients[[1]] + fit3$coefficients[[2]]*x + fit3$coefficients[[3]]*x^2 + fit3$coefficients[[4]]*x^3
  }
  
  # curve( est_3(x), 0, 20)
  # abline(h = 0, lty = 3)
  # plot( x, y, col="red")
  # points(aux$t, aux$V_cap)
  bal_cum_sua <- cbind( bal_cum_sua, est_3( 0:parametros$horizonte ) )
  val_root <- c( val_root,ifelse( resu$balance[i] < 0 & balance_anual$V0[1] > 0, 
                      format( uniroot(est_3, c( 0, parametros$horizonte ))$root,
                              digits = 4, nsmall = 4, decimal.mark = "," ),
                      paste0( "mayor a ", parametros$horizonte ) ) )

}  

colnames( bal_cum ) <- col_nams
colnames( bal_cum_sua ) <- col_nams

sensi <- cbind(resu, raiz=val_root, tasa=format( escenarios_part *100, digits = 2, nsmall=2, decimal.mark = ",") )
sensi <- sensi[ , list( tasa, balance, raiz, ultimo)]
sensi[, ultimo:=as.character( ultimo )]

sensi <- cbind(sensi, escenarios_part)

lista <- c('sensi', 'bal_cum', 'bal_cum_sua')
save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad.RData' ) )

