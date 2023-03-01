message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGraficando resultados del balance' )



# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

escenarios_lista <- paste0( 'escenario_', 1:3) 
for(i in 1:length(escenarios_lista))  #i<-1
{
  escenario <- escenarios_lista[i]
  load( paste0( parametros$RData_seg, 'IESS_TNRH_balances_', escenario, '.RData' ) )
  
  # grafico balance actuarial ----------------------------------------------------------------------
  x_lim <- c( parametros$anio_ini+1, parametros$anio_ini + parametros$horizonte )
  x_brk <- seq( x_lim[1], x_lim[2], 3 )
  x_lbl <- formatC( x_brk, digits = 0, format = 'f' )
  
  y_lim <- c( min( agregado_financiero$reserva ), max( agregado_financiero$reserva ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC(y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')
  
  
  plt_bal_act <- ggplot() +
    geom_line( data = agregado_financiero, aes( x = anio, y = reserva ), 
               size = graf_line_size, color = parametros$iess_blue ) +
    geom_hline( aes( yintercept = 0 ), size = 0.5*graf_line_size, color = parametros$iess_green, linetype = 2 ) +
    xlab( 'Año') +
    ylab( 'Reserva (millones)' ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl  ) +
    scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl  ) +
    theme_bw() +
    plt_theme
  
  # plt_bal_act
  
  ggsave( plot = plt_bal_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_tnrh_reserva_', escenario, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  #Aporte y beneficio
  
  y_lim <- c( min( agregado_financiero[ , list(contribuciones, total3)] ), max( agregado_financiero[ , list(contribuciones, total3)] ) )
  y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
  y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
  y_lbl <- formatC(y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')
  
  aux1 <- agregado_financiero[ , list(item = 'Aportes', t = anio, valor = contribuciones) ]
  aux2 <- agregado_financiero[ , list(item = 'Beneficios', t = anio, valor = total3) ] 
  
  aux <- rbind(aux1, aux2)
  plt_apo_ben_act <- ggplot( aux, aes( x = t, y = valor ) ) +
    geom_line( aes(colour = item), size = graf_line_size ) +
    xlab( 'Año' ) +
    ylab( 'Aportes y Beneficios Balance Corriente (millones)' ) +
    scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
    scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
    theme_bw() +
    scale_colour_manual( values = c('Aportes' = parametros$iess_blue,
                                    'Beneficios' = parametros$iess_green) ) +
    plt_theme_legend
  
  # plt_apo_ben_act
  ggsave( plot = plt_apo_ben_act, 
          filename = paste0( parametros$resultado_graficos, 'iess_tnrh_apo_ben_bal_corriente_', escenario, parametros$graf_ext ),
          width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
  
  
}



