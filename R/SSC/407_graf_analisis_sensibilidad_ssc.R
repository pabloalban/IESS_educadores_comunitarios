message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad.RData' ) )

aux <- copy( sensi )

x_lim <- c( min(aux$escenarios_part), max(aux$escenarios_part))
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- paste0( format( x_brk * 100 , digits = 2, nsmall = 2, decimal.mark = "," ), '%')

y_lim <- c( min(aux$balance ), max( aux$balance) )
y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
y_lbl <- formatC(y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')


plt_sensibilidad <- ggplot( aux, aes( x = escenarios_part , y =  balance ) ) +
  geom_line( size = graf_line_size, aes(colour = 'Balance') ) +
  xlab( 'Tasa de descuento' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  theme_bw() +
  scale_colour_manual( "",  breaks = c( 'Balance'),
                       values = c( 'Balance' = parametros$iess_blue) ) +
  plt_theme_legend 

ggsave( plot = plt_sensibilidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_sensibilidad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Simulación del balance actuarial tras la modificación de la tasa de descuento---------------------
message( '\t\tSimulación del balance actuarial tras la modificación de la tasa de descuento' )
aux <- as.data.table( copy( bal_cum ) )
aux[ , t:= t + 2020]

num_anios <- length( colnames(aux)[-1] )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( parametros$anio , parametros$anio+ parametros$horizonte  )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( min( aux[ , -1 ] ), max( aux[ , -1 ]  ) )/1e6
y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
y_lbl <- formatC(y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')

aux <- as.data.table( melt( aux,  id.vars='t', variable.name = 'part', value.name = 'balance') )

#aux[, part:=as.character( part)]
aux[, part1:=as.numeric( part  )] 
aux[, part:=as.numeric( as.character( part) )]

plt_bal_tasa_des <- ggplot( data = aux, aes( x=t, y=balance/1e6, group=part, colour=part1) ) + 
  geom_line()+
  scale_colour_gradientn( NULL,colors = cols_graf, breaks = seq(min(aux$part1),max(aux$part1), length.out=6),
                          labels = paste0(format( seq(min(aux$part),max(aux$part), length.out=6) * 100, digits = 2, nsmall = 2, 
                                           decimal.mark = ',' ), '%') )+
 
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'Millones de USD' ) + xlab('Año') +
  geom_hline( aes( yintercept = 0 ), size = 0.5*graf_line_size, color = parametros$iess_green, linetype = 2 ) +
  theme_bw()  + plt_theme_legend +
  guides(color = guide_colorbar(barheight = 7, label.theme = element_text( size = 5.5,  angle = 0, family = tipo_letra),
                                ticks.colour = "white", ticks=F,
                                frame.colour = "black", frame.linewidth = 1,
                                label.position = "left" ))+
  theme(legend.position = 'right',legend.direction = "vertical")

ggsave( plot = plt_bal_tasa_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_bal_tasa_des_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Graficando análisis de sensbilidad para la inflación médica ---------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_sensibilidad_medica.RData' ) )

aux <- copy(sensi)

x_lim <- c( min(aux$escenarios_part), max(aux$escenarios_part))
x_brk <- seq( x_lim[1], x_lim[2], length.out = 5 )
x_lbl <- paste0( format( x_brk * 100 , digits = 2, nsmall = 2, decimal.mark = "," ), '%')

y_lim <- c( min(aux$balance ), max( aux$balance) )
y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
y_lbl <- formatC(y_brk/1e6, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')

plt_sensibilidad <- ggplot( aux, aes( x = escenarios_part , y =  balance ) ) +
  geom_line( size = graf_line_size, aes(colour = 'Balance') ) +
  xlab( 'Inflación médica' ) +
  ylab( 'Millones de USD' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl ) +
  scale_x_continuous( limits = x_lim, breaks = x_brk, labels = x_lbl ) +
  theme_bw() +
  scale_colour_manual( "",  breaks = c( 'Balance'),
                       values = c( 'Balance' = parametros$iess_blue) ) +
  plt_theme_legend 

ggsave( plot = plt_sensibilidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_sensibilidad_medica', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Simulación del balance actuarial tras la modificación de la tasa de inflación médica -------------
message( '\t\tSimulación del balance actuarial tras la modificación de la tasa de inflación médica' )
aux <- as.data.table( copy( bal_cum ) )
aux[ , t:= t + 2020]

num_anios <- length( colnames(aux)[-1] )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( parametros$anio , parametros$anio + parametros$horizonte )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( min( aux[ , -1 ] ), max( aux[ , -1 ]  ) )/1e6
y_brk <- unique( pretty( seq(y_lim[1], y_lim[2], length.out = 5) ) )
y_lim <- c( min(y_brk), max(y_brk) ) # redefiniendo limites por razones estéticas
y_lbl <- formatC(y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')


aux <- as.data.table( melt( aux,  id.vars='t', variable.name = 'part', value.name = 'balance') )

#aux[, part:=as.character( part)]
aux[, part1:=as.numeric( part  )] 
aux[, part:=as.numeric( as.character( part) )]


plt_bal_inf_med<- ggplot( data = aux, aes( x=t, y=balance/1e6, group=part, colour=part1) ) + 
  geom_line()+
  scale_colour_gradientn( NULL,colors = cols_graf, breaks = seq(min(aux$part1),max(aux$part1), length.out=6),
                          labels = paste0(format( seq(min(aux$part),max(aux$part), length.out=6) * 100, digits = 2, nsmall = 2, 
                                                  decimal.mark = ',' ), '%') )+
  
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'Millones de USD' ) + xlab('Año') +
  geom_hline( aes( yintercept = 0 ), size = 0.5*graf_line_size, color = parametros$iess_green, linetype = 2 ) +
  theme_bw()  + plt_theme_legend +
  guides(color = guide_colorbar(barheight = 7, label.theme = element_text( size = 5.5,  angle = 0, family = tipo_letra),
                                ticks.colour = "white", ticks=F,
                                frame.colour = "black", frame.linewidth = 1,
                                label.position = "left" ))+
  theme(legend.position = 'right',legend.direction = "vertical")

ggsave( plot = plt_bal_inf_med, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_bal_inf_med_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()