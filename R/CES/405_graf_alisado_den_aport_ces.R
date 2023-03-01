message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData' ) )

#Alisado de la densidad de aportación hombres ------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación hombres' )
densidad_cotizacion_int <- as_tibble(densidad_cotizacion_int)
aux<-densidad_cotizacion_int
aux_f <- aux %>%
  filter(genero == 'F' )

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

den_cot_f <-  ggplot( data = aux_f ) + 
  geom_point( aes( x = edad, y = den_cot, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = den_cot_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\phi_{1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = den_cot_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_den_cot_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de la densidad de aportación hombres ------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación hombres' )

aux<-densidad_cotizacion_int
aux_m <- aux %>%
  filter(genero == 'M' )

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


den_cot_m <-  ggplot( data = aux_f ) + 
  geom_point( aes( x = edad, y = den_cot, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = den_cot_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\phi_{2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = den_cot_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_den_cot_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# ------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()