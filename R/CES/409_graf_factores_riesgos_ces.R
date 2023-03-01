message( paste( rep('-', 100 ), collapse = '' ) )

# Cargando información -----------------------------------------------------------------------------
message("\tCargando datos")
load( paste0( parametros$RData_seg, 'IESS_CES_f_i.RData' ) )
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_ces_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_jub_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_deb_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_p_des_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_f_i.RData" ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Calculando los cotizantes y beneficiarios del seguro de cesantía --------------------------------
message( '\tGenerando tablas de las tasas siniestralidad' )
ts <- full_join( p_ces, p_deb, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_des, by = c( 'x', 'sexo' ) ) %>%
  full_join(., p_jub, by = c( 'x', 'sexo' ) ) %>%
  full_join(., densidad_cotizacion_int, by = c( 'x'='edad', 'sexo'='genero' ) ) %>%
  select( x, sexo, 
          phi_int := den_cot_int, 
          p9_int := p_ces_int, 
          p10_int := log_p_jub_int, 
          p11_int := p_deb_int,
          p12_int := p_des_int,
          phi := den_cot,  
          p9 := p_ces,
          p10 := log_p_jub,
          p11 := p_deb,
          p12 := p_des)


#Alisado de p9 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p9 hombres' )
aux <- ts %>% filter( sexo == 'M')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p9_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p9, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p9_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\p^{9}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_p9_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de p9 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p9 mujeres' )
aux <- ts %>% filter( sexo == 'F')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p9_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p9, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p9_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\p^{9}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p9_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_p9_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de p10 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p9 hombres' )
aux <- ts %>% filter( sexo == 'M')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -1.5, 0 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p10_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p10, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p10_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,p^{10}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p10_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_p10_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de p10 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p10 mujeres' )
aux <- ts %>% filter( sexo == 'F')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -1.5, 0 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p10_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p10, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p10_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,p^{10}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p10_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_p10_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Alisado de p11 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p11 hombres' )
aux <- ts %>% filter( sexo == 'M')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.75 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p11_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p11, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p11_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\p^{11}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p11_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_p11_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de p11 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p11 mujeres' )
aux <- ts %>% filter( sexo == 'F')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.75 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

p11_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p9, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p9_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\p^{11}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p11_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_p11_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Alisado de p12 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p12 hombres' )
aux <- ts %>% filter( sexo == 'M')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.01 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' )

p12_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p12, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p12_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\p^{12}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p12_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_p12_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de p9 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la p12 mujeres' )
aux <- ts %>% filter( sexo == 'F')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.01 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' )

p12_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = p12, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = p12_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\p^{12}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = p12_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_p12_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(p10_f,
              p9_f,
              p10_m,
              p9_m,
              ncol = 2, nrow = 2)

ggsave( plot = g1, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_siniestralidad_1', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )


g2<-ggarrange(p12_f,
              p11_f,
              p12_m,
              p11_m,
              ncol = 2, nrow = 2)

ggsave( plot = g2, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_siniestralidad_2', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )


#Alisado de f9 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f9 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '9')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f9_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{9}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f9_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f9 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f9 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '9')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f9_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{9}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f9_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f9_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f10 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f10 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '10')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f10_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{10}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f10_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f10_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f10 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f10 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '10')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f10_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{10}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f10_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f10_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f11 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f11 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '11')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f11_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{11}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f11_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f11_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f11 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f11 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '11')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f11_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{11}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f11_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f11_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Alisado de f12 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f12 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '12')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f12_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{12}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f12_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f12_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f12 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f12 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '12')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f12_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{12}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f12_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f12_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Alisado de f13 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f13 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '13')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f13_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{13}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f13_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f13_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f13 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f13 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '13')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f13_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{13}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f13_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f13_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Alisado de f14 hombres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f14 hombres' )
aux <- f_i_int %>% filter( sexo == 'M', i == '14')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f14_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i , color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, \\log\\,f^{14}_{t,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f14_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_f14_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Alisado de f14 mujeres -----------------------------------------------------------------------------
message( '\tGraficando alisado de la f14 mujeres' )
aux <- f_i_int %>% filter( sexo == 'F', i == '14')

x_lim <- c( 15, 115 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( -4, 3 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

f14_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = x, y = log_f_i, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = x, y = log_f_i_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, \\log\\,f^{14}_{t,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme

ggsave( plot = f14_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_f14_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(f9_f,
              f10_f,
              f11_f,
              f9_m,
              f10_m,
              f11_m,
              ncol = 3, nrow = 2)

ggsave( plot = g1, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_f_i_1', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )


g2<-ggarrange(f12_f,
              f13_f,
              f14_f,
              f12_m,
              f13_m,
              f14_m,
              ncol = 3, nrow = 2)

ggsave( plot = g2, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_f_i_2', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )


# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()