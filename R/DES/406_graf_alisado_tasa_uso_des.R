message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_DES_tasa_uso_edad_sexo_int.RData' ) )

#Gráfico del alisado de tasa de uso del seguro hombres ---------------------------------------------
aux1<-tasa_siniestralidad %>% filter(edad<96)
a<-(0)
b<-0.5
c<-(0.5)
d<-(0)

#Pago 1 HOmbres-------------------------------------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación hombres' )
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='1' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_1_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{1,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_1_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p1_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 2 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='2' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.04 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_2_m <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{2,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_2_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p2_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 3 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='3' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_3_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{3,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_3_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p3_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 4 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='4' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.03 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_4_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{4,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ), limits = y_lim  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_4_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p4_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pago 5 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='5' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.03 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_5_m <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{5,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ), limits = y_lim  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(plot.margin = unit(c(a,b,c,d), "lines"))


ggsave( plot = tasa_uso_5_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p5_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(tasa_uso_1_m,
              tasa_uso_2_m,
              ncol = 2, nrow = 1)

g2<-ggarrange(
  tasa_uso_3_m,
  tasa_uso_4_m,
  tasa_uso_5_m, ncol = 3)

g<- ggarrange(g1,g2, nrow = 2)
g

ggsave( plot = g, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_m', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )

#Gráfico del alisado de tasa de uso del seguro mujeres ---------------------------------------------
aux1<-tasa_siniestralidad %>% filter(edad<96)

#Pago 1 mujer-------------------------------------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación mujeres' )
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='1' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_1_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{1,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_1_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p1_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 2 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='2' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_2_f <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{2,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_2_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p2_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 3 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='3' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_3_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{3,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_3_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p3_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 4 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='4' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_4_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{4,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_4_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p4_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pago 5 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='5' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_5_f <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{5,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(plot.margin = unit(c(a,b,c,d), "lines"))


ggsave( plot = tasa_uso_5_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p5_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(tasa_uso_1_f,
              tasa_uso_2_f,
              ncol = 2, nrow = 1)

g2<-ggarrange(
  tasa_uso_3_f,
  tasa_uso_4_f,
  tasa_uso_5_f, ncol = 3)

g<- ggarrange(g1,g2, nrow = 2)

ggsave( plot = g, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_f', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )


###Gráfico del alisado de tasa de uso con pagos indebidos hombres ---------------------------------------------
aux1<-tasa_siniestralidad_pagos_ind %>% filter(edad<96)
a<-(0)
b<-0.5
c<-(0.5)
d<-(0)

#Pago 1 HOmbres-------------------------------------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación hombres con pagos indebidos' )
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='1' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_1_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{1,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_1_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p1_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 2 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='2' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.04 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_2_m <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{2,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_2_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p2_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 3 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='3' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.05 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_3_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{3,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_3_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p3_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 4 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='4' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.03 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_4_m <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{4,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ), limits = y_lim  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_4_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p4_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pago 5 HOmbres-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'M', sd_numero_pagos =='5' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.03 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_5_m <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$hombres \\, \\, p_{5,2,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ), limits = y_lim  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(plot.margin = unit(c(a,b,c,d), "lines"))


ggsave( plot = tasa_uso_5_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p5_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(tasa_uso_1_m,
              tasa_uso_2_m,
              ncol = 2, nrow = 1)

g2<-ggarrange(
  tasa_uso_3_m,
  tasa_uso_4_m,
  tasa_uso_5_m, ncol = 3)

g<- ggarrange(g1,g2, nrow = 2)

ggsave( plot = g, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_m_ind', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )

#Gráfico del alisado de tasa de uso del seguro mujeres ---------------------------------------------
aux1<-tasa_siniestralidad_pagos_ind %>% filter(edad<96)

#Pago 1 mujer-------------------------------------------------------------------------------------
message( '\tGraficando alisado de la densidad de aportación mujeres con pagos indebidos' )
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='1' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_1_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{1,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_1_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p1_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 2 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='2' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_2_f <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{2,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_2_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p2_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 3 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='3' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_3_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{3,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_3_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p3_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pago 4 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='4' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_4_f <-  ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{4,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme+
  theme(plot.margin = unit(c(a,b,c,d), "lines"))

ggsave( plot = tasa_uso_4_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p4_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pago 5 mujer-------------------------------------------------------------------------------------
aux<-aux1
aux <- aux %>%
  filter(sd_genero == 'F', sd_numero_pagos =='5' )

x_lim <- c( 15, 95 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- x_brk

y_lim <- c( 0, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )


tasa_uso_5_f <- ggplot( data = aux ) + 
  geom_point( aes( x = edad, y = tasa_uso, color = parametros$iess_green ), size = graf_point_size ) + 
  geom_line( aes( x = edad, y = tasa_uso_int, color = parametros$iess_blue ), size = graf_line_size ) + 
  xlab(TeX("edad $x$"))+
  ylab(TeX("$mujeres \\, \\, p_{5,1,x}$")) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( 'alisado', 'estimado' ) ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) , limits = y_lim) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(plot.margin = unit(c(a,b,c,d), "lines"))


ggsave( plot = tasa_uso_5_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_p5_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Agrupar en solo gráfico----------------------------------------------------------------------------
g1<-ggarrange(tasa_uso_1_f,
              tasa_uso_2_f,
              ncol = 2, nrow = 1)

g2<-ggarrange(
  tasa_uso_3_f,
  tasa_uso_4_f,
  tasa_uso_5_f, ncol = 3)

g<- ggarrange(g1,g2, nrow = 2)

ggsave( plot = g, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_uso_f_ind', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )
# ------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()