message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_contexto_economico.RData' ) )

# Graficando Evolución histórica de aportes solidarios del SGO al SSC ------------------------------
message( '\tGraficando Evolución histórica de aportes solidarios del SGO al SSC' )
unidad <- 1e6
aux <- copy(aporte_sgo[ , list(anio, aporte)] )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 80000000, 230000000)/unidad
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aporte_sgo_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = aporte/unidad, 
                  color = parametros$iess_green ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones de USD' ) +
  scale_color_manual( values =  c( parametros$iess_green ), 
                      labels = c( '') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ))

ggsave( plot = iess_aporte_sgo_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aporte_sgo_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución histórica de aportes de jefes de familia ------------------------------------
message( '\tGraficando Evolución histórica de aportes de jefes de familia ' )
unidad <- 1e6
aux <- copy( aporte_jefes[ , list(anio, aporte)] )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 3000000, 14000000)/unidad
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aporte_jefes_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = aporte/unidad, 
                  color = parametros$iess_green ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones de USD' ) +
  scale_color_manual( values =  c( parametros$iess_green ), 
                      labels = c( '') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ))

ggsave( plot = iess_aporte_jefes_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aporte_jefes_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución histórica de aportes del ISSFA e ISSPOL -------------------------------------
message( '\tGraficando Evolución histórica de aportes del ISSFA e ISSPOL' )
aux <- data.table(aporte_issfa_isspol)
aux[ , anio:= as.Date( anio ) ]

y_lim <- c( 0, 100000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aporte_issfa_isspol_ssc <- ggplot(aux, aes(anio)) + 
    geom_line(aes(y = issfa, colour ="issfa"),size = graf_line_size ) + 
    geom_line(aes(y = isspol, colour ="isspol"), size = graf_line_size ) +
    scale_x_date( date_breaks = "2 month", date_labels = "%b %Y", 
                  limits = c( min(aux$anio), max(aux$anio) ),expand=c(0,0)) +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
    scale_colour_manual(breaks = c("issfa", "isspol"), 
                      values = c("issfa" = parametros$iess_green , 
                                 "isspol" = parametros$iess_blue),
                      label =c("Aporte ISSFA", "Aporte ISSPOL"))+
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = 'USD' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ) )

ggsave( plot = iess_aporte_issfa_isspol_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aporte_issfa_isspol_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución histórica de la contribución de los Seguros Privados al SSC -----------------
message( '\tGraficando Evolución histórica de la contribución de los Seguros Privados al SSC ' )
unidad <- 1e6
aux <- copy(aporte_sp[ , list(anio, aporte)] )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 6000000, 20000000)/unidad
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aporte_sp_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = aporte/unidad, 
                  color = parametros$iess_green ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones de USD' ) +
  scale_color_manual( values =  c( parametros$iess_green ), 
                      labels = c( '') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ))

ggsave( plot = iess_aporte_sp_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aporte_sp_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución histórica de la contribución del 0,3% del Estado al SSC ---------------------
message( '\tGraficando Evolución histórica de la contribución del 0,3% del Estado al SSC' )
unidad <- 1e6
aux <- copy( aporte_est[ , list(anio, aporte)] )

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 40000000, 100000000)/unidad
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aporte_estado_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = aporte/unidad, 
                  color = parametros$iess_green ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Millones de USD' ) +
  scale_color_manual( values =  c( parametros$iess_green ), 
                      labels = c( '') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ))

ggsave( plot = iess_aporte_estado_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aporte_estado_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución histórica del crecimiento de las pensiones en el SSC ------------------------
message( '\tGraficando Evolución histórica del crecimiento de las pensiones en el SSC' )
aux <- copy(cre_pen[ , list(anio, pension)] )

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 110)
y_brk <- seq( y_lim[1], y_lim[2], 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cre_pension_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = pension, 
                  color = parametros$iess_green ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'USD' ) +
  scale_color_manual( values =  c( parametros$iess_green ), 
                      labels = c( '') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 ))

ggsave( plot = iess_cre_pension_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_cre_pension_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando tasas de crecimiento de aportes del SSC -----------------------------------------------
message( '\tGraficando tasas de crecimiento de aportes del SSC' )
aux <- copy(tasas_aporte)
aux[ , sgo:= 100*sgo ]
aux[ , jefes:= 100*jefes ]
aux[ , sp:= 100*sp ]
aux[ , estado:= 100*estado ]

x_lim <- c( 2011, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( -60, 160)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tasas_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, y = sgo, colour ='sgo' ), size = graf_line_size,lineend = "round" ) + 
  geom_line( aes( x = anio, y = jefes, colour ='jefes' ), size = graf_line_size,lineend = "round" ) +
  geom_line( aes( x = anio, y = sp, colour ='sp' ), size = graf_line_size,lineend = "round" ) +
  geom_line( aes( x = anio, y = estado, colour ='estado' ), size = graf_line_size,lineend = "round" ) +
  labs( x = 'Año', y = 'Porcentaje (\\%)' ) +
  scale_color_manual( breaks = c('sgo', 'jefes', 'sp', 'estado'),
                      values =  c( 'sgo'= parametros$iess_green, 
                                   'jefes'= parametros$iess_blue, 
                                   "sp" = "#F0E442",
                                   "estado" = "#D55E00"), 
                      labels = c( "Aporte Afiliados SGO", "Aporte Jefes de Familia",
                                  "Contribución Seguros Privados", "Contribución del Estado")
                      ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5 )) +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 2, override.aes = list(size = 4))) 

ggsave( plot = iess_tasas_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasas_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()