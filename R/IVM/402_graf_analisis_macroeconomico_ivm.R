message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gr?fica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura variables macroeconómicas' )
load( file = paste0( parametros$RData_seg, 'IESS_IVM_variables_macroeconomicas.RData') )

#IPC---------------------------------------------------------------------------------

# aux <- IPCgraf_ivm
# scl <- 5
# aux[ , inf := inflacion * scl + 1 ]
# 
# iess_ipc_hist  <- ggplot( data = aux ) +
#   geom_line( aes( x = Anio, y = IPC, colour = "IPC" ), size = graf_line_size ) +
#   geom_line( aes( x = Anio, y = inf, colour = "Inflación" ), size = graf_line_size ) +
#   scale_colour_manual( "",
#                        breaks = c( "IPC", "Inflación" ),
#                        values = c( "IPC" = parametros$iess_green ,
#                                    "Inflación" = parametros$iess_blue ) ) +
#   theme_bw( ) +
#   plt_theme +
#   labs( x = 'Año', y = 'IPC') +
#   theme( legend.position = "bottom",
#          axis.text.x = element_text( angle = 90, hjust = 1 ) )
# 
# ggsave( plot = iess_ipc_hist,
#         filename = paste0( parametros$resultado_graficos, 'iess_ipc_hist', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#SBU---------------------------------------------------------------------------------

aux<-copy(SBU_ivm[, .(Anio, SBU)] )

x_lim <- c( 2002, 2021)
x_brk <- seq( x_lim[1], x_lim[2], 1)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 100, 410 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sbu <- ggplot( data = aux ) + 
  geom_line( aes(x = Anio,
                 y = SBU,
            color = parametros$iess_green),
            size = graf_line_size ) + 
  labs( x = 'Año', y = 'Salario Básico Unificado (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_sbu, 
        filename = paste0( parametros$resultado_graficos, 'iess_sbu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# SALARIO PROMEDIO -----------------------
aux <- copy( Salprom_ivm [, .(Anio, Salario_declarado_promedio)] )

x_lim <- c( 2010, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( 500, 800 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_spd_hist <- ggplot( data = aux, 
                         aes( x = Anio, y = Salario_declarado_promedio ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Salario Promedio Anual (USD)' ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_spd_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_spd_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#TASA DEL PIB -------------------------------------------------------

aux <- crec_PIB_ivm

x_lim <- c( 1960, 2020 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

y_lim <- c( -0.05, 0.20)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 9 )
y_lbl <- paste0( formatC( 100 * y_brk, digits = 1, format = 'f', big.mark = '.', decimal.mark = ',' ), '%' )

iess_pib_hist <- ggplot( data = aux) +
  geom_line( aes( x = Anio, y = CrecimientoPIB, colour = "Crecimiento real del PIB" ), group = 1, size = graf_line_size ) +
  theme_bw( ) +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )

 ggsave( plot = iess_pib_hist,
         filename = paste0( parametros$resultado_graficos, 'iess_pib_hist', parametros$graf_ext ),
         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# EVOLUCIÓN Y COMPORTAMIENTO DEL PIB---------------------------------------------------------------------------------
aux <- PIBvsIPC_ivm
scl <- 1
aux[ , inf := inflacion * scl + 1 ]

iess_ipcpib_hist  <- ggplot( data = aux ) +
  geom_line( aes( x = Anio, y = CrecimientoPIB, colour = "Crecimiento real del PIB" ), size = graf_line_size ) +
  geom_line( aes( x = Anio, y = inf, colour = "Inflación Acumulada Anual (%)" ), size = graf_line_size ) +
  scale_colour_manual( "",
                       breaks = c( "Crecimiento real del PIB", "Inflación Acumulada Anual (%)" ),
                       values = c( "Crecimiento real del PIB" = parametros$iess_green ,
                                   "Inflación Acumulada Anual (%)" = parametros$iess_blue ) ) +
  theme_bw( ) +
  plt_theme +
  labs( x = 'Año', y = 'Crecimiento real del PIB') +
  theme( legend.position = "bottom",
         axis.text.x = element_text( angle = 90, hjust = 1 ) )

ggsave( plot = iess_ipcpib_hist,
        filename = paste0( parametros$resultado_graficos, 'iess_ipcpib_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

