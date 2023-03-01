message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_DES_balances_financieros.RData' ) )
message( '\tGraficando cuentas contables del Seguro de Desempleo' )
#1. Total de activos del Seguro de Desempleo--------------------------------------------------------
unidad<-1e6
aux<-activos

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(400000000, 1200000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_act_desempleo_des <- ggplot( data = aux ) + 
                              geom_line( aes( x = ano, 
                                              y = activo, 
                                              color = parametros$iess_blue ), 
                                         size = graf_line_size,
                                         lineend = "round" ) + 
                              labs( x = NULL, y = 'Millones  (USD)' ) +
                              scale_color_manual( values =  c( parametros$iess_green, 
                                                               parametros$iess_blue ), 
                                                  labels = c( '', '' ) ) +
                              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                              theme_bw() +
                              plt_theme +
                              theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_tot_act_desempleo_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_act_desempleo_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#1. 2. Gráfico de fondos disponibles----------------------------------------------------------------
unidad<-1e6
aux<-fondos_disponibles

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 60000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_fondos_disponibles_des<- ggplot( data = aux ) + 
                              geom_line( aes( x = ano, 
                                              y = fondos_disponibles, 
                                              color = parametros$iess_blue ), 
                                         size = graf_line_size,
                                         lineend = "round" ) + 
                              labs( x = NULL, y = 'Millones  (USD)' ) +
                              scale_color_manual( values =  c( parametros$iess_green, 
                                                               parametros$iess_blue ), 
                                                  labels = c( '', '' ) ) +
                              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                              theme_bw() +
                              plt_theme +
                              theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_fondos_disponibles_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_fondos_disponibles_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#1. 3. Gráfico de cuentas por cobrar----------------------------------------------------------------
unidad<-1e6
aux<-cuentas_cobrar

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 150000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_cobrar_des<- ggplot( data = aux ) + 
                          geom_line( aes( x = ano, 
                                          y = cuentas_por_cobrar, 
                                          color = parametros$iess_blue ), 
                                     size = graf_line_size,
                                     lineend = "round" ) + 
                          labs( x = NULL, y = 'Millones  (USD)' ) +
                          scale_color_manual( values =  c( parametros$iess_green, 
                                                           parametros$iess_blue ), 
                                              labels = c( '', '' ) ) +
                          scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                          scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                          theme_bw() +
                          plt_theme +
                          theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_cuentas_cobrar_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_cobrar_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. Gráfico del Pasivo---------------------------------------------------------------------
unidad<-1e6
aux<-pasivo

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 150000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivo_des<- ggplot( data = aux ) + 
                  geom_line( aes( x = ano, 
                                  y = pasivo, 
                                  color = parametros$iess_blue ), 
                             size = graf_line_size,
                             lineend = "round" ) + 
                  labs( x = NULL, y = 'Millones  (USD)' ) +
                  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                      labels = c( '', '' ) ) +
                  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                  theme_bw() +
                  plt_theme +
                  theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_pasivo_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivo_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#2. 1. Gráfico de las cuentas por pagar-------------------------------------------------------------
unidad<-1e6
aux<-cuentas_pagar

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 300000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cuentas_por_pagar_des <- ggplot( data = aux ) + 
                              geom_line( aes( x = ano, 
                                              y = cuentas_por_pagar, 
                                              color = parametros$iess_blue ), 
                                         size = graf_line_size,
                                         lineend = "round" ) + 
                              labs( x = NULL, y = 'USD' ) +
                              scale_color_manual( values =  c( parametros$iess_green, 
                                                               parametros$iess_blue ), 
                                                  labels = c( '', '' ) ) +
                              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                              theme_bw() +
                              plt_theme +
                              theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_cuentas_por_pagar_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_cuentas_por_pagar_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#2. 2. Gráfico del pasivo no corriente--------------------------------------------------------------
unidad<-1e6
aux<-pasivos_no_corrientes

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 150000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC(  y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pasivos_no_corrientes_des <- ggplot( data = aux ) + 
                                  geom_line( aes( x = ano, 
                                                  y = pasivos_no_corrientes, 
                                                  color = parametros$iess_blue ), 
                                             size = graf_line_size,
                                             lineend = "round" ) + 
                                  labs( x = NULL, y = 'Millones  (USD)' ) +
                                  scale_color_manual( values =  c( parametros$iess_green, 
                                                                   parametros$iess_blue ), 
                                                      labels = c( '', '' ) ) +
                                  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                                  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                                  theme_bw() +
                                  plt_theme +
                                  theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_pasivos_no_corrientes_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivos_no_corrientes_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. Gráfico del patrimonio-----------------------------------------------------------------------------
unidad<-1e6
aux<-patrimonio

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1000000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_patrimonio_des <- ggplot( data = aux ) + 
                        geom_line( aes( x = ano, 
                                        y = patrimonio, 
                                        color = parametros$iess_blue ), 
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        labs( x = NULL, y = 'Millones  (USD)' ) +
                        scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                            labels = c( '', '' ) ) +
                        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                        theme_bw() +
                        plt_theme +
                        theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_patrimonio_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#3. 1. Gráfico de lo fondos capitalizados-----------------------------------------------------------
unidad<-1e6
aux<-fondo_cap

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1000000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 8 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_fondo_cap_des <- ggplot( data = aux ) + 
                      geom_line( aes( x = ano, 
                                      y = fondo_cap, 
                                      color = parametros$iess_blue ), 
                                 size = graf_line_size,
                                 lineend = "round" ) + 
                      labs( x = NULL, y = 'Millones  (USD)' ) +
                      scale_color_manual( values =  c( parametros$iess_green, 
                                                       parametros$iess_blue ), 
                                          labels = c( '', '' ) ) +
                      scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                      scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                      theme_bw() +
                      plt_theme +
                      theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_fondo_cap_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_fondo_cap_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#3. 2. Gráfico del resultado del ejercicio actual---------------------------------------------------
unidad<-1e6
aux<-resultados

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 120000000, 300000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_resultados_des <-  ggplot( data = aux ) + 
                        geom_line( aes( x = ano, 
                                        y = resultados, 
                                        color = parametros$iess_blue ), 
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        labs( x = NULL, y = 'Millones  (USD)' ) +
                        scale_color_manual( values =  c( parametros$iess_green, 
                                                         parametros$iess_blue ), 
                                            labels = c( '', '' ) ) +
                        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                        theme_bw() +
                        plt_theme +
                        theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_resultados_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_resultados_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#4. Gráfico de los ingresos-----------------------------------------------------------------------------
unidad<-1e6
aux<-ingresos

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 160000000, 320000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ingresos_des <- ggplot( data = aux ) + 
                      geom_line( aes( x = ano, 
                                      y = ingresos, 
                                      color = parametros$iess_blue ), 
                                 size = graf_line_size,
                                 lineend = "round" ) + 
                      labs( x = NULL, y = 'Millones  (USD)' ) +
                      scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                          labels = c( '', '' ) ) +
                      scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                      scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                      theme_bw() +
                      plt_theme +
                      theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_ingresos_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_ingresos_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#5. Gráfico de los egresos--------------------------------------------------------------------------
unidad<-1e6
aux<-gastos

x_lim <- c( 2017, 2020 )
x_brk <- 2017:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 70000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_gastos_des <- ggplot( data = aux ) + 
                    geom_line( aes( x = ano, 
                                    y = gastos, 
                                    color = parametros$iess_blue ), 
                               size = graf_line_size,
                               lineend = "round" ) + 
                    labs( x = NULL, y = 'Millones  (USD)' ) +
                    scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                                        labels = c( '', '' ) ) +
                    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim) +
                    theme_bw() +
                    plt_theme +
                    theme( axis.text.x = element_text(angle = 0, hjust = 1 ) )

ggsave( plot = iess_gastos_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

###################################################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()