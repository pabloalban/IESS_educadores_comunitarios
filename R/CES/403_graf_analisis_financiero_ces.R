message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
file_balances <- paste0( parametros$RData_seg, 'IESS_CES_balances_financieros.RData' )
load( file = file_balances )
message( '\tGraficando cuentas contables del Seguro de Cesantía' )
#1. Total de activos del Seguro de Cesantía--------------------------------------------------------
unidad<-1e6
aux<-activos

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(3000000000, 9000000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_act_ces <- ggplot( data = aux ) + 
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

ggsave( plot = iess_tot_act_ces, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_act_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#2. Total de pasivos del Seguro de Cesantía---------------------------------------------------------
unidad<-1e6
aux<-pasivo

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(3000000000, 8200000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_pas_ces <- ggplot( data = aux ) + 
                      geom_line( aes( x = ano, 
                                      y = pasivo, 
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
                    
ggsave( plot = iess_tot_pas_ces, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_pas_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#3. Total de patrimonio del Seguro de Cesantía---------------------------------------------------------
unidad<-1e6
aux<-patrimonio

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(100000000, 700000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_pat_ces <- ggplot( data = aux ) + 
                    geom_line( aes( x = ano, 
                                    y = patrimonio, 
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

ggsave( plot = iess_tot_pat_ces, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_pat_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#4. Total de ingresos del Seguro de Cesantía--------------------------------------------------------
unidad<-1e6
aux<-ingresos

x_lim <- c( 2013, 2020 )
x_brk <- 2013:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(100000000, 800000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_ing_ces <- ggplot( data = aux ) + 
                    geom_line( aes( x = ano, 
                                    y = ingresos, 
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

ggsave( plot = iess_tot_ing_ces, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_ing_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#5. Total de gastos del Seguro de Cesantía----------------------------------------------------------
unidad<-1e6
aux<-gastos

x_lim <- c( 2013, 2020 )
x_brk <- 2013:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c(0, 150000000)
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/unidad, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tot_gast_ces <-  ggplot( data = aux ) + 
                      geom_line( aes( x = ano, 
                                      y = gastos, 
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

ggsave( plot = iess_tot_gast_ces, 
        filename = paste0( parametros$resultado_graficos, 'iess_tot_gast_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

###################################################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()