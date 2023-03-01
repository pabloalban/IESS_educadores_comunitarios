message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_DES_proyeccion_poblacion.RData' ) )

#Predicciones de los beneficiarios por pago en cada año de valuación--------------------------------
message( '\tGraficando los beneficios futuros por pago')

pob_proy_edad_sexo<-as.data.table(pob_proy_edad_sexo)
aux_f <- pob_proy_edad_sexo[ sexo == 'F', list( t = t+parametros$anio_ini, 
                                                l1_f = lp1, l2_f = lp2, l3_f = lp3, l4_f = lp4, l5_f = lp5 ) ]
aux_f<-aux_f[, .( l1_f = sum( l1_f ) ,
                  l2_f = sum( l2_f ),
                  l3_f = sum( l3_f ),
                  l4_f = sum( l4_f ),
                  l5_f = sum( l5_f )),by = .( t )]
aux_f<-melt(aux_f, id.vars = c("t"),
            measure.vars = c("l5_f","l4_f","l3_f", "l2_f","l1_f"),
            variable.name = "pago",
            value.name = "beneficiarios")

aux_m <- pob_proy_edad_sexo[ sexo == 'M', list( t = t+parametros$anio_ini, 
                                                l1_m = lp1, l2_m = lp2, l3_m = lp3, l4_m = lp4, l5_m = lp5 ) ]
aux_m<-aux_m[, .( l1_m = sum( l1_m ) ,
                  l2_m = sum( l2_m ),
                  l3_m = sum( l3_m ),
                  l4_m = sum( l4_m ),
                  l5_m = sum( l5_m )),by = .( t )]

aux_m<-melt(aux_m, id.vars = c("t"),
            measure.vars = c("l5_m","l4_m","l3_m", "l2_m","l1_m"),
            variable.name = "pago",
            value.name = "beneficiarios")
#Mujeres--------------------------------------------------------------------------------------------
x_lim <- c( 2019, 2058 )
x_brk <- seq(2019,2058,3)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 82000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_proy_benf_f<-ggplot(aux_f, aes(x=t, y=beneficiarios, fill=pago)) + 
                  geom_area(alpha=0.9 , size=0.1, colour="grey") +
                  scale_fill_brewer(palette= "BuGn",
                                    labels = c("Pago 5", "Pago 4", "Pago 3", "Pago 2", "Pago 1"))+
                  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                  theme_bw() +
                  plt_theme +
                  theme(legend.position="bottom") +
                  ylab(TeX("$Pagos\\; de \\; prestaciones \\; \\; mujeres \\; \\; l_{i,t,1}$")) +
                  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_proy_benf_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_benf_f', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Hombres--------------------------------------------------------------------------------------------

x_lim <- c( 2019, 2058 )
x_brk <- seq(2019,2058,3)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 140000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_benf_m<-ggplot(aux_m, aes(x=t, y=beneficiarios, fill=pago)) + 
                  geom_area(alpha=0.9 , size=0.1, colour="grey") +
                  scale_fill_brewer(palette= "BuGn",
                                    labels = c("Pago 5", "Pago 4", "Pago 3", "Pago 2", "Pago 1"))+
                  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                  theme_bw() +
                  plt_theme +
                  theme(legend.position="bottom") +
                  ylab(TeX("$Pagos  \\; de\\; prestaciones \\; hombres  \\; \\; l_{i,t,2}$")) +
                  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_proy_benf_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_benf_m', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Predicciones de los beneficiarios por pago en cada año de valuación, con pagos indebidos-----------
message( '\tGraficando los beneficios futuros por pago, con pagos indebidos')
pob_proy_edad_sexo_pagos_ind<-as.data.table(pob_proy_edad_sexo_pagos_ind)
aux_f <- pob_proy_edad_sexo_pagos_ind[ sexo == 'F', list( t = t+parametros$anio_ini, 
                                                l1_f = lp1, l2_f = lp2, l3_f = lp3, l4_f = lp4, l5_f = lp5 ) ]
aux_f<-aux_f[, .( l1_f = sum( l1_f ) ,
                  l2_f = sum( l2_f ),
                  l3_f = sum( l3_f ),
                  l4_f = sum( l4_f ),
                  l5_f = sum( l5_f )),by = .( t )]
aux_f<-melt(aux_f, id.vars = c("t"),
            measure.vars = c("l5_f","l4_f","l3_f", "l2_f","l1_f"),
            variable.name = "pago",
            value.name = "beneficiarios")

aux_m <- pob_proy_edad_sexo_pagos_ind[ sexo == 'M', list( t = t+parametros$anio_ini, 
                                                l1_m = lp1, l2_m = lp2, l3_m = lp3, l4_m = lp4, l5_m = lp5 ) ]
aux_m<-aux_m[, .( l1_m = sum( l1_m ) ,
                  l2_m = sum( l2_m ),
                  l3_m = sum( l3_m ),
                  l4_m = sum( l4_m ),
                  l5_m = sum( l5_m )),by = .( t )]

aux_m<-melt(aux_m, id.vars = c("t"),
            measure.vars = c("l5_m","l4_m","l3_m", "l2_m","l1_m"),
            variable.name = "pago",
            value.name = "beneficiarios")
#Mujeres--------------------------------------------------------------------------------------------
x_lim <- c( 2019, 2058 )
x_brk <- seq(2019,2058,3)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 85000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_proy_benf_f_ind<-ggplot(aux_f, aes(x=t, y=beneficiarios, fill=pago)) + 
                      geom_area(alpha=0.9 , size=0.1, colour="grey") +
                      scale_fill_brewer(palette= "BuGn",
                                        labels = c("Pago 5", "Pago 4", "Pago 3", "Pago 2", "Pago 1"))+
                      scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                      scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                      theme_bw() +
                      plt_theme +
                      theme(legend.position="bottom") +
                      ylab(TeX("$Pagos\\; de \\; prestaciones \\; \\; mujeres \\; \\; l_{i,t,1}$")) +
                      theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_proy_benf_f_ind, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_benf_f_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
#Hombres--------------------------------------------------------------------------------------------
x_lim <- c( 2019, 2058 )
x_brk <- seq(2019,2058,3)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 150000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_benf_m_ind<-ggplot(aux_m, aes(x=t, y=beneficiarios, fill=pago)) + 
                      geom_area(alpha=0.9 , size=0.1, colour="grey") +
                      scale_fill_brewer(palette= "BuGn",
                                        labels = c("Pago 5", "Pago 4", "Pago 3", "Pago 2", "Pago 1"))+
                      scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                      scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                      theme_bw() +
                      plt_theme +
                      theme(legend.position="bottom") +
                      ylab(TeX("$Pagos  \\; de\\; prestaciones \\; hombres  \\; \\; l_{i,t,2}$")) +
                      theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_proy_benf_m_ind, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_benf_m_ind', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()