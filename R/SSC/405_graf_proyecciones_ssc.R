message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_indicadores.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_outputs_modelo_ilo_pensions_ssc.RData' ) )

# Graficando tasa de cobertura del SSC -------------------------------------------------------------
message( '\tGraficando tasa de cobertura del SSC' )
aux <- copy( indicadores )

x_lim <- c( 2012, 2040 )
x_brk <- seq( 2012, 2040, by = 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 30)
y_brk <- seq( y_lim[1], y_lim[2],by=5 )
y_lbl <- paste0( formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                 '%')

iess_tc_ssc <- ggplot( data = aux ) +  
  geom_line( aes( x = t, 
                  y = tc, colour = "Tasa" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Tasa"),
                       values = c( "Tasa" = parametros$iess_total), 
                       label = c('Tasa'='Tasa de Cobertura') ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Porcentaje' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_tc_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_tc_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando carga pensional -----------------------------------------------------------------------
message( '\tGraficando carga pensional' )
aux <- copy( indicadores )

x_lim <- c( 2012, 2040 )
x_brk <- 2012:2040
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 12)
y_brk <- seq( y_lim[1], y_lim[2],by = 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_cp_ssc <- ggplot( aux ) +  
  geom_line( aes( x = t, 
                  y = cpm, colour = "Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = t, 
                  y = cpf, colour = "Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = t, 
                  y = cpt, colour = "Total" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Total" ,"Masculino","Femenino"),
                       values = c( "Masculino" = parametros$iess_blue,
                                   "Femenino" =  parametros$iess_green,
                                   "Total" = parametros$iess_total) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = NULL) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_cp_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_cp_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia activos e inactivos  sexo masculino-----------------------------------
message( '\tGraficando jefes de familia activos e inactivos' )

aux <- copy( acum_dem )
x_lim <- c( 2011.5, 2040.5 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 700000)
y_brk <- seq( y_lim[1], y_lim[2], by = 100000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_act_inac_male <- ggplot( ) +
  geom_bar( data= aux[sexo=='Male' & t!=2020],
            aes( x = t, y = l2, fill = 'activos' ), width=0.5, position = "dodge", stat = "identity" ) +
  geom_area( data = aux[sexo=='Male' & t!=2020], aes( x = t, y=l2_inac, fill = "inactivos"), alpha=0.5)  +
  geom_bar( data= aux[sexo=='Male' & t==2020],
            aes( x = t, y = l2 ), width=0.5, position = "dodge", stat = "identity", fill="deepskyblue3") +
  geom_bar( data = aux[sexo=='Male' & t==2020], aes( x = t, y=l2_inac),
            position = "dodge", stat = "identity", fill="chartreuse4", width=1) +
  geom_point( data = aux[, list(afi = l2+l2_inac), by=list(t, sexo)][sexo=='Male'],
              aes( x = t, y = afi, colour = 'Total' ),
              size=3, shape=95) +
  scale_fill_manual( "",
                     breaks = c("activos", "inactivos"),
                     values = c( 
                       "activos" = parametros$iess_blue,
                       "inactivos" = parametros$iess_green),
                     label = c(
                       "activos"= "Activos",
                       "inactivos"= "Inactivos")) +
  scale_colour_manual( "",
                       breaks = c("Total"),
                       values = c("Total" = parametros$iess_total),
                       label = c("Total"='Total Jefes Activos+Inactivos'))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Jefes de Familia' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"),
         legend.box.spacing=  unit('0.75', 'cm'))

ggsave( plot = iess_act_inac_male,
        filename = paste0( parametros$resultado_graficos, 'iess_act_inac_male', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia activos e inactivos  sexo femenino -----------------------------------

aux <- copy( acum_dem )
x_lim <- c( 2011.5, 2040.5 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 350000)
y_brk <- seq( y_lim[1], y_lim[2], by = 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_act_inac_female <- ggplot( ) +
  geom_bar( data= aux[sexo=='Female' & t!=2020],
            aes( x = t, y = l2, fill = 'activos' ), width=0.5, position = "dodge", stat = "identity" ) +
  geom_area( data = aux[sexo=='Female' & t!=2020], aes( x = t, y=l2_inac, fill = "inactivos"), alpha=0.5)+
  geom_bar( data= aux[sexo=='Female' & t==2020],
            aes( x = t, y = l2 ), width=0.5, position = "dodge", stat = "identity", fill="deepskyblue3") +
  geom_bar( data = aux[sexo=='Female' & t==2020], aes( x = t, y=l2_inac),
            position = "dodge", stat = "identity", fill="chartreuse4", width=1) +
  geom_point( data = aux[, list(afi = l2+l2_inac), by=list(t, sexo)][sexo=='Female'],
              aes( x = t, y = afi, colour = 'Total' ),
              size=3, shape=95) +
  scale_fill_manual( "",
                     breaks = c("activos", "inactivos"),
                     values = c( 
                       "activos" = parametros$iess_blue,
                       "inactivos" = parametros$iess_green),
                     label = c(
                       "activos"= "Activas",
                       "inactivos"= "Inactivas")) +
  scale_colour_manual( "",
                       breaks = c("Total"),
                       values = c("Total" = parametros$iess_total),
                       label = c("Total"='Total Jefas Activas+Inactivas'))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Jefas de Familia' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"),
         legend.box.spacing=  unit('0.75', 'cm'))

ggsave( plot = iess_act_inac_female,
        filename = paste0( parametros$resultado_graficos, 'iess_act_inac_female', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando fuerza laboral y total jefes de familia -----------------------------------------------
message( '\tGraficando fuerza laboral y total jefes de familia' )
aux <- copy( acum_dem )

aux <- aux[, list( l1=sum(l1,na.rm=T), l2=sum(l2, na.rm=T) ), by=t]

x_lim <- c( 2011.5, 2040.5)
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 4000000)
y_brk <- seq( y_lim[1], y_lim[2],by = 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

# y_lab <- seq(0,4,1)
# 
# ymax <- min(ceil(aux$l1/1000000))
# linea <- (aux$l2/aux$l1)
# seg_eje <- paste0(seq(0,100,5),'%')

  iess_act_lf <- ggplot( ) +
    geom_bar( data= aux[ t!=2020],
              aes( x = t, y = l1, fill = 'LF' ), width=0.8, position = "dodge", stat = "identity" ) +
    geom_area( data = aux[t!=2020], aes( x = t, y=l2, fill = "activos"), alpha=0.5) +
    geom_bar( data= aux[t==2020],
              aes( x = t, y = l1 ), width=0.8, position = "dodge", stat = "identity", fill="chartreuse4") +
    geom_bar( data = aux[ t==2020], aes( x = t, y=l2),
              position = "dodge", stat = "identity", fill="deepskyblue3", width=1) +
    labs(  x = 'Año', y = 'Personas' ) +
    scale_fill_manual( "",
                       breaks = c("activos", "LF"),
                       values = c(
                         "activos" = parametros$iess_blue,
                         "LF" = parametros$iess_green),
                       label = c(
                         "activos"= "Activos",
                         "LF"= "Fuerza Laboral Empl.")) +
    geom_line( aes( x=aux$t, y = (aux$l2/aux$l1)*(3200000/20)*100, linetype = 'Activos/Fuerza Laboral Empl.' ), size = 1.3) +
    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim )  +
    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim, 
                        sec.axis = sec_axis(~.*(20/3200000), name='Activos/Fuerza Laboral Empleada (%)',
                                           labels = function(b){paste0(round(b*1,0))})) +
  theme_bw() +
  plt_theme +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ),
         legend.box.spacing = unit(0.8, 'cm'))

ggsave( plot = iess_act_lf,
        filename = paste0( parametros$resultado_graficos, 'iess_act_lf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando Evolución de los Pensionistas de vejez ------------------------------------------------
message( '\tGraficando Evolución de los Pensionistas de vejez' )
aux <- copy( acum_dem )

x_lim <- c( 2012, 2040 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 250000)
y_brk <- seq( y_lim[1], y_lim[2], by = 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen_vej <- ggplot(  ) +
  geom_line( data= aux[ sexo=='Male'],
             aes( x = t, y = l3, colour = "Masculino" ), size = graf_line_size  ) +
  geom_line( data= aux[ sexo=='Female'],
             aes( x = t, y = l3, colour = "Femenino" ), size = graf_line_size  ) +
  geom_area( data = aux[ , list( pen=sum(l3, na.rm=T)), by=t ],
             aes(x = t, y = pen, fill = "Total"), alpha=0.5) +
  scale_fill_manual( "",
                     breaks = c("Total"),
                     values = c( "Total" = '#69b3a2'),
                     label = c("Total" = 'Total')) +
  scale_colour_manual( "",
                       breaks = c("Masculino", "Femenino"),
                       values = c("Masculino" = parametros$iess_blue,
                                  "Femenino" = parametros$iess_green),
                       label = c("Masculino"='Masculino',
                                 "Femenino" = "Femenino"))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Pensionistas de Vejez' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_pen_vej,
        filename = paste0( parametros$resultado_graficos, 'iess_pen_vej', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución de los Pensionistas de invalidez---------------------------------------------
message( '\tGraficando Evolución de los Pensionistas de invalidez' )
aux <- copy( acum_dem )

x_lim <- c( 2012, 2040 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 700)
y_brk <- seq( y_lim[1], y_lim[2], by = 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen_inv <- ggplot(  ) +
  geom_line( data= aux[ sexo=='Male'],
             aes( x = t, y = l4, colour = "Masculino" ), size = graf_line_size  ) +
  geom_line( data= aux[ sexo=='Female'],
             aes( x = t, y = l4, colour = "Femenino" ), size = graf_line_size  ) +
  geom_area( data = aux[ , list( pen=sum(l4, na.rm=T)), by=t ],
             aes(x = t, y = pen, fill = "Total"), alpha=0.5) +
  scale_fill_manual( "",
                     breaks = c("Total"),
                     values = c( "Total" = '#69b3a2'),
                     label = c("Total" = 'Total')) +
  scale_colour_manual( "",
                       breaks = c("Masculino", "Femenino"),
                       values = c("Masculino" = parametros$iess_blue,
                                  "Femenino" = parametros$iess_green),
                       label = c("Masculino"='Masculino',
                                 "Femenino" = "Femenino"))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Pensionistas de Invalidez' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_pen_inv,
        filename = paste0( parametros$resultado_graficos, 'iess_pen_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución de los Pensionistas de viudez -----------------------------------------------
message( '\tGraficando Evolución de los Pensionistas de viudez ' )
aux <- copy( acum_dem )

x_lim <- c( 2012, 2040 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 70000)
y_brk <- seq( y_lim[1], y_lim[2], by = 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen_viu <- ggplot(  ) +
  geom_line( data= aux[ sexo=='Male'],
             aes( x = t, y = l9, colour = "Masculino" ), size = graf_line_size  ) +
  geom_line( data= aux[ sexo=='Female'],
             aes( x = t, y = l9, colour = "Femenino" ), size = graf_line_size  ) +
  geom_area( data = aux[ , list( pen=sum(l9, na.rm=T)), by=t ],
             aes(x = t, y = pen, fill = "Total"), alpha=0.5) +
  scale_fill_manual( "",
                     breaks = c("Total"),
                     values = c( "Total" = '#69b3a2'),
                     label = c("Total" = 'Total')) +
  scale_colour_manual( "",
                       breaks = c("Masculino", "Femenino"),
                       values = c("Masculino" = parametros$iess_blue,
                                  "Femenino" = parametros$iess_green),
                       label = c("Masculino"='Masculino',
                                 "Femenino" = "Femenino"))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Pensionistas de Viudedad' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_pen_viu,
        filename = paste0( parametros$resultado_graficos, 'iess_pen_viu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Evolución de los Pensionistas de orfandad-----------------------------------------------
message( '\tGraficando Evolución de los Pensionistas de orfandad ' )
aux <- copy( acum_dem )

x_lim <- c( 2012, 2040 )
x_brk <- seq( 2012, 2040, 2)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 6000)
y_brk <- seq( y_lim[1], y_lim[2], by = 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pen_orf <- ggplot(  ) +
  geom_line( data= aux[ sexo=='Male'],
             aes( x = t, y = l8, colour = "Masculino" ), size = graf_line_size  ) +
  geom_line( data= aux[ sexo=='Female'],
             aes( x = t, y = l8, colour = "Femenino" ), size = graf_line_size  ) +
  geom_area( data = aux[ , list( pen=sum(l8, na.rm=T)), by=t ],
             aes(x = t, y = pen, fill = "Total"), alpha=0.5) +
  scale_fill_manual( "",
                     breaks = c("Total"),
                     values = c( "Total" = '#69b3a2'),
                     label = c("Total" = 'Total')) +
  scale_colour_manual( "",
                       breaks = c("Masculino", "Femenino"),
                       values = c("Masculino" = parametros$iess_blue,
                                  "Femenino" = parametros$iess_green),
                       label = c("Masculino"='Masculino',
                                 "Femenino" = "Femenino"))+
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Pensionistas de Orfandad' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_pen_orf,
        filename = paste0( parametros$resultado_graficos, 'iess_pen_orf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando Nuevos pensionistas del SSC -----------------------------------------------------------
message( '\tGraficando nuevos pensionistas del SSC ' )
aux <- copy( new_acum_dem )
aux <- aux[ , list( l23=sum(l23, na.rm=T), l24=sum(l24, na.rm=T), l8=sum(l8, na.rm=T),
                    l9=sum(l9, na.rm=T) ), by = t ]
aux[ , t_new:= l23 + l24 + l8 + l9 ]

aux <- data.table( melt( aux, id.vars = c('t'), value.name = 'num', variable.name = 'tipo' ) )

x_lim <- seq( 2012, 2040,1 )
x_brk <- seq( 2012, 2040,2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 24000)
y_brk <- seq( y_lim[1], y_lim[2], 4000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_nuevos_pensi <- ggplot() +
  geom_bar( data = aux[ tipo!='t_new'] , aes(x = t, y = num, fill= tipo ), 
            position = "dodge", stat = "identity", size=0.1) +
  geom_point( data = aux[tipo=='t_new'],aes( x = t, y=num, color = 't_new' ),
              size=3, shape=95) +
  labs( x = NULL, y = NULL ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl) +
  scale_fill_manual(breaks = c("l23", "l24", "l8", "l9"),
                    values=c( 'l24' = "black",
                              'l8' = "purple",
                              'l23' = parametros$iess_green,
                              'l9'= "red"),
                    label=c('l24'='Invalidez', 'l23'='Vejez', 'l8'= 'Orfandad', 'l9'='Viudedad'))+
  scale_color_manual( values = c( 't_new'= parametros$iess_total), 
                      label = c('t_new'='Total')) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = NULL ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_nuevos_pensi,
        filename = paste0( parametros$resultado_graficos, 'iess_nuevos_pensi', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando proyeccion de la población desagregada por sex en cada estado--------------------------
message( '\tGraficando proyeccion de la población desagregada por sex en cada estado' )
message( '\t\tProyeccion de Jefes de Familia' )
num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 15, 85 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 10000 )
y_brk <- seq( y_lim[1], y_lim[2], 2000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2 ) ]
plt_l2_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l2' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


aux_m <- pob_proy[ sexo == 'Male', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l2 = l2 ) ]
plt_l2_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l2, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l2' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l2_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l2_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Proyeccion de Pensionistas de Vejez --------------------------------------------------------------
message( '\t\tProyeccion de Pensionistas de Vejez' )
num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 65, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 7500 )
y_brk <- seq( y_lim[1], y_lim[2], 1500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l3' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- proy[ sexo == 'Male', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l3 ) ]
plt_l3_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l3, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l3' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l3_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l3_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Proyeccion de Pensionistas de Invalidez ----------------------------------------------------------
message( '\t\tProyeccion de Pensionistas de invalidez' )
num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 40 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l4' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'Male', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l4 ) ]
plt_l4_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l4, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l4' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l4_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l4_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plt_pob <- marrangeGrob( list( plt_l2_f, plt_l2_m, plt_l3_f, plt_l3_m, plt_l4_f, plt_l4_m ),
                         nrow = 2, ncol = 3, top = '' )

ggsave( plot = plt_pob, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_ssc', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

# Proyeccion de Pensionistas de orfandad -----------------------------------------------------------
message( '\t\tProyección montepio por orfandad' )

num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 500 )
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ x<=18 & sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l8 ) ]
aux_f[x==18, l8:=0]
plt_l8_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l8, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l8' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l8_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l8_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ x<=18 & sexo == 'Male', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l8 ) ]
aux_m[x==18, l8:=0]
plt_l8_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l8, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l8' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l8_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l8_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Proyección de pensionistas de viudedad -----------------------------------------------------------
message( '\t\tProyección montepio por viudedad' )

num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 15 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 2400 )
y_brk <- seq( y_lim[1], y_lim[2], 300 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- pob_proy[ sexo == 'Female', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l9 ) ]
plt_l9_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l9, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l9' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l9_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- pob_proy[ sexo == 'Male', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l9 ) ]
plt_l9_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l9, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l9' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l9_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l9_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plt_pob_mont <- marrangeGrob( list( plt_l8_f, plt_l8_m ,
                                    plt_l9_f, plt_l9_m),
                              nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_mont, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_mont_ssc', parametros$graf_ext ),
        width = 17.5, height = 12, units = graf_units, dpi = graf_dpi )

# Proyección de dependientes hijos -----------------------------------------------------------------
message( '\t\tProyección de dependientes hijos' )
num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 0, 18 )
x_brk <- seq( x_lim[1], x_lim[2], 2 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 20000 )
y_brk <- seq( y_lim[1], y_lim[2], 4000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ x <= 18 & sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l6 ) ]
plt_l6_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l6, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l6' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l6_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l6_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <- proy[ x<=18 & sexo == 'Male', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l6 ) ]
plt_l6_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l6, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l6' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l6_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l6_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Proyección de dependientes cónyuges---------------------------------------------------------------
message( '\t\tProyección de dependientes cónyuges' )
num_anios <- length( unique( proy$t ) )
cols_fun <- colorRampPalette( c( 'gold', parametros$iess_green, parametros$iess_blue ) )
cols_graf <- cols_fun( num_anios )

x_lim <- c( 15, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 10000 )
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <- proy[ x>=15 & sexo == 'Female', list( t = factor( t , levels = 0:parametros$horizonte, ordered = TRUE ), x, l7 ) ]
plt_l7_f <- ggplot() +
  geom_line( data = aux_f, aes( x = x, y = l7, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'mujeres l7' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l7_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l6_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

y_lim <- c( 0, 300 )
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
aux_m <- proy[ x>=15 & sexo == 'Male', list( t = factor( t, levels = 0:parametros$horizonte, ordered = TRUE ), x, l7 ) ]
plt_l7_m <- ggplot() +
  geom_line( data = aux_m, aes( x = x, y = l7, color = t ), size = graf_line_size ) +
  scale_color_manual( values = cols_graf ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  ylab( 'hombres l7' ) +
  theme_bw() +
  plt_theme

ggsave( plot = plt_l7_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_l7_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plt_pob_dep <- marrangeGrob( list( plt_l6_f, plt_l6_m ,
                                   plt_l7_f, plt_l7_m),
                              nrow = 2, ncol = 2, top = '' )

ggsave( plot = plt_pob_dep, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_proy_dep_ssc', parametros$graf_ext ),
        width = 17.5, height = 12, units = graf_units, dpi = graf_dpi )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()