message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_demografico.RData' ) )
load( file = paste0( parametros$RData, 'IVM/', 'IESS_IVM_analisis_demografico.RData' ) )

# Graficando población afiliada al SGO--------------------------------------------------------------
message( '\tGraficando población afiliada al SGO' )

aux <- copy( pob_afi_ini[, .(anio, Afiliados_m, Afiliados_f, Afiliados)] )

x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 400000, 3600000)
y_brk <- seq( y_lim[1], y_lim[2],by=400000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pob_afi_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados_m, colour = "Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados_f, colour = "Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados, colour = "Total Afiliados" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Total Afiliados" ,"Masculino","Femenino"),
                       values = c( "Masculino" = parametros$iess_blue,
                                   "Femenino" =  parametros$iess_green,
                                   "Total Afiliados" = parametros$iess_total) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Afiliados' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))
  
ggsave( plot = iess_pob_afi_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_ini_sgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando distribución de población afiliada por edad y sexo del SGO-----------------------------
message( '\tGraficando distribución de población afiliada por edad y sexo' )

aux <-copy( pob_afi_edad_sexo_ini )
max_edad<-100
min_edad<-15
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_afiliados <-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_afiliados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+60")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1001+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ))

# Masa salarial del SGO ----------------------------------------------------------------------------
message( '\tGraficando masa salarial del SGO' )
unidad <- 1e6
aux <- copy( masa_salarial_ini[, .(anio, Masa_Anual,Masa_Anual_Mas,Masa_Anual_Fem)] )
aux[, Masa_Anual := Masa_Anual / unidad ]
aux[, Masa_Anual_Mas := Masa_Anual_Mas / unidad ]
aux[, Masa_Anual_Fem := Masa_Anual_Fem / unidad ]

x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 30000)
y_brk <- seq( y_lim[1], y_lim[2], by=5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual_Mas, colour = "Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual_Fem, colour = "Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual, colour = "Masa Salarial Total" ), size = graf_line_size  ) + 
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_colour_manual( "",
                       breaks = c( "Masa Salarial Total",
                                   "Masculino", 
                                   "Femenino"),
                       values = c( "Masculino" = parametros$iess_blue,
                                   "Femenino" =  parametros$iess_green,
                                   "Masa Salarial Total" = parametros$iess_total) ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Millones de USD' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5), 
         legend.text = element_text(colour = "black"))


ggsave( plot = iess_masa_salarial_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando masa salarial del SGO por edad y sexo------------------------------------------------
message( '\tGraficando masa salarial del SGO por edad y sexo' )
aux <-copy( masa_sal_monto )
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 100
salto_x <- 0.05
brks_y <- seq(-0.45, 0.45,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- c(seq( 100,1500,salto_y),1600)
lbls_x <- c( formatC(brks_x[-length(brks_x)], digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),'mayor a 1.500')

iess_pir_masa_salarial<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Salario Declarado (USD)' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white",  size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white",  size=0.1) +
  scale_y_continuous( breaks = brks_y, labels = lbls_y) +
  scale_x_continuous( breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend( title = NULL, label.position = "right", label.hjust = 0))+
  theme(legend.position="none",
        axis.text.y = element_text( angle = 0, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+680+60")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1140+70")
image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ))

# Graficando Evolución de la población afiliada al SSC ---------------------------------------------
message( '\tGraficando jefes de familia del SSC' )
aux <- copy( afi_hist_ssc[, .(Anio, Male, Female, Afiliados_activos)] )

x_lim <- c( 2011, 2020 )
x_brk <- 2011:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 50000, 400000)
y_brk <- seq( y_lim[1], y_lim[2],by = 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_afi_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = Male, colour = "Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = Anio, 
                  y = Female, colour = "Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = Anio, 
                  y = Afiliados_activos, colour = "Total Afiliados" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Total Afiliados" ,"Masculino","Femenino"),
                       values = c( "Masculino" = parametros$iess_blue,
                                   "Femenino" =  parametros$iess_green,
                                   "Total Afiliados" = parametros$iess_total) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Afiliados' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_pob_afi_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_afi_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando distribución de los jefes de familia del  SSC por sexo y edad------------------------
message( '\tGraficando distribución de los jefes de familia del  SSC por sexo y edad' )
aux <-copy( afi_edad_sexo_ssc )
max_edad <- 110
min_edad <- 15
aux <-aux[ edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.02 ,0.02,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_afi_edad_sexo_ssc <- ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="none")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_afi_edad_sexo_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_afi_edad_sexo_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_afi_edad_sexo_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  image_crop("280x150+165+0")
plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+60")

final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1065+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_afi_edad_sexo_ssc', parametros$graf_ext ))

# Ingresos de los jefes de familia del SSC----------------------------------------------------------
message( '\tGraficando Ingresos de los jefes de familia del SSC' )
aux <- copy( masa_ssc[, .(anio, masa_anual, Male_A, Female_A )] )

x_lim <- c( 2009, 2020 )
x_brk <- 2009:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 12000000)
y_brk <- seq( y_lim[1], y_lim[2], by=1000000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Male_A, colour = "Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Female_A, colour = "Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = masa_anual, colour = "Masa Salarial Total" ), size = graf_line_size  ) + 
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_colour_manual( "",
                       breaks = c( "Masa Salarial Total",
                                   "Masculino", 
                                   "Femenino"),
                       values = c( "Masculino" = parametros$iess_blue,
                                   "Femenino" =  parametros$iess_green,
                                   "Masa Salarial Total" = parametros$iess_total),
                       labels = c( "Masculino" = "Masculino",
                                   "Femenino" = " Femenino",
                                   "Masa Salarial Total" = " Ingreso Total Aportes")) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'USD' ) +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_masa_salarial_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Crecimiento de la población jubilada por vejez del SSC -------------------------------------------
message( '\tGraficando crecimiento de la población jubilada por vejez del SSC' )
aux <- copy( jub_vejez_ssc[, .(tipo, Anio, Jubilados_vejez)] )

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 100000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_jub_vejez_ssc <- ggplot( aux ) + 
  geom_line( data= aux[ tipo=='Total'],aes( x = Anio, y = Jubilados_vejez, colour='Total'),
             size = graf_line_size ) + 
  geom_line( data= aux[ tipo=='Hombres'],aes( x = Anio, y = Jubilados_vejez , colour = 'Hombres'),
             size = graf_line_size ) + 
  geom_line( data= aux[ tipo=='Mujeres'], aes( x = Anio, y = Jubilados_vejez , colour = 'Mujeres'),
             size = graf_line_size ) +
  labs( x = 'Año', y = 'Pensionistas' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_colour_manual( "",
                       breaks = c( "Total",
                                   "Hombres", 
                                   "Mujeres"),
                       values = c( "Hombres" = parametros$iess_blue,
                                   "Mujeres" =  parametros$iess_green,
                                   "Total" = parametros$iess_total),
                       labels = c( "Hombres" = "Masculino",
                                   "Mujeres" =  "Femenino",
                                   "Total" = "Total Vejez")) +
  theme_bw() +
  plt_theme +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_jub_vejez_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_jub_vejez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficando Pirámide Jubilados de vejez del SSC por edad y sexo -----------------------------------
message( '\tGráficando Pirámide Jubilados de vejez del SSC por edad y sexo' )
aux <- copy( jub_vejez_edad_sexo_ssc )
max_edad <- 110
min_edad <- 15
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_jub_vejez_edad_sexo <-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="none")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_jub_vejez_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_jub_vejez_edad_sexo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_jub_vejez_edad_sexo', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+880+60")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1210+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_jub_vejez_edad_sexo', parametros$graf_ext ))

# Gráficando Jubilados de invalidez del SSC --------------------------------------------------------
message( '\tGráficando Jubilados de invalidez del SSC' )
aux <- copy( jub_invalidez_ssc[, .(tipo, Anio, Jubilados_invalidez)] )

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 450)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_jub_inv_ssc <- ggplot( aux ) + 
  geom_line( data= aux[ tipo=='Total'],aes( x = Anio, y = Jubilados_invalidez, colour='Total'),
             size = graf_line_size ) + 
  geom_line( data= aux[ tipo=='Hombres'],aes( x = Anio, y = Jubilados_invalidez , colour = 'Hombres'),
             size = graf_line_size ) + 
  geom_line( data= aux[ tipo=='Mujeres'], aes( x = Anio, y = Jubilados_invalidez , colour = 'Mujeres'),
             size = graf_line_size ) +
  labs( x = 'Año', y = 'Pensionistas' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_colour_manual( "",
                       breaks = c( "Total",
                                   "Hombres", 
                                   "Mujeres"),
                       values = c( "Hombres" = parametros$iess_blue,
                                   "Mujeres" =  parametros$iess_green,
                                   "Total" = parametros$iess_total),
                       labels = c( "Hombres" = "Masculino",
                                   "Mujeres" =  "Femenino",
                                   "Total" = "Total Invalidez")) +
  theme_bw() +
  plt_theme +
  theme( legend.position = "bottom",legend.direction = "horizontal",
         axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5), 
         legend.text = element_text(colour = "black"))

ggsave( plot = iess_jub_inv_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_jub_invalidez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Graficando jubilados de invalidez del SSC  por edad y sexo ----------------------------------------
message( '\tGraficando jubilados de invalidez del SSC  por edad y sexo' )
aux <-copy( jub_invalidez_edad_sexo_ssc )
max_edad <- 100
min_edad <- 15
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_jub_invalidez_edad_sexo <-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="none")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_jub_invalidez_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_jub_invalidez_edad_sexo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_jub_invalidez_edad_sexo', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+880+60")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1250+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_jub_invalidez_edad_sexo', parametros$graf_ext ))

#Graficando dependientes del SSC  por edad y sexo --------------------------------------------------
message( '\tGraficando dependientes del SSC  por edad y sexo' )
aux <-copy( dep_edad_sexo )
max_edad <- 105
min_edad <- 15
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

# Para hijos
aux_h <- copy( dep_edad_sexo_hijos )
aux_h <- aux_h[edad>=0 & edad <=max_edad]  #Condición para extraer los datos
aux_h[is.na(n),n:=0]  #reemplazo datos NA por cero

Nh <- data.frame((aux_h[,sum(n,na.rm = TRUE)]))

aux_h[ , nh:=n]
aux_h[sexo=="H", nh:=-nh]
aux_h[sexo=="H", nh:=nh/N[1,1]]
aux_h[sexo=="M", nh:=nh/N[1,1]]

Mh <- data.frame((aux_h[,max(abs(n),na.rm = TRUE),by=sexo]))

# Para conyuges
aux_c <- copy( dep_edad_sexo_cony )
aux_c <- aux_c[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux_c[is.na(n),n:=0]  #reemplazo datos NA por cero

Nc <- data.frame((aux_c[,sum(n,na.rm = TRUE)]))

aux_c[ , nc:=n]
aux_c[sexo=="H", nc:=-nc]
aux_c[sexo=="H", nc:=nc/N[1,1]]
aux_c[sexo=="M", nc:=nc/N[1,1]]

Mc <- data.frame((aux_c[,max(abs(n),na.rm = TRUE),by=sexo]))

data <- data.table( merge( expand.grid( x=0:105, sexo=c('H', 'M')) , aux, by.x=c('x', 'sexo'), 
                           by.y=c('edad', 'sexo'), all.x=T ))
setorder( data, sexo, x)
data <- merge( data, aux_h[ , list( edad, sexo, nh)],  by.x=c('x', 'sexo'), by.y=c('edad', 'sexo'), all.x=T)
setorder( data, sexo, x)

data <- merge( data, aux_c[ , list( edad, sexo, nc)],  by.x=c('x', 'sexo'), by.y=c('edad', 'sexo'), all.x=T)
setorder( data, sexo, x)
data[ , otdep:= n - nh - nc ]

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq( 0,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

data1 <- data.table( melt( data, id.vars = c('x', 'sexo'), variable.name = 'vn', value.name = 'ob') )

iess_dep_edad_sexo <- ggplot() +
  geom_bar( data=data1[vn!='n'], aes(x=x, y=ob, fill=vn, colour = sexo), stat="identity", size=0.1)+
  xlab( 'Edad' ) +
  ylab( '' ) +
  # scale_alpha_manual( "",
  #                     breaks = c( "nh", 
  #                                 "nc",
  #                                 "otdep"),
  #                     values = c(1, 1, 1)) +
  scale_colour_manual( "",
                       breaks = c( "H", 
                                   "M"),
                       values = c(parametros$iess_blue, parametros$iess_green)) +

  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides( fill = guide_legend( label.position = "right", label.hjust = 0, label.vjust = 0.5),
          color = FALSE ) +
  theme( legend.position="bottom")+  
  scale_fill_manual("",
                    breaks = c( "nh",
                                "nc",
                                "otdep"),
                    values = c(parametros$iess_total, 'red', 'orange'),
                    labels = c('Hijos', 'Cónyuges', 'Otros Dependientes')) 



# iess_dep_edad_sexo <- ggplot(data) +
#   xlab( 'Edad' ) +
#   ylab( '' ) +
#   geom_bar( data = data[ sexo == 'M' ],  aes(x = x, y = otdep, fill='red'),
#             stat = 'identity',colour="white", size=0.1) +
#   geom_bar( data = data[ sexo == 'H' ],  aes(x = x, y = otdep, fill='blue'),
#             stat = 'identity',colour="white", size=0.1) +
#   geom_bar( data = data[ sexo == 'M' ],  aes(x = x, y = nh, fill='green'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   geom_bar( data = data[ sexo == 'H' ],  aes(x = x, y = nh, fill='yellow'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   geom_bar( data = data[ sexo == 'M' ],  aes(x = x, y = nc, fill='black'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   geom_bar( data = data[ sexo == 'H' ],  aes(x = x, y = nc, fill='orange'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   geom_bar( data = data[ sexo == 'M' ],  aes(x = x, y = n, fill='black'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   geom_bar( data = data[ sexo == 'H' ],  aes(x = x, y = n, fill='black'),
#             stat = 'identity',colour="white", size=0.1, alpha=0.3) +
#   
#   scale_y_continuous(breaks = brks_y, labels = lbls_y) +
#   scale_x_continuous(breaks = brks_x, labels = lbls_x) +
#   coord_flip() +
#   #theme_tufte()+
#   theme_bw() +
#   plt_theme +
#   guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
#   theme(legend.position="none")+   #legend.position = c(0.8, 0.2)
#   scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_dep_edad_sexo,
        filename = paste0( parametros$resultado_graficos, 'iess_dep_edad_sexo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_dep_edad_sexo', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+680+60")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+920+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_dep_edad_sexo', parametros$graf_ext ))


#Graficando dependientes del SSC por provincia ----------------------------------------------------
message( '\tGraficando dependientes del SSC  por provincia' )
Mapa <- readShapeSpatial( paste0(str_replace_all( paste0(parametros$data_server, 'Data/SSC/'), "/","\\\\") ,"Mapa\\2021\\SHP\\nxprovincias.shp") )
Mapa <- thinnedSpatialPoly(Mapa, tolerance =  32, topologyPreserve = TRUE)
Mapa <- Mapa[ order(Mapa$DPA_DESPRO), ]

aux <- copy(prov_ssc)
aux_jef <- copy( prov_jef_ssc )
Mapa$Dependientes <- aux[Provincia!='Total']$Numero
Mapa$Jefes <- aux_jef[Provincia!='Total']$Numero
Mapa$Relacion <- Mapa$Jefes/Mapa$Dependientes

Mapa$Provincia = c('Azuay', 'Bolívar', 'Cañar'
                   , 'Carchi', 'Chimborazo', 'Cotopaxi'
                   , 'El Oro', 'Esmeraldas', 'Galápagos'
                   , 'Guayas', 'Imbabura', 'Loja'
                   , '', 'Manabí', ''
                   , 'Napo', 'Orellana', 'Pastaza'
                   , 'Pichincha', '', ''
                   , 'Sucumbíos', 'Tungurahua', '')
Mapa$Provincia1 = c('', '', ''
                    , '', '', ''
                    , '', '', ''
                    , '', '', ''
                    , 'Los', '', 'Morona'
                    , '', '', ''
                    , '', 'Santa', 'Santo'
                    , '', '', 'Zamora')
Mapa$Provincia2 = c('', '', ''
                    , '', '', ''
                    , '', '', ''
                    , '', '', ''
                    , 'Ríos', '', 'Santiago'
                    , '', '', ''
                    , '', 'Elena', 'Domingo'
                    , '', '', 'Chinchipe')
Mapa$x1 = c(0, 0, 0
            , 15000, 7000, 0
            , 0, 0, 100000
            , -10000, 5000, 0
            , -11000, 0, 0
            , -10000, 0, 0
            , 10000, 5000, 4000
            , 0, 0, 10000)
Mapa$y1 = c(0, 15000, 0
            , -5000, 12000, 0
            , 0, 0, -50000
            , 10000, 0, 0
            , -15000, -10000, 0
            , -5000, 0, 0
            , 10000, 0, 11000
            , 10000, 0, 40000)

colores <- paste(colorRampPalette(c(Gris, Azul, Verde))(100), '80', sep='')

MapaContinental <- Mapa[Mapa$DPA_DESPRO != 'GALAPAGOS',]

pdf(paste0(parametros$resultado_graficos,'iess_dependientes_provincia.pdf')
    , width = 1.0*textwidth +.0 +.0
    , height = 1.0*textwidth +.0 +.0
    , family = 'CM Roman'
)
par(pin = c(1.0*textwidth, 1.0*textwidth)
    , mai = c(.0, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)
plot(MapaContinental
     , border = NegroTransparente
     , col = colores[round(100*(MapaContinental$Dependientes - 200)/(max(MapaContinental$Dependientes) - 200), 0)]
)

legend("bottomright"
       , bty = 'n'
       , legend = rev(format(round(200 + ((max(Mapa$Dependientes) - 200)*c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)/100), 0), big.mark = '.', decimal.mark="," ))
       , x.intersp = -2, adj = 1
       , col = rev(colores[c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)])
       , pch = 15
       , pt.cex = 2.7
)

text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     , labels = MapaContinental$Provincia)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(6500, 23)), labels = MapaContinental$Provincia1)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(-6500, 23)), labels = MapaContinental$Provincia2)

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_dependientes_provincia.pdf'))

MapaGalapagos = Mapa[Mapa$DPA_DESPRO == 'GALAPAGOS',]
pdf(paste0(parametros$resultado_graficos,'iess_dependientes_provincia_galapagos.pdf')
    , width = .6*textwidth + .0 + .0
    , height = .6*textwidth + 1.4 + .0
    , family = 'CM Roman'
)
par(pin = c(.6*textwidth, .6*textwidth + 1.4)
    , mai = c(1.4, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)
plot(MapaGalapagos
     , border = NegroTransparente
     , col = colores[round(100*(MapaGalapagos$Dependientes - 1000)/(max(Mapa$Dependientes) - 1000), 0)]
)
text(coordinates(MapaGalapagos) + cbind(MapaGalapagos$x1, MapaGalapagos$y1)
     , labels = MapaGalapagos$Provincia)
#map.scale()
invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_dependientes_provincia_galapagos.pdf'))

# Jefes por provincia ------------------------------------------------------------------------------
MapaContinental <- Mapa[Mapa$DPA_DESPRO != 'GALAPAGOS',]

pdf(paste0(parametros$resultado_graficos,'iess_jefes_provincia.pdf')
    , width = 1.0*textwidth +.0 +.0
    , height = 1.0*textwidth +.0 +.0
    , family = 'CM Roman'
)
par(pin = c(1.0*textwidth, 1.0*textwidth)
    , mai = c(.0, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)
plot(MapaContinental
     , border = NegroTransparente
     , col = colores[round(100*(MapaContinental$Jefes - 200)/(max(MapaContinental$Jefes) - 200), 0)]
)

legend("bottomright"
       , bty = 'n'
       , legend = rev(format(round(200 + ((max(Mapa$Jefes) - 200)*c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)/100), 0), big.mark = '.', decimal.mark="," ))
       , x.intersp = -2, adj = 1
       , col = rev(colores[c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)])
       , pch = 15
       , pt.cex = 2.7
)

text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     , labels = MapaContinental$Provincia)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(6500, 23)), labels = MapaContinental$Provincia1)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(-6500, 23)), labels = MapaContinental$Provincia2)

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_jefes_provincia.pdf'))

MapaGalapagos = Mapa[Mapa$DPA_DESPRO == 'GALAPAGOS',]
pdf(paste0(parametros$resultado_graficos,'iess_jefes_provincia_galapagos.pdf')
    , width = .6*textwidth + .0 + .0
    , height = .6*textwidth + 1.4 + .0
    , family = 'CM Roman'
)
par(pin = c(.6*textwidth, .6*textwidth + 1.4)
    , mai = c(1.4, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)
plot(MapaGalapagos
     , border = NegroTransparente
     , col = colores[round(100*(MapaGalapagos$Jefes - 1000)/(max(Mapa$Jefes) - 1000), 0)]
)
text(coordinates(MapaGalapagos) + cbind(MapaGalapagos$x1, MapaGalapagos$y1)
     , labels = MapaGalapagos$Provincia)
#map.scale()
invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_jefes_provincia_galapagos.pdf'))

# Relacion -----------------------------------------------------------------------------------------
MapaContinental = Mapa[Mapa$DPA_DESPRO != 'GALAPAGOS',]
pdf(paste0(parametros$resultado_graficos,'iess_relacion_provincia.pdf')
    , width = 1.0*textwidth +.0 +.0
    , height = 1.0*textwidth +.0 +.0
    , family = 'CM Roman'
)
par(pin = c(1.0*textwidth, 1.0*textwidth)
    , mai = c(.0, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)

plot(MapaContinental
     , border = NegroTransparente
     , col = colores[round(100*(MapaContinental$Relacion - .01)/(max(MapaContinental$Relacion) - .01), 0)]
)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     , labels = MapaContinental$Provincia)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(6500, 23)), labels = MapaContinental$Provincia1)
text(coordinates(MapaContinental) + cbind(MapaContinental$x1, MapaContinental$y1)
     + cbind(rep(0, 23), rep(-6500, 23)), labels = MapaContinental$Provincia2)
legend("bottomright"
       , bty = 'n'
       , legend = rev(paste(format(100*round(.01 + ((max(MapaContinental$Relacion) - .01) * 
                                                      c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)/100), 3),
                                   decimal.mark = ",", digits = 3),'%'))
       , x.intersp = -2, adj = 1
       , col = rev(colores[c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)])
       , pch = 15
       , pt.cex = 2.7
)

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_relacion_provincia.pdf'))

MapaGalapagos = Mapa[Mapa$DPA_DESPRO == 'GALAPAGOS',]
pdf(paste0(parametros$resultado_graficos,'iess_relacion_relacion_galapagos.pdf')
    , width = .6*textwidth + .0 + .0
    , height = .6*textwidth + 1.4 + .0
    , family = 'CM Roman'
)
par(pin = c(.6*textwidth, .6*textwidth + 1.4)
    , mai = c(1.4, .0, .0, .0)
    , cex = .7
    , xpd = TRUE
)
plot(MapaGalapagos
     , border = NegroTransparente
     , col = colores[round(100*(MapaGalapagos$Relacion - .01)/(max(Mapa$Relacion) - .01), 0)]
)
text(coordinates(MapaGalapagos) + cbind(MapaGalapagos$x1, MapaGalapagos$y1)
     , labels = MapaGalapagos$Provincia)

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_relacion_relacion_galapagos.pdf'))


# Graficando proporcion de jefes activos e inactivos -----------------------------------------------
message( '\tGraficando proporcion de jefes activos e inactivos ' ) 

aux <- copy( prop_jef_act_inac)
colnames(aux) = c("Des","Descripcion","Total", "Por")
aux <- aux[, -c(1,3) ]
aux[ , tipo:=str_split_fixed(aux$Descripcion, " ", 2)[,1]]
aux[ , sexo:=str_split_fixed(aux$Descripcion, " ", 2)[,2]]
setorder( aux, sexo)

genero <- c(rep('F', 2 ), rep('M', 2 ))
tipo<- c('Activo', 'Inactivo', 'Activo', 'Inactivo')
value <- aux$Por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_jefes_act_inact.pdf')
    , width = .75*textwidth +0 +0
    , height = .75*textwidth +0 +0
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 0, 0)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(30, 20, 15)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(2, 1, 0),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_jefes_act_inact.pdf'))

# Graficando pastel jefes activos sexo masculino por tipo de riesgos -------------------------------
message( '\tGraficando pastel jefes activos sexo masculino por tipo de riesgos' )

aux <- copy( past_jef_male_tipo_riesgo )
aux[ riesgo=='Requisitos no cumplidos', grupo:='Grupo 1']
aux[ riesgo=='Requisitos cumplidos para invalidez y muerte', grupo:='Grupo 2']
aux[ riesgo=='Cumple requisitos para vejez en 5 años', grupo:='Grupo 3']
aux[ riesgo=='Cumple requisitos para vejez en 1 año o menos', grupo:='Grupo 4']

setorder( aux, grupo)

genero <- c(rep('M', 4 ))
tipo <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
value <- aux$por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pastel_jefes_male_riesgo.pdf')
    , width = .75*textwidth +0 +0
    , height = .75*textwidth +0 +0
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 0, 0)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(40, 15, 18)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(2, 1, 0),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_jefes_male_riesgo.pdf'))

# Graficando jefes activos por edad, años de imposiciones y riesgo, sexo masculino -----------------
message( '\tGraficando jefes activos por edad, años de imposiciones y riesgo, sexo masculino' )
aux <- copy( jef_male_tipo_riesgo )
aux <- aux[ , -1]

df2 <- aux
df2 <- df2[ edad <= 80 & impo_anios <= 45] #Corrección para OIT

#x_lim <- c(0,70)
x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

#y_lim <- c( -125, -15 )
y_lim <- c(-80,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#esc_cl <- paste(colorRampPalette(c('gray35', "yellow"))(4), '50', sep='')
esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_jef_male_edad_riesgo <- ggplot(df2, aes(x = impo_anios , y = -edad) ) +
  geom_point( aes( size = n_afi , colour = riesgo3 ),alpha=0.5) +
  scale_size(range = c(1,3)) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F) +
  theme_bw() +
  plt_theme_legend + 
  theme(legend.position = c(0.7, 0.85), legend.direction = "vertical",
        legend.key.size = unit(0.2, "cm"), legend.text = element_text( size = 7) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust =1) ) +
  guides(color = guide_legend(override.aes = list(size = 4)))


ggsave( plot = iess_jef_male_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_jef_male_edad_riesgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia por años de cotizaciones, sexo masculino -----------------------------
message( '\tGraficando jefes de familia por años de cotizaciones, sexo masculino' )
cotizantes <- copy( Jefes_male_num_cotiz )

x_lim <- c( -0.5, 62.5 )
x_brk <- seq( x_lim[1]+0.5, x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f')

y_lim <- c ( 0, 25 )
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC(y_brk, digits = 0, format = 'f')

iess_jefe_male_num_cotiz <- ggplot(cotizantes, aes(x=cot, y = num/1000, fill = " ")) +
  geom_bar( data = cotizantes, stat = 'identity',colour="white", size=0.1) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  scale_x_continuous(breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  theme_bw() +
  plt_theme +
  scale_fill_manual(values = parametros$iess_blue)
  # theme(text = element_text( color = 'black' ),  
  #       legend.text = element_text( size = rel( 0.6), colour = 'black', 
  #                                   face = 'plain', family = tipo_letra ))

ggsave( plot = iess_jefe_male_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_jefe_male_num_cotiz', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando pastel jefes activos sexo femenino por tipo de riesgos --------------------------------
message( '\tGraficando pastel jefes activos sexo femenino por tipo de riesgos' )
aux <- copy( past_jef_female_tipo_riesgo )
aux[ riesgo=='Requisitos no cumplidos', grupo:='Grupo 1']
aux[ riesgo=='Requisitos cumplidos para invalidez y muerte', grupo:='Grupo 2']
aux[ riesgo=='Cumple requisitos para vejez en 5 años', grupo:='Grupo 3']
aux[ riesgo=='Cumple requisitos para vejez en 1 año o menos', grupo:='Grupo 4']

setorder( aux, grupo)

genero <- c(rep('F', 4 ))
tipo <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
value <- aux$por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo.pdf')
    , width = .75*textwidth +0 +0
    , height = .75*textwidth +0 +1
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 0, 1)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(40, 15, 14)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
          border.lwds = c(1),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo.pdf'))

# Graficando jefes activos por edad, años de imposiciones y riesgo, sexo femenino ------------------
message( '\tGraficando jefes activos por edad, años de imposiciones y riesgo, sexo femenino' )
aux <- copy( jef_female_tipo_riesgo )
aux <- aux[ , -1]

df2 <- aux
df2 <- df2[ edad <= 80 & impo_anios <= 45] #Corrección para OIT

#x_lim <- c(0,70)
x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

#y_lim <- c( -125, -15 )
y_lim <- c(-80,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#esc_cl <- paste(colorRampPalette(c('gray35', "yellow"))(4), '50', sep='')
esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_jef_female_edad_riesgo <- ggplot(df2, aes(x = impo_anios , y = -edad) ) +
  geom_point( aes( size = n_afi , colour = riesgo3 ),alpha=0.5) +
  scale_size(range = c(1,3)) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F) +
  theme_bw() +
  plt_theme_legend + 
  theme(legend.position = c(0.7, 0.85), legend.direction = "vertical",
        legend.key.size = unit(0.2, "cm"), legend.text = element_text( size = 7) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust =1) ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave( plot = iess_jef_female_edad_riesgo,
        filename = paste0( parametros$resultado_graficos, 'iess_jef_female_edad_riesgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia por años de cotizaciones, sexo femenino -----------------------------
message( '\tGraficando jefes de familia por años de cotizaciones, sexo femenino' )
cotizantes <- copy( Jefes_female_num_cotiz )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[1]+0.5, x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f')

y_lim <- c ( 0, 22 )
y_brk <- seq( y_lim[1], y_lim[2], 2 )
y_lbl <- formatC(y_brk, digits = 0, format = 'f')

iess_jefe_female_num_cotiz <- ggplot(cotizantes, aes(x=cot, y = num/1000, fill = " ")) +
  geom_bar( data = cotizantes, stat = 'identity',colour="white", size=0.1) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  
  theme_tufte()+
  scale_x_continuous(breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  theme_bw() +
  plt_theme +
  scale_fill_manual(values = parametros$iess_blue)
  # theme(text = element_text( color = 'black' ),  
  #       legend.text = element_text( size = rel( 0.6), colour = 'black', 
  #                                   face = 'plain', family = tipo_letra ))

ggsave( plot = iess_jefe_female_num_cotiz ,
        filename = paste0( parametros$resultado_graficos, 'iess_jefe_female_num_cotiz', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Para INACTIVOS-----------
# Graficando pastel jefes inacactivos sexo masculino por tipo de riesgos -------------------------------
message( '\tGraficando pastel jefes inactivos sexo masculino por tipo de riesgos' )
aux <- copy( past_jef_male_tipo_riesgo_inac  )
aux[ riesgo=='Requisitos no cumplidos', grupo:='Grupo 1']
aux[ riesgo=='Requisitos cumplidos para invalidez y muerte', grupo:='Grupo 2']
aux[ riesgo=='Cumple requisitos para vejez en 5 años', grupo:='Grupo 3']
aux[ riesgo=='Cumple requisitos para vejez en 1 año o menos', grupo:='Grupo 4']

setorder( aux, grupo)

genero <- c(rep('M', 4 ))
tipo <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
value <- aux$por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pastel_jefes_male_riesgo_inac.pdf')
    , width = .75*textwidth +0 +5
    , height = .75*textwidth +0 +1
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 5, 1)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(40, 25, 20)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(1),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_jefes_male_riesgo_inac.pdf'))

# Graficando jefes inactivos por edad, años de imposiciones y riesgo, sexo masculino -----------------
message( '\tGraficando jefes inactivos por edad, años de imposiciones y riesgo, sexo masculino' )
aux <- copy( jef_male_tipo_riesgo_inac )
aux <- aux[ , -1]

df2 <- aux
df2 <- df2[ edad <= 80 & impo_anios <= 45] #Corrección para OIT

#x_lim <- c(0,70)
x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

#y_lim <- c( -125, -15 )
y_lim <- c(-80,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#esc_cl <- paste(colorRampPalette(c('gray35', "yellow"))(4), '50', sep='')
esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_jef_male_edad_riesgo_inac <- ggplot(df2, aes(x = impo_anios , y = -edad) ) +
  geom_point( aes( size = n_afi , colour = riesgo3 ),alpha=0.5) +
  scale_size(range = c(1,3)) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F) +
  theme_bw() +
  plt_theme_legend + 
  theme(legend.position = c(0.7, 0.85), legend.direction = "vertical",
        legend.key.size = unit(0.2, "cm"), legend.text = element_text( size = 7) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust =1) ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave( plot = iess_jef_male_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_jef_male_edad_riesgo_inac', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia inactivos por años de cotizaciones, sexo masculino -------------------
message( '\tGraficando jefes de familia inactivos por años de cotizaciones, sexo masculino' )
cotizantes <- copy( Jefes_male_num_cotiz_inac )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[1]+0.5, x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f')

y_lim <- c ( 0, 12 )
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC(y_brk, digits = 0, format = 'f')

iess_jefe_male_num_cotiz_inac <- ggplot(cotizantes, aes(x=cot, y = num/1000, fill = " ")) +
  geom_bar( data = cotizantes, stat = 'identity',colour="white", size=0.1) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  
  theme_tufte()+
  scale_x_continuous(breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  theme_bw() +
  plt_theme +
  scale_fill_manual(values = parametros$iess_blue)
  # theme(text = element_text( color = 'black' ),  
  #       legend.text = element_text( size = rel( 0.6), colour = 'black', 
  #                                   face = 'plain', family = tipo_letra ))

ggsave( plot = iess_jefe_male_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_jefe_male_num_cotiz_inac', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando pastel jefes inactivos sexo femenino por tipo de riesgos --------------------------------
message( '\tGraficando pastel jefes inactivos sexo femenino por tipo de riesgos' )

aux <- copy( past_jef_female_tipo_riesgo_inac )
aux[ riesgo=='Requisitos no cumplidos', grupo:='Grupo 1']
aux[ riesgo=='Requisitos cumplidos para invalidez y muerte', grupo:='Grupo 2']
aux[ riesgo=='Cumple requisitos para vejez en 5 años', grupo:='Grupo 3']
aux[ riesgo=='Cumple requisitos para vejez en 1 año o menos', grupo:='Grupo 4']

setorder( aux, grupo)

genero <- c(rep('F', 4 ))
tipo <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
value <- aux$por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo_inac.pdf')
    , width = .75*textwidth +0 +5
    , height = .75*textwidth +0 +2
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 5, 2)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(40, 25, 16)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(1),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo_inac.pdf'))

# Graficando jefes inactivos por edad, años de imposiciones y riesgo, sexo femenino ----------------
message( '\tGraficando jefes inactivos por edad, años de imposiciones y riesgo, sexo femenino' )
aux <- copy( jef_female_tipo_riesgo_inac )
aux <- aux[ , -1]

df2 <- aux
df2 <- df2[ edad <= 80 & impo_anios <= 45] #Corrección para OIT

#x_lim <- c(0,70)
x_lim <- c( 0, 45 )
x_brk <- seq( x_lim[1], x_lim[2], 5)
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '', decimal.mark = ',' )

#y_lim <- c( -125, -15 )
y_lim <- c(-80,-15)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( -y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

#esc_cl <- paste(colorRampPalette(c('gray35', "yellow"))(4), '50', sep='')
esc_cl <- c('firebrick4', 'gold', 'brown1', 'black')

iess_jef_female_edad_riesgo_inac <- ggplot(df2, aes(x = impo_anios , y = -edad) ) +
  geom_point( aes( size = n_afi , colour = riesgo3 ),alpha=0.5) +
  scale_size(range = c(1,3)) +
  scale_color_manual(values=c( 'Requisitos no cumplidos' = esc_cl[1],
                               'Requisitos cumplidos para invalidez y muerte' = esc_cl[2],
                               'Cumple requisitos para vejez en 5 años' = esc_cl[3],
                               'Cumple requisitos para vejez en 1 año o menos' = esc_cl[4])
  ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  labs( x = 'Imposiciones en años', y = 'Edad' ) +
  guides( size = F) +
  theme_bw() +
  plt_theme_legend + 
  theme(legend.position = c(0.7, 0.85), legend.direction = "vertical",
        legend.key.size = unit(0.2, "cm"), legend.text = element_text( size = 7) ) +
  theme( axis.text.x = element_text( angle = 0 , vjust =1) ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave( plot = iess_jef_female_edad_riesgo_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_jef_female_edad_riesgo_inac', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando jefes de familia inactivos por años de cotizaciones, sexo femenino --------------------
message( '\tGraficando jefes de familia por años de cotizaciones, sexo femenino' )
cotizantes <- copy( Jefes_female_num_cotiz_inac )

x_lim <- c( -0.5, 60 )
x_brk <- seq( x_lim[1]+0.5, x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f')

y_lim <- c ( 0, 11 )
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC(y_brk, digits = 0, format = 'f')

iess_jefe_female_num_cotiz_inac <- ggplot(cotizantes, aes(x=cot, y = num/1000, fill = " ")) +
  geom_bar( data = cotizantes, stat = 'identity',colour="white", size=0.1) +
  xlab( 'Imposiciones en años' ) +
  ylab( 'Miles de personas' ) +
  
  theme_tufte()+
  scale_x_continuous(breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim )+
  theme_bw() +
  plt_theme +
  scale_fill_manual(values = parametros$iess_blue)
  # theme(text = element_text( color = 'black' ),  
  #       legend.text = element_text( size = rel( 0.6), colour = 'black', 
  #                                   face = 'plain', family = tipo_letra ))

ggsave( plot = iess_jefe_female_num_cotiz_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_jefe_female_num_cotiz_inac', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pensionistas --------------------------------------------------------------------------------------
message( '\tGraficando proporcion de pensionistas por sexo y tipo' ) 

valor <- c( jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_vejez,
            jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_vejez,
            jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Jubilados_invalidez,
            jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Jubilados_invalidez)
genero <- c( 'Masculino', 'Femenino', 'Masculino', 'Femenino')

tipo <- c(rep('Vejez', 2), rep('Invalidez', 2))

aux <- data.table( valor, genero, tipo )
setorder( aux, genero)
tot <- aux[ , sum(valor, na.rm=T)]

genero <- c(rep('F', 2 ), rep('M', 2 ))
tipo<- c('Vejez', 'Invalidez', 'Vejez', 'Invalidez')
value <- aux$valor/tot * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pensionistas_tipo.pdf')
    , width = .75*textwidth +0 +0
    , height = .75*textwidth +0 +0
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 0, 0)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(30, 20, 15)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(2, 1, 0),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pensionistas_tipo.pdf'))

# Todos los pensionistas por edad y sexo -----------------------------------------------------------
message( '\tTotal de Pensionistas del SSC' )
vej <- copy( jub_vejez_edad_sexo_ssc )
inv <- copy( jub_invalidez_edad_sexo_ssc )

vej_m <- vej[ sexo=='H']
vej_m[ , sexo:='Male']
vej_m[ , tipo:='Vejez']

inv_m <- inv[ sexo=='H']
inv_m[ , sexo:='Male']
inv_m[ , tipo:='Invalidez']

male_pen <- rbind(vej_m, inv_m)

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, 4000)
y_brk <- seq( y_lim[1], y_lim[2], 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pension_ssc_male <- ggplot( male_pen ) +
  geom_bar(data= male_pen[tipo=='Vejez'], aes(x = edad, y = n, fill='Vejez'),
           stat = 'identity', width = 1, size=0.1, alpha=0.5 ) +
  geom_bar(data= male_pen[tipo=='Invalidez'], aes(x = edad, y = n,fill='Invalidez'),
           stat = 'identity', width = 1, size=0.1, alpha=0.7  ) +
  scale_fill_manual( values=c( 'Vejez' = parametros$iess_blue,
                               'Invalidez' = parametros$iess_green ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) + 
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs(x = NULL, y = NULL, fill = NULL )+ 
  theme_bw() +   plt_theme_legend +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) ) +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(0.5, 'cm')) +
  theme(legend.position = 'bottom', legend.direction = "horizontal",
        legend.key.size = unit(0.5,'cm'),
        #text = element_text(size=10 ),
        legend.spacing.y=unit(-0.9,'cm')) +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  guides( shape = guide_legend(title = NULL)) 

# theme( legend.position = "bottom",legend.direction = "horizontal",
#        axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ), 
#        legend.text = element_text(colour = "black"))

ggsave( plot = iess_pension_ssc_male ,
        filename = paste0( parametros$resultado_graficos, 'iess_pension_ssc_male', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

vej_m <- vej[ sexo=='M']
vej_m[ , sexo:='Female']
vej_m[ , tipo:='Vejez']

inv_m <- inv[ sexo=='M']
inv_m[ , sexo:='Female']
inv_m[ , tipo:='Invalidez']

male_pen <- rbind(vej_m, inv_m)

x_lim <- c( 20, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

y_lim <- c( 0, 1600)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pension_ssc_female <- ggplot( male_pen ) +
  geom_bar(data= male_pen[tipo=='Vejez'], aes(x = edad, y = n, fill='Vejez'),
           stat = 'identity', width = 1, size=0.1, alpha=0.5 ) +
  geom_bar(data= male_pen[tipo=='Invalidez'], aes(x = edad, y = n,fill='Invalidez'),
           stat = 'identity', width = 1, size=0.1, alpha=0.7  ) +
  scale_fill_manual( values=c( 'Vejez' = parametros$iess_blue,
                               'Invalidez' = parametros$iess_green ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim) + 
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  labs(x = NULL, y = NULL, fill = NULL )+ 
  theme_bw() +   plt_theme_legend +
  theme( axis.text.x = element_text(angle = 0, hjust = 0.5 ) ) +
  theme(legend.position="bottom") +
  theme(legend.key.size = unit(0.5, 'cm')) +
  theme(legend.position = 'bottom', legend.direction = "horizontal",
        legend.key.size = unit(0.5,'cm'),
        #text = element_text(size=10 ),
        legend.spacing.y=unit(-0.9,'cm')) +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  guides( shape = guide_legend(title = NULL)) 

ggsave( plot = iess_pension_ssc_female ,
        filename = paste0( parametros$resultado_graficos, 'iess_pension_ssc_female', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Beneficios pagados por pensiones en el SSC--------------------------------------------------------
message( '\tGraficando beneficios pagados por pensionistas por sexo y tipo' ) 

valor <- c( jub_vejez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual,
            jub_vejez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual,
            jub_invalidez_ssc[tipo=='Hombres' & Anio==2020]$Beneficio_pagado_anual,
            jub_invalidez_ssc[tipo=='Mujeres' & Anio==2020]$Beneficio_pagado_anual)
genero <- c( 'Masculino', 'Femenino', 'Masculino', 'Femenino')

tipo <- c(rep('Vejez', 2), rep('Invalidez', 2))

aux <- data.table( valor, genero, tipo )
setorder( aux, genero)
tot <- aux[ , sum(valor, na.rm=T)]

genero <- c(rep('F', 2 ), rep('M', 2 ))
tipo<- c('Vejez', 'Invalidez', 'Vejez', 'Invalidez')
value <- aux$valor/tot * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_gasto_pensional_tipo.pdf')
    , width = .75*textwidth +0 +0
    , height = .75*textwidth +0 +0
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 0, 0)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(30, 20, 15)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(2, 1, 0),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_gasto_pensional_tipo.pdf'))


# 
# message( '\tBeneifico total pagado por pensiones del SSC' )
# aux1 <- copy( jub_vejez_ssc[, .(tipo, Anio, ben_vej=Beneficio_pagado_anual)] )
# aux2 <- copy( jub_invalidez_ssc[, .(tipo, Anio, ben_inv=Beneficio_pagado_anual)] )
# pen <- merge( aux1, aux2,
#               by.x=c('tipo', 'Anio'), by.y=c('tipo', 'Anio'), all.x=TRUE)
# 
# tot_pen <- pen[tipo=='Total' & Anio=='2020']
# t <- rowSums( tot_pen[ , 3:4])
# tot_pen <- data.table( melt(tot_pen, id.vars = c('tipo', 'Anio'), 
#                             value.name = 'pen', variable.name = 'tipo_pen') )
# tot_pen[ , por_pen:= pen/t]
# tot_pen <- tot_pen[ , c('tipo_pen', 'por_pen')]
# tot_pen[ tipo_pen=='ben_vej', tipo_pen:='Vejez']
# tot_pen[ tipo_pen=='ben_inv', tipo_pen:='Invalidez']
# 
# aux <- data.frame( tot_pen )  
# 
# aux <- aux %>% mutate(Por_total = 1) %>% 
#   mutate( end_angle = 2*pi*cumsum(por_pen)/Por_total,      
#           start_angle = lag(end_angle, default = 0),   
#           mid_angle = 0.5*(start_angle + end_angle)) %>% 
#   mutate( aux,
#           hjust = ifelse(mid_angle>pi, 1, 0),
#           vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))
# rpie = 1 
# rlabel = 0.6 * rpie
# rlabel2= 1.05 * rpie
# aux <- data.table( aux)
# 
# iess_tipo_beneficio_ambsex_ssc <- ggplot(aux) + 
#   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
#                    start = start_angle, end = end_angle, fill = tipo_pen),
#                colour='white') +
#   geom_label( 
#     aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#         label = paste0(formatC(round( por_pen*100,2), decimal.mark = ','), '%'),
#     ), family = tipo_letra, size=1, 
#     fill='white', color = "black") +
#   coord_fixed() + scale_fill_brewer(palette="Greens") +
#   scale_x_continuous(limits = c(-1, 1.0), name = "", breaks = NULL, labels = NULL) +
#   scale_y_continuous(limits = c(-1, 1.1), name = "", breaks = NULL, labels = NULL) +
#   theme_bw() +   plt_theme_legend +
#   theme(legend.position = 'bottom', legend.direction = "horizontal",
#         legend.key.size = unit(0.4,'cm'),
#         text = element_text(size = 5 ),
#         legend.spacing.y = unit(-0.1,'cm'),
#         legend.box.spacing=unit(-0.5,'cm') )
# 
# 
# ggsave( plot = iess_tipo_beneficio_ambsex_ssc ,
#         filename = paste0( parametros$resultado_graficos, 'iess_tipo_beneficio_ambsex_ssc', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# #Male
# male_pen <- pen[tipo=='Hombres' & Anio=='2020']
# t <- rowSums( male_pen[ , 3:4])
# male_pen <- data.table( melt(male_pen, id.vars = c('tipo', 'Anio'), 
#                              value.name = 'pen', variable.name = 'tipo_pen') )
# male_pen[ , por_pen:= pen/t]
# male_pen <- male_pen[ , c('tipo_pen', 'por_pen')]
# male_pen[ tipo_pen=='ben_vej', tipo_pen:='Vejez']
# male_pen[ tipo_pen=='ben_inv', tipo_pen:='Invalidez']
# 
# aux <- data.frame( male_pen )  
# 
# aux <- aux %>% mutate(Por_total = 1) %>% 
#   mutate( end_angle = 2*pi*cumsum(por_pen)/Por_total,      
#           start_angle = lag(end_angle, default = 0),   
#           mid_angle = 0.5*(start_angle + end_angle)) %>% 
#   mutate( aux,
#           hjust = ifelse(mid_angle>pi, 1, 0),
#           vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))
# rpie = 1 
# rlabel = 0.6 * rpie
# rlabel2= 1.05 * rpie
# aux <- data.table( aux)
# 
# iess_tipo_beneficio_male_ssc <- ggplot(aux) + 
#   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
#                    start = start_angle, end = end_angle, fill = tipo_pen),
#                colour='white') +
#   geom_label( 
#     aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#         label = paste0(formatC(round( por_pen*100,2), decimal.mark = ','), '%'),
#     ), family = tipo_letra, size=1, 
#     fill='white', color = "black") +
#   coord_fixed() + scale_fill_brewer(palette="Blues") +
#   scale_x_continuous(limits = c(-1, 1.0), name = "", breaks = NULL, labels = NULL) +
#   scale_y_continuous(limits = c(-1, 1.1), name = "", breaks = NULL, labels = NULL) +
#   theme_bw() +   plt_theme_legend +
#   theme(legend.position = 'bottom', legend.direction = "horizontal",
#         legend.key.size = unit(0.4,'cm'),
#         text = element_text(size = 5 ),
#         legend.spacing.y = unit(-0.1,'cm'),
#         legend.box.spacing=unit(-0.5,'cm') )
# 
# ggsave( plot = iess_tipo_beneficio_male_ssc ,
#         filename = paste0( parametros$resultado_graficos, 'iess_tipo_beneficio_male_ssc', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# #Female
# female_pen <- pen[tipo=='Mujeres' & Anio=='2020']
# t <- rowSums( female_pen[ , 3:4])
# female_pen <- data.table( melt(female_pen, id.vars = c('tipo', 'Anio'), 
#                                value.name = 'pen', variable.name = 'tipo_pen') )
# female_pen[ , por_pen:= pen/t]
# female_pen <- female_pen[ , c('tipo_pen', 'por_pen')]
# female_pen[ tipo_pen=='Jubilados_vejez', tipo_pen:='Vejez']
# female_pen[ tipo_pen=='Jubilados_invalidez', tipo_pen:='Invalidez']
# 
# aux <- data.frame( female_pen )  
# 
# aux <- aux %>% mutate(Por_total = 1) %>% 
#   mutate( end_angle = 2*pi*cumsum(por_pen)/Por_total,      
#           start_angle = lag(end_angle, default = 0),   
#           mid_angle = 0.5*(start_angle + end_angle)) %>% 
#   mutate( aux,
#           hjust = ifelse(mid_angle>pi, 1, 0),
#           vjust = ifelse(mid_angle<pi/2 | mid_angle>3*pi/2, 0, 1))
# rpie = 1 
# rlabel = 0.6 * rpie
# rlabel2= 1.05 * rpie
# aux <- data.table( aux)
# 
# iess_tipo_beneficio_female_ssc <- ggplot(aux) + 
#   geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0, r = rpie,
#                    start = start_angle, end = end_angle, fill = tipo_pen),
#                colour='white') +
#   geom_label( 
#     aes(x = rlabel*sin(mid_angle), y = rlabel*cos(mid_angle),
#         label = paste0(formatC(round( por_pen*100,2), decimal.mark = ','), '%'),
#     ), family = tipo_letra, size=1, 
#     fill='white', color = "black") +
#   coord_fixed() + scale_fill_brewer(palette="Greens") +
#   scale_x_continuous(limits = c(-1, 1.0), name = "", breaks = NULL, labels = NULL) +
#   scale_y_continuous(limits = c(-1, 1.1), name = "", breaks = NULL, labels = NULL) +
#   theme_bw() +   plt_theme_legend +
#   theme(legend.position = 'bottom', legend.direction = "horizontal",
#         legend.key.size = unit(0.4,'cm'),
#         text = element_text(size = 5 ),
#         legend.spacing.y = unit(-0.1,'cm'),
#         legend.box.spacing=unit(-0.5,'cm') )
# 
# 
# ggsave( plot = iess_tipo_beneficio_female_ssc,
#         filename = paste0( parametros$resultado_graficos, 'iess_tipo_beneficio_female_ssc', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# #dev.off() 
# M <- matrix( c( rep( 1,2 ), 2:3), nrow = 2, byrow = F ) 
# 
# 
# graf <- grid.arrange(iess_tipo_beneficio_ambsex_ssc,
#                      iess_tipo_beneficio_male_ssc,
#                      iess_tipo_beneficio_female_ssc,
#                      layout_matrix= M,
#                      widths=c(3, 2), # con "widths" configuramos la anchura 
#                      heights=c(3, 3))# con "heights" configuramos la altura
# 
# ggsave( plot = graf,
#         filename = paste0( parametros$resultado_graficos, 'iess_graf', parametros$graf_ext ),
#         width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 
# # M #visualiza la matriz
# # layout(M)
# # layout(M, widths=c(3, 2.5), # con "widths" configuramos la anchura
# # heights = c(3, 3)) # con "heights" configuramos la altura
# # layout.show(3)
#############################################


aux <- copy( past_jef_female_tipo_riesgo_inac )
aux[ riesgo=='Requisitos no cumplidos', grupo:='Grupo 1']
aux[ riesgo=='Requisitos cumplidos para invalidez y muerte', grupo:='Grupo 2']
aux[ riesgo=='Cumple requisitos para vejez en 5 años', grupo:='Grupo 3']
aux[ riesgo=='Cumple requisitos para vejez en 1 año o menos', grupo:='Grupo 4']

setorder( aux, grupo)

genero <- c(rep('F', 4 ))
tipo <- c('Grupo 1', 'Grupo 2', 'Grupo 3', 'Grupo 4')
value <- aux$por * 100
ch <- paste0(formatC(round( value,2), decimal.mark = ','), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue))
                  , c('90', '40'), sep = '')
data <- data.frame( genero, tipo, value, ch, colores  )

pdf(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo_inac.pdf')
    , width = .75*textwidth +0 +5
    , height = .75*textwidth +0 +2
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 5, 2)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("genero","tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("left", "top"), 
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(40, 25, 16)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(1),
        title = ''
)  


invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_jefes_female_riesgo_inac.pdf'))

#Graficando atenciones médicas de los dispensarios del SSC por mes y año -------------------------
message( '\tGraficando atenciones médicas de los dispensarios del SSC por mes y año' )
aux <- copy( aten_med_mes[mes!='Total'] )
aux <- as.data.table( aux )
aux[, mes2:= 1:12]
x_lim <- 1:12
x_brk <- seq(1,12)
x_lbl <- aux$mes

y_lim <- c( 80000, 300000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aten_mes_ssc <- ggplot( ) + 
  geom_line( data = aux, aes( x = mes2, y = A2016,colour ="A2016"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2017,colour ="A2017"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2018,colour ="A2018"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2019,colour ="A2019"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2020,colour ="A2020"),size = graf_line_size ) + 
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl) +
  scale_colour_manual("", 
                      breaks = c("A2016", "A2017", "A2018", "A2019", "A2020"), 
                      
                      values = c("A2016" = parametros$iess_green , 
                                 "A2017" = parametros$iess_blue ,
                                 "A2018" = "orange",
                                 "A2019" = parametros$iess_total,
                                 "A2020"= 'black'
                      ),
                      labels = c("A2016"='2016', "A2017"='2017', "A2018"='2018', 
                                 "A2019"='2019', "A2020"='2020')
  ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) +
  labs( x = 'Mes', y = 'Número de Atenciones' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2 ) )

ggsave( plot = iess_aten_mes_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_mes_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando atenciones de los dispensarios del SSC por mes y día de todos los anios ---------------
message( '\tGraficando atenciones de los dispensarios del SSC por mes y día de todos los anios' )
aux <- copy( aten_med_dia )
aux <- aux[ mes=='Total general']
aux <- melt( aux[ , -c('mes', 'total') ], id.vars = 'anio', 
             variable.name = 'dia', value.name = 'valor')
aux <- data.table( dcast( aux , dia ~ anio, value.var = 'valor',fun.aggregate =  sum, na.rm = TRUE ) ) 
colnames(aux) <- c('dia','A2016','A2017', 'A2018', 'A2019', 'A2020')
aux[ , dia2:= 1:7]

x_lim <- 1:7
x_brk <- seq(1,7)
x_lbl <- aux$dia

y_lim <- c( 0, 700000)
y_brk <- seq( y_lim[1], y_lim[2], 100000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aten_dia_ssc <- ggplot(aux, aes( x = dia2 ) ) + 
  geom_line(aes(y = A2016, colour ="A2016"),size = graf_line_size ) + 
  geom_line(aes(y = A2017, colour ="A2017"),size = graf_line_size ) + 
  geom_line(aes(y = A2018, colour ="A2018"),size = graf_line_size ) + 
  geom_line(aes(y = A2019, colour ="A2019"),size = graf_line_size ) +
  geom_line(aes(y = A2020, colour ="A2020"),size = graf_line_size ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl) +
  scale_colour_manual("", 
                      breaks = c("A2016", "A2017", "A2018", "A2019", "A2020"), 
                      values = c("A2016" = parametros$iess_green , 
                                 "A2017" = parametros$iess_blue ,
                                 "A2018" = "orange",
                                 "A2019" = parametros$iess_total,
                                 "A2020"= 'black'
                      ),
                      labels = c("A2016"='2016', "A2017"='2017', "A2018"='2018', 
                                 "A2019"='2019', "A2020"='2020')
  ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) +
  labs( x = 'Mes', y = 'Número de Atenciones' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2 ) )

ggsave( plot = iess_aten_dia_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_dia_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando atenciones por edad de todos los anios ------------------------------------------------
message( '\tGraficando atenciones por edad de todos los anios' )
aux <- copy( aten_med_edad )
aux <- aux[ edad <= 105]

x_lim <- c( 0, 105 )
x_brk <- seq( x_lim[1], x_lim[2], 5 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 60000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aten_edad_ssc <- ggplot( aux, aes(edad)) + 
  geom_line(aes(y = A2016,colour ="A2016"),size = graf_line_size ) + 
  geom_line(aes(y = A2017,colour ="A2017"),size = graf_line_size ) + 
  geom_line(aes(y = A2018,colour ="A2018"),size = graf_line_size ) + 
  geom_line(aes(y = A2019,colour ="A2019"),size = graf_line_size ) +
  geom_line(aes(y = A2020,colour ="A2020"),size = graf_line_size ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_colour_manual("", 
                      breaks = c("A2016", "A2017", "A2018", "A2019", "A2020"), 
                      values = c("A2016" = parametros$iess_green , 
                                 "A2017" = parametros$iess_blue ,
                                 "A2018" = "orange",
                                 "A2019" = parametros$iess_total,
                                 "A2020"= 'black'
                      ),
                      labels = c("A2016"='2016', "A2017"='2017', "A2018"='2018', 
                                 "A2019"='2019', "A2020"='2020')
  ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1, ncol = 5, override.aes = list(size = 2))) +
  labs( x = 'Edad', y = 'Número de Pacientes' )+
  theme( axis.text.x = element_text( angle = 90, hjust = 0.5, vjust=0.5 ),
         legend.box.spacing=  unit('0.8', 'cm'))

ggsave( plot = iess_aten_edad_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_edad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando atenciones de los dispensarios por edad y sexo ----------------------------------------
message( '\tGraficando atenciones de los dispensarios por edad y sexo año 2016' )
aux1 <- copy( aten_med_edad_sexo )
aux <- aux1[ anio == 2016 ]

max_edad <- 105
min_edad <- 0

aux <- aux[edad >= min_edad & edad <=max_edad] 
aux[is.na(n_pac), n_pac:=0]  

N <- data.frame((aux[,sum(n_pac,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n_pac:=-n_pac]
aux[sexo=="H", n_pac:=n_pac/N[1,1]]
aux[sexo=="M", n_pac:=n_pac/N[1,1]]

M <- data.frame((aux[,max(abs(n_pac),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_aten_2016 <- ggplot( aux, aes(x = edad, y = n_pac, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( 'Año 2016' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_aten_2016, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aten_2016', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_aten_2016', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+580+60")
final_plot_2016 <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1001+70")

image_write(final_plot_2016, paste0( parametros$resultado_graficos, 'iess_pir_aten_2016', parametros$graf_ext ))

message( '\tGraficando atenciones de los dispensarios por edad y sexo año 2017' )
aux1 <- copy( aten_med_edad_sexo )
aux <- aux1[ anio == 2017 ]

max_edad <- 105
min_edad <- 0

aux <- aux[edad >= min_edad & edad <=max_edad] 
aux[is.na(n_pac), n_pac:=0]  

N <- data.frame((aux[,sum(n_pac,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n_pac:=-n_pac]
aux[sexo=="H", n_pac:=n_pac/N[1,1]]
aux[sexo=="M", n_pac:=n_pac/N[1,1]]

M <- data.frame((aux[,max(abs(n_pac),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_aten_2017 <- ggplot( aux, aes(x = edad, y = n_pac, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( 'Año 2017' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_aten_2017, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aten_2017', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_aten_2017', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+560+60")
final_plot_2017 <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1001+70")

image_write(final_plot_2017, paste0( parametros$resultado_graficos, 'iess_pir_aten_2017', parametros$graf_ext ))

message( '\tGraficando atenciones de los dispensarios por edad y sexo año 2018' )
aux1 <- copy( aten_med_edad_sexo )
aux <- aux1[ anio == 2018 ]

max_edad <- 105
min_edad <- 0

aux <- aux[edad >= min_edad & edad <=max_edad] 
aux[is.na(n_pac), n_pac:=0]  

N <- data.frame((aux[,sum(n_pac,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n_pac:=-n_pac]
aux[sexo=="H", n_pac:=n_pac/N[1,1]]
aux[sexo=="M", n_pac:=n_pac/N[1,1]]

M <- data.frame((aux[,max(abs(n_pac),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_aten_2018 <- ggplot( aux, aes(x = edad, y = n_pac, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( 'Año 2018' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_aten_2018, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aten_2018', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_aten_2018', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+560+60")
final_plot_2018 <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1001+70")

image_write(final_plot_2018, paste0( parametros$resultado_graficos, 'iess_pir_aten_2018', parametros$graf_ext ))

message( '\tGraficando atenciones de los dispensarios por edad y sexo año 2019' )
aux1 <- copy( aten_med_edad_sexo )
aux <- aux1[ anio == 2019 ]

max_edad <- 105
min_edad <- 0

aux <- aux[edad >= min_edad & edad <=max_edad] 
aux[is.na(n_pac), n_pac:=0]  

N <- data.frame((aux[,sum(n_pac,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n_pac:=-n_pac]
aux[sexo=="H", n_pac:=n_pac/N[1,1]]
aux[sexo=="M", n_pac:=n_pac/N[1,1]]

M <- data.frame((aux[,max(abs(n_pac),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_aten_2019 <- ggplot( aux, aes(x = edad, y = n_pac, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( 'Año 2019' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_aten_2019, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aten_2019', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_aten_2019', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+500+60")
final_plot_2019 <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+900+70")

image_write(final_plot_2019, paste0( parametros$resultado_graficos, 'iess_pir_aten_2019', parametros$graf_ext ))

message( '\tGraficando atenciones de los dispensarios por edad y sexo año 2020' )
aux1 <- copy( aten_med_edad_sexo )
aux <- aux1[ anio == 2020 ]

max_edad <- 105
min_edad <- 0

aux <- aux[edad >= min_edad & edad <=max_edad] 
aux[is.na(n_pac), n_pac:=0]  

N <- data.frame((aux[,sum(n_pac,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n_pac:=-n_pac]
aux[sexo=="H", n_pac:=n_pac/N[1,1]]
aux[sexo=="M", n_pac:=n_pac/N[1,1]]

M <- data.frame((aux[,max(abs(n_pac),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x <- 0.005
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_aten_2020 <- ggplot( aux, aes(x = edad, y = n_pac, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( 'Año 2020' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green))

ggsave( plot = iess_pir_aten_2020, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_aten_2020', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_aten_2020', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+500+60")
final_plot_2020 <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+901+70")

image_write(final_plot_2020, paste0( parametros$resultado_graficos, 'iess_pir_aten_2020', parametros$graf_ext ))


plt_aten_med <- marrangeGrob( list( rasterGrob(final_plot_2016, interpolate=TRUE), 
                                    rasterGrob(final_plot_2019, interpolate=TRUE),
                                    rasterGrob(final_plot_2017, interpolate=TRUE),
                                    rasterGrob(final_plot_2020, interpolate=TRUE),
                                    rasterGrob(final_plot_2018, interpolate=TRUE)
                                    ),
                              nrow = 2, ncol = 3, top = '' )
#iess_aten_edad_ssc
ggsave( plot = plt_aten_med, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_aten_med_ssc', parametros$graf_ext ),
        width = 24, height = 15, units = graf_units, dpi = graf_dpi )

#Graficando atenciones médicas de los dispensarios del año 2020 ------------------------------------
message( '\tGraficando atenciones médicas de los dispensarios del año 2020' )
aux <- copy( aten_med_prov )
aux <- aux[prov!='Zona no definida' & prov!='Total', list( prov, A2020freq, A2020P)]
aux[ , A2020freq:=as.numeric(A2020freq)]
aux[ , A2020P:=as.numeric(A2020P)*100]
aux[ prov=="Bol\\'\\i{}var" , prov:='Bolivar']
aux[ prov=="Ca\\~{n}ar" , prov:='Cañar']
aux[ prov=="Gal\\'apagos" , prov:='Galápagos']
aux[ prov=="Los R\\'\\i{}os" , prov:='Los Ríos']
aux[ prov=="Manab\\'\\i{}" , prov:='Manabí']
aux[ prov=="Santo Domingo de los Ts\\'achilas" , prov:='S.D. Tsáchilas']
aux[ prov=="Sucumb\\'\\i{}os" , prov:='Sucumbíos']

y_lim <- c( 000000, 600000)
y_brk <- seq( y_lim[1], y_lim[2],by=100000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_fre20_provincia_ssc <- ggplot(data=aux, aes(x=reorder(prov, A2020P), y = A2020freq)) + 
  geom_bar(stat = "identity", width=0.9, fill = parametros$iess_green, alpha = 0.5) +
  geom_text( aes(label = formatC( A2020freq, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')), 
             colour="blue", size = 2.0, vjust=0.5, hjust= 0,
             position = position_fill(vjust=0.5)) +
  #labs(title= 'Atenciones Médicas del SSC por provincias, 2017')+
  ylab('Frecuencia') +
  xlab('Provincia') +
  coord_flip() +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme 

ggsave( plot = iess_fre20_provincia_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_fre20_provincia_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando atenciones médicas por zonas de los dispensarios --------------------------------------
message( '\tGraficando atenciones médicas por zonas de los dispensarios' )
aux <- copy( aten_med_zonas )
aux <- aux[ zona!='Total']
iess_zona_dis_ssc <- ggplot( aux , aes( x = reorder(zona , n), y = n, fill=zona ) ) +
  geom_bar(stat="identity", position="dodge", fill = parametros$iess_green, alpha= 0.5,width=0.6) +
  ylab(' Número de Dispensarios') +
  xlab('Zonas') +
  coord_flip() +
  geom_text(aes(label=formatC(n, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')),
            colour="blue", size = 3.0, vjust=0.5, hjust= 0,
            position = position_fill(vjust=0.5)) +
  #labs(title= 'Distribución de dispensarios del SSC a nivel nacional')+
  theme_bw() +
  plt_theme +
  #theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1, ncol = 3, override.aes = list(size = 2))) 

ggsave( plot = iess_zona_dis_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_zona_dis_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando atenciones médicas de los dispensarios del SSC por año y sexo -------------------------
message( '\tGraficando atenciones médicas de los dispensarios del SSC por año y sexo' )
aux <- copy( aten_med_zonas_sexo ) 
aux <- aux[ zona=='Total']
aux <- data.table( melt( aux, id.vars = 'zona', variable.name = 'sexo', value.name = 'n') )
aux[ , sexo:=as.character( sexo )]
aux[ sexo %in% grep(pattern='16', aux$sexo, value = T), anio:='2016']
aux[ sexo %in% grep(pattern='17', aux$sexo, value = T), anio:='2017']
aux[ sexo %in% grep(pattern='18', aux$sexo, value = T), anio:='2018']
aux[ sexo %in% grep(pattern='19', aux$sexo, value = T), anio:='2019']
aux[ sexo %in% grep(pattern='20', aux$sexo, value = T), anio:='2020']
aux[ sexo %in% grep(pattern='H', aux$sexo, value = T), sex:='Hombre']
aux[ sexo %in% grep(pattern='M', aux$sexo, value = T), sex:='Mujer']
aux <- aux[ !is.na(sex), list(anio, sex, n)]
aux[ , n:=as.numeric(n)]

y_lim <- c( 0, 2500000)
y_brk <- seq( y_lim[1], y_lim[2], 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_aten_sexo_ssc <- ggplot( aux, aes(x=anio, y=n,fill=sex)) + 
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=formatC(n, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',')),
            size = 2.3, vjust = - 0.8,
            position = position_dodge(0.9)) +
  #labs(title= 'Atención médica de los dispensarios del SSC por año y sexo') + 
  ylab('Atenciones') + xlab('Año')+
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
     #legend.position = c(0.8, 0.2)
  scale_fill_manual('', breaks=c('Hombre', 'Mujer'),
                    values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres") ) +
  theme(legend.position="none")

ggsave( plot = iess_aten_sexo_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_sexo_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

plot <- image_read( paste0( parametros$resultado_graficos, 'iess_aten_sexo_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )
image_info( fig_H)
fig_H <- image_scale( fig_H, "x100")
fig_H <- fig_H %>%
  image_scale("100") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "330x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+200-30") %>%
  image_crop("430x150+325+0")

fig_M <- image_read( paste0( parametros$resultado_graficos, 'mujer.png' ) )
fig_M <- image_scale( fig_M, "x100")

fig_M <- fig_M %>%
  image_scale("125") %>%
  image_background("white", flatten = TRUE) %>%
  image_border("white", "170x2") %>%
  image_annotate( text = "", size = 45, color = "black", font="Times",
                  gravity = "center", location = "+140-25"
  ) %>%
  #image_crop("430x150+325+0")
  image_crop("280x150+165+0")

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+38")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+900+38")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_aten_sexo_ssc', parametros$graf_ext ))


# Graficando atenciones médicas de los dispensarios del SSC por año y código CIE-10 ----------------
message( '\tGraficando atenciones médicas de los dispensarios del SSC por año y código CIE-10')
aux <- copy( aten_med_cie )
aux <- data.table( melt( aux, id.vars = c('cap','cod', 'cie') , variable.name = 'anio', value.name = 'n' ) )
aux <- aux[ anio%in%grep(pattern='P', aux$anio, value = T)]
aux[ , n:=n*100]
aux[ , anio:=as.character(anio)]

y_lim <- c( 0, 60)
y_brk <- seq( y_lim[1], y_lim[2],  10 )
y_lbl <- formatC( y_brk, digits = 3, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_morbilidad_ssc <- ggplot( aux, aes( x = reorder(cap, -n), y = n, fill = anio ) ) + 
  geom_bar(stat="identity", position="dodge") +
  #labs(title= 'Morbilidad por capítulo del CIE-10') + 
  xlab( 'Capítulo CIE-10' ) +
  ylab( 'Porcentaje (%)' ) +
  scale_y_continuous( limits = y_lim, breaks = y_brk, labels = y_lbl) +
  theme_bw() +                      
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom",legend.box.spacing=unit(0.8,"cm")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) ) +
  scale_fill_manual('', breaks = c('P2016', 'P2017', 'P2018', 'P2019', 'P2020'),
                        values = c( parametros$iess_green, parametros$iess_blue, "orange", parametros$iess_total, 'black' ),
                        labels = c("2016", "2017", "2018", '2019', '2020'))

ggsave( plot = iess_morbilidad_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_morbilidad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Perfil Epidemiológico -----------------------------------------------------------------------------
# Graficando pasteles de atenciones médicas por años -----------------------------------------------


aux <- copy( aten_med_mes[mes!='Total'] )
aux <- as.data.table( aux )
aux <- data.table( melt( aux, id.vars = "mes" , variable.name = 'anio', value.name = 'n' ) )
aux[ , anio:=as.character(anio)]
aux <- aux[ , list( n =sum(n, na.rm=T)), by=list(anio )]
aux[ , por := n/sum( aux$n) *100 ]
setorder( aux, anio )

tipo <- c('2016', '2017', '2018', '2019','2020')
value <- aux$por 
ch <- paste0(round(value,2), '%')
colores <-  paste(col2hex(c( parametros$iess_green, parametros$iess_green, parametros$iess_blue, parametros$iess_blue,"orange"))
                  , c('90', '40'), sep = '')
data <- data.frame( tipo, value, ch, colores  )


pdf(paste0(parametros$resultado_graficos,'iess_pastel_aten_med_anios.pdf')
    , width = .75*textwidth +0 +5
    , height = .75*textwidth +0 +2
    , family = 'CM Roman'
)
par(pin = c(textwidth, .75*textwidth)
    , mai = c(0, 0, 5, 2)
    , cex = .7
    , xpd = TRUE
)

treemap(data,
        index=c("tipo", "ch"),
        vSize="value",
        vColor = "colores",
        type="color",
        bg.labels=0,
        align.labels=list(
          c("center", "top"), 
          c("center", "center")
        )  
        , fontsize.labels = c(30, 25, 10)
        , fontface.labels = 'plain'
        , fontcolor.labels = "black"
        , fontfamily.labels = 'serif',
        border.lwds = c(1),
        title = ''
)  

invisible(dev.off())
embed_fonts(paste0(parametros$resultado_graficos,'iess_pastel_aten_med_anios.pdf'))



message( '\tGraficando pasteles de atenciones médicas por años')
aux <- copy( aten_med_mes[mes!="Total"] )
aux <- as.data.table( aux )
totales <- c(sum(aux$A2016),sum(aux$A2017), sum(aux$A2018), sum(aux$A2019), sum(aux$A2020))
total <- sum (totales)
porcentaje <- (round((totales/total),2))*100
lab <- c("2016","2017","2018","2019","2020")
lab <- paste(lab,"- ")
años <- paste0(lab, porcentaje, "%")

data <- data.frame(totales,porcentaje)                 
iess_aten_anios <- ggplot(data, aes(x="", y=porcentaje, fill=años)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

ggsave( plot = iess_aten_anios  ,
        filename = paste0( parametros$resultado_graficos, 'iess_aten_anios', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando barras de atenciones para el año 2020 -------------------------------------------------

aux <- copy( aten_med_mes[mes!='Total'] )
aux <- as.data.table( aux )
aux[, mes2:= 1:12]
aux <- data.table( melt( aux, id.vars = c('mes','mes2') , variable.name = 'anio', value.name = 'n' ) )
aux[ , anio:=as.character(anio)]
aux  <- aux[anio=="A2020", ]
y_lim <- c( 0, 300000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aten_med_2020 <- ggplot( aux, aes( x = reorder(mes, mes2), y = n, fill = anio ) ) + 
  geom_bar(stat="identity", position="dodge")  +
  xlab(' ')+
  ylab( 'Pacientes atendidos' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position=" ",legend.box.spacing=unit(0.8,"cm")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) ) +
  scale_fill_manual('',
                    values =parametros$iess_green
                   )
ggsave( plot = iess_aten_med_2020, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_med_2020', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




# Graficando atenciones medicas 2016 - 2019 rectas----------------------------------------------------------

message( '\tGraficando atenciones médicas de los dispensarios del SSC por mes y año' )
aux <- copy( aten_med_mes[mes!='Total'] )
aux <- as.data.table( aux )
aux[, mes2:= 1:12]
x_lim <- 1:12
x_brk <- seq(1,12)
x_lbl <- aux$mes

y_lim <- c( 80000, 300000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_aten_mes_ssc_16_19 <- ggplot( ) + 
  geom_line( data = aux, aes( x = mes2, y = A2016,colour ="A2016"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2017,colour ="A2017"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2018,colour ="A2018"),size = graf_line_size ) + 
  geom_line( data = aux, aes( x = mes2, y = A2019,colour ="A2019"),size = graf_line_size ) + 
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl) +
  scale_colour_manual("", 
                      breaks = c("A2016", "A2017", "A2018", "A2019"), 
                      
                      values = c("A2016" = parametros$iess_green , 
                                 "A2017" = parametros$iess_blue ,
                                 "A2018" = "orange",
                                 "A2019" = parametros$iess_total
                      ),
                      labels = c("A2016"='2016', "A2017"='2017', "A2018"='2018', 
                                 "A2019"='2019')
  ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1, override.aes = list(size = 4))) +
  labs( x = ' ', y = 'Número de Atenciones' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.2 ) )

ggsave( plot = iess_aten_mes_ssc_16_19, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_mes_ssc_16_19', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando atenciones medicas 2016 - 2019 barras----------------------------------------------------------
aux <- copy( aten_med_mes[mes!='Total'] )
aux <- as.data.table( aux )
aux[, mes2:= 1:12]
aux <- data.table( melt( aux, id.vars = c('mes','mes2') , variable.name = 'anio', value.name = 'n' ) )
aux[ , anio:=as.character(anio)]
aux  <- aux[anio!="A2020", ]
y_lim <- c( 0, 300000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
iess_aten_med_2016_2019 <- ggplot( aux, aes( x = reorder(mes, mes2), y = n, fill = anio ) ) + 
  geom_bar(stat="identity", position="dodge") + 
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  xlab( ' ' ) +
  ylab( 'Pacientes atendidos' ) +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom",legend.box.spacing=unit(0.8,"cm")) +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) ) +
  scale_fill_manual('', breaks = c('A2016', 'A2017', 'A2018', 'A2019'),
                    values = c( parametros$iess_green, parametros$iess_blue, "orange", parametros$iess_total),
                    labels = c("2016", "2017", "2018", '2019'))

ggsave( plot = iess_aten_med_2016_2019, 
        filename = paste0( parametros$resultado_graficos, 'iess_aten_med_2016_2019', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )





message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

