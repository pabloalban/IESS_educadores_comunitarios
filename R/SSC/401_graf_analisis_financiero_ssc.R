message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando análisis financiero SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------

load( file = paste0( parametros$RData_seg, 'IESS_SSC_analisis_financiero.RData' ) )

# Graficando variación de activos SSC--------------------------------------------------------------
message( '\tGraficando variación de activos SSC' )

unidad<-1e6
aux <-copy( activos_hist_ssc[, .(Anio, `Activo`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,Activo:=Activo/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 200, 2000)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_activos_hist_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Activo`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_activos_hist_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_activos_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando variación de cuentas por cobrar SSC--------------------------------------------------------------
message( '\tGraficando variación de cuentas por cobrar SSC' )

unidad<-1e6
aux <-copy( variacion_cuentas_por_cobrar_ssc[, .(`Año`,`Cuentas por Cobrar`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,`Cuentas por Cobrar`:=`Cuentas por Cobrar`/unidad]
aux <- aux[,`Año`:=as.numeric(`Año`)]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 100, 700)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_variacion_cuentas_por_cobrar_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = `Año`, 
                  y = `Cuentas por Cobrar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_variacion_cuentas_por_cobrar_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_variacion_cuentas_por_cobrar_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando variación de pasivos SSC--------------------------------------------------------------
message( '\tGraficando variación de pasivos SSC' )

unidad<-1e6
aux <-copy( pasivos_hist_ssc[, .(Anio, `Pasivo`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,Pasivo:=Pasivo/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 20, 160)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_pasivos_hist_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Pasivo`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_pasivos_hist_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pasivos_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando variación de cuentas por pagar SSC--------------------------------------------------------------
message( '\tGraficando variación de cuentas por pagar SSC' )

unidad<-1e6
aux <-copy( variacion_cuentas_por_pagar_ssc[, .(`Año`,`Cuentas por Pagar`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,`Cuentas por Pagar`:=`Cuentas por Pagar`/unidad]
aux <- aux[,`Año`:=as.numeric(`Año`)]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 20, 120)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_variacion_cuentas_por_pagar_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = `Año`, 
                  y = `Cuentas por Pagar`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_variacion_cuentas_por_pagar_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_variacion_cuentas_por_pagar_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando variación de patrimonio SSC--------------------------------------------------------------
message( '\tGraficando variación de patrimonio SSC' )

unidad<-1e6
aux <-copy( patrimonio_hist_ssc[, .(Anio, `Patrimonio`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,`Patrimonio`:=`Patrimonio`/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 200, 2000)
y_brk <- seq( y_lim[1], y_lim[2], 200 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_patrimonio_hist_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Patrimonio`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_patrimonio_hist_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_patrimonio_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Graficando variación de Ingresos SSC--------------------------------------------------------------
message( '\tGraficando variación de Ingresos SSC' )

unidad<-1e6
aux <-copy( Ingresos_hist_ssc[, .(Anio, `Ingresos`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,`Ingresos`:=`Ingresos`/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 500)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_Ingresos_hist_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Ingresos`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_Ingresos_hist_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_Ingresos_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando variación de Aportes SSC--------------------------------------------------------------
message( '\tGraficando variación de Aportes SSC' )

unidad<-1e6
aux <-copy( Aportes_hist_ssc[, .(Anio, `Aportes IESS`, `Incremento Anual`)] )
aux <- aux[,`Incremento Anual`:=`Incremento Anual`/unidad]
aux <- aux[,`Aportes IESS`:=`Aportes IESS`/unidad]

x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 400)
y_brk <- seq( y_lim[1], y_lim[2], 50 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_Aportes_hist_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = Anio, 
                  y = `Aportes IESS`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Incremento Anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_Aportes_hist_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_Aportes_hist_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Evolución de los egresos prestacionales del Fondo de SSC --------------------------------------------------------------
message( '\tGraficando Evolución de los egresos prestacionales del Fondo de SSC ' )

unidad<-1e6
aux <-copy( egresos_prestacionales_ssc[,`Egresos Prestacionales`:=`Egresos Prestacionales`/unidad])


x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 00, 180)
y_brk <- seq( y_lim[1], y_lim[2], 20 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_egresos_prestacionales_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = `Año`, 
                  y = `Egresos Prestacionales`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Variación anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_egresos_prestacionales_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_egresos_prestacionales_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución de los gastos de administacion del Fondo de SSC --------------------------------------------------------------
message( '\tGraficando Evolución de los gastos de administacion del Fondo de SSC ' )

unidad<-1e6
aux <-copy( gastos_de_administacion_ssc[,`Gastos de Administración`:=`Gastos de Administración`/unidad])


x_lim <- c( 2010, 2020 )
x_brk <- 2010:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 16)
y_brk <- seq( y_lim[1], y_lim[2], 2 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', decimal.mark = '.' )

iess_gastos_de_administacion_ssc <- ggplot( data = aux ) + 
  geom_line( aes( x = `Año`, 
                  y = `Gastos de Administración`, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Variación anual (Millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 ) )

ggsave( plot = iess_gastos_de_administacion_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_gastos_de_administacion_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

###PASTEL
# Porcentaje promedio de representatividad de los egresos prestacionales de SSC --------------------------------------------------------------
message( '\tGraficando Evolución de los gastos de administacion del Fondo de SSC ' )

aux <-copy( porcentaje_promedio_egresos_pres_ssc)
colnames(aux)<-c("Cuentas","...2")

# aux<-data.frame(A=c("Egresos Prestacionales","Gastos de Administración (contribución)","Otros Gastos"),
                # B=c("0.5163","0.0468","0.4369"))

  iess_porcentaje_promedio_egresos_pres_ssc <- ggplot(aux ,aes(x=" ",y=`...2`, fill=`Cuentas`))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(`...2`)),
            position=position_stack(vjust=0.5),color="black",size=4)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c(parametros$iess_green,"yellow",parametros$iess_blue))+
  theme_void()+
  labs(title=" ")

ggsave( plot = iess_porcentaje_promedio_egresos_pres_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_promedio_egresos_pres_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )






























##########------------------------------------------------------------------------------------------------------------------------------------------
## BARRAS HORIZONTALES
###Graficos porcentaje promedio de representatividad de los componentes del Activo ----------------------------------------------------------
 message( '\tGraficos porcentaje promedio de representatividad de los componentes del Activo' )

aux<-copy(porcentaje_promedio_representatividad_ssc[,`...2`:=`...2`/100])
aux<-aux[order(`...2`,decreasing = F),]
aux<-aux[,`...2`:=round(`...2`,4)]

c<-as.vector(1:10)
c[1]<-"0,0003%"
c[2]<-"0,0025%"
c[3]<-"0,08%"
c[4]<-"0,73%"
c[5]<-"1,91%"
c[6]<-"2,20%"
c[7]<-"4,11%"
c[8]<-"22,10%"
c[9]<-"25,01%"
c[10]<-"62,54%"


iess_porcentaje_promedio_representatividad_ssc<-ggplot(aux, aes(x = `...1`, y = `...2`)) +
  xlab( '' ) +
  ylab( '' ) +
  geom_bar( data = aux,
            stat = 'identity',
            colour="white",
            size=0.1, fill = parametros$iess_green) +
  
  geom_text(aes( label = c,
                 y= `...2`), stat= 'identity', nudge_y =0.04, size=2)+
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")



ggsave( plot = iess_porcentaje_promedio_representatividad_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_promedio_representatividad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
##########------------------------------------------------------------------------------------------------------------------------------------------
###Graficos porcentaje promedio de representatividad de los componentes del Pasivo ----------------------------------------------------------
message( '\tGraficos porcentaje promedio de representatividad de los componentes del Pasivo' )

aux<-copy(porcentaje_promedio_pasivos_representatividad_ssc[,`...2`:=`...2`/100])
aux<-aux[order(`...2`,decreasing = F),]
aux<-aux[,`...2`:=round(`...2`,2)]

c<-as.vector(1:7)
c[1]<-"0,06%"
c[2]<-"8,24%"
c[3]<-"33,10%"
c[4]<-"33,24%"
c[5]<-"33,42%"
c[6]<-"61,83%"
c[7]<-"66,58%"



iess_porcentaje_promedio_pasivos_representatividad_ssc<-ggplot(aux, aes(x = `...1`, y = `...2`)) +
  xlab( '' ) +
  ylab( '' ) +
  geom_bar( data = aux,
            stat = 'identity',
            colour="white",
            size=0.1, fill = parametros$iess_green) +
  
  geom_text(aes( label = c,
                 y= `...2`), stat= 'identity', nudge_y =0.04, size=2)+
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")



ggsave( plot = iess_porcentaje_promedio_pasivos_representatividad_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_promedio_pasivos_representatividad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

###Graficos porcentaje promedio de representatividad de los componentes del Patrimonio ----------------------------------------------------------
message( '\tGraficos porcentaje promedio de representatividad de los componentes del Patrimonio' )

aux<-copy(porcentaje_promedio_patrimonio_representatividad_ssc[,`...2`:=`...2`/100])
aux<-aux[order(`...2`,decreasing = F),]
aux<-aux[,`...2`:=round(`...2`,2)]

c<-as.vector(1:6)
c[1]<-"-0,55%"
c[2]<-"0,00%"
c[3]<-"0,01%"
c[4]<-"1,47%"
c[5]<-"24,33%"
c[6]<-"75,29%"

iess_porcentaje_promedio_patrimonio_representatividad_ssc<-ggplot(aux, aes(x = `...1`, y = `...2`)) +
  xlab( '' ) +
  ylab( '' ) +
  geom_bar( data = aux,
            stat = 'identity',
            colour="white",
            size=0.1, fill = parametros$iess_green) +
  
  geom_text(aes( label = c,
                 y= `...2`), stat= 'identity', nudge_y =0.04, size=2)+
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")



ggsave( plot = iess_porcentaje_promedio_patrimonio_representatividad_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_promedio_patrimonio_representatividad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

###Graficos porcentaje promedio de representatividad de los componentes del Ingresos ----------------------------------------------------------
message( '\tGraficos porcentaje promedio de representatividad de los componentes del Ingresos' )

aux<-copy(porcentaje_promedio_Ingresos_representatividad_ssc[,`...2`:=`...2`/100])
aux<-aux[order(`...2`,decreasing = F),]
aux<-aux[,`...2`:=round(`...2`,2)]

c<-as.vector(1:5)
c[1]<-"0,01%"
c[2]<-"0,02%"
c[3]<-"2,48%"
c[4]<-"16,06%"
c[5]<-"82,13%"


iess_porcentaje_promedio_Ingresos_representatividad_ssc<-ggplot(aux, aes(x = `...1`, y = `...2`)) +
  xlab( '' ) +
  ylab( '' ) +
  geom_bar( data = aux,
            stat = 'identity',
            colour="white",
            size=0.1, fill = parametros$iess_green) +
  
  geom_text(aes( label = c,
                 y= `...2`), stat= 'identity', nudge_y =0.04, size=2)+
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")



ggsave( plot = iess_porcentaje_promedio_Ingresos_representatividad_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_promedio_Ingresos_representatividad_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


###Graficos porcentaje Aportes SSC ----------------------------------------------------------
message( '\tGraficos porcentaje Aportes SSC' )


aux<-copy(porcentaje_de_Aportes_ssc)
aux<-aux[,`% de Participación`:=round(`% de Participación`,4)]
aux<-aux[c(1:12),]

c<-as.vector(1:12)
c[1]<-"A"
c[2]<-"B"
c[3]<-"C"
c[4]<-"D"
c[5]<-"E"
c[6]<-"F"
c[7]<-"G"
c[8]<-"H"
c[9]<-"I"
c[10]<-"J"
c[11]<-"K"
c[12]<-"L"
c<-as.data.frame(c)

v<-as.vector(1:12)
v[12]<-"0,01%"
v[11]<-"1%"
v[10]<-"0,1%"
v[9]<-"0,5%"
v[8]<-"0,1%"
v[7]<-"5%"
v[6]<-"26%"
v[5]<-"4%"
v[4]<-"0,2%"
v[3]<-"3%"
v[2]<-"24%"
v[1]<-"35%"

aux<-cbind(aux,c)

iess_porcentaje_de_Aportes_ssc<-ggplot(aux, aes(x = `c`, y = `% de Participación`)) +
  xlab( '' ) +
  ylab( '' ) +
  geom_bar( data = aux,
            stat = 'identity',
            colour="white",
            size=0.1, fill = parametros$iess_green) +
  
  geom_text(aes( label = v,
                 y= `% de Participación`), stat= 'identity', nudge_y =0.04, size=2)+
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")


ggsave( plot = iess_porcentaje_de_Aportes_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_porcentaje_de_Aportes_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()



