message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada activa del TNRH del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_TNRH_analisis_demografico.RData' ) )

# Graficos Afiliados Activos------------------------------------------------------------------------
#unidad<-1e6
aux<-copy(afi_acti2_tnrh[, .(anio, Afiliados_Activos)] )

x_lim <- c( 2015, 2020 )
x_brk <- 2015:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 82000, 308000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

afi_acti2_tnrh <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados_Activos, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Afiliados' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

# afi_acti2_tnrh 
ggsave( plot = afi_acti2_tnrh, 
        filename = paste0( parametros$resultado_graficos, 'afi_acti2_tnrh', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos de la pirámide de Afiliados Activos------------------------------------------------------

message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )

aux<-copy(pob_afi_edad_sexo_tnrh)
max_edad<-73
min_edad<-18
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

aux[sexo=="M", n:=n]

M <- data.frame(aux) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.01
brks_y <- seq(0.01, 0.3,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor de 69')


iess_pir_afiliados_tnrh<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_green),
                    labels = c("Mujeres"))

ggsave( plot = iess_pir_afiliados_tnrh,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_tnrh', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Masa salarial-------------------------------------------------------------------------------------

message( '\tGraficando masa salarial inicial SGO del IESS' )

unidad<-1e6
aux<-copy(masa_salarial_tnrh[, .(anio, Masa_Anual)] )
aux[, Masa_Anual := Masa_Anual / unidad ]

x_lim <- c( 2015, 2020 )
x_brk <- 2015:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 21, 310)
y_brk <- seq( y_lim[1], y_lim[2],by=40 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_tnrh <- ggplot( data = aux ) +
  geom_line( aes( x = anio,
                  y = Masa_Anual,
                  color = parametros$iess_blue ),
             size = graf_line_size,
             lineend = "round" ) +
  labs( x = 'Año', y = 'Masa Salarial (millones)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ),
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

# iess_masa_salarial_tnrh
ggsave( plot = iess_masa_salarial_tnrh,
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_tnrh', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

