# Datos para gráfico de Tabla 2 del documento "causas de posible desfinanciamiento".
# Tabla 2: Comparación de las primas de aportes: Resolución C.D. 501 – Resolución C.D. 261.

# Carga de datos -----------------------------------------------------------------------------------
message( '\tGráfica de cooparación entre primas de la CD515 y CD501' )
file_prima <- paste0( parametros$RData_seg, 'IESS_CES_causas_desfinanciamiento.RData' )
load( file = file_prima )
Comparacion_Primas <- (comparacion_primas)
setnames(Comparacion_Primas, c("Año", "C.D.515", "C.D.501", "Etiqueta"))
com_pri_apo <- Comparacion_Primas
com_pri_apo[5,3]<-0.03
message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGrafico: Comparación de las primas de aportes' )

source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

#Gráfico de comparación de tasas de aportación------------------------------------------------------
x_lim <- c( 2012, 2022 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
y_brk<- seq(0,0.04,0.01)
y_lbl <- paste0(formatC(100 * y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"%")

plt_com_pri_apo <-  ggplot(com_pri_apo, aes(Año)) + 
  geom_line(aes(y = C.D.515,colour ="C.D.515")) +
  geom_line(aes(y = C.D.501,colour ="C.D.501 y C.D. 261"), size=1) +
  geom_point(aes(y = C.D.515,colour ="C.D.515"), 
             shape = 20, 
             # size = graf_line_size,
             size = 4,
             color = parametros$iess_green)+
  geom_point(aes(y = C.D.501,colour ="C.D.501 y C.D. 261"), 
             shape = 20, 
             # size = graf_line_size,
             size = 3,
             color = 'red' ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(breaks = y_brk,
                     labels = y_lbl,
                     limits = c(y_brk[1], max(y_brk))) +
  scale_colour_manual("", 
                      breaks = c("C.D.515", "C.D.501 y C.D. 261"), 
                      values = c("C.D.515" = parametros$iess_green ,
                                 "C.D.501 y C.D. 261" = parametros$iess_blue))+
  geom_text_repel(data = com_pri_apo[-5,],aes(Año, C.D.501, label =Etiqueta ),
                  point.padding = unit(1, 'lines'),
                  arrow = arrow(length = unit(0.01, 'npc')),
                  segment.size = 0.01,
                  segment.color = 'black'
  ) +
  geom_text_repel(data = com_pri_apo,aes(Año, C.D.515, label =Etiqueta ),
                  point.padding = unit(0.07, 'lines'),
                  arrow = arrow(length = unit(0.01, 'npc')),
                  segment.size = 0.1,
                  segment.color = '#cccccc'
  ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = '' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )


#Guaradando gráfica en formato png------------------------------------------------------------------
ggsave( plot = plt_com_pri_apo, 
        filename = paste0( parametros$resultado_graficos, 'iess_grafico_tasas_aporte', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()