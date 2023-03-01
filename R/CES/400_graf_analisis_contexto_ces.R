message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando Total desempleo por periodos de tiempo' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-1

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_DES_estadistica_desempleo.RData' ) )

# indice de desempleo------------------------------------------------------------------------
unidad<-1e6
aux<-copy(porc_desempleo)
aux$periodo1<-as.Date(aux$periodo1,"%Y-%m-%d")
aux$periodo2<-as.Date(aux$periodo2,"%Y-%m-%d")
aux1<-as.data.frame(aux)
aux2<-data.frame(f=seq( 1, 24, 1 ),Fecha=c(aux1[,1],aux1[,3]),n=c(aux1[,2],aux1[,4])*100 )

x_lim <- c( 1, 24 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux2[,2])

y_lim <- c( 2, 6)
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ind_desempleo <- ggplot(aux2, aes(Fecha)) + 
  geom_line(aes(y = n),colour = parametros$iess_green) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous( breaks = y_brk, labels = paste0(y_lbl,"%"), limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = 'Año', y = 'Tasa de desempleo' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_ind_desempleo, 
        filename = paste0( parametros$resultado_graficos, 'iess_indice_desempleo_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# indice de desempleo URBANO y RURAL----------------------------------------------------------------

message( '\tGraficando índice de población urbana y rural desempleada' )

unidad<-1e6
aux<-copy(urb_rur_desempleo)
aux$periodo1<-as.Date(aux$periodo1,"%Y-%m-%d")
aux$periodo2<-as.Date(aux$periodo2,"%Y-%m-%d")
aux1<-as.data.frame(aux)
aux2<-data.frame(f=seq( 1, 24, 1 ),Fecha=c(aux1[,1],aux1[,4]),U=c(aux1[,2],aux1[,5])*100,
                 R=c(aux1[,3],aux1[,6])*100 )

x_lim <- c( 1, 24 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux2[,2])

y_lim <- c( 1, 8)
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_urb_rur_desempleo <- ggplot(aux2, aes(Fecha)) + 
  geom_line(aes(y = U,colour ="Urbana")) + 
  geom_line(aes(y = R,colour ="Rural")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous( breaks = y_brk, labels = paste0(y_lbl,"%"),
                      limits = y_lim ) +
  scale_colour_manual("", 
                      breaks = c("Urbana", "Rural"), 
                      values = c("Urbana" = parametros$iess_green , 
                                 "Rural" = parametros$iess_blue))+
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = 'Tasa de desempleo' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_urb_rur_desempleo, 
        filename = paste0( parametros$resultado_graficos, 'iess_urb_rur_desempleo_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# indice de desempleo popr sexo---------------------------------------------------------------------
message( '\tGraficando índice de población desempleada por sexo' )
# Carga de datos -----------------------------------------------------------------------------------
unidad<-1e6
aux<-copy(sexo_desempleo)
aux$periodo1<-as.Date(aux$periodo1,"%Y-%m-%d")
aux$periodo2<-as.Date(aux$periodo2,"%Y-%m-%d")
aux1<-as.data.frame(aux)
aux2<-data.frame(f=seq( 1, 24, 1 ),Fecha=c(aux1[,1],aux1[,4]),H=c(aux1[,2],aux1[,5])*100,
                 M=c(aux1[,3],aux1[,6])*100 )

x_lim <- c( 1, 24 )
x_brk <- seq( x_lim[1], x_lim[2], 1 )
x_lbl <- c(aux2[,2])

y_lim <- c( 1, 8)
y_brk <- seq( y_lim[1], y_lim[2], 1 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sexo_desempleo <-  ggplot(aux2, aes(Fecha)) + 
  geom_line(aes(y = H,colour ="Hombre")) + 
  geom_line(aes(y = M,colour ="Mujer")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous( breaks = y_brk, labels = paste0(y_lbl,"%"), 
                      limits = y_lim ) +
  scale_colour_manual("", 
                      breaks = c("Hombre", "Mujer"), 
                      values = c("Hombre" = parametros$iess_green , 
                                 "Mujer" = parametros$iess_blue))+
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = 'Tasa de desempleo' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )


ggsave( plot = iess_sexo_desempleo,
        filename = paste0( parametros$resultado_graficos, 'iess_sexo_desempleo_des',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámides poblacionales por sexo al 2018--------------------------------------------------------
aux<-copy( pira_edad_desempleo )
t<-dim(aux)[1]-1
aux <- aux[, .(edad,H1,M1)][1:t]
N <- c(sum(aux$H1), sum(aux$M1))
aux$H1 <- -1*aux$H1/N[1]
aux$M1 <- aux$M1/N[2]
aux$edad <- factor(aux$edad, levels=aux$edad, labels=aux$edad)
aux.melt<-melt.data.table(aux,
                          value.name='Poblacion', 
                          variable.name= 'Sexo',
                          id.vars='edad')
M <- c(max(abs(aux$H1)), max(aux$M1)) # En base a este valor poner los límites del eje x
salto_y<-1
salto_x<-0.02
brks_y <- seq(-0.10,0.10,salto_x)
lbls_y <- paste0(as.character(c(seq(0.10, 0, -salto_x)*100, seq(salto_x, 0.10, salto_x)*100)), "%")


iess_pir_pob_2018<- ggplot(aux.melt, aes(x = edad, y = Poblacion, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data=aux.melt[ Sexo == 'M1' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data=aux.melt[ Sexo == 'H1' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  #scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green), 
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_pob_2018, 
        filename = paste0( parametros$resultado_graficos, 'iess_iess_pir_pob_2018_des',
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámides poblacionales por sexo al 2038--------------------------------------------------------
aux<-copy( pira_edad_desempleo )
t<-dim(aux)[1]-1
aux <- aux[, .(edad,H2,M2)][1:t]
N <- c(sum(aux$H2), sum(aux$M2))
aux$H2 <- -1*aux$H2/N[1]
aux$M2 <- aux$M2/N[2]
aux$edad <- factor(aux$edad, levels=aux$edad, labels=aux$edad)
aux.melt<-melt.data.table(aux,
                          value.name='Poblacion', 
                          variable.name= 'Sexo',
                          id.vars='edad')
M <- c(max(abs(aux$H2)), max(aux$M2)) # En base a este valor poner los límites del eje x
M
salto_y<-1
salto_x<-0.02
brks_y <- seq(-0.08,0.08,salto_x)
lbls_y <- paste0(as.character(c(seq(0.08, 0, -salto_x)*100, seq(salto_x, 0.08, salto_x)*100)), "%")


iess_pir_pob_2038<- ggplot(aux.melt, aes(x = edad, y = Poblacion, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data=aux.melt[ Sexo == 'M2' ], 
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data=aux.melt[ Sexo == 'H2' ], 
            stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  #scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green), 
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_pob_2038, 
        filename = paste0( parametros$resultado_graficos, 'iess_iess_pir_pob_2038_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámides poblacionales por sexo al 2058
aux<-copy( pira_edad_desempleo )
t<-dim(aux)[1]-1
aux <- aux[, .(edad,H3,M3)][1:t]
N <- c(sum(aux$H3), sum(aux$M3))
aux$H3 <- -1*aux$H3/N[1]
aux$M3 <- aux$M3/N[2]
aux$edad <- factor(aux$edad, levels=aux$edad, labels=aux$edad)
aux.melt<-melt.data.table(aux,
                          value.name='Poblacion', 
                          variable.name= 'Sexo',
                          id.vars='edad')
M <- c(max(abs(aux$H3)), max(aux$M3)) # En base a este valor poner los límites del eje x
M
salto_y<-1
salto_x<-0.01
brks_y <- seq(-0.07,0.07,salto_x)
lbls_y <- paste0(as.character(c(seq(0.07, 0, -salto_x)*100, seq(salto_x, 0.07, salto_x)*100)), "%")



iess_pir_pob_2058<- ggplot(aux.melt, aes(x = edad, y = Poblacion, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data=aux.melt[ Sexo == 'M3' ],
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data=aux.melt[ Sexo == 'H3' ],
            stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  #scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right",
                             label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green), 
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_pob_2058, 
        filename = paste0( parametros$resultado_graficos, 'iess_iess_pir_pob_2058_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
