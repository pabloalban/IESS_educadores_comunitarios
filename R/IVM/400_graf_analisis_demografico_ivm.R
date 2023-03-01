message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada activa inicial SGO del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_analisis_demografico.RData' ) )

# Graficos Afiliados Activos------------------------------------------------------------------------
unidad<-1e6

aux<-copy( pob_afi_ini[, .(anio, Afiliados_m, Afiliados_f, Afiliados)] )


x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 400000, 3500000)
y_brk <- seq( y_lim[1], y_lim[2],by=500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pob_afi_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados_m, colour = "Afiliados Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados_f, colour = "Afiliados Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Afiliados, colour = "Afiliados" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c("Afiliados" ,"Afiliados Masculino","Afiliados Femenino"),
                       values = c( "Afiliados Masculino" = parametros$male,
                                   "Afiliados Femenino" =  parametros$female,
                                   "Afiliados" = parametros$iess_green) ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Afiliados' ) +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 

#iess_pob_afi_ini
ggsave( plot = iess_pob_afi_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráficos de la pirámide de Afiliados Activos------------------------------------------------------
message( '\tGraficando población afiliada activa inicial por edad y sexo SGO del IESS' )
                                       # ---------------- jaime
aux<-copy( pob_afi_edad_sexo_ini )
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
salto_x<-0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')


iess_pir_afiliados<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_afiliados, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Masa salarial por sexo y total-------------------------------------------------------------------------------------
message( '\tGraficando masa salarial inicial SGO del IESS' )
unidad<-1e6
aux<-copy( masa_salarial_ini[, .(anio, Masa_Anual,Masa_Anual_Mas,Masa_Anual_Fem)] )
aux[, Masa_Anual := Masa_Anual / unidad ]
aux[, Masa_Anual_Mas := Masa_Anual_Mas / unidad ]
aux[, Masa_Anual_Fem := Masa_Anual_Fem / unidad ]

x_lim <- c( 2005, 2020 )
x_brk <- 2005:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 30000)
y_brk <- seq( y_lim[1], y_lim[2],by=5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_masa_salarial_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual_Mas, colour = "Masa Salarial anual (USD) Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual_Fem, colour = "Masa Salarial anual (USD) Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = Masa_Anual, colour = "Masa Salarial anual (USD)" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c( "Masa Salarial anual (USD)","Masa Salarial anual (USD) Masculino", "Masa Salarial anual (USD) Femenino"),
                       values = c( "Masa Salarial anual (USD) Masculino" = parametros$male,
                                   "Masa Salarial anual (USD) Femenino" =  parametros$female,
                                   "Masa Salarial anual (USD)" = parametros$iess_green) ) +
  theme_bw() +
  plt_theme +
  labs(  x = 'Año', y = 'Masa Salarial (millones)' ) +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 

# iess_masa_salarial
ggsave( plot = iess_masa_salarial_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Piramide Masa Salarial ------------------------------------------------------------------------------------
message( '\tGraficando masa salarial por monto y sexo SGO del IESS' )

# Graficos de la pirámide de Masa Salarial----------------------------------------------------------
aux<-copy( masa_sal_monto )
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]
aux[,monto:=as.numeric(monto)]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-100
salto_x<-0.1
brks_y <- seq(-0.25,0.25,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- c(seq(100,1500,salto_y),1600)
#nb <- length(brks_x)-1   
lbls_x<- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor a 1500')

iess_pir_masa_salarial<-ggplot(aux, aes(x = monto, y = n , fill=sexo)) +
  xlab( 'Salario (USD)' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white",  size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white",  size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_masa_salarial, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Jubilados de Vejez por sexo--------------------------------------------------------------------------------
message( '\tGraficando jubilados de vejez inicial SGO del IESS' )

unidad<-1e6

aux <- copy( jub_vejez_h[, .(anio,jub_vjz)])
aux1 <- copy( jub_vejez_m[, .(anio,jub_vjz)] )
aux2 <- copy( jub_vejez[, .(anio,jub_vjz)])
aux<- merge(aux, aux1 ,by= "anio")
aux<- merge(aux, aux2 ,by= "anio")
rm(aux1)
rm(aux2)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 80000, 430000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pen_vejez_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = jub_vjz.x, colour = "Jubilados de Vejez Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_vjz.y, colour = "Jubilados de Vejez Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_vjz, colour = "Jubilados de Vejez" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c( "Jubilados de Vejez", "Jubilados de Vejez Masculino", "Jubilados de Vejez Femenino" ),
                       values = c( "Jubilados de Vejez" = parametros$iess_green ,
                         "Jubilados de Vejez Masculino" = parametros$male ,
                                   "Jubilados de Vejez Femenino" = parametros$female ) ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Pensionistas') +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 

# iess_pen_vejez
ggsave( plot = iess_pen_vejez_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pen_vejez_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Piramide Jubilados de vejez -------------------------------------------------------------------------------
message( '\tGraficando población jubilada de vejez por edad y sexo SGO del IESS' )

# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux<-copy( jub_vjz_edad_sexo )
max_edad<-105
min_edad<-45
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.01

brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_jub_vejez<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green), 
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_jub_vejez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_vejez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( '\tGraficando pensionistas por monto y sexo SGO del IESS' )
# Gráficos de la pirámide de la pension de jubilados por vejez--------------------------------------
aux<-copy( montos_vjz_sexo )
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-100
salto_x<-0.05

brks_y <- seq(-0.25,0.25,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- c(seq(100,1500,salto_y),1600)
lbls_x<- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor a 1500')

iess_pir_monto_jub_vejez<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_monto_jub_vejez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_vejez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




# Jubilados de Invalidez por sexo----------------------------------------------------------------------------
message( '\tGraficando jubilados por invalidez inicial SGO del IESS' )


unidad<-1e6

aux <- copy( jub_inv_h[, .(anio,jub_invlz)])
aux1 <- copy( jub_inv_m[, .(anio,jub_invlz)] )
aux2 <- copy( jub_inv[, .(anio,jub_invlz)] )
aux<- merge(aux,aux1, by= "anio")
aux<- merge(aux,aux2, by= "anio")
rm(aux1)
rm(aux2)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0,35000)
y_brk <- seq( y_lim[1], y_lim[2], 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pen_invalidez_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = jub_invlz.x, colour = "Jubilados de Invalidez Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_invlz.y, colour = "Jubilados de Invalidez Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_invlz, colour = "Jubilados de Invalidez" ), size = graf_line_size  ) + 
  scale_colour_manual( "",
                       breaks = c( "Jubilados de Invalidez", "Jubilados de Invalidez Masculino", "Jubilados de Invalidez Femenino" ),
                       values = c( "Jubilados de Invalidez" = parametros$iess_green,
                                   "Jubilados de Invalidez Masculino" = parametros$male ,
                                   "Jubilados de Invalidez Femenino" = parametros$female ) ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Pensionitas') +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 


# iess_pen_invalidez_ini
ggsave( plot = iess_pen_invalidez_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pen_invalidez_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( '\tGraficando población jubilada de invalidez por edad y sexo SGO del IESS' )
# Gráficos de la pirámide de jubilados de vejez ----------------------------------------------------
aux<-copy( jub_inv_edad_sexo )
max_edad<-105
min_edad<-25
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.01

brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_jub_invalidez<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_jub_invalidez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_invalidez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( '\tGraficando pensionistas de invalidez por monto y sexo SGO del IESS' )
#Graficos de la pirámide de la pension de jubilados por monto y sexo SGO del IESS--------------------------------------
aux<-copy( montos_inv_sexo )
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_x<-100
salto_y<-0.06
brks_y <- seq(-0.30,0.30,by=salto_y)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(100,900,salto_x)
lbls_x<- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor a 800')


iess_pir_monto_jub_invalidez<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))


ggsave( plot = iess_pir_monto_jub_invalidez, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_invalidez', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Jubilados de Discapacidad-------------------------------------------------------------------------
message( '\tGraficando jubilados por discapacidad inicial SGO del IESS' )

unidad<-1e6

aux <- copy( jub_vjz_especial_h[, .(anio,jub_dsc)])
aux1 <- copy( jub_vjz_especial_m[, .(anio,jub_dsc)] )
aux2 <- copy( jub_vjz_especial[, .(anio,jub_dsc)] )
aux<- merge(aux,aux1, by= "anio")
aux<- merge(aux,aux2, by= "anio")
rm(aux1)
rm(aux2)

x_lim <- c( 2014, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0,12000)
y_brk <- seq( y_lim[1], y_lim[2], 2000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pen_discapacidad_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = jub_dsc.x, colour = "Jubilados de Discapacidad Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_dsc.y, colour = "Jubilados de Discapacidad Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = jub_dsc, colour = "Jubilados de Discapacidad" ), size = graf_line_size  ) + 
  
  scale_colour_manual( "",
                       breaks = c( "Jubilados de Discapacidad","Jubilados de Discapacidad Masculino", "Jubilados de Discapacidad Femenino" ),
                       values = c( "Jubilados de Discapacidad" = parametros$iess_green ,
                                   "Jubilados de Discapacidad Masculino" = parametros$male ,
                                   "Jubilados de Discapacidad Femenino" = parametros$female ) ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Pensionitas') +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 



# iess_pen_discapacidad_ini
ggsave( plot = iess_pen_discapacidad_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pen_discapacidad_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Distribución de los jubilados especiales de vejez por edad y gén --------
message( '\tGraficando población jubilada de discapacidad por edad y sexo SGO del IESS' )

aux<-copy( jub_vjz_especial_edad_sexo )
max_edad<-69
min_edad<-30
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.02
brks_y <- seq(-0.1,0.1,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad+1,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_jub_discapacidad<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_jub_discapacidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_jub_discapacidad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( '\tGraficando pensionistas de discapacidad por monto y sexo SGO del IESS' )
# Gráficos de la pirámide de la pension de jubilados por vejez--------------------------------------

aux<-copy( montos_vjz_esp )
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-100
salto_x<-0.08
brks_y <- seq(-0.4,0.4,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(200,900,salto_y)
lbls_x<- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor a 800')


iess_pir_monto_jub_discapacidad<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))


ggsave( plot = iess_pir_monto_jub_discapacidad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_jub_discapacidad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Pensionistas de Viudedad por sexo--------------------------------------------------------------------------
message( '\tGraficando pensionistas por viudedad inicial SGO del IESS' )

unidad<-1e6

aux <- copy( pen_viud_h[, .(anio,pension)])
aux1 <- copy( pen_viud_m[, .(anio,pension)] )
aux2 <- copy( pen_viud[, .(anio,pension)] )
aux<- merge(aux,aux1, by= "anio")
aux<- merge(aux,aux2, by= "anio")
rm(aux1)
rm(aux2)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0,120000)
y_brk <- seq( y_lim[1], y_lim[2], 20000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_pen_viudedad_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = pension.x, colour = "Pensionistas de Viudedad Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = pension.y, colour = "Pensionistas de Viudedad Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = pension, colour = "Pensionistas de Viudedad" ), size = graf_line_size  ) + 
  
  scale_colour_manual( "",
                       breaks = c( "Pensionistas de Viudedad","Pensionistas de Viudedad Masculino", "Pensionistas de Viudedad Femenino" ),
                       values = c( "Pensionistas de Viudedad" = parametros$iess_green ,
                                   "Pensionistas de Viudedad Masculino" = parametros$male ,
                                   "Pensionistas de Viudedad Femenino" = parametros$female ) ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Pensionitas') +
  theme( legend.position = "bottom",legend.direction = "vertical",
         axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
  
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 



# iess_pen_viudedad
ggsave( plot = iess_pen_viudedad_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pen_viudedad_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#--------------------------------------------------------

message( '\tGraficando población pensionista de viudedad por edad y sexo SGO del IESS' )
aux<-copy( pen_viu_edad_sexo )
max_edad<-105
min_edad<-18
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(n),n:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(19,108,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pen_viudedad<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_pen_viudedad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pen_viudedad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( '\tGraficando pensionistas de viudedad por monto y sexo SGO del IESS' )
# Gráficos de la pirámide de la pension de viudedad por monto y sexo SGO del IESS -------------------------------------
aux<-copy( montos_pen_viu )
#aux<-aux[cat=="porfa" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-100
salto_x<-0.12
brks_y <- seq(-0.60,0.60,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(100,900,salto_y)
lbls_x <- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor a 800')

iess_pir_monto_pen_viudedad<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_monto_pen_viudedad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_pen_viudedad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Jubilados de orfandad por sexo-----------------------------------------------------------------------------
message( '\tGraficando pensionistas por orfandad inicial SGO del IESS' )

unidad<-1e6

aux <- copy( pen_orf_h[, .(anio,pension)])
aux1 <- copy( pen_orf_m[, .(anio,pension)] )
aux2 <- copy( pen_orf[, .(anio,pension)] )
aux<- merge(aux,aux1, by= "anio")
aux<- merge(aux,aux2, by= "anio")
rm(aux1)
rm(aux2)

x_lim <- c( 2012, 2020 )
x_brk <- 2012:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 8000,38000)
y_brk <- seq( y_lim[1], y_lim[2], 5000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


  iess_pen_orfandad_ini <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = pension.x, colour = "Pensionistas de Orfandad Masculino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                  y = pension.y, colour = "Pensionistas de Orfandad Femenino" ), size = graf_line_size  ) + 
  geom_line( aes( x = anio, 
                    y = pension, colour = "Pensionistas de Orfandad" ), size = graf_line_size  ) + 
    
   scale_colour_manual( "",
                       breaks = c("Pensionistas de Orfandad", "Pensionistas de Orfandad Masculino", "Pensionistas de Orfandad Femenino" ),
                       values = c( "Pensionistas de Orfandad" = parametros$iess_green ,
                                   "Pensionistas de Orfandad Masculino" = parametros$male ,
                                   "Pensionistas de Orfandad Femenino" = parametros$female ) ) +
  theme_bw() +
  plt_theme +
  labs( x = 'Año', y = 'Pensionitas') +
  theme( legend.position = "bottom",legend.direction = "vertical",
           axis.text.x = element_text( angle = 90, hjust = 1), legend.text = element_text(size = 6, colour = "black"))+
    
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) 
  
  

# iess_pen_orfandad_ini
ggsave( plot = iess_pen_orfandad_ini, 
        filename = paste0( parametros$resultado_graficos, 'iess_pen_orfandad_ini', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( '\tGraficando pensionistas de orfandad por edad y sexo SGO del IESS' )

# Gráficos de la pirámide de de orfandad por edad y sexo SGO del IESS -------------------------------------
aux<-copy( pen_orf_edad_sexo )
#aux<-aux[cat=="porfa" & sexo!="D"]  #Condición para extraer los datos
edad_min<-0
edad_max<-108
aux<-aux[edad>=edad_min & edad<=edad_max]
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-5
salto_x<-0.01
brks_y <- seq(-0.1,0.1,salto_x)
lbls_y <- paste0(abs(brks_y)*100,'%')
brks_x <- seq(edad_min,edad_max,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_pen_orfandad<-ggplot(aux, aes(x = edad, y = n, fill=sexo)) +
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
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_pen_orfandad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pen_orfandad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( '\tGraficando pensionistas de viudedad por monto y sexo SGO del IESS' )

# Gráficos de la pirámide de la pension de orfandad por monto y sexo -------------------------------------
aux<-copy( montos_pen_orf )
#aux<-aux[cat=="porfa" & sexo!="D"]  #Condición para extraer los datos
aux[is.na(n),n:=0]     #reemplazo datos NA por cero

N <- data.frame((aux[,sum(n,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", n:=-n]
aux[sexo=="H", n:=n/N[1,1]]
aux[sexo=="M", n:=n/N[1,1]]

M <- data.frame((aux[,max(abs(n),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y<-100
salto_x<-0.12
brks_y <- seq(-0.6,0.60,salto_x)
lbls_y <- paste0(abs(brks_y)*100, "%")
brks_x <- seq(100,500,salto_y)
lbls_x <- c(formatC(brks_x[-length(brks_x)],digits = 0,format = 'f'),'mayor 400')

iess_pir_monto_pen_orfandad<-ggplot(aux, aes(x = monto, y = n, fill=sexo)) +
  xlab( 'Monto' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'H' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", label.hjust = 0))+
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_monto_pen_orfandad, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_monto_pen_orfandad', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

