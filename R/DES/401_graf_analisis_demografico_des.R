message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando demografía de desempleo' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
graf_line_size<-1

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_DES_analisis_demografico.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_historicos.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_masa_salarial_historico.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_DES_pagos_edad_sex.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_salarios.RData' ) ) 
# Evolución del número de cotizantes a desempleo y cesantía-----------------------------------------
aux<-evo_anual_cotizantes_ces_des

x_lim <- c( 2006, 2020 )
x_brk <- 2006:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 500000, 3500000)
y_brk <- seq( y_lim[1], y_lim[2], 500000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pob_afi_ces_des <- ggplot( data = aux ) + 
                        geom_line( aes( x = aniper, 
                                        y = total, 
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

ggsave( plot = iess_pob_afi_ces_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_pob_afi_ces_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide poblacional de cotizantes a desempleo y cesantía------------------------------------------
aux<-as.data.table( cotizantes_ces_des %>% filter(aniper=='2020') ) 
max_edad<-90
min_edad<-15
aux<-aux[edad>=min_edad & edad <=max_edad]  #Condición para extraer los datos
aux[is.na(cotizantes),cotizantes:=0]  #reemplazo datos NA por cero

N <- data.frame((aux[,sum(cotizantes,na.rm = TRUE),by=genero]))  # número total por sexo

aux[genero=="M", cotizantes:=-cotizantes]
aux[genero=="F", cotizantes:=cotizantes/N[1,2]]
aux[genero=="M", cotizantes:=cotizantes/N[2,2]]

M <- data.frame((aux[,max(abs(cotizantes),na.rm = TRUE),by=genero])) # En base a este valor poner los límites del eje x

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_afiliados_ces_des<-ggplot(aux, aes(x = edad, y = cotizantes, fill=genero)) +
                    xlab( 'Edad' ) +
                    ylab( '' ) +
                    geom_bar( data = aux[ genero == 'F' ], stat = 'identity',colour="white", size=0.1) +
                    geom_bar( data = aux[ genero == 'M' ], stat = 'identity',colour="white", size=0.1) +
                    scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                    scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                    coord_flip() +
                    #theme_tufte()+
                    theme_bw() +
                    plt_theme +
                    guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
                    theme(legend.position="bottom") +   
                    scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                                      labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_afiliados_ces_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_afiliados_ces_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




#Piramide de la dsitribución de los salarios de los cotizantes de desempleo-------------------------
aux <-tabla_pir %>% 
      group_by(sexo) %>%
      mutate(suma=sum(cotizantes,na.rm = TRUE)) %>%
      mutate(fdp=cotizantes/suma) %>%
      ungroup() %>%
      mutate(fdp=if_else(sexo=='M',fdp*(-1),fdp)) %>%
      arrange(rango_sal)


salto_y<-0.05
brks_y <- round(seq(-0.3,0.35,salto_y),2)
lbls_y <- paste0(as.character(abs(brks_y)*100),"%")
lbls_x<- aux %>%filter(sexo=='M') %>% select(rango_sal)


iess_pir_masa_salarial_des<-ggplot(aux, aes(x =  rango_sal, y = fdp, fill=sexo)) +
                            xlab( 'Rangos salario (USD)' ) +
                            ylab( '' ) +
                            geom_bar( data = aux[which(aux$sexo == 'F'), ], stat = 'identity',
                                      colour="white",  size=0.1) +
                            geom_bar( data = aux[which(aux$sexo == 'M'), ], stat = 'identity',
                                      colour="white",  size=0.1) +
                            scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                            #scale_x_continuous(labels = lbls_x) +
                            coord_flip() +
                            #theme_tufte()+
                            theme_bw() +
                            plt_theme +
                            guides(fill = guide_legend(title = NULL,
                                                       label.position = "right", 
                                                       label.hjust = 0,
                                                       reverse = TRUE))+
                            theme(legend.position="bottom")+
                            scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                                              labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_masa_salarial_des,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_masa_salarial_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Evolución de la masa salarial de los cotizantes a desempleo y cesantía----------------------------
aux<-evo_masa_sal_ces_des

x_lim <- c( 2006, 2020 )
x_brk <- 2006:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 30000000000)
y_brk <- seq( y_lim[1], y_lim[2], 5000000000 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_ces_des <- ggplot( data = aux ) + 
                              geom_line( aes( x = aniper, 
                                              y = total),
                                         colour=parametros$iess_green,
                                         size = graf_line_size,
                                         lineend = "round" ) + 
                              labs( x = 'Año', y = 'Masa Salarial (Millones USD)' ) +
                              scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                              scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                              theme_bw() +
                              plt_theme +
                              theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_masa_salarial_ces_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_ces_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



#Número de beneficiarios de desempleo---------------------------------------------------------------
aux <-siniestros_desempleo %>% 
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_tipo_pago,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_tipo_pago) %>%
      mutate(suma=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_tipo_pago,suma) %>%
      ungroup()

x_lim <- c( 2016, 2020 )
x_brk <- 2016:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 70000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_ben_des <- ggplot( ) + 
                    geom_line( data = subset(aux,sd_tipo_pago =='F') ,aes( x = anio, 
                                    y = suma),
                               colour=parametros$iess_green,
                               size = graf_line_size,
                               lineend = "round" ) + 
                    labs( x = 'Año', y = 'Masa Salarial (Millones USD)' ) +
                    scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                    scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                    theme_bw() +
                    plt_theme +
                    theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_ben_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_ben_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Beneficiarios por pago de parte fija Número de beneficiarios de desempleo---------------------------------------------------------------
aux <-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='F') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(beneficiarios=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,beneficiarios)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE)

x_lim <- c( 2016, 2020 )
x_brk <- 2016:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 70000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ben_pago_pf_des <- ggplot( ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='1'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 1"),
                                   size = graf_line_size,
                                   lineend = "round" ) +
                        geom_line( data = aux[which(aux$sd_numero_pagos=='2'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 2"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='3'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 3"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='4'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 4"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='5'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 5"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        labs( x = 'Año', y = 'Beneficiarios' ) +
                        scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                        scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                          scale_colour_manual("", 
                        breaks = c("Pago 1", "Pago 2", "Pago 3", "Pago 4", "Pago 5"), 
                        values = c("Pago 1" = "#004203",
                                   "Pago 2" = "#006E05",
                                   "Pago 3" = "#00A808",
                                   "Pago 4" = "#04CD0E",
                                   "Pago 5" = "#00FF0C"))+
                        theme_bw() +
                        plt_theme +
                        theme(legend.position="bottom") +
                        labs( x = '', y = 'Beneficiarios' )+
                        theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )


ggsave( plot = iess_ben_pago_pf_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_ben_pago_pf_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Piramide (pf) de la población beneficiaría de desempleo------------------------------------------------
aux <-siniestros_desempleo %>% 
  filter(sd_tipo_pago=='F') %>%
  mutate(anio=year(sd_fecha_ord)) %>% 
  distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
  filter(anio_pago %in% c('2016','2017','2018','2019','2020'), sd_numero_pagos=='1') %>%
  group_by(edad,sd_genero) %>%
  mutate(beneficiarios= n()) %>%
  distinct(edad,sd_genero,.keep_all = TRUE) %>%
  ungroup() %>%
  group_by(sd_genero) %>%
  mutate(dist=beneficiarios/sum(beneficiarios)) %>%
  ungroup() %>%
  mutate(dist=if_else(sd_genero=='M',(-1)*dist,dist)) %>%
  select(sd_genero, edad, dist)

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.06, salto_x)*100)), "%")
brks_x <- seq(20,80,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_ben_pf_des<-ggplot(aux, aes(x = edad, y = dist, fill=sd_genero)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[which(aux$sd_genero == 'M'), ],
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[which(aux$sd_genero == 'F'), ], 
            stat = 'identity',colour="white", size=0.1) +
  
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels =lbls_x, limits = c(18,70)) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c( "Mujeres","Hombres"))

ggsave( plot = iess_pir_ben_pf_des,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_pf_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Beneficiarios por pago de parte variable de desempleo---------------------------------------------------------------
aux <-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      group_by(anio,sd_numero_pagos) %>%
      mutate(beneficiarios=n()) %>%
      #filter(anio<2019) %>%
      arrange(anio) %>%
      select(anio,sd_numero_pagos,beneficiarios)%>%
      distinct(anio,sd_numero_pagos,.keep_all = TRUE)

x_lim <- c( 2016, 2020 )
x_brk <- 2016:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 9000)
y_brk <- seq( y_lim[1], y_lim[2], 2000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ben_pago_pv_des <- ggplot( ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='1'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 1"),
                                   size = graf_line_size,
                                   lineend = "round" ) +
                        geom_line( data = aux[which(aux$sd_numero_pagos=='2'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 2"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='3'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 3"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='4'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 4"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        geom_line( data = aux[which(aux$sd_numero_pagos=='5'),] ,
                                   aes( x = anio, y = beneficiarios,colour ="Pago 5"),
                                   size = graf_line_size,
                                   lineend = "round" ) + 
                        labs( x = 'Año', y = 'Beneficiarios' ) +
                        #scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
                        #scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
                        scale_colour_manual("", 
                                            breaks = c("Pago 1", "Pago 2", "Pago 3", "Pago 4", "Pago 5"), 
                                            values = c("Pago 1" = "#004203",
                                                       "Pago 2" = "#006E05",
                                                       "Pago 3" = "#00A808",
                                                       "Pago 4" = "#04CD0E",
                                                       "Pago 5" = "#00FF0C"))+
                        theme_bw() +
                        plt_theme +
                        theme(legend.position="bottom") +
                        labs( x = '', y = 'Beneficiarios' )+
                        theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )


ggsave( plot = iess_ben_pago_pv_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_ben_pago_pv_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Piramide (pv) de la población beneficiaría de desempleo------------------------------------------------
aux <-siniestros_desempleo %>% 
      filter(sd_tipo_pago=='V') %>%
      mutate(anio=year(sd_fecha_ord)) %>% 
      distinct(anio,sd_numero_pagos,sd_cedula, .keep_all = TRUE) %>%
      filter(anio_pago %in% c('2016','2017','2018','2019','2020'), sd_numero_pagos=='1') %>%
      group_by(edad,sd_genero) %>%
      mutate(beneficiarios= n()) %>%
      distinct(edad,sd_genero,.keep_all = TRUE) %>%
      ungroup() %>%
      group_by(sd_genero) %>%
      mutate(dist=beneficiarios/sum(beneficiarios)) %>%
      ungroup() %>%
      mutate(dist=if_else(sd_genero=='M',(-1)*dist,dist)) %>%
      select(sd_genero, edad, dist)

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.06,0.06,salto_x)
lbls_y <- paste0(as.character(c(seq(0.06, 0, -salto_x)*100, seq(salto_x, 0.06, salto_x)*100)), "%")
brks_x <- seq(20,80,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_ben_pv_des<-ggplot(aux, aes(x = edad, y = dist, fill=sd_genero)) +
                  xlab( 'Edad' ) +
                  ylab( '' ) +
                  geom_bar( data = aux[which(aux$sd_genero == 'M'), ],
                            stat = 'identity',colour="white", size=0.1) +
                  geom_bar( data = aux[which(aux$sd_genero == 'F'), ], 
                            stat = 'identity',colour="white", size=0.1) +
                  
                  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                  scale_x_continuous(breaks = brks_x, labels =lbls_x, limits = c(18,80)) +
                  coord_flip() +
                  #theme_tufte()+
                  theme_bw() +
                  plt_theme +
                  guides(fill = guide_legend(title = NULL,label.position = "right", 
                                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
                  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                                    labels = c( "Mujeres","Hombres"))

ggsave( plot = iess_pir_ben_pv_des,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_ben_pv_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Piramides Montos pagados  por edad-----------------------------------------------------------------------
aux<-copy( tot_pag_rango_edad_sexo )
t<-dim(aux)[1]-1
aux <- aux[, .(rango,H1,M1)][1:t]
N <- c(sum(aux$H1, na.rm=TRUE), sum(aux$M1, na.rm=TRUE))
aux$H1 <- -1*aux$H1/N[1]
aux$M1 <- aux$M1/N[2]
aux[is.na(H1),H1:=0]
aux[is.na(M1),M1:=0]
aux$rango <- factor(aux$rango, levels=aux$rango, labels=aux$rango)
aux.melt<-melt.data.table(aux,
               value.name='Poblacion', 
               variable.name= 'Sexo',
               id.vars='rango')

M <- c(max(abs(aux$H1)), max(aux$M1)) # En base a este valor poner los límites del eje x
M
salto_y<-1
salto_x<-0.1
brks_y <- seq(-0.40,0.40,salto_x)
lbls_y <- paste0(as.character(c(seq(0.40, 0, -salto_x)*100, seq(salto_x, 0.40, salto_x)*100)), "%")


iess_monto_pag_2016<-ggplot(aux.melt, aes(x = rango, y = Poblacion, fill=Sexo)) +
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

ggsave( plot = iess_monto_pag_2016, 
        filename = paste0( parametros$resultado_graficos, 'iess_monto_pag_2016_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


aux<-copy( tot_pag_rango_edad_sexo )
t<-dim(aux)[1]-1
aux <- aux[, .(rango,H2,M2)][1:t]
N <- c(sum(aux$H2, na.rm=TRUE), sum(aux$M2, na.rm=TRUE))
aux$H2 <- -1*aux$H2/N[1]
aux$M2 <- aux$M2/N[2]
aux[is.na(H2),H2:=0]
aux[is.na(M2),M2:=0]
aux$rango <- factor(aux$rango, levels=aux$rango, labels=aux$rango)
aux.melt<-melt.data.table(aux,
               value.name='Poblacion', 
               variable.name= 'Sexo',
               id.vars='rango')

M <- c(max(abs(aux$H2)), max(aux$M2)) # En base a este valor poner los límites del eje x
M
salto_y<-1
salto_x<-0.1
brks_y <- seq(-0.42,0.42,salto_x)
lbls_y <- paste0(as.character(c(seq(0.42, 0, -salto_x)*100, seq(salto_x, 0.42, salto_x)*100)), "%")
# brks_x <- seq(1,max(t1,t2),salto_y)
# lbls_x <- paste0(as.character(edad))


iess_monto_pag_2017<-ggplot(aux.melt, aes(x = rango, y = Poblacion, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data=aux.melt[ Sexo == 'M2' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data=aux.melt[ Sexo == 'H2' ], stat = 'identity',colour="white", size=0.1) +
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

ggsave( plot = iess_monto_pag_2017, 
        filename = paste0( parametros$resultado_graficos, 'iess_monto_pag_2017_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


aux<-copy( tot_pag_rango_edad_sexo )
t<-dim(aux)[1]-1
aux <- aux[, .(rango,H3,M3)][1:t]
N <- c(sum(aux$H3, na.rm=TRUE), sum(aux$M3, na.rm=TRUE))
aux$H3 <- -1*aux$H3/N[1]
aux$M3 <- aux$M3/N[2]
aux[is.na(H3),H3:=0]
aux[is.na(M3),M3:=0]
aux$rango <- factor(aux$rango, levels=aux$rango, labels=aux$rango)
aux.melt<-melt.data.table(aux,
               value.name='Poblacion', 
               variable.name= 'Sexo',
               id.vars='rango')

M <- c(max(abs(aux$H3)), max(aux$M3)) # En base a este valor poner los límites del eje x
M
salto_y<-1
salto_x<-0.1
brks_y <- seq(-0.44,0.44,salto_x)
lbls_y <- paste0(as.character(c(seq(0.44, 0, -salto_x)*100, seq(salto_x, 0.44, salto_x)*100)), "%")
# brks_x <- seq(1,max(t1,t2),salto_y)
# lbls_x <- paste0(as.character(edad))


iess_monto_pag_2020<-ggplot(aux.melt, aes(x = rango, y = Poblacion, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data=aux.melt[ Sexo == 'M3' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data=aux.melt[ Sexo == 'H3' ], stat = 'identity',colour="white", size=0.1) +
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

ggsave( plot = iess_monto_pag_2020, 
        filename = paste0( parametros$resultado_graficos, 'iess_monto_pag_2020_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
