message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tCargando Rdatas' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_historicos.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_masa_salarial_historico.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_salarios.RData' ) ) 
load( file = paste0( parametros$RData_seg, 'IESS_CES_demografia.RData' ) ) 

anio_ini <- 2012
anio_fin <- 2020

# Evolución del número de cotizantes a desempleo y cesantía-----------------------------------------
message( '\tGraficando demografía de los cotizantes de cesantía' )
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

#Piramide de la distribución de los salarios de los cotizantes de desempleo-------------------------
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



message( '\tGraficando demografía de los beneficiarios de cesantía' )

# 1. 1. Débitos automáticos-------------------------------------------------------------------------
aux<-evo_hist_debitos

x_lim <- c( anio_ini, anio_fin )
x_brk <- seq(anio_ini, anio_fin,1)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 200000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_debitos <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_debitos, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_debitos_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# 1. 2. Parte variable de desempleo-----------------------------------------------------------------
aux <-evo_hist_pv_des

x_lim <- c( 2016, 2020 )
x_brk <- 2016:2020
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 8000)
y_brk <- seq( y_lim[1], y_lim[2], 2000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_ben_pago_pv_des <- ggplot( ) + 
  geom_line( data = aux ,
             aes( x = anio, y = benf_p1,colour ="Pago 1"),
             size = graf_line_size,
             lineend = "round" ) +
  geom_line( data = aux ,
             aes( x = anio, y = benf_p2,colour ="Pago 2"),
             size = graf_line_size,
             lineend = "round" ) + 
  geom_line( data = aux ,
             aes( x = anio, y = benf_p3,colour ="Pago 3"),
             size = graf_line_size,
             lineend = "round" ) + 
  geom_line( data = aux ,
             aes( x = anio, y = benf_p4,colour ="Pago 4"),
             size = graf_line_size,
             lineend = "round" ) + 
  geom_line( data = aux ,
             aes( x = anio, y = benf_p5,colour ="Pago 5"),
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

ggsave( plot = iess_ben_pago_pv_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_ben_pago_pv_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 3. Cesantía por jubilación---------------------------------------------------------------------
aux<-evo_hist_jub

x_lim <- c( anio_ini, anio_fin )
x_brk <- seq( anio_ini, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 50000)
y_brk <- seq( y_lim[1], y_lim[2], 10000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_jub <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Jubilados' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_jub, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_jub_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 4. Cesantía por fallecimiento del afiliado-----------------------------------------------------
aux<-evo_hist_fall

x_lim <- c( anio_ini, anio_fin )
x_brk <- seq( anio_ini, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 5000)
y_brk <- seq( y_lim[1], y_lim[2], 1000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_fall <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Derechohabientes' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_fall, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_fall_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# 1. 5. Cesantía Normal-----------------------------------------------------------------------------
aux<-evo_hist_ces_normal

x_lim <- c( anio_ini, anio_fin )
x_brk <- seq( anio_ini, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 250000)
y_brk <- seq( y_lim[1], y_lim[2], 50000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_ces_normal <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_ces_normal, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_ces_normal_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 6. Cesantía a afiliados voluntarios------------------------------------------------------------
aux<-evo_hist_vol

x_lim <- c( 2016, anio_fin )
x_brk <- seq(2016, anio_fin,1)
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 10000)
y_brk <- seq( y_lim[1], y_lim[2], 2000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_vol <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_vol, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_vol_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# 1. 7. Cesantía por licencia de maternidad---------------------------------------------------------
aux<-evo_hist_lic_mat

x_lim <- c( 2017, anio_fin )
x_brk <- seq( 2017, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 25)
y_brk <- seq( y_lim[1], y_lim[2], 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_lic_mat <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_lic_mat, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_lic_mat_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 8. Cesantía por cruze de obligaciones----------------------------------------------------------
aux<-evo_hist_obli

x_lim <- c( 2018, anio_fin )
x_brk <- seq( 2018, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 750)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_obli <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_obli, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_obli_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 9. Cesantía a Zafreros-------------------------------------------------------------------------

# 1. 10. Reliquidaciones----------------------------------------------------------------------------
aux<-evo_hist_rel

x_lim <- c( anio_ini, anio_fin )
x_brk <- seq( anio_ini, anio_fin,1 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 600)
y_brk <- seq( y_lim[1], y_lim[2], 100 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_evo_hist_rel <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = beneficiarios, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Beneficiarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_evo_hist_rel, 
        filename = paste0( parametros$resultado_graficos, 'iess_evo_hist_rel_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 1. 11. Cesantía General y adicional---------------------------------------------------------------
#evo_hist_ces 



# 2. Piramide poblacional en 2020---------------------------------------------------------------
message( '\tGraficando piramides poblacionales de los beneficiarios de cesantía' )
# 2. 1. Débitos automáticos-------------------------------------------------------------------------
aux<-as.data.table( edad_sexo_debitos ) 
aux <- aux[anio=='2020']
max_edad<-85
min_edad<-15

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(15,85,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_debitos<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_debitos, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_debitos_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 2. Parte variable de desempleo-----------------------------------------------------------------
aux<-as.data.table( edad_sexo_pv_des ) 
aux <- aux[anio=='2020']
max_edad<-75
min_edad<-20

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.07)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_pv_des<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_pv_des, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_pv_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 3. Cesantía por jubilación---------------------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_jub ) 
aux <- aux[anio=='2020']
max_edad<-85
min_edad<-45

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.05
lim_y<- c(-0.15,0.25)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_jub<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_jub, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_jub', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 2. 4. Cesantía por fallecimiento del afiliado-----------------------------------------------------
aux<-as.data.table( edad_sexo_ces_fal ) 
aux <- aux[anio=='2020']
max_edad<-100
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_fal<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_fal, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_fal', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 5. Cesantía Normal-----------------------------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_normal ) 
aux <- aux[anio=='2020']
max_edad<-90
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_normal<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_normal, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_normal', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 6. Cesantía a afiliados voluntarios------------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_vol ) 
aux <- aux[anio=='2020']
max_edad<-90
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_vol<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_vol, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_vol', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# 2. 7. Cesantía por licencia de maternidad---------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_lic_mat ) 
aux <- aux[anio=='2020']
max_edad<-42
min_edad<-28

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-2
salto_x<-0.1
lim_y<- c(-1,0.25)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_lic_mat<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_lic_mat, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_lic_mat', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# 2. 8. Cesantía por cruze de obligaciones----------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_obli ) 
aux <- aux[anio=='2020']
max_edad<-72
min_edad<-22

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.07)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_obli<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_obli, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_obli', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 9. Cesantía a Zafreros-------------------------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_zaf ) 
aux <- aux[anio=='2020']
max_edad<-90
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.20,0.05)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_zaf<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y, limits = c(-0.20,0)) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_blue,parametros$iess_green),
                    labels = c("Hombres","Mujeres"))

ggsave( plot = iess_pir_edad_sexo_ces_zaf, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_zaf', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# 2. 10. Reliquidaciones----------------------------------------------------------------------------
aux<-as.data.table( edad_sexo_ces_rel ) 
aux <- aux[order(-sexo)]
aux <- aux[anio=='2020']
max_edad<-90
min_edad<-18

aux[sexo=="M", fdp:=-fdp]
aux[sexo=="F", fdp:=fdp]

salto_y<-10
salto_x<-0.01
lim_y<- c(-0.05,0.07)
brks_y <- seq(lim_y[1],lim_y[2],salto_x)
lbls_y <- paste0(as.character(c(seq(abs(lim_y[1]), 0, -salto_x)*100, seq(salto_x, lim_y[2], salto_x)*100)), "%")
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- paste0(as.character(brks_x))
lim_x<-c(min_edad,max_edad)

iess_pir_edad_sexo_ces_rel<-ggplot(aux, aes(x = x, y = fdp, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ sexo == 'F' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ sexo == 'M' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x, limits = lim_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom") +   
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c("Mujeres","Hombres"))

ggsave( plot = iess_pir_edad_sexo_ces_rel, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edad_sexo_ces_rel', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar memoria------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
