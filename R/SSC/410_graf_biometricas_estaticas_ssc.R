message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_SSC_tasas_mortalidad_estimadas.RData' ) )

# Graficando distribución de expuesto de las tablas biometricas para activos------------------------
message( '\tGraficando distribución de expuesto de las tablas biometricas para activos' )

aux <- copy( prob_mue_act )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(N_exp,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_exp:=-N_exp]
aux[sexo=="H", N_exp:=N_exp/N[1,1]]
aux[sexo=="M", N_exp:=N_exp/N[1,1]]

M <- data.frame(( aux[,max(abs(N_exp),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_exp_ssc <- ggplot( aux, aes(x = edad, y = N_exp, fill=sexo)) +
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

ggsave( plot = iess_pir_exp_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_exp_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_exp_ssc', parametros$graf_ext ))
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
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1201+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_exp_ssc', parametros$graf_ext ))

# Graficando distribución de muertos de las tablas biometricas para activos ------------------------
message( '\tGraficando distribución de muertos de las tablas biometricas para activos' )

aux <- copy( prob_mue_act )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(N_mue,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_mue:=-N_mue]
aux[sexo=="H", N_mue:=N_mue/N[1,1]]
aux[sexo=="M", N_mue:=N_mue/N[1,1]]

M <- data.frame(( aux[,max(abs(N_mue),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_mue_ssc <- ggplot( aux, aes(x = edad, y = N_mue, fill=sexo)) +
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

ggsave( plot = iess_pir_mue_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_mue_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_mue_ssc', parametros$graf_ext ))
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

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+20")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1301+25")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_mue_ssc', parametros$graf_ext ))

# Graficando tablas biometricas para activos ------------------ ------------------------------------
message( '\tGraficando tablas biometricas para activos' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

aux <- copy( iess_mort )
aux <- aux[ , list( edad, sexo, qx_est, qx )]
setorder( aux, sexo, edad)
min_edad <- 15
max_edad <- 105
aux <- aux[ edad>=min_edad & edad <=max_edad] 

x_lim <- c( min_edad, max_edad )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  aux[ sexo == 'F' & is.finite( qx_est ) & is.finite( qx ) ]

plt_afi_f <- ggplot( data = aux_f ) + 
  geom_point( aes( x = edad, y = qx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) + 
  geom_line( aes( x = edad, y = qx, linetype ='Suavizado' ), size = graf_line_size, 
             colour=parametros$iess_green ) + 
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5), 
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_afi_f, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_afi_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  aux[ sexo == 'M' & is.finite( qx_est ) & is.finite( qx ) ]

plt_afi_m <- ggplot( data = aux_m ) + 
  geom_point( aes( x = edad, y = qx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) + 
  geom_line( aes( x = edad, y = qx, linetype ='Suavizado' ), size = graf_line_size, 
             colour=parametros$iess_green ) + 
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5), 
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_afi_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_afi_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando distribución de expuesto de las tablas biometricas para inactivos------------------------
message( '\tGraficando distribución de expuesto de las tablas biometricas para inactivos' )

aux <- copy( prob_mue_inac )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(N_exp,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_exp:=-N_exp]
aux[sexo=="H", N_exp:=N_exp/N[1,1]]
aux[sexo=="M", N_exp:=N_exp/N[1,1]]

M <- data.frame(( aux[,max(abs(N_exp),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_exp_inac_ssc <- ggplot( aux, aes(x = edad, y = N_exp, fill=sexo)) +
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

ggsave( plot = iess_pir_exp_inac_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_exp_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_exp_inac_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1201+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_exp_inac_ssc', parametros$graf_ext ))

# Graficando distribución de muertos de las tablas biometricas para activos ------------------------
message( '\tGraficando distribución de muertos de las tablas biometricas para activos' )

aux <- copy( prob_mue_inac )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(N_mue,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_mue:=-N_mue]
aux[sexo=="H", N_mue:=N_mue/N[1,1]]
aux[sexo=="M", N_mue:=N_mue/N[1,1]]

M <- data.frame(( aux[,max(abs(N_mue),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_mue_inac_ssc <- ggplot( aux, aes(x = edad, y = N_mue, fill=sexo)) +
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

ggsave( plot = iess_pir_mue_inac_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_mue_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_mue_inac_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+20")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1301+25")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_mue_inac_ssc', parametros$graf_ext ))

# Graficando tablas biometricas para inactivo-------------------------------------------------------
message( '\tGraficando tablas biometricas para inactivos' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

aux <- copy( iess_mort )
aux <- aux[ , list( edad, sexo, qinacx_est, qinacx )]
setorder( aux, sexo, edad)
min_edad <- 15
max_edad <- 105
aux <- aux[ edad>=min_edad & edad <=max_edad] 

x_lim <- c( min_edad, max_edad )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  aux[ sexo == 'F' & is.finite( qinacx_est ) & is.finite( qinacx ) ]

plt_afi_inac_f <- ggplot( data = aux_f ) + 
  geom_point( aes( x = edad, y = qinacx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) + 
  geom_line( aes( x = edad, y = qinacx, linetype ='Suavizado' ), size = graf_line_size, 
             colour=parametros$iess_green ) + 
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5), 
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_afi_inac_f , 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_afi_inac_f_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  aux[ sexo == 'M' & is.finite( qinacx_est ) & is.finite( qinacx ) ]

plt_afi_inac_m <- ggplot( data = aux_m ) + 
  geom_point( aes( x = edad, y = qinacx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) + 
  geom_line( aes( x = edad, y = qinacx, linetype ='Suavizado' ), size = graf_line_size, 
             colour=parametros$iess_green ) + 
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5), 
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_afi_inac_m, 
        filename = paste0( parametros$resultado_graficos, 'iess_plt_afi_inac_m_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando distribución de expuesto de las tablas biometricas para vejez -------------------------
message( '\tGraficando distribución de expuesto de las tablas biometricas para vejez' )

aux <- copy( prob_mue_ben )
max_edad <- 105
min_edad <- 65

aux <- aux[ tipo=='VEJEZ' & edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(N_exp,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_exp:=-N_exp]
aux[sexo=="H", N_exp:=N_exp/N[1,1]]
aux[sexo=="M", N_exp:=N_exp/N[1,1]]

M <- data.frame(( aux[,max(abs(N_exp),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_exp_vej_ssc <- ggplot( aux, aes(x = edad, y = N_exp, fill=sexo)) +
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

ggsave( plot = iess_pir_exp_vej_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1201+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_ssc', parametros$graf_ext ))

# Graficando distribución de muertos de las tablas biometricas para pensionistas de vejez -----------
message( '\tGraficando distribución de muertos de las tablas biometricas para pensionistas de vejez' )

aux <- copy( prob_mue_ben )
max_edad <- 105
min_edad <- 65

aux <- aux[ tipo=='VEJEZ' & edad>=min_edad & edad <=max_edad]
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0

N <- data.frame((aux[, sum(N_mue,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_mue:=-N_mue]
aux[sexo=="H", N_mue:=N_mue/N[1,1]]
aux[sexo=="M", N_mue:=N_mue/N[1,1]]

M <- data.frame(( aux[,max(abs(N_mue),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_mue_vejez_ssc <- ggplot( aux, aes(x = edad, y = N_mue, fill=sexo)) +
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

ggsave( plot = iess_pir_mue_vejez_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+20")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1301+25")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_ssc', parametros$graf_ext ))

# Graficando tablas biometricas para pensionistas de vejez------------------------------------------
message( '\tGraficando tablas biometricas para pensionistas de vejez' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

aux <- copy( iess_mort )
aux <- aux[ , list( edad, sexo, qvx_est, qvx )]
setorder( aux, sexo, edad)
min_edad <- 65
max_edad <- 105
aux <- aux[ edad>=min_edad & edad <=max_edad]

x_lim <- c( min_edad, max_edad )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  aux[ sexo == 'F' & is.finite( qvx_est ) & is.finite( qvx ) ]

plt_qx_f_vejez <- ggplot( data = aux_f ) +
  geom_point( aes( x = edad, y = qvx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qvx, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_f_vejez ,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_f_vejez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  aux[ sexo == 'M' & is.finite( qvx_est ) & is.finite( qvx ) ]

plt_qx_m_vejez <- ggplot( data = aux_m ) +
  geom_point( aes( x = edad, y = qvx_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qvx, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_m_vejez,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_m_vejez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando distribución de expuesto de las tablas biometricas para vejez y jefes inactivos--------
message( '\tGraficando distribución de expuesto de las tablas biometricas para vejez y jefes inactivos' )

aux <- copy( iess_mort[ , list( edad, sexo, Nvi_exp)] )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

N <- data.frame((aux[, sum(Nvi_exp,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", Nvi_exp:=-Nvi_exp]
aux[sexo=="H", Nvi_exp:=Nvi_exp/N[1,1]]
aux[sexo=="M", Nvi_exp:=Nvi_exp/N[1,1]]

M <- data.frame(( aux[,max(abs(Nvi_exp),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_exp_vej_inac_ssc <- ggplot( aux, aes(x = edad, y = Nvi_exp, fill=sexo)) +
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

ggsave( plot = iess_pir_exp_vej_inac_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_inac_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1201+70")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_exp_vej_inac_ssc', parametros$graf_ext ))

# Graficando distribución de muertos de las tablas biometricas para pensionistas de vejez y jefes inactivos -----------
message( '\tGraficando distribución de muertos de las tablas biometricas para pensionistas de vejez y jefes inactivos' )

aux <- copy( iess_mort[ , list( edad, sexo, Nvi_mue)] )
max_edad <- 105
min_edad <- 15

aux <- aux[ edad>=min_edad & edad <=max_edad]
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0

N <- data.frame((aux[, sum(Nvi_mue,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", Nvi_mue:=-Nvi_mue]
aux[sexo=="H", Nvi_mue:=Nvi_mue/N[1,1]]
aux[sexo=="M", Nvi_mue:=Nvi_mue/N[1,1]]

M <- data.frame(( aux[,max(abs(Nvi_mue),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_mue_vejez_inac_ssc <- ggplot( aux, aes(x = edad, y = Nvi_mue, fill=sexo)) +
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

ggsave( plot = iess_pir_mue_vejez_inac_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_inac_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+20")
final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1301+25")

image_write(final_plot, paste0( parametros$resultado_graficos, 'iess_pir_mue_vejez_inac_ssc', parametros$graf_ext ))

# Graficando tablas biometricas para pensionistas de vejez------------------------------------------
message( '\tGraficando tablas biometricas para pensionistas de vejez' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

aux <- copy( iess_mort )
aux <- aux[ , list( edad, sexo, qvix_est, qvinax )]
setorder( aux, sexo, edad)
min_edad <- 15
max_edad <- 105
aux <- aux[ edad>=min_edad & edad <=max_edad]

x_lim <- c( min_edad, max_edad )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  aux[ sexo == 'F' & is.finite( qvix_est ) & is.finite( qvinax ) ]

plt_qx_f_vejez_inac <- ggplot( data = aux_f ) +
  geom_point( aes( x = edad, y = qvix_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qvinax, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_f_vejez_inac ,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_f_vejez_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  aux[ sexo == 'M' & is.finite( qvix_est ) & is.finite( qvinax ) ]

plt_qx_m_vejez_inac <- ggplot( data = aux_m ) +
  geom_point( aes( x = edad, y = qvix_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qvinax, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_m_vejez_inac,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_m_vejez_inac_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Graficando distribución de expuesto de las tablas biometricas para invalidez ---------------------
message( '\tGraficando distribución de expuesto de las tablas biometricas para invalidez' )

aux <- copy( prob_mue_ben )
max_edad <- 105
min_edad <- 20

aux <- aux[ tipo=='INVALIDEZ' & edad>=min_edad & edad <=max_edad] 
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0 

aux <- aux[ sexo=='H']

N <- data.frame((aux[, sum(N_exp,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_exp:=-N_exp]
aux[sexo=="H", N_exp:=N_exp/N[1,1]]


M <- data.frame(( aux[,max(abs(N_exp),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.005
brks_y <- seq( -0.06,0.06,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_exp_inv_ssc <- ggplot( aux, aes(x = edad, y = N_exp, fill=sexo)) +
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

ggsave( plot = iess_pir_exp_inv_ssc, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_exp_inv_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_exp_inv_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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
#final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1201+70")

image_write( plot, paste0( parametros$resultado_graficos, 'iess_pir_exp_inv_ssc', parametros$graf_ext ))

# Graficando distribución de muertos de las tablas biometricas para pensionistas de vejez -----------
message( '\tGraficando distribución de muertos de las tablas biometricas para pensionistas de vejez' )

aux <- copy( prob_mue_ben )
max_edad <- 105
min_edad <- 20

aux <- aux[ tipo=='INVALIDEZ' & edad>=min_edad & edad <=max_edad]
aux[ sexo=='M', sexo:='H']
aux[ sexo=='F', sexo:='M']
#aux[ is.na(aux) ] <- 0
aux <- aux[ sexo=='H']

N <- data.frame((aux[, sum(N_mue,na.rm = TRUE)]))  # número total por sexo

aux[sexo=="H", N_mue:=-N_mue]
aux[sexo=="H", N_mue:=N_mue/N[1,1]]
aux[sexo=="M", N_mue:=N_mue/N[1,1]]

M <- data.frame(( aux[,max(abs(N_mue),na.rm = TRUE),by=sexo])) # En base a este valor poner los límites del eje x

salto_y <- 5
salto_x<- 0.01
brks_y <- seq( -0.09,0.09,salto_x)
lbls_y <- paste0( abs(brks_y)*100,'%')
brks_x <- seq(min_edad,max_edad,salto_y)
lbls_x <- formatC(brks_x,digits = 0,format = 'f')

iess_pir_mue_invalidez_ssc <- ggplot( aux, aes(x = edad, y = N_mue, fill=sexo)) +
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

ggsave( plot = iess_pir_mue_invalidez_ssc,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_mue_invalidez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


plot <- image_read( paste0( parametros$resultado_graficos, 'iess_pir_mue_invalidez_ssc', parametros$graf_ext ))
fig_H <- image_read( paste0( parametros$resultado_graficos, 'hombre.png' ) )

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

plot <- image_composite(plot, image_fill(fig_H, color = "transparent"), offset = "+740+20")
#final_plot <- image_composite(plot, image_fill(fig_M, color = "transparent"), offset = "+1301+25")

image_write(plot, paste0( parametros$resultado_graficos, 'iess_pir_mue_invalidez_ssc', parametros$graf_ext ))

# Graficando tablas biometricas para pensionistas de invalidez--------------------------------------
message( '\tGraficando tablas biometricas para pensionistas de invalidez' )
load( file = paste0( parametros$RData_seg, 'IESS_SSC_suavizamiento_tasas_mortalidad.RData' ) )

aux <- copy( iess_mort )
aux <- aux[ , list( edad, sexo, qix_est, qix )]
setorder( aux, sexo, edad)
min_edad <- 20
max_edad <- 105
aux <- aux[ edad>=min_edad & edad <=max_edad]

x_lim <- c( min_edad, max_edad )
x_brk <- seq( x_lim[1], x_lim[2], 10 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

aux_f <-  aux[ sexo == 'M' & is.finite( qix_est ) & is.finite( qix ) ]

plt_qx_f_invalidez <- ggplot( data = aux_f ) +
  geom_point( aes( x = edad, y = qix_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qix, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_f_invalidez ,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_f_invalidez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

aux_m <-  aux[ sexo == 'M' & is.finite( qix_est ) & is.finite( qix ) ]

plt_qx_m_invalidez <- ggplot( data = aux_m ) +
  geom_point( aes( x = edad, y = qix_est, shape='Estimado'), size = graf_point_size,
              colour=parametros$iess_blue) +
  geom_line( aes( x = edad, y = qix, linetype ='Suavizado' ), size = graf_line_size,
             colour=parametros$iess_green ) +
  labs( x = 'Edad', y = TeX('$q_x$') ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom", legend.box.spacing = unit(0.75, 'cm'),
        axis.text.x = element_text( angle = 0, hjust = 0.5, vjust = 0.5),
        legend.text = element_text(colour = "black"))+
  guides(shape = guide_legend( override.aes = list(size = 1)))

ggsave( plot = plt_qx_m_invalidez,
        filename = paste0( parametros$resultado_graficos, 'iess_plt_qx_m_invalidez_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()