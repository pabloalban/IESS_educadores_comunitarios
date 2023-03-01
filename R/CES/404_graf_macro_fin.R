message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( parametros$graf_modelo_1, encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura proyecciones macroeconómicas' )
load( file = paste0( parametros$RData, 'IESS_macro_estudio.RData' ) )
load( file = paste0( parametros$RData_seg, 'ONU_proyeccion_poblacion.RData' ) )
load( file = paste0( parametros$RData_seg, 'IESS_onu_pea_ecu_int.RData' ) )

message( '\tGraficando la evolución histórica de las hipotésis económicas' )
# Evolución histórica de la inflación promedio anual y del índice de precios (IPC) -----------------
aux<-as.data.table(IPC_Inflacion_Anual)
aux<-subset(aux,Fecha>as.Date("2002-12-31","%Y-%m-%d"))
colnames(aux)<-c('Fecha','IPC','Inflación')
scl = 5
iess_ipc_hist <-ggplot( data = aux ) + 
  geom_line( aes( x = Fecha, y = IPC,colour ="IPC"), size = graf_line_size ) + 
  geom_line( aes( x = Fecha, y = Inflación*scl +60, colour ="Inflación"), size = graf_line_size ) +
  scale_x_date( date_breaks = "2 year", date_labels = "%Y" ) +
  scale_y_continuous( name = "IPC",
                      sec.axis = sec_axis(~./scl-12,
                                          name = "Inflación",
                                          labels = function(b) { paste0(round(b * 1, 0), "%")})) +
  scale_colour_manual( "",
                       breaks = c( "IPC", "Inflación" ),
                       values = c( "IPC" = parametros$iess_green ,
                                   "Inflación" = parametros$iess_blue ) ) +
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = '' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_ipc_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_ipc_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Serie histórica del Salario Básico Unificado -----------------------------------------------------
aux <- as.data.table(SBU_Anual)
aux[ , Tasa_Crecimiento_SBU := SBU / shift(x = SBU, n = 1) - 1]
aux <- aux[ Item == 'Observado', 
            list(Fecha, SBU, Tasa_Crecimiento_SBU) ]
aux[ , Fecha := ymd( paste0(Fecha, '/01/01') ) ]
aux$Fecha<-as.Date(aux$Fecha,"%Y-%m-%d")
aux<-subset(aux,Fecha>=as.Date("2002-01-01","%Y-%m-%d"))

y_lim <- c( 100, 400 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_sbu_hist <- ggplot( data = aux, aes( x = Fecha, y = SBU ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Salario Básico Unificado (USD)' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_date( date_breaks = "2 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_sbu_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_sbu_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución histórica del salario nominal promedio aportado ----------------------------------------
aux <- as.data.table(Salario_Promedio_Anual)
aux <- aux[ Item == 'Observado',
            list(Fecha, Salario_Promedio_Anual) ]
aux[ , Fecha := ymd( paste0(Fecha, '/01/01') ) ]
aux$Fecha<-as.Date(aux$Fecha,"%Y-%m-%d")

y_lim <- c( 500, 750 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_spd_hist <- ggplot( data = aux, aes( x = Fecha, y = Salario_Promedio_Anual ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Salario Promedio Anual (USD)' ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_date( date_breaks = "1 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_spd_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_spd_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución crecimiento real del PIB-----------------------------------------------------
aux <- as.data.table(Crecimiento_PIB_Anual)
aux <- aux[ Item == 'Observado', 
            list(Fecha, Crecimiento=Crecimiento_Pib_Observado) ]
aux[ , Fecha := ymd( paste0(Fecha, '/01/01') ) ]
aux$Fecha<-as.Date(aux$Fecha,"%Y-%m-%d")

y_lim <- c( -6, 16 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 8 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_pib_hist <- ggplot( data = aux, aes( x = Fecha, y = Crecimiento ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Crecimiento real del PIB' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_date( date_breaks = "4 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_pib_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_pib_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución historica de las tasas refernciales activa y pasiva ------------------------------------
aux<-as.data.frame(Tasas)
aux<-select(aux,fecha,activa,pasiva)
aux<-na.omit(aux)
colnames(aux)<-c("Fecha","Tasa Activa", "Tasa Pasiva")
aux <- melt(aux,measure.vars =c("Tasa Activa", "Tasa Pasiva"), 
            value.name = "Valor")

iess_tasas_ref_hist<- ggplot(aux, aes(Fecha)) + 
  geom_line(aes(y = Valor, color = variable),size = graf_line_size ) + 
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y")+
  scale_colour_manual("", 
                      breaks = c("Tasa Activa", "Tasa Pasiva"), 
                      values = c("Tasa Activa" = parametros$iess_green , 
                                 "Tasa Pasiva" = parametros$iess_blue))+
  theme_bw() +
  plt_theme +
  theme(legend.position="bottom") +
  labs( x = '', y = 'Tasas de interés' )+
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_tasas_ref_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasas_ref_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución historica del Rendimiento del BIESS------------------------------------
aux<-as.data.table(Rendimiento_BIESS_Anual)
aux <- aux[ Item == 'Observado', 
            list(Fecha,Rendimiento_BIESS) ]
aux$Fecha<-year(aux$Fecha)
aux[ , Fecha := ymd( paste0(Fecha, '/01/01') ) ]
aux$Fecha<-as.Date(aux$Fecha,"%Y-%m-%d")

y_lim <- c( 0.05, 0.1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 4 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_portaf_hist <- ggplot( data = aux, aes( x = Fecha, y = Rendimiento_BIESS ) ) + 
  geom_line( color = parametros$iess_green, size = graf_line_size ) + 
  labs( x = 'Año', y = 'Rendimiento neto del BIESS' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )) +
  scale_x_date( date_breaks = "1 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_portaf_hist, 
        filename = paste0( parametros$resultado_graficos, 'iess_portaf_hist', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( '\tGraficando las predicciones de las hipotésis económicas' )
# Predicciones modelo para tasas activas referenciales ---------------------------------------------
TablaActiva <- as.data.table(Tasas)
TablaActiva <- TablaActiva[ , list(fecha, activa, p_activa, se_activa) ]
TablaActiva[ , LI := p_activa - 1.96 * se_activa ]
TablaActiva[ , LS := p_activa + 1.96 * se_activa ]
TablaActiva<-subset(TablaActiva,fecha<=as.Date("2059-01-01","%Y-%m-%d"))
y_lim <- c( 0, 0.15 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tasa_act <-  ggplot( TablaActiva, aes(fecha) ) + 
  geom_line( aes(y = activa), 
             color = parametros$iess_green, 
             size = graf_line_size, 
             alpha = 0.8, 
             linetype = 1 ) + 
  geom_line( aes(y = p_activa), 
             color = parametros$iess_blue, 
             size = graf_line_size, 
             alpha = 0.8, 
             linetype = 2 ) +
  geom_line( aes(y = LI), color = 'red', 
             size = graf_line_size, alpha = 0.9, linetype = 2 ) +
  geom_line( aes(y = LS), color = 'red', 
             size = graf_line_size, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Tasa activa referencial' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline(xintercept=as.Date(c("2019-01-01")), color="black", linetype = 2)

ggsave( plot = iess_tasa_act, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_act', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones para tasas pasivas referenciales ----------------------------------------------------
TablaPasiva <- as.data.table(Tasas)
TablaPasiva <- TablaPasiva[ , list(fecha, pasiva, p_pasiva, se_pasiva) ]
TablaPasiva[ , LI := p_pasiva - 1.96 * se_pasiva ]
TablaPasiva[ , LS := p_pasiva + 1.96 * se_pasiva ]
TablaPasiva<-subset(TablaPasiva,fecha<=as.Date("2059-01-01","%Y-%m-%d"))
y_lim <- c( 0.03, 0.07 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk, digits = 2, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_tasa_pas  <- ggplot( TablaPasiva, aes(fecha) ) + 
                  geom_line( aes(y = pasiva), 
                             color = parametros$iess_green, 
                             size = graf_line_size, 
                             alpha = 0.8, 
                             linetype = 1 ) + 
                  geom_line( aes(y = p_pasiva), 
                             color = parametros$iess_blue, 
                             size = graf_line_size, 
                             alpha = 0.8,
                             linetype = 2 ) +
                  geom_line( aes(y = LI), 
                             color = 'red', 
                             size = graf_line_size, 
                             alpha = 0.9, 
                             linetype = 2 ) +
                  geom_line( aes(y = LS), color = 'red', 
                             size = graf_line_size, alpha = 0.8, linetype = 1 ) +
                  labs( x = 'Año', y = 'Tasa pasiva referencial' ) +
                  scale_y_continuous(labels = scales::percent_format( accuracy = 0 ),
                                     breaks = y_brk, limits = y_lim  ) +
                  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
                  theme_bw() +
                  plt_theme +
                  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
                  geom_vline(xintercept=as.Date(c("2019-01-01")),color="black",linetype = 2)

ggsave( plot = iess_tasa_pas, 
        filename = paste0( parametros$resultado_graficos, 'iess_tasa_pas', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones del modelo para el IPC --------------------------------------------------------------
aux <- as.data.frame(IPC_Mensual)
aux <- dcast(aux, Fecha + Intervalo_Superior+ Intervalo_Inferior~ Item,value.var="IPC")
y_lim <- c( 0, 280 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_ipc <- ggplot( aux, aes( Fecha ) ) + 
  geom_line( aes( y = Observada ), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) + 
  geom_line( aes( y = Prediccion ), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha = 0.8, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Inferior ), 
             color = 'red', 
             size = 0.3, alpha = 0.9, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Superior ),
             color = 'red',
             size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'IPC' ) +
  scale_y_continuous( labels = y_lbl, breaks = y_brk, limits = y_lim ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) ) +
  geom_vline( xintercept = as.Date(c("2018-01-01")), color = "black", linetype = 2 )

ggsave( plot = iess_proy_ipc, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_ipc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones la inflación ------------------------------------------------------------------------
aux <- as.data.frame(Inflacion_Anual)
aux<-dcast(aux, Fecha + Intervalo_Superior_Inflacion + Intervalo_Inferior_Inflacion ~ Item,
           value.var="Inflacion")
aux$Fecha<-as.Date(paste0(aux$Fecha, '/01/01'),"%Y/%m/%d")
y_lim <- c( 0, 0.04 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_inflacion <- ggplot( aux, aes(Fecha) ) +
  geom_line( aes(y = Observada), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes(y = Prediccion), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha = 0.8, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Inferior_Inflacion), color = 'red', size = 0.3, alpha = 0.9, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Superior_Inflacion), color = 'red', size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Infalción acumulada anual' ) +
  scale_y_continuous( labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline(xintercept=as.Date(c("2018-01-01")),color="black",linetype = 2)

ggsave( plot = iess_proy_inflacion,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_inflacion', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones la inflación promedio anual ------------------------------------------------------------------------
aux<-select(as.data.frame(IPC_Mensual),Fecha,IPC,Item)
aux['Mes']<-month(aux$Fecha)
aux<-aux%>%
  group_by(Mes)%>%
  mutate(variacion_anual=log(IPC/lag(IPC,1)))
aux['Anio']<-year(aux$Fecha)
aux<-aux%>%
  group_by(Anio)%>%
  mutate(infacion_promedio=mean(variacion_anual)) %>% 
  ungroup() %>%
  dplyr::distinct(Anio,.keep_all = TRUE) %>% 
  select(Anio,Item,infacion_promedio) %>% filter(Anio>2002)

aux<-dcast(aux, Anio + infacion_promedio ~ Item,
           value.var="infacion_promedio")
aux[which(aux$Anio=='2018'),]$Prediccion<-aux[which(aux$Anio=='2018'),]$Observada
aux$Anio<-as.Date(paste0(aux$Anio, '/01/01'),"%Y/%m/%d")
y_lim <- c( 0, 0.12 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_inflacion_promedio <- ggplot( aux, aes(Anio) ) +
  geom_line( aes(y = Observada), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes(y = Prediccion), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha = 0.8, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Inferior_Inflacion), color = 'red', size = 0.3, alpha = 0.9, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Superior_Inflacion), color = 'red', size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Infalción promedio anual' ) +
  scale_y_continuous( labels = scales::percent_format( accuracy = 1 ) ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline(xintercept=as.Date(c("2018-01-01")),color="black",linetype = 2)

ggsave( plot = iess_proy_inflacion_promedio,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_inflacion_promedio', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones del modelo para el SBU --------------------------------------------------------------
aux <- as.data.frame(SBU_Anual)
aux<-dcast(aux, Fecha + Intervalo_Superior+ Intervalo_Inferior~ Item,value.var="SBU")
aux$Fecha<-as.Date(paste0(aux$Fecha, '/01/01'),"%Y/%m/%d")
y_lim <- c( 0, 1300 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_sbu <- ggplot( aux, aes( Fecha ) ) + 
  geom_line( aes( y = Observado ), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) + 
  geom_line( aes( y = Proyeccion ), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha = 0.8,linetype = 2 ) +
  geom_line( aes( y = Intervalo_Inferior ), 
             color = 'red', 
             size = 0.3, alpha = 0.9, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Superior ),
             color = 'red', 
             size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'SBU (USD)' ) +
  scale_y_continuous( labels = y_lbl, limits = y_lim,breaks = y_brk ) +
  scale_x_date( date_breaks = "6 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) ) +
  geom_vline( xintercept = as.Date( "2018-01-01" ), color = "black", linetype = 2 )

ggsave( plot = iess_proy_sbu, 
        filename = paste0( parametros$resultado_graficos, 'iess_proy_sbu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones la inflación ------------------------------------------------------------------------
aux <- as.data.frame( Inflacion_Anual)
aux[which(aux$Fecha == '2018'),]$Item<-'Observada'
aux[which(aux$Fecha == '2018'),]$Inflacion<-0.027
aux<-dcast(aux, Fecha + Intervalo_Superior_Inflacion + Intervalo_Inferior_Inflacion ~ Item,
           value.var = "Inflacion")
aux[which(aux$Fecha == '2018'),]$Prediccion<-aux[which(aux$Fecha=='2018'),]$Observada
aux$Fecha<-as.Date(paste0(aux$Fecha, '/01/01'),"%Y/%m/%d")
y_lim <- c( 0, 0.04 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 11 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_inflacion <- ggplot( aux, aes( Fecha ) ) +
  geom_line( aes( y = Observada ), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes( y = Prediccion ), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha = 0.8, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Inferior_Inflacion), color = 'red', size = 0.3, alpha = 0.9, linetype = 2 ) +
  #geom_line( aes(y = Intervalo_Superior_Inflacion), color = 'red', size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Infalción acumulada anual' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )  ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text( angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = as.Date( "2018-01-01" ), color = "black", linetype = 2 )

ggsave( plot = iess_proy_inflacion,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_inflacion', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones del salario promedio ------------------------------------------------------------------------
aux <- as.data.frame(Salario_Promedio_Anual)
aux<-dcast(aux, Fecha + Intervalo_Inferior_Anual + Intervalo_Superior_Anual ~ Item,
           value.var = "Salario_Promedio_Anual" )
aux$Fecha<-as.Date( paste0( aux$Fecha, '/01/01'),"%Y/%m/%d")
y_lim <- c( -2000, 5000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_salarios <- ggplot( aux, aes( Fecha ) ) +
  geom_line( aes( y = Observado ), 
             color = parametros$iess_green, 
             size =  graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes( y = Prediccion ), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha=0.8, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Inferior_Anual ), 
             color = 'red', 
             size = 0.3, alpha = 0.9, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Superior_Anual ), 
             color = 'red', 
             size = 0.3, alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Salario Promedio (USD)' ) +
  scale_y_continuous( labels = y_lbl, limits = y_lim, breaks = y_brk ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline( xintercept = as.Date( "2019-01-01" ), 
              color = "black", linetype = 2 )

ggsave( plot = iess_proy_salarios,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_salarios', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Predicciones del crecimiento del PIB ------------------------------------------------------------------------
aux <- as.data.frame(Crecimiento_PIB_Anual)
aux<-dcast(aux, Fecha + Intervalo_Superior + Intervalo_Inferior~ Item,
           value.var = "Crecimiento_Pib_Observado")
aux$Fecha<-as.Date( paste0(aux$Fecha, '/01/01'),"%Y/%m/%d")
y_lim <- c( -0.01, 0.06 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_proy_pib <- ggplot( aux, aes(Fecha) ) +
  geom_line( aes( y = Observado ), 
             color = parametros$iess_green,
             size = graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes( y = Prediccion ), 
             color = parametros$iess_blue, 
             size = graf_line_size , alpha=0.8, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Inferior ), 
             color = 'red', size = 0.3, 
             alpha = 0.9, linetype = 2 ) +
  geom_line( aes( y = Intervalo_Superior ), 
             color = 'red', size = 0.3, 
             alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Crecimiento real del PIB' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )  ) +
  scale_x_date( date_breaks = "4 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline(xintercept=as.Date(c("2018-01-01")),
             color="black",linetype = 2)

ggsave( plot = iess_proy_pib,
        filename = paste0( parametros$resultado_graficos, 'iess_proy_pib', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Predicciones del rendimiento del portafolio global de inversiones del BIESS ---------------
aux <- as.data.table(Rendimiento_BIESS_Anual)
aux$Fecha<-year(aux$Fecha)
aux$Fecha<-as.Date(paste0(aux$Fecha, '/01/01'),"%Y/%m/%d")
aux<-dcast(aux, Fecha + Intervalo_Inferior + Intervalo_Superior ~ Item,
           value.var="Rendimiento_BIESS")
aux[which(aux$Fecha==as.Date("2018/01/01","%Y/%m/%d")),]$Prediccion<-aux[which(aux$Fecha==as.Date("2018/01/01","%Y/%m/%d")),]$Observado
y_lim <- c( 0.03, 0.1 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_portaf_proy <- ggplot( aux, aes(Fecha) ) +
  geom_line( aes(y = Observado), 
             color = parametros$iess_green, 
             size = graf_line_size , alpha = 0.8, linetype = 1 ) +
  geom_line( aes(y = Prediccion),
             color = parametros$iess_blue, 
             size = graf_line_size , alpha=0.8, linetype = 2 ) +
  geom_line( aes(y = Intervalo_Inferior), 
             color = 'red', size = 0.3, 
             alpha = 0.9, linetype = 2 ) +
  geom_line( aes(y = Intervalo_Superior),
             color = 'red', size = 0.3, 
             alpha = 0.8, linetype = 2 ) +
  labs( x = 'Año', y = 'Rendimiento BIESS' ) +
  scale_y_continuous(labels = scales::percent_format( accuracy = 1 )  ) +
  scale_x_date( date_breaks = "3 year", date_labels = "%Y" ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )+
  geom_vline(xintercept=as.Date(c("2018-01-01")),
             color="black",linetype = 2)

ggsave( plot = iess_portaf_proy,
        filename = paste0( parametros$resultado_graficos, 'iess_portaf_proy', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
# Evolución histórica del rendimiento promedio de los bonos del Estado ecuatoriano -----------------
Colocacion_Bonos <- as.data.frame(Colocacion_Bonos)
setnames(Colocacion_Bonos, c('Año', 'Monto_Nominal_Colocado', 'Rendimiento_Promedio_Ponderado',
                             'Plazo_Promedio_Ponderado'))
aux <- Colocacion_Bonos
aux$Año<-ymd( paste0(aux$Año, '/01/01') )
aux<-as.data.frame(aux)
aux$Monto_Nominal_Colocado<-as.numeric(aux$Monto_Nominal_Colocado)
aux['Instrumento']<-"Monto nonimal colocado"
aux<-aux %>% select(Año,Instrumento,Monto_Nominal_Colocado,Rendimiento_Promedio_Ponderado)
df_bar <- aux %>% select(-Rendimiento_Promedio_Ponderado)
df_line = aux %>% select(Año, Rendimiento_Promedio_Ponderado)

scl = 1000000000  # escala de miles de millones
hmts=1 #homotecia

y_lim <- c( 0, 8000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_bonos_rend<- ggplot(data = df_bar, aes(x = Año, 
                                            y = df_bar$Monto_Nominal_Colocado, fill = Instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Año,
                y = df_line$Rendimiento_Promedio_Ponderado*hmts*scl*100-400000000, group = 1, 
                linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=1) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Monto nominal colocado (millones USD)', 
                     labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*100)+ 0.004, 
                                         name = 'Rendimiento Promedio Ponderado',
                                         labels = function(b) { paste0(round(b * 100, 2), '%')})) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_bonos_rend, 
        filename = paste0( parametros$resultado_graficos, 'iess_bonos_rend', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Las pirámides poblacionales de la población nacional --------------------------------------------------------
poblacion <- as.data.table(ONU_proyeccion_poblacion)
poblacion<-poblacion[,list(Orden, Etiqueta,H2018=(-1)*H2018/sum(H2018),M2018=M2018/sum(M2018),H2038=(-1)*H2038/sum(H2038),M2038=M2038/sum(M2038),H2058=(-1)*H2058/sum(H2058),M2058=M2058/sum(M2058))]


#Pirámide población 2018------------------------------------------------------------------
aux<-select(as.data.frame(poblacion),Orden,Etiqueta,H2018,M2018)
aux <- melt(aux,measure.vars =c("H2018", "M2018"),
            value.name = "Valor",
            variable.name = "Sexo")
aux <- data.table(aux)

salto_y<-5
salto_x<-0.02
brks_y <- seq(-0.10,0.10,salto_x)
lbls_y <- paste0(as.character(c(seq(0.10, 0, -salto_x)*100, seq(salto_x, 0.10, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_poblacion2018<-ggplot(aux, aes(x = Orden, y = Valor, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ Sexo == 'M2018' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ Sexo == 'H2018' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = aux$Orden, labels = aux$Etiqueta) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, 
                             label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_poblacion2018,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_poblacion2018', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Pirámide población 2038------------------------------------------------------------------
aux<-select(as.data.frame(poblacion),Orden,Etiqueta,H2038,M2038)
aux <- melt(aux,measure.vars =c("H2038", "M2038"),
            value.name = "Valor",
            variable.name = "Sexo")
aux <- as.data.table(aux)

salto_y<-5
salto_x<-0.02
brks_y <- seq(-0.08,0.08,salto_x)
lbls_y <- paste0(as.character(c(seq(0.08, 0, -salto_x)*100, seq(salto_x, 0.08, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_poblacion2038<-ggplot(aux, aes(x = Orden, y = Valor, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ Sexo == 'M2038' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ Sexo == 'H2038' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = aux$Orden, labels = aux$Etiqueta) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, 
                             label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_poblacion2038,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_poblacion2038', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pirámide población 2058------------------------------------------------------------------
aux<-select(as.data.frame(poblacion),Orden,Etiqueta,H2058,M2058)
aux <- melt(aux,measure.vars =c("H2058", "M2058"),
            value.name = "Valor",
            variable.name = "Sexo")
aux <- as.data.table(aux)

salto_y<-5
salto_x<-0.02
brks_y <- seq(-0.08,0.08,salto_x)
lbls_y <- paste0(as.character(c(seq(0.08, 0, -salto_x)*100, seq(salto_x, 0.08, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_poblacion2058<-ggplot(aux, aes(x = Orden, y = Valor, fill=Sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = aux[ Sexo == 'M2058' ], stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = aux[ Sexo == 'H2058' ], stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = aux$Orden, labels = aux$Etiqueta) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_poblacion2058,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_poblacion2058', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Pirámide pea 2018------------------------------------------------------------------
pea <-as.data.frame(onu_pea_tot_int) %>% 
  dplyr::select(year,sex,x,pea_int) %>% 
  filter(year %in% c('2018','2038','2058'))
pea<- pea %>% group_by(sex,year) %>% mutate(dist=pea_int/sum(pea_int)) %>% ungroup()
pea[which(pea$sex=='M'),]$dist<-(-1)*pea[which(pea$sex=='M'),]$dist

salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_pea2018<-ggplot(pea, aes(x = x, y = dist, fill=sex)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = pea[which(pea$sex == 'M' & pea$year=='2018'), ],
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = pea[which(pea$sex == 'F' & pea$year=='2018'), ], 
            stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels =lbls_x ) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0, label.vjust = 0.5,reverse = TRUE))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c( "Mujeres","Hombres"))

ggsave( plot = iess_pir_pea2018,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pea2018', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Piramide del pea a 2038----------------------------------------------------------------
salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_pea2038<-ggplot(pea, aes(x = x, y = dist, fill=sex)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = pea[which(pea$sex == 'M' & pea$year=='2038'), ], 
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = pea[which(pea$sex == 'F' & pea$year=='2038'), ], 
            stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels =lbls_x ) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right", 
                             label.hjust = 0, 
                             label.vjust = 0.5,
                             reverse = TRUE))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c( "Mujeres","Hombres"))

ggsave( plot = iess_pir_pea2038,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pea2038', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Piramide del pea a 2058----------------------------------------------------------------
salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(15,100,salto_y)
lbls_x <- paste0(as.character(brks_x))


iess_pir_pea2058<-ggplot(pea, aes(x = x, y = dist, fill=sex)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  geom_bar( data = pea[which(pea$sex == 'M' & pea$year=='2058'), ], 
            stat = 'identity',colour="white", size=0.1) +
  geom_bar( data = pea[which(pea$sex == 'F' & pea$year=='2058'), ], 
            stat = 'identity',colour="white", size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels =lbls_x ) +
  coord_flip() +
  #theme_tufte()+
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,
                             label.position = "right",
                             label.hjust = 0, 
                             label.vjust = 0.5,
                             reverse = TRUE))+
  theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
  scale_fill_manual(values = c(parametros$iess_green,parametros$iess_blue),
                    labels = c( "Mujeres","Hombres"))

ggsave( plot = iess_pir_pea2058,
        filename = paste0( parametros$resultado_graficos, 'iess_pir_pea2058', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# --------------------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

