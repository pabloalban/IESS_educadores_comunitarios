message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_IVM_inversiones.RData' ) )

#Portafolio de inversiones -------------------------------------------------------------------------
message( '\tGraficando evolución histórica de las inversiones en valor nominal' )

# Evolución histórica de la inversiones del Fondo del Seguro IVM -----------------------------------
aux<-as.data.table(recurs_adm_biess)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux['Intrumento']<-'Saldo valor nominal'
aux<-dplyr::select(aux,Periodo,inversiones,inversiones,rendimiento_neto,Intrumento)
df_bar <- aux %>% dplyr::select(-rendimiento_neto)
df_line = aux %>% dplyr::select(Periodo, rendimiento_neto)

scl = 1000000000

y_lim <- c( 0, 10000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_inv_tot<- ggplot(data = df_bar, aes(x = Periodo, y = inversiones, fill = Intrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line, aes(x = Periodo, y = rendimiento_neto*scl*100-900000000, group = 1,
                                linetype = 'Rendimiento Neto'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Inversiones (millones USD)', labels = y_lbl, breaks = y_brk,limits =y_lim, 
                     sec.axis = sec_axis(~./(scl*100)+0.009, name = 'Rendimiento Neto',
                                         labels = function(b) { paste0(round(b * 100, 0), '%')},
                                         breaks=c(0,0.02,0.04,0.06,0.08,0.10))) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_inv_tot, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_tot', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolucion de los inv en creditos hipotecarios y quirografarios------------------------------------
aux<-as.data.table(creditos)
aux[ , Año := ymd( paste0(ano, '/01/01') ) ]
aux<-data.frame(select(aux,Año,quirografarios,hipotecarios,rendimiento))
df_bar<- melt(aux, id.vars = 'Año', 
              measure.vars = c('quirografarios', 'hipotecarios', 'rendimiento'),
              variable.name = 'prestamos', 
              value.name = 'monto') %>% filter(prestamos != 'rendimiento')
df_line = aux %>% select(Año, rendimiento)

scl = 1000000000 # escala en miles de millones
hmts= 6 #homotecia
y_lim <- c( 0, 5100000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.083,0.085,0.087,0.089,0.091,0.093)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_creditos_inv<- ggplot(data = df_bar, aes(x = Año, y = monto, fill = prestamos)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line, aes(x = Año, y = rendimiento*scl*hmts*100-50660000000, group = 1,linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Inversiones (millones USD)', labels = y_lbl, breaks = y_brk, limits = y_lim,
                     sec.axis = sec_axis(~./(scl*hmts*100)+0.08443333, name = 'Rendimiento Promedio',
                                         labels = ydual_lbl,breaks = ydual_brk)) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_creditos_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_creditos_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en bonos del Estado ----------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='Bonos del Estado') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000000  # escala de miles de millones
hmts=2 #homotecia

y_lim <- c( 0, 5000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.05,0.055,0.06,0.065,0.07,0.075,0.08)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_bonos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*100-11000000000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk, limits = y_lim,
                     sec.axis = sec_axis(~./(scl*hmts*100)+ 0.0513, name = 'Rendimiento Promedio',
                                         labels = ydual_lbl,
                                         breaks = ydual_brk)) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_bonos_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_bonos_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en CETES ---------------------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='CETES') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000  # escala de millones
hmts=2 #homotecia

y_lim <- c( 0, 80000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_cetes_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*1000-1000000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*1000)-0.0005, name = 'Rendimiento Promedio',
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


ggsave( plot = iess_cetes_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_cetes_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Obligaciones --------------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='Obligaciones') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000  # escala de millones
hmts=7 #homotecia

y_lim <- c( 0, 15000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.08,0.085,0.09,0.095)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_obligaciones_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*100-55000000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*100)+0.07857143, name = 'Rendimiento Promedio',
                                         labels = ydual_lbl,
                                         breaks=ydual_brk)) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_obligaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_obligaciones_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Titularizaciones ----------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='Titularizaciones') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000  # escala de millones
hmts=35 #homotecia

y_lim <- c( 0, 90000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.075,0.08,0.085,0.09,0.095)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_titularizaciones_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*100-250000000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk, limits = y_lim,
                     sec.axis = sec_axis(~./(scl*hmts*100)+0.07142857, name = 'Rendimiento Promedio',
                                         labels = ydual_lbl,
                                         breaks = ydual_brk)) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_titularizaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_titularizaciones_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en fidecomisos ---------------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='Fideicomisos') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000  # escala de millones
hmts=20 #homotecia

y_lim <- c( 0, 280000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0,0.02,0.04,0.06,0.08,0.10,0.12,0.14)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_fidecomisos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*100-1000000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,limits = y_lim,
                     sec.axis = sec_axis(~./(scl*hmts*100)-0.0005, name = 'Rendimiento Promedio',
                                         #breaks = c(0,0.3,0.6,0.9,0.12),
                                         labels = ydual_lbl,
                                         breaks = ydual_brk)) + 
  scale_fill_manual(values = c('#007435', '#003F8A'))+
  theme_bw() +
  plt_theme+
  theme(legend.position='bottom') +
  labs( x = '', y = '' )+
  theme(legend.background = element_rect(fill = 'transparent'), 
        legend.box.background = element_rect(fill = 'transparent', colour = NA),
        legend.key = element_rect(fill = 'transparent'), 
        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_fidecomisos_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_fidecomisos_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en renta variable ------------------------------------------------------------------
aux<-as.data.table(inv_instrumento)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux<-aux %>% 
  filter(instrumento=='Renta') %>% 
  select(Periodo,instrumento,saldo,rendimiento_ponderado)
df_bar <- aux %>% select(-rendimiento_ponderado)
df_line = aux %>% select(Periodo, rendimiento_ponderado)

scl = 1000000  # escala de miles de millones
hmts=0.6 #homotecia

y_lim <- c( 0, 125000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_rv_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = saldo, fill = instrumento)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_line(data = df_line,
            aes(x = Periodo,
                y = rendimiento_ponderado*hmts*scl*1000, group = 1, linetype = 'Rendimiento Promedio'),
            inherit.aes = FALSE,
            size=graf_line_size) +
  scale_linetype_manual(NULL, values = 1) +
  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, breaks = y_brk,
                     sec.axis = sec_axis(~./(scl*hmts*1000), name = 'Rendimiento Promedio',
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

ggsave( plot = iess_rv_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_rv_hist_inv', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
#Limpiando Ram--------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()