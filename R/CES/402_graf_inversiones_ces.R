message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura portafolio de inversiones' )
file_inversiones <- paste0( parametros$RData_seg, 'IESS_DES_inversiones.RData' )
load( file = file_inversiones )

# Evolución histórica de la inversiones del Fondo del Seguro DES -----------------------------------
message( '\tGráficos del portafolio de inversiones' )
aux<-as.data.table(inv_total_ces)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux['instrumento']<-'Saldo valor nominal'
aux<-select(aux,Periodo,instrumento,inversiones,rendimiento_neto)
df_bar <- aux %>% select(-rendimiento_neto)
df_line = aux %>% select(Periodo, rendimiento_neto)

scl = 100000000
hmts=8 #homotecia

y_lim <- c( 0, 8000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

inv_total_ces<- ggplot(data = df_bar, aes(x = Periodo, y = inversiones, fill = instrumento)) +
                geom_bar(stat='identity',colour='black') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_line(data = df_line, aes(x = Periodo,
                                              y = rendimiento_neto*hmts*scl*100-2000000000, group = 1,
                                              linetype = 'Rendimiento Neto'),
                          inherit.aes = FALSE,
                          size=1) +
                scale_linetype_manual(NULL, values = 1) +
                scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                scale_y_continuous(name = 'Inversiones (millones USD)', 
                                   labels = y_lbl, breaks = y_brk,limits =y_lim, 
                                   sec.axis = sec_axis(~./(scl*hmts*100)+0.025, name = 'Rendimiento Neto',
                                                       labels = function(b) { paste0(round(b * 100, 0), '%')},
                                                       breaks=c(0,0.02,0.04,0.06,0.08,0.10,0.12))) + 
                scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue))+
                theme_bw() +
                plt_theme+
                theme(legend.position='bottom') +
                labs( x = '', y = '' )+
                theme(legend.background = element_rect(fill = 'transparent'), 
                      legend.box.background = element_rect(fill = 'transparent', colour = NA),
                      legend.key = element_rect(fill = 'transparent'), 
                      legend.spacing = unit(-1, 'lines'))

ggsave( plot = inv_total_ces, 
        filename = paste0( parametros$resultado_graficos, 'inv_total_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolucion de los inv en creditos hipotecarios y quirografarios------------------------------------
aux<-as.data.table(inv_creditos_ces)
aux[ , Año := ymd( paste0(ano, '/01/01') ) ]
aux<-select(aux,Año,quirografarios,hipotecarios,rendimiento_promedio_ponderado)
colnames(aux)<-c('Año','Quirografarios','Hipotecarios', 'rendimiento_promedio_ponderado')
df_bar = melt(aux, id.vars = 'Año', 
              measure.vars = c('Quirografarios', 'Hipotecarios', 'rendimiento_promedio_ponderado'),
              variable.name = 'prestamos', 
              value.name = 'monto') %>% filter(prestamos != 'rendimiento_promedio_ponderado')
df_line = aux %>% select(Año, rendimiento_promedio_ponderado)

scl = 1000000000 # escala en miles de millones
hmts= 6 #homotecia

y_lim <- c( 0, 5000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.087,0.088,0.089,0.09,0.091,0.092, 0.093, 0.094)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

inv_creditos_ces <- ggplot(data = df_bar, aes(x = Año, y = monto, fill = prestamos)) +
                    geom_bar(stat='identity',colour='black') +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    geom_line(data = df_line, 
                              aes(x = Año,
                                          y = rendimiento_promedio_ponderado*scl*hmts*100-52400000000,
                                          group = 1,
                                          linetype = 'Rendimiento Promedio'),
                              inherit.aes = FALSE,
                              size=1) +
                    scale_linetype_manual(NULL, values = 1) +
                    scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                    scale_y_continuous(name = 'Inversiones (millones USD)', 
                                       labels = y_lbl, breaks = y_brk,
                                       sec.axis = sec_axis(~./(scl*hmts*100)+0.08733333, 
                                                           name = 'Rendimiento Promedio',
                                                           labels = ydual_lbl,breaks = ydual_brk)) + 
                    scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue))+
                    theme_bw() +
                    plt_theme+
                    theme(legend.position='bottom') +
                    labs( x = '', y = '' )+
                    theme(legend.background = element_rect(fill = 'transparent'), 
                          legend.box.background = element_rect(fill = 'transparent', colour = NA),
                          legend.key = element_rect(fill = 'transparent'), 
                          legend.spacing = unit(-1, 'lines'))

ggsave( plot = inv_creditos_ces, 
        filename = paste0( parametros$resultado_graficos, 'inv_creditos_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en bonos del Estado ----------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Bonos del Estado') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000000  # escala de miles de millones
hmts=3 #homotecia

y_lim <- c( 0, 3000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.065,0.0675,0.07, 0.0725 ,0.075)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_bonos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, fill = Instrumento)) +
                      geom_bar(stat='identity',colour='black') +
                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                      geom_line(data = df_line,
                                aes(x = Periodo,
                                    y =rdto_prom_pond*hmts*scl*100-19520000000,
                                    group = 1, linetype = 'Rendimiento Promedio'),
                                inherit.aes = FALSE,
                                size=1) +
                      scale_linetype_manual(NULL, values = 1) +
                      scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                      scale_y_continuous(name = 'Saldo (millones USD)', 
                                         labels = y_lbl, breaks = y_brk, limits = y_lim,
                                         sec.axis = sec_axis(~./(scl*hmts*100)+ 0.06506667,
                                                             name = 'Rendimiento Promedio',
                                                             labels = ydual_lbl,
                                                             breaks = ydual_brk)) + 
                      scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue))+
                      theme_bw() +
                      plt_theme+
                      theme(legend.position='bottom') +
                      labs( x = '', y = '' )+
                      theme(legend.background = element_rect(fill = 'transparent'), 
                            legend.box.background = element_rect(fill = 'transparent', colour = NA),
                            legend.key = element_rect(fill = 'transparent'), 
                            legend.spacing = unit(-1, 'lines'))


ggsave( plot = iess_bonos_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_bonos_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en certificados de inversion de la CFN ---------------------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Certificados de Inversión CFN') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)


scl = 1000000  # escala de millones
hmts=13 #homotecia

y_lim <- c( 0, 140000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.06,0.0625,0.065,0.0675,0.07)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 2, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_cert_cfn_hist_inv <- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, fill = Instrumento)) +
                          geom_bar(stat='identity',colour='black') +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                          geom_line(data = df_line,
                                    aes(x = Periodo,
                                        y = rdto_prom_pond*hmts*scl*1000-780800000, 
                                        group = 1, linetype = 'Rendimiento Promedio'),
                                    inherit.aes = FALSE,
                                    size=1) +
                          scale_linetype_manual(NULL, values = 1) +
                          scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                          scale_y_continuous(name = 'Saldo (millones USD)', 
                                             labels = y_lbl, breaks = y_brk, limits = y_lim,
                                             sec.axis = sec_axis(~./(scl*hmts*1000)+ 0.06006154, 
                                                                 name = 'Rendimiento Promedio',
                                                                 labels = ydual_lbl,
                                                                 breaks = ydual_brk)) + 
                          scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue))+
                          theme_bw() +
                          plt_theme+
                          theme(legend.position='bottom') +
                          labs( x = '', y = '' )+
                          theme(legend.background = element_rect(fill = 'transparent'), 
                                legend.box.background = element_rect(fill = 'transparent', colour = NA),
                                legend.key = element_rect(fill = 'transparent'), 
                                legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_cert_cfn_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_cert_cfn_des', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Obligaciones --------------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Obligaciones') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000  # escala de millones
hmts=3.8 #homotecia

y_lim <- c( 0, 50000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.0775,0.08,0.0825,0.085,0.0875,0.09)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 2, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_obligaciones_hist_inv <- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal,
                                                        fill = Instrumento)) +
                              geom_bar(stat='identity',colour='black') +
                              theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                              geom_line(data = df_line,
                                        aes(x = Periodo,
                                            y = rdto_prom_pond*hmts*scl*1000-298300000+5000000, 
                                            group = 1, linetype = 'Rendimiento Promedio'),
                                        inherit.aes = FALSE,
                                        size=1) +
                              scale_linetype_manual(NULL, values = 1) +
                              scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                              scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl,
                                                 breaks = y_brk, limits = c(0,52000000),
                                                 sec.axis = sec_axis(~./(scl*hmts*1000)+0.07718421, 
                                                                     name = 'Rendimiento Promedio',
                                                                     labels = ydual_lbl,
                                                                     breaks=ydual_brk)) + 
                              scale_fill_manual(values = c(parametros$iess_green,
                                                           parametros$iess_blue))+
                              theme_bw() +
                              plt_theme+
                              theme(legend.position='bottom') +
                              labs( x = '', y = '' )+
                              theme(legend.background = element_rect(fill = 'transparent'), 
                                    legend.box.background = element_rect(fill = 'transparent', 
                                                                         colour = NA),
                                    legend.key = element_rect(fill = 'transparent'), 
                                    legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_obligaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_obligaciones_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Titularizaciones ----------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Titularizaciones') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 10000000  # escala de millones
hmts=40 #homotecia

y_lim <- c( 0, 280000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.079,0.08,0.081,0.082,0.083,0.084,0.085,0.086)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_titularizaciones_hist_inv <- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, 
                                                            fill = Instrumento)) +
                                  geom_bar(stat='identity',colour='black') +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                                  geom_line(data = df_line,
                                            aes(x = Periodo,
                                                y = rdto_prom_pond*hmts*scl*100-3204000000+40000000, 
                                                group = 1, linetype = 'Rendimiento Promedio'),
                                            inherit.aes = FALSE,
                                            size=1) +
                                  scale_linetype_manual(NULL, values = 1) +
                                  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                                  scale_y_continuous(name = 'Saldo (millones USD)',
                                                     labels = y_lbl, breaks = y_brk, limits = y_lim,
                                                     sec.axis = sec_axis(~./(scl*hmts*100)+0.0791,
                                                                         name = 'Rendimiento Promedio',
                                                                         labels = ydual_lbl,
                                                                         breaks = ydual_brk)) + 
                                  scale_fill_manual(values = c(parametros$iess_green, 
                                                               parametros$iess_blue))+
                                  theme_bw() +
                                  plt_theme+
                                  theme(legend.position='bottom') +
                                  labs( x = '', y = '' )+
                                  theme(legend.background = element_rect(fill = 'transparent'), 
                                        legend.box.background = element_rect(fill = 'transparent', 
                                                                             colour = NA),
                                        legend.key = element_rect(fill = 'transparent'), 
                                        legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_titularizaciones_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_titularizaciones_ces', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
iess_titularizaciones_hist_inv

# Evolución inv en fidecomisos ---------------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Fideicomisos y Negocios Fiduciarios') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)


scl = 10000000  # escala de millones
hmts=10#homotecia

y_lim <- c( 0, 500000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.05,0.06,0.07,0.08,0.09,0.1)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 0, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_fidecomisos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, 
                                                      fill = Instrumento)) +
                            geom_bar(stat='identity',colour='black') +
                            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                            geom_line(data = df_line,
                                      aes(x = Periodo,
                                          y = rdto_prom_pond*hmts*scl*100-510000000+30000000,
                                          group = 1, linetype = 'Rendimiento Promedio'),
                                      inherit.aes = FALSE,
                                      size=1) +
                            scale_linetype_manual(NULL, values = 1) +
                            scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                            scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, 
                                               breaks = y_brk,limits = y_lim,
                                               sec.axis = sec_axis(~./(scl*hmts*100)+0.048,
                                                                   name = 'Rendimiento Promedio',
                                                                   labels = ydual_lbl,
                                                                   breaks = ydual_brk)) + 
                            scale_fill_manual(values = c(parametros$iess_green, 
                                                         parametros$iess_blue))+
                            theme_bw() +
                            plt_theme+
                            theme(legend.position='bottom') +
                            labs( x = '', y = '' )+
                            theme(legend.background = element_rect(fill = 'transparent'), 
                                  legend.box.background = element_rect(fill = 'transparent', 
                                                                       colour = NA),
                                  legend.key = element_rect(fill = 'transparent'), 
                                  legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_fidecomisos_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_fidecomisos_des', 
                           parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en renta variable ------------------------------------------------------------------
aux <- inv_instrumento_ces %>% 
  filter(sectores_y_seguros=='Renta Variable') %>% 
  select(-partic,-inflacion,-fondo, -sectores_y_seguros)
aux['Instrumento']<-'Saldo valor nominal'
aux<-as.data.table(aux)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000  # escala de miles de millones
hmts=0.6 #homotecia

y_lim <- c( 0, 180000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(-0.05,0,0.05,0.1,0.15,0.2,0.25,0.3)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 0, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_rv_hist_inv <- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, fill = Instrumento)) +
                    geom_bar(stat='identity',colour='black') +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    geom_line(data = df_line,
                              aes(x = Periodo,
                                  y = rdto_prom_pond*hmts*scl*1000, group = 1, 
                                  linetype = 'Rendimiento Promedio'),
                              inherit.aes = FALSE,
                              size=1) +
                    scale_linetype_manual(NULL, values = 1) +
                    scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                    scale_y_continuous(name = 'Saldo (millones USD)', 
                                       labels = y_lbl, breaks = y_brk,
                                       sec.axis = sec_axis(~./(scl*hmts*1000), 
                                                           name = 'Rendimiento Promedio',
                                                           labels = ydual_lbl,
                                                           breaks = ydual_brk)) +  
                    scale_fill_manual(values = c(parametros$iess_green, parametros$iess_blue))+
                    theme_bw() +
                    plt_theme+
                    theme(legend.position='bottom') +
                    labs( x = '', y = '' )+
                    theme(legend.background = element_rect(fill = 'transparent'), 
                          legend.box.background = element_rect(fill = 'transparent', colour = NA),
                          legend.key = element_rect(fill = 'transparent'), 
                          legend.spacing = unit(-1, 'lines'))

ggsave( plot = iess_rv_hist_inv, 
        filename = paste0( parametros$resultado_graficos, 'iess_inv_rv_ces', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar RAM----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()