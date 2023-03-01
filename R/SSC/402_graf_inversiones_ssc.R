message( paste( rep('-', 100 ), collapse = '' ) )

# Plantilla gráficos -------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------
message( '\tLectura portafolio de inversiones' )
file_inversiones <- paste0( parametros$RData_seg, 'IESS_SSC_inversiones.RData' )
load( file = file_inversiones )
# Evolución histórica de la inversiones del Fondo del Seguro DES -----------------------------------
message( '\tGráficos del portafolio de inversiones' )
aux<-as.data.table(recurs_adm_biess)
aux[ , Periodo := ymd( paste0(ano, '/01/01') ) ]
aux<-as.data.frame(aux)
aux['instrumento']<-'Saldo valor nominal'
aux<-select(aux,Periodo,instrumento,inversiones,rendimiento_neto)
df_bar <- aux %>% select(-rendimiento_neto)
df_line = aux %>% select(Periodo, rendimiento_neto)

scl = 100000000
hmts=1.9 #homotecia

y_lim <- c( 0, 1000000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

inv_total_ssc<- ggplot(data = df_bar, aes(x = Periodo, y = inversiones, fill = instrumento)) +
                geom_bar(stat='identity',colour='black') +
                theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                geom_line(data = df_line, aes(x = Periodo, y = rendimiento_neto*hmts*scl*100-1280000000, 
                                              group = 1,
                                              linetype = 'Rendimiento Neto'),
                          inherit.aes = FALSE,
                          size=1) +
                scale_linetype_manual(NULL, values = 1) +
                scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                scale_y_continuous(name = 'Inversiones (millones USD)', 
                                   labels = y_lbl,
                                   breaks = y_brk,
                                   limits =y_lim, 
                                   sec.axis = sec_axis(~./(scl*hmts*100)+0.06736842,
                                                       name = 'Rendimiento Neto',
                                                       labels = function(b) { paste0(round(b * 100, 0), '%')}
                                                       ,breaks=c(0.06,0.07,0.08,0.09,0.10,0.11,0.12))) + 
                scale_fill_manual(values = c('#007435', '#003F8A'))+
                theme_bw() +
                plt_theme+
                theme(legend.position='bottom') +
                labs( x = '', y = '' )+
                theme(legend.background = element_rect(fill = 'transparent'), 
                      legend.box.background = element_rect(fill = 'transparent', colour = NA),
                      legend.key = element_rect(fill = 'transparent'), 
                      legend.spacing = unit(-1, 'lines'))

ggsave( plot = inv_total_ssc, 
        filename = paste0( parametros$resultado_graficos, 'inv_total_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolucion de los inv en creditos quirografarios------------------------------------
aux <- inv_instrumento %>%
        filter(instrumento=='Créditos') %>%
        na.omit() %>%
        mutate(rdto_prom_pond=rdto_prom_pond*100,
               rend_promedio_real=rend_promedio_real*100,
               ano=as.character(ano)) %>%
        select(-inflacion,-instrumento)

aux["Periodo"]<-ymd( paste0(aux$ano, '/01/01') )
aux['instrumento']<-'Préstamos quirografarios'
aux<-select(aux,Periodo,instrumento,valor_nominal ,rdto_prom_pond)
df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 10000000
hmts=10 #homotecia

y_lim <- c( 0, 480000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 7 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

inv_creditos_ssc <- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, fill = instrumento)) +
                    geom_bar(stat='identity',colour='black') +
                    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                    geom_line(data = df_line, aes(x = Periodo, y = rdto_prom_pond*hmts*scl-900000000, 
                                                  group = 1,
                                                  linetype = 'Rendimiento promedio'),
                              inherit.aes = FALSE,
                              size=1) +
                    scale_linetype_manual(NULL, values = 1) +
                    scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                    scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl, 
                                       breaks = y_brk,
                                       limits =y_lim, 
                                       sec.axis = sec_axis(~./(scl*hmts*100)+0.09,
                                                           name = 'Rendimiento promedio',
                                                           labels = function(b) { paste0(round(b * 100, 0), '%')}
                                                           ,breaks=c(0.09,0.10,0.11,0.12,0.13))) + 
                    scale_fill_manual(values = c('#007435', '#003F8A'))+
                    theme_bw() +
                    plt_theme+
                    theme(legend.position='bottom') +
                    labs( x = '', y = '' )+
                    theme(legend.background = element_rect(fill = 'transparent'), 
                          legend.box.background = element_rect(fill = 'transparent', colour = NA),
                          legend.key = element_rect(fill = 'transparent'), 
                          legend.spacing = unit(-1, 'lines'))

ggsave( plot = inv_creditos_ssc, 
        filename = paste0( parametros$resultado_graficos, 'inv_creditos_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en bonos del Estado ----------------------------------------------------------------
aux <-  inv_instrumento %>% 
        filter(instrumento=='Bonos del Estado') %>% 
        select(-inflacion, -instrumento)
aux['Instrumento']<-'Saldo valor nominal'
aux['Periodo']<- ymd( paste0(aux$ano, '/01/01') ) 

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000000 # escala de miles de millones
hmts=35 #homotecia

y_lim <- c( 0, 400000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.07,0.072,0.074, 0.076 ,0.078,0.08)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_bonos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo, y = valor_nominal, 
                                                fill = Instrumento)) +
                      geom_bar(stat='identity',colour='black') +
                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                      geom_line(data = df_line,
                                aes(x = Periodo,
                                    y =rdto_prom_pond*hmts*scl-2400000000, group = 1, 
                                    linetype = 'Rendimiento promedio'),
                                inherit.aes = FALSE,
                                size=1) +
                      scale_linetype_manual(NULL, values = 1) +
                      scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                      scale_y_continuous(name = 'Saldo (millones USD)', 
                                         labels = y_lbl, 
                                         breaks = y_brk,
                                         limits = y_lim,
                                         sec.axis = sec_axis(~./(scl*hmts)+ 0.06857143,
                                                             name = 'Rendimiento promedio',
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
        filename = paste0( parametros$resultado_graficos, 'iess_inv_bonos_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Obligaciones --------------------------------------------------------------------
aux <-  inv_instrumento %>% 
        filter(instrumento=='Obligaciones') %>% 
        select(-inflacion, -instrumento)
aux['Instrumento']<-'Saldo valor nominal'
aux['Periodo']<- ymd( paste0(aux$ano, '/01/01') ) 

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000  # escala de millones
hmts=150 #homotecia

y_lim <- c( 0, 30000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.075,0.0775,0.08,0.0825,0.085)
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
                                            y = rdto_prom_pond*hmts*scl*10-108000000, group = 1, 
                                            linetype = 'Rendimiento promedio'),
                                        inherit.aes = FALSE,
                                        size=1) +
                              scale_linetype_manual(NULL, values = 1) +
                              scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                              scale_y_continuous(name = 'Saldo (millones USD)',
                                                 labels = y_lbl,
                                                 breaks = y_brk, 
                                                 limits = y_lim,
                                                 sec.axis = sec_axis(~./(scl*hmts*100)+0.072, 
                                                                     name = 'Rendimiento promedio',
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
        filename = paste0( parametros$resultado_graficos, 'iess_inv_obligaciones_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Evolución inv en Titularizaciones ----------------------------------------------------------------
aux <-  inv_instrumento %>% 
        filter(instrumento=='Titularizaciones') %>% 
        select(-inflacion, -instrumento)
aux['Instrumento']<-'Saldo valor nominal'
aux['Periodo']<- ymd( paste0(aux$ano, '/01/01') ) 

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)

scl = 1000000  # escala de millones
hmts=20 #homotecia

y_lim <- c( 0, 40000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 5 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.077,0.079,0.081,0.083,0.085)
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
                                                y = rdto_prom_pond*hmts*scl*100-150000000,
                                                group = 1, linetype = 'Rendimiento promedio'),
                                            inherit.aes = FALSE,
                                            size=1) +
                                  scale_linetype_manual(NULL, values = 1) +
                                  scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                                  scale_y_continuous(name = 'Saldo (millones USD)', labels = y_lbl,
                                                     breaks = y_brk, 
                                                     limits = y_lim,
                                                     sec.axis = sec_axis(~./(scl*hmts*100)+0.075, 
                                                                         name = 'Rendimiento promedio',
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
        filename = paste0( parametros$resultado_graficos, 'iess_inv_titularizaciones_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )
iess_titularizaciones_hist_inv

# Evolución inv en fidecomisos ---------------------------------------------------------------------
aux <-  inv_instrumento %>% 
        filter(instrumento=='Fideicomisos y Negocios Fiduciarios') %>% 
        select(-inflacion, -instrumento)
aux['Instrumento']<-'Saldo valor nominal'
aux['Periodo']<- ymd( paste0(aux$ano, '/01/01') ) 

df_bar <- aux %>% select(-rdto_prom_pond)
df_line = aux %>% select(Periodo, rdto_prom_pond)


scl = 10000000  # escala de millones
hmts=60#homotecia

y_lim <- c( 0, 70000000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 6 )
y_lbl <- formatC( y_brk/1000000, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )
ydual_brk<-c(0.073,0.075,0.077,0.079,0.081,0.083,0.085)
ydual_lbl <- paste0(formatC( ydual_brk*100, 
                             digits = 1, 
                             format = 'f', 
                             big.mark = '.', 
                             decimal.mark = ',' ),"%")

iess_fidecomisos_hist_inv<- ggplot(data = df_bar, aes(x = Periodo,
                                                      y = valor_nominal, 
                                                      fill = Instrumento)) +
                            geom_bar(stat='identity',colour='black') +
                            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                            geom_line(data = df_line,
                                      aes(x = Periodo,
                                          y = rdto_prom_pond*hmts*scl*10-439000000,
                                          group = 1, 
                                          linetype = 'Rendimiento promedio'),
                                      inherit.aes = FALSE,
                                      size=1) +
                            scale_linetype_manual(NULL, values = 1) +
                            scale_x_date(date_breaks = '1 year', date_labels = '%Y')+
                            scale_y_continuous(name = 'Saldo (millones USD)', 
                                               labels = y_lbl, breaks = y_brk,limits = y_lim,
                                               sec.axis = sec_axis(~./(scl*hmts*10)+0.07316667,
                                                                   name = 'Rendimiento promedio',
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
        filename = paste0( parametros$resultado_graficos, 'iess_inv_fidecomisos_ssc', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

#Limpiar Ram---------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()