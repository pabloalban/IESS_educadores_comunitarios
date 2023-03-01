message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando población afiliada activa inicial SGO del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )
# graf_width <- 15
# graf_height <- 9.2
# graf_line_size_old <-graf_line_size
# graf_line_size<-2

# Carga de datos -----------------------------------------------------------------------------------
load( file = paste0( parametros$RData_seg, 'IESS_ECO_tablas_estadisticas.RData' ) )


# Masa salarial de educadores comunicadores---------------------------------------------------------
message( '\tGraficando masa salarial de ECO' )

tab_masa <- tab_masa %>%
  mutate( anio = as.integer( anio ) ) %>%
  filter( anio > 0)


unidad<-1e6
aux<- tab_masa %>% dplyr::select( anio, total_edu )

x_lim <- c( 1947, 2022 )
x_brk <- seq( 1947, 2022, 6 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 6000000)
y_brk <- seq( y_lim[1], y_lim[2], 1000000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_edu <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = total_edu, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Masa Salarial (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_masa_salarial_edu, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_edu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )



# Masa salarial al SGO------------------------------------------------------------------------------
message( '\tGraficando masa salarial del SGO' )


unidad<-1e6
aux<- tab_masa %>% dplyr::select( anio, total_sgo )

x_lim <- c( 2000, 2020 )
x_brk <- seq( 2000, 2020, 4 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 30000000)
y_brk <- seq( y_lim[1], y_lim[2], 5000000 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_masa_salarial_sgo <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = total_sgo, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Masa Salarial (USD)' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_masa_salarial_sgo, 
        filename = paste0( parametros$resultado_graficos, 'iess_masa_salarial_sgo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


#Piramide por edad y sexo---------------------------------------------------------------------------
message( '\tGraficando ECO por edad y sexo' )

aux<- tab_edad_sexo %>%
  mutate( dist = if_else( sexo == "H",
                          -dist,
                           dist ) )
  
salto_y<-10
salto_x<-0.01
brks_y <- seq(-0.04,0.04,salto_x)
lbls_y <- paste0(as.character(c(seq(0.04, 0, -salto_x)*100, seq(salto_x, 0.04, salto_x)*100)), "%")
brks_x <- seq(20, 100, salto_y)
lbls_x <- paste0(as.character(brks_x))

iess_pir_edu <- ggplot(aux, aes(x = edad, y = dist, fill=sexo)) +
                      xlab( 'Edad' ) +
                      ylab( '' ) +
                      geom_bar( data = filter( aux, sexo == 'M' ), stat = 'identity',colour="white", size=0.1) +
                      geom_bar( data = filter( aux,  sexo == 'H' ), stat = 'identity',colour="white", size=0.1) +
                      scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                      scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                      coord_flip() +
                      theme_bw() +
                      plt_theme +
                      guides(fill = guide_legend(title = NULL,label.position = "right",
                                                 label.hjust = 0, label.vjust = 0.5))+
                      theme(legend.position="bottom")+   #legend.position = c(0.8, 0.2)
                      scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                        labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_edu, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_edu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Pirámide de sueldo EDU por edad-----------------------------------------------------------------------
message( '\tGraficando sueldo de edu por edad' )


aux<- tab_sal_edu_edad_sexo %>%
  mutate( salario = if_else( sexo == "H",
                          -salario,
                          salario ) )

salto_y <- 40
salto_x <- 10
brks_y <- seq(-200, 200, salto_y)
lbls_y <- paste0("$" , as.character(c( seq( 200, 0, -salto_y ), seq(salto_y, 200, salto_y ))) )
brks_x <- seq(20, 100, salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_pir_sal_edu <- ggplot(aux, aes(x = edad, y = salario, fill=sexo)) +
                    xlab( 'Edad' ) +
                    ylab( '' ) +
                    ggtitle("Salarios promedios de los Educadores Comunitarios en MINEDUC") +
                      geom_bar( data = filter( aux, sexo == 'M' ),
                              stat = 'identity',
                              colour="white",  
                              size=0.1) +
                    geom_bar( data = filter( aux, sexo == 'H' ), 
                              stat = 'identity',
                              colour="white",  
                              size=0.1) +
                    scale_y_continuous(breaks = brks_y, labels = lbls_y) +
                    scale_x_continuous(breaks = brks_x, labels = lbls_x) +
                    coord_flip() +
                    theme_bw() +
                    plt_theme +
                    guides(fill = guide_legend(title = NULL,label.position = "right", 
                                               label.hjust = 0)) +
                    theme(legend.position="bottom")+
                    scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                                      labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_sal_edu, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_sal_edu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Pirámide de sueldo SGO por edad-----------------------------------------------------------------------
message( '\tGraficando sueldo de sgo por edad' )


aux<- tab_sal_sgo_edad_sexo %>%
  mutate( salario = if_else( sexo == "H",
                             -salario,
                             salario ) )

salto_y <- 200
salto_x <- 10
brks_y <- seq(-1000, 1000, salto_y)
lbls_y <- paste0("$" , as.character(c( seq( 1000, 0, -salto_y ), seq(salto_y, 1000, salto_y ))) )
brks_x <- seq(20, 100, salto_x)
lbls_x <- paste0(as.character(brks_x))

iess_pir_sal_sgo <- ggplot(aux, aes(x = edad, y = salario, fill=sexo)) +
  xlab( 'Edad' ) +
  ylab( '' ) +
  ggtitle("Salarios promedios de los Educadores Comunitarios en el SGO") +
  geom_bar( data = filter( aux, sexo == 'M' ),
            stat = 'identity',
            colour="white",  
            size=0.1) +
  geom_bar( data = filter( aux, sexo == 'H' ), 
            stat = 'identity',
            colour="white",  
            size=0.1) +
  scale_y_continuous(breaks = brks_y, labels = lbls_y) +
  scale_x_continuous(breaks = brks_x, labels = lbls_x) +
  coord_flip() +
  theme_bw() +
  plt_theme +
  guides(fill = guide_legend(title = NULL,label.position = "right", 
                             label.hjust = 0)) +
  theme(legend.position="bottom")+
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres"))

ggsave( plot = iess_pir_sal_edu, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_sal_edu', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )


# Unir pirámides de sueldos por edad----------------------------------------------------------------
g<-ggarrange(iess_pir_sal_edu,
              iess_pir_sal_sgo,
              ncol = 2, nrow = 1,
             common.legend = TRUE, legend = "bottom" ) 


ggsave( plot = g, 
        filename = paste0( parametros$resultado_graficos, 'iess_pir_sal', parametros$graf_ext ),
        width = 24, height = 14, units = graf_units, dpi = graf_dpi )

#Gráfico por provincias-----------------------------------------------------------------------------

aux <- tab_provincia %>%
  arrange( total  ) %>%
  filter( provincia!='Total' ) %>%
  tidyr::gather(.,
                key = "sexo",
                value = "value",
                -H_dist,
                -M_dist,
                -total,
                -total_dist,
                -provincia)


scl = 1 # escala en miles de millones
hmts= 6 #homotecia

y_lim <- c( 0, 1000 )
y_brk <- seq( y_lim[1], y_lim[2], length.out = 10 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )


iess_edu_provincia <- ggplot(data = aux, aes(x = reorder(provincia, -value), y = value, fill = sexo)) +
  geom_bar(stat='identity',colour='white') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(name = 'Educadores Comunitarios', 
                     labels = y_lbl, breaks = y_brk ) + 
  scale_fill_manual(values = c(parametros$iess_blue, parametros$iess_green),
                    labels = c("Hombres", "Mujeres")) + 
  theme_bw() +
  plt_theme+
  theme( axis.text.x = element_text( face = 'plain', angle = 90, colour = 'black', 
                                     size = tam_letra, family = tipo_letra, 
                                     vjust = 0, hjust = 0.5 ) )+
  labs( x = '', y = '' )+
  theme(legend.position="bottom")


ggsave( plot = iess_edu_provincia, 
        filename = paste0( parametros$resultado_graficos, 'iess_edu_provincia', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )

# Gráfico de la evolución histórica de los educadores comunitarios----------------------------------
message( '\tGraficando evolución histórica de los educadores comunitarios' )

aux <- tab_edu_anio %>%
  mutate( anio = as.integer( anio ) ) %>%
  rbind( ., data.frame( anio = c(2009:2022),
                        edu = rep( 0, 14 ) ) )
  
unidad<-1e6

x_lim <- c( 1947, 2022 )
x_brk <- seq( 1947, 2022, 6 )
x_lbl <- formatC( x_brk, digits = 0, format = 'f' )

y_lim <- c( 0, 3500)
y_brk <- seq( y_lim[1], y_lim[2], 500 )
y_lbl <- formatC( y_brk, digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' )

iess_edu_evo <- ggplot( data = aux ) + 
  geom_line( aes( x = anio, 
                  y = edu, 
                  color = parametros$iess_blue ), 
             size = graf_line_size,
             lineend = "round" ) + 
  labs( x = 'Año', y = 'Educadores Comunitarios' ) +
  scale_color_manual( values =  c( parametros$iess_green, parametros$iess_blue ), 
                      labels = c( '', '' ) ) +
  scale_y_continuous( breaks = y_brk, labels = y_lbl, limits = y_lim ) +
  scale_x_continuous( breaks = x_brk, labels = x_lbl, limits = x_lim ) +
  theme_bw() +
  plt_theme +
  theme( axis.text.x = element_text(angle = 90, hjust = 1 ) )

ggsave( plot = iess_edu_evo, 
        filename = paste0( parametros$resultado_graficos, 'iess_edu_evo', parametros$graf_ext ),
        width = graf_width, height = graf_height, units = graf_units, dpi = graf_dpi )




# Limpiar Memoria RAM-------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

