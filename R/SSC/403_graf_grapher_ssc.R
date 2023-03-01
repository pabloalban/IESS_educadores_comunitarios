message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tGraficando tasas de mortalidad ór sexo SSC del IESS' )

# Plantilla gráfica --------------------------------------------------------------------------------
source( 'R/401_graf_plantilla.R', encoding = 'UTF-8', echo = FALSE )

# Carga de datos -----------------------------------------------------------------------------------

load( file = paste0( parametros$RData_seg, 'tasa_de_mortalidad.RData' ) )

library(plotly)


#######################------------------------   1 (base_q_sgo_50_)   -----------------###########################

fig <- plot_ly( base_q_sgo_50_ssc, x = ~Edad )

fig <- fig %>%
  add_lines(y = ~`2020_m`, name = "2020" , visible = T) %>%
  add_lines(y = ~`2020_h`, name = "2020", visible = F) %>%
  # add_lines(y = ~`2021_m`, name = "2021" , visible = T) %>%
  # add_lines(y = ~`2021_h`, name = "2021", visible = F) %>%
  # add_lines(y = ~`2022_m`, name = "2022" , visible = T) %>%
  # add_lines(y = ~`2022_h`, name = "2022", visible = F) %>%
  #  add_lines(y = ~`2023_m`, name = "2023" , visible = T) %>%
  # add_lines(y = ~`2023_h`, name = "2023", visible = F)  %>%
  # add_lines(y = ~`2024_m`, name = "2024" , visible = T) %>%
  # add_lines(y = ~`2024_h`, name = "2024" , visible = F) %>%
  add_lines(y = ~`2025_m`, name = "2025" , visible = T) %>%
  add_lines(y = ~`2025_h`, name = "2025", visible = F) %>%
  # add_lines(y = ~`2026_m`, name = "2026" , visible = T) %>%
  # add_lines(y = ~`2026_h`, name = "2026", visible = F) %>%
  # add_lines(y = ~`2027_m`, name = "2027" , visible = T) %>%
  # add_lines(y = ~`2027_h`, name = "2027", visible = F)  %>%
  # add_lines(y = ~`2028_m`, name = "2028"  , visible = T) %>%
  # add_lines(y = ~`2028_h`, name = "2028" , visible = F) %>%
  # add_lines(y = ~`2029_m`, name = "2029" , visible = T) %>%
  # add_lines(y = ~`2029_h`, name = "2029", visible = F) %>%
  add_lines(y = ~`2030_m`, name = "2030" , visible = T) %>%
  add_lines(y = ~`2030_h`, name = "2030", visible = F) %>%
  # add_lines(y = ~`2031_m`, name = "2031" , visible = T) %>%
  # add_lines(y = ~`2031_h`, name = "2031", visible = F)  %>%
  # add_lines(y = ~`2032_m`, name = "2032" , visible = T) %>%
  # add_lines(y = ~`2032_h`, name = "2032" , visible = F) %>%
  # add_lines(y = ~`2033_m`, name = "2033" , visible = T) %>%
  # add_lines(y = ~`2033_h`, name = "2033", visible = F) %>%
  # add_lines(y = ~`2034_m`, name = "2034" , visible = T) %>%
  # add_lines(y = ~`2034_h`, name = "2034", visible = F) %>%
  add_lines(y = ~`2035_m`, name = "2035" , visible = T) %>%
  add_lines(y = ~`2035_h`, name = "2035", visible = F)  %>%
  # add_lines(y = ~`2036_m`, name = "2036" , visible = T) %>%
  # add_lines(y = ~`2036_h`, name = "2036", visible = F)  %>%
  # add_lines(y = ~`2037_m`, name = "2037" , visible = T) %>%
  # add_lines(y = ~`2037_h`, name = "2037", visible = F)  %>%
  # add_lines(y = ~`2038_m`, name = "2038" , visible = T) %>%
  # add_lines(y = ~`2038_h`, name = "2038", visible = F)  %>%
  # add_lines(y = ~`2039_m`, name = "2039" , visible = T) %>%
  # add_lines(y = ~`2039_h`, name = "2039", visible = F)  %>%
  add_lines(y = ~`2040_m`, name = "2040" , visible = T) %>%
  add_lines(y = ~`2040_h`, name = "2040", visible = F)  %>%
  # add_lines(y = ~`2041_m`, name = "2041" , visible = T) %>%
  # add_lines(y = ~`2041_h`, name = "2041", visible = F)  %>%
  # add_lines(y = ~`2042_m`, name = "2042" , visible = T) %>%
  # add_lines(y = ~`2042_h`, name = "2042", visible = F)  %>%
  # add_lines(y = ~`2043_m`, name = "2043" , visible = T) %>%
  # add_lines(y = ~`2043_h`, name = "2043", visible = F)  %>%
  # add_lines(y = ~`2044_m`, name = "2044" , visible = T) %>%
  # add_lines(y = ~`2044_h`, name = "2044", visible = F)  %>%
  add_lines(y = ~`2045_m`, name = "2045" , visible = T) %>%
  add_lines(y = ~`2045_h`, name = "2045", visible = F)  %>%
  # add_lines(y = ~`2046_m`, name = "2046" , visible = T) %>%
  # add_lines(y = ~`2046_h`, name = "2046", visible = F)  %>%
  # add_lines(y = ~`2047_m`, name = "2047" , visible = T) %>%
  # add_lines(y = ~`2047_h`, name = "2047", visible = F)  %>%
  # add_lines(y = ~`2048_m`, name = "2048" , visible = T) %>%
  # add_lines(y = ~`2048_h`, name = "2048", visible = F)  %>%
  # add_lines(y = ~`2049_m`, name = "2049" , visible = T) %>%
  # add_lines(y = ~`2049_h`, name = "2049", visible = F)  %>%
  add_lines(y = ~`2050_m`, name = "2050" , visible = T) %>%
  add_lines(y = ~`2050_h`, name = "2050", visible = F)  %>%
  # add_lines(y = ~`2051_m`, name = "2051" , visible = T) %>%
  # add_lines(y = ~`2051_h`, name = "2051", visible = F)  %>%
  # add_lines(y = ~`2052_m`, name = "2052" , visible = T) %>%
  # add_lines(y = ~`2052_h`, name = "2052", visible = F)  %>%
  # add_lines(y = ~`2053_m`, name = "2053" , visible = T) %>%
  # add_lines(y = ~`2053_h`, name = "2053", visible = F)  %>%
  # add_lines(y = ~`2054_m`, name = "2054" , visible = T) %>%
  # add_lines(y = ~`2054_h`, name = "2054", visible = F)  %>%
  add_lines(y = ~`2055_m`, name = "2055" , visible = T) %>%
  add_lines(y = ~`2055_h`, name = "2055", visible = F)  %>%
  # add_lines(y = ~`2056_m`, name = "2056" , visible = T) %>%
  # add_lines(y = ~`2056_h`, name = "2056", visible = F)  %>%
  # add_lines(y = ~`2057_m`, name = "2057" , visible = T) %>%
  # add_lines(y = ~`2057_h`, name = "2057", visible = F)  %>%
  # add_lines(y = ~`2058_m`, name = "2058" , visible = T) %>%
  # add_lines(y = ~`2058_h`, name = "2058", visible = F)  %>%
  # add_lines(y = ~`2059_m`, name = "2059" , visible = T) %>%
  # add_lines(y = ~`2059_h`, name = "2059", visible = F)  %>%
  add_lines(y = ~`2060_m`, name = "2060" , visible = T) %>%
  add_lines(y = ~`2060_h`, name = "2060", visible = F)  %>%
  
  layout( legend = list(orientation = 'h') )



# Creacion de botones------------------------------------------------------------------------------
fig <- fig %>%  layout(
  title = "(base_q_sgo_50_) Mortalidad evolucion años",
  xaxis = list(domain = c(0.1, 1),
               title = ""),
  yaxis2 = list(side = 'right', tickformat = "%", overlaying = "y", showgrid = FALSE, zeroline = FALSE),
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        # list(method = "restyle",
        #      args = list("visible", list(T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F,T, F) ),
        #      label = "Mujer(todos años)"),
        # 
        # list(method = "restyle",
        #      args = list("visible", list(F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T) ),
        #      label = "Hombre(todos años)"),
        
        list(method = "restyle",
             args = list("visible", list(T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F) ),
             label = "Mujer(cada 5 años)" ,colors="Dark2"),
        
        list(method = "restyle",
             args = list("visible", list(F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T) ),
             label = "Hombre(cada 5 años)")
        
        
        
      )
    )
  ) )



fig

htmlwidgets::saveWidget(as_widget( fig ), "Graph_shiny/(base_q_sgo_50_) Mortalidad evolucion años.html")


####---------------------Figure 2---------------------------
base_q_sgo_50_ssc1<-as.data.table(base_q_sgo_50_ssc1)
base_q_sgo_50_ssc1[,Age:=as.integer(Age)]
fig2 <- plot_ly(`base_q_sgo_50_ssc1`, x = ~Age )

fig2 <- fig2 %>% add_lines(y = ~`Prob_m`, frame = ~`year`, name = "Tasa de mortalidad Mujer",color=I("red")) %>%
  add_lines(y = ~`Prob_h`, frame = ~`year`, name = "Tasa de mortalidad Hombre",color=I("blue"))%>% 
  layout( legend = list(orientation = 'rigth') )

fig2 <- fig2 %>%  layout(
  title = "(base_q_sgo_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"), barmode = 'stack')

fig2

htmlwidgets::saveWidget(as_widget( fig2), "Graph_shiny/(base_q_sgo_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo.html")






#######################------------------------   2 (base_qi_50_)   -----------------###########################

fig <- plot_ly( base_qi_50_ssc, x = ~Edad )

fig <- fig %>%
  add_lines(y = ~`2020_m`, name = "2020" , visible = T) %>%
  add_lines(y = ~`2020_h`, name = "2020", visible = F) %>%
  add_lines(y = ~`2025_m`, name = "2025" , visible = T) %>%
  add_lines(y = ~`2025_h`, name = "2025", visible = F) %>%
  add_lines(y = ~`2030_m`, name = "2030" , visible = T) %>%
  add_lines(y = ~`2030_h`, name = "2030", visible = F) %>%
  add_lines(y = ~`2035_m`, name = "2035" , visible = T) %>%
  add_lines(y = ~`2035_h`, name = "2035", visible = F)  %>%
  add_lines(y = ~`2040_m`, name = "2040" , visible = T) %>%
  add_lines(y = ~`2040_h`, name = "2040", visible = F)  %>%
  add_lines(y = ~`2045_m`, name = "2045" , visible = T) %>%
  add_lines(y = ~`2045_h`, name = "2045", visible = F)  %>%
  add_lines(y = ~`2050_m`, name = "2050" , visible = T) %>%
  add_lines(y = ~`2050_h`, name = "2050", visible = F)  %>%
  add_lines(y = ~`2055_m`, name = "2055" , visible = T) %>%
  add_lines(y = ~`2055_h`, name = "2055", visible = F)  %>%
  add_lines(y = ~`2060_m`, name = "2060" , visible = T) %>%
  add_lines(y = ~`2060_h`, name = "2060", visible = F)  %>%
  
  layout( legend = list(orientation = 'h') )



# Creacion de botones------------------------------------------------------------------------------
fig <- fig %>%  layout(
  title = "(base_qi_50_) Mortalidad evolucion años",
  xaxis = list(domain = c(0.1, 1),
               title = ""),
  yaxis2 = list(side = 'right', tickformat = "%", overlaying = "y", showgrid = FALSE, zeroline = FALSE),
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        
        list(method = "restyle",
             args = list("visible", list(T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F) ),
             label = "Mujer(cada 5 años)" ,colors="Dark2"),
        
        list(method = "restyle",
             args = list("visible", list(F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T) ),
             label = "Hombre(cada 5 años)")
        
        
        
      )
    )
  ) )



fig

htmlwidgets::saveWidget(as_widget( fig ), "Graph_shiny/(base_qi_50_) Mortalidad evolucion años.html")


####---------------------Figure 2---------------------------
base_qi_50_ssc1<-as.data.table(base_qi_50_ssc1)
base_qi_50_ssc1[,Age:=as.integer(Age)]
fig2 <- plot_ly(`base_qi_50_ssc1`, x = ~Age )

fig2 <- fig2 %>% add_lines(y = ~`Prob_m`, frame = ~`year`, name = "Tasa de mortalidad Mujer",color=I("red")) %>%
  add_lines(y = ~`Prob_h`, frame = ~`year`, name = "Tasa de mortalidad Hombre",color=I("blue"))%>% 
  layout( legend = list(orientation = 'rigth') )

fig2 <- fig2 %>%  layout(
  title = "(base_qi_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"), barmode = 'stack')

fig2

htmlwidgets::saveWidget(as_widget( fig2), "Graph_shiny/(base_qi_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo.html")







#######################------------------------   3 (base_qo_50_)   -----------------###########################

fig <- plot_ly( base_qo_50_ssc, x = ~Edad )

fig <- fig %>%
  add_lines(y = ~`2020_m`, name = "2020" , visible = T) %>%
  add_lines(y = ~`2020_h`, name = "2020", visible = F) %>%
  add_lines(y = ~`2025_m`, name = "2025" , visible = T) %>%
  add_lines(y = ~`2025_h`, name = "2025", visible = F) %>%
  add_lines(y = ~`2030_m`, name = "2030" , visible = T) %>%
  add_lines(y = ~`2030_h`, name = "2030", visible = F) %>%
  add_lines(y = ~`2035_m`, name = "2035" , visible = T) %>%
  add_lines(y = ~`2035_h`, name = "2035", visible = F)  %>%
  add_lines(y = ~`2040_m`, name = "2040" , visible = T) %>%
  add_lines(y = ~`2040_h`, name = "2040", visible = F)  %>%
  add_lines(y = ~`2045_m`, name = "2045" , visible = T) %>%
  add_lines(y = ~`2045_h`, name = "2045", visible = F)  %>%
  add_lines(y = ~`2050_m`, name = "2050" , visible = T) %>%
  add_lines(y = ~`2050_h`, name = "2050", visible = F)  %>%
  add_lines(y = ~`2055_m`, name = "2055" , visible = T) %>%
  add_lines(y = ~`2055_h`, name = "2055", visible = F)  %>%
  add_lines(y = ~`2060_m`, name = "2060" , visible = T) %>%
  add_lines(y = ~`2060_h`, name = "2060", visible = F)  %>%
  
  layout( legend = list(orientation = 'h') )



# Creacion de botones------------------------------------------------------------------------------
fig <- fig %>%  layout(
  title = "(base_qo_50_) Mortalidad evolucion años",
  xaxis = list(domain = c(0.1, 1),
               title = ""),
  yaxis2 = list(side = 'right', tickformat = "%", overlaying = "y", showgrid = FALSE, zeroline = FALSE),
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        
        list(method = "restyle",
             args = list("visible", list(T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F) ),
             label = "Mujer(cada 5 años)" ,colors="Dark2"),
        
        list(method = "restyle",
             args = list("visible", list(F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T) ),
             label = "Hombre(cada 5 años)")
        
        
        
      )
    )
  ) )



fig

htmlwidgets::saveWidget(as_widget( fig ), "Graph_shiny/(base_qo_50_) Mortalidad evolucion años.html")


####---------------------Figure 2---------------------------
base_qo_50_ssc1<-as.data.table(base_qo_50_ssc1)
base_qo_50_ssc1[,Age:=as.integer(Age)]
fig2 <- plot_ly(`base_qo_50_ssc1`, x = ~Age )

fig2 <- fig2 %>% add_lines(y = ~`Prob_m`, frame = ~`year`, name = "Tasa de mortalidad Mujer",color=I("red")) %>%
  add_lines(y = ~`Prob_h`, frame = ~`year`, name = "Tasa de mortalidad Hombre",color=I("blue"))%>% 
  layout( legend = list(orientation = 'rigth') )

fig2 <- fig2 %>%  layout(
  title = "(base_qo_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"), barmode = 'stack')

fig2

htmlwidgets::saveWidget(as_widget( fig2), "Graph_shiny/(base_qo_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo.html")






#######################------------------------   4 (base_qw_50_)   -----------------###########################

fig <- plot_ly( base_qw_50_ssc, x = ~Edad )

fig <- fig %>%
  add_lines(y = ~`2020_m`, name = "2020" , visible = T) %>%
  add_lines(y = ~`2020_h`, name = "2020", visible = F) %>%
  add_lines(y = ~`2025_m`, name = "2025" , visible = T) %>%
  add_lines(y = ~`2025_h`, name = "2025", visible = F) %>%
  add_lines(y = ~`2030_m`, name = "2030" , visible = T) %>%
  add_lines(y = ~`2030_h`, name = "2030", visible = F) %>%
  add_lines(y = ~`2035_m`, name = "2035" , visible = T) %>%
  add_lines(y = ~`2035_h`, name = "2035", visible = F)  %>%
  add_lines(y = ~`2040_m`, name = "2040" , visible = T) %>%
  add_lines(y = ~`2040_h`, name = "2040", visible = F)  %>%
  add_lines(y = ~`2045_m`, name = "2045" , visible = T) %>%
  add_lines(y = ~`2045_h`, name = "2045", visible = F)  %>%
  add_lines(y = ~`2050_m`, name = "2050" , visible = T) %>%
  add_lines(y = ~`2050_h`, name = "2050", visible = F)  %>%
  add_lines(y = ~`2055_m`, name = "2055" , visible = T) %>%
  add_lines(y = ~`2055_h`, name = "2055", visible = F)  %>%
  add_lines(y = ~`2060_m`, name = "2060" , visible = T) %>%
  add_lines(y = ~`2060_h`, name = "2060", visible = F)  %>%
  
  layout( legend = list(orientation = 'h') )



# Creacion de botones------------------------------------------------------------------------------
fig <- fig %>%  layout(
  title = "(base_qw_50_) Mortalidad evolucion años",
  xaxis = list(domain = c(0.1, 1),
               title = ""),
  yaxis2 = list(side = 'right', tickformat = "%", overlaying = "y", showgrid = FALSE, zeroline = FALSE),
  updatemenus = list(
    list(
      y = 0.7,
      buttons = list(
        
        list(method = "restyle",
             args = list("visible", list(T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F) ),
             label = "Mujer(cada 5 años)" ,colors="Dark2"),
        
        list(method = "restyle",
             args = list("visible", list(F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T,F,T) ),
             label = "Hombre(cada 5 años)")
        
        
        
      )
    )
  ) )



fig

htmlwidgets::saveWidget(as_widget( fig ), "Graph_shiny/(base_qw_50_) Mortalidad evolucion años.html")


####---------------------Figure 2---------------------------
base_qw_50_ssc1<-as.data.table(base_qw_50_ssc1)
base_qw_50_ssc1[,Age:=as.integer(Age)]
fig2 <- plot_ly(`base_qw_50_ssc1`, x = ~Age )

fig2 <- fig2 %>% add_lines(y = ~`Prob_m`, frame = ~`year`, name = "Tasa de mortalidad Mujer",color=I("red")) %>%
  add_lines(y = ~`Prob_h`, frame = ~`year`, name = "Tasa de mortalidad Hombre",color=I("blue"))%>% 
  layout( legend = list(orientation = 'rigth') )

fig2 <- fig2 %>%  layout(
  title = "(base_qw_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"), barmode = 'stack')

fig2

htmlwidgets::saveWidget(as_widget( fig2), "Graph_shiny/(base_qw_50_) Comparación Tasas de Mortalidad Sexo a través del tiempo.html")





















