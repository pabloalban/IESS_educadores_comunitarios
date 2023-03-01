# 9 = del retiro de la cesantía del afiliado cesante;
# 10= del retiro de la cesantía del jubilado;
# 11 = débito automático por ejecución de las garantías constituidas en créditos 
# quirografarios en el BIESS;
# 12 = parte variable del Seguro de Desempleo;
# 13 = del retiro de la cesantía del afiliado sin relación de dependencia y del afiliado del
# régimen Especial del Seguro Voluntario
# 14 = derechohabientes en caso de fallecimiento del afiliado; 
# 15 = cruce de fondos de cesantía con obligaciones patronales;
# 16 = del retiro de la cesantía del afiliado de la industria azucarera;
# 17 = del retiro de la cesantía por licencia de maternidad o paternidad; y
# 18 = reliquidación de fondos de Cesantía por aportes extemporáneos.

message( '\tEstableciendo función de aujuste' )

# Función-------------------------------------------------------------------------------------------

# age_grid <- c(seq(18,115,1))
# data <- f_i
# dg = 9
# g <- 'F'
# age_set <- c('93', '92', '72')
# i <- 17
# 
# ajuste_f_i( data, i, dg, age_grid, g, age_set )

ajuste_f_i <- function( data, i, dg, age_grid, g, age_set ) {
  message( '\tSuavisamiento de f_', i, ' para sexo ', g )

  data <- data %>% 
    filter( sexo == g,
            codigo == i)
  
  aux <- data %>% 
    filter( !(x %in% age_set ) )
  
  mod <- smooth.spline(aux$x, log(aux$f_i), df = dg)
  
  pred <- data.frame(x = age_grid, log_f_i_int = predict(mod, age_grid, deriv = 0)[["y"]])
  
  data <- left_join(pred, data, by = "x") %>%
    mutate(sexo := g,
           i := i,
           #log_f_i_int = ifelse(  log_f_i_int < 0, 0,  log_f_i_int ),
           log_f_i = log( f_i ),) %>%
    select(i, x, sexo, log_f_i, log_f_i_int)
  
  dev.off()
  plot( data$x, data$log_f_i )
  lines( data$x, data$log_f_i_int )
  
  return(data)
}
