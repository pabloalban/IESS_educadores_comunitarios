message(paste(rep("-", 100), collapse = ""))

# 0 . Cargando Rdata--------------------------------------------------------------------------------
message("\tCargando datos para la tasa de siniestralidad")
load(paste0(parametros$RData, "IESS_onu_pea_ecu_int.RData"))
load(paste0(parametros$RData, "IESS_estimacion_tasa_actividad.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_demografia.RData"))
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )
load( paste0( parametros$RData, "IESS_proyeccion_poblacion.RData" ) )
load(paste0(parametros$RData_seg, "IESS_CES_pagos_prestaciones.RData"))

#Años
anio_ini <- 2016
anio_fin <- 2018

# 1. Se estima el número de cesantes históricos, utilizando la tasa de actividad estimada-----------
message("\tCalculando la evolución historica de cesantes por edad y sexo")
tau <- as_tibble( tasa_act_proy ) %>%
  select(sexo, x, tau)

colnames(cotizantes_sgo) <- c("x", "sexo", "anio", "l2_cot")

den_cot <-  as_tibble( densidad_cotizacion_int )%>%
  select( sexo := genero,
          x := edad, 
          den_cot_int ) 

pob_hist <- left_join(cotizantes_sgo, tau,
                      by = c("x", "sexo")
) %>%
  left_join(., den_cot,
            by = c("x", "sexo")
  ) %>%
  mutate(l2 = l2_cot / tau) %>%
  mutate(l2_ces = l2 - l2_cot) %>%
  mutate(l2_cesc = l2_ces * den_cot_int)

ER_2018 <- as_tibble(pob_proy) %>%
  left_join(., den_cot,
            by = c("x", "sexo") ) %>%
  filter( t == 0 ) %>%
  mutate( l2_cesc = l2_ces * den_cot_int ) %>%
  select( x, sexo, ER := l2_cesc)

ER <- pob_hist %>%
  select(anio, x, sexo, ER := l2_cesc)

# 2 . Se calcula la tasa de retiro de cesantía de los cesantes--------------------------------------
retiros_pv_des <- pagos_desempleo %>%
  filter( sd_numero_pagos == '1', sd_tipo_pago =='V') %>%
  distinct( sd_cedula, anio , .keep_all = TRUE ) %>%
  group_by( sexo, edad_siniestro, anio ) %>%
  mutate( lx = n() ) %>%
  ungroup() %>%
  distinct( edad_siniestro, sexo, anio, .keep_all = TRUE) %>%
  select( anio, sexo, x := edad_siniestro, lx)

t_pv_desempleo <- left_join(retiros_pv_des,
                              ER,
                              by = c("sexo", "x", "anio")) %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by( x, sexo ) %>%
  mutate(ER = sum( ER, na.rm = TRUE )) %>%
  mutate(lx = sum( lx, na.rm = TRUE )) %>%
  ungroup() %>%
  distinct(x, sexo, .keep_all = TRUE) %>%
  mutate(p_des = lx/ER) %>%
  select( -anio )


# 3 . Suavisamiento de la tasa de retiro en cesantes------------------------------------------------
message("\tSuavisamiento de la tasa de retiro en cesantes")
age.grid <- c(seq(15,115,1))

# 3 . 1 . Hombres-----------------------------------------------------------------------------------
p_des_m <- t_pv_desempleo %>%
  filter(sexo == "M", is.finite(p_des))
aux <- p_des_m %>% filter( !(x %in% c('80', '76', '73', '72')) )

mod <- smooth.spline(aux$x, aux$p_des, df = 9)

pred <- data.frame(x = age.grid, p_des_int = predict(mod, age.grid, deriv = 0)[["y"]])

p_des_m <- left_join(pred, p_des_m, by = "x") %>%
  mutate(sexo := "M", p_des_int = ifelse(  p_des_int < 0, 0,  p_des_int )) %>%
  select(x, sexo, p_des, p_des_int)

# 3 . 2 . Mujeres ----------------------------------------------------------------------------------
p_des_f <- t_pv_desempleo %>%
  filter(sexo == "F", is.finite(p_des))
aux <- p_des_f %>% filter( !(x %in% c('80','71','70','69','68','65')) )

mod <- smooth.spline(aux$x, aux$p_des, df = 9)

pred <- data.frame(x = age.grid, p_des_int = predict(mod, age.grid, deriv = 0)[["y"]])

p_des_f <- left_join(pred, p_des_f, by = "x") %>%
  mutate(sexo := "F", p_des_int = ifelse(  p_des_int < 0, 0,  p_des_int)) %>%
  select(x, sexo, p_des, p_des_int)

# 4 . Gráfica del suvisamiento----------------------------------------------------------------------
# 4 . 1 . Hombres-----------------------------------------------------------------------------------
plot(p_des_m$x,p_des_m$p_des)
lines(p_des_m$x,p_des_m$p_des_int)

# 4 . 2 . Mujeres-----------------------------------------------------------------------------------
plot(p_des_f$x,p_des_f$p_des)
lines(p_des_f$x,p_des_f$p_des_int)

# 5. Consolidad bases-------------------------------------------------------------------------------
p_des <- rbind(
  p_des_m,
  p_des_f
)

# 6 . Se aplica una proporción para coincidir con el año 2018---------------------------------------

p_des_2018 <- retiros_pv_des %>%
  filter( anio == anio_fin ) %>%
  left_join( . ,
             ER_2018,
             by = c("sexo", "x")) %>%
  group_by( x, sexo ) %>%
  mutate(ER = sum( ER, na.rm = TRUE )) %>%
  mutate(lx = sum( lx, na.rm = TRUE )) %>%
  ungroup() %>%
  distinct(x, sexo, .keep_all = TRUE) %>%
  mutate(p_des_2018 = lx/ER) %>%
  mutate(p_des_2018 = ifelse( !is.finite(p_des_2018), 0, p_des_2018 )) %>%
  select( x, sexo, lx, ER, p_des_2018 )

p_des <- left_join(p_des, p_des_2018, by=c('x', 'sexo' )) %>%
  mutate( p_des_int = p_des_int * sum( p_des_2018 * ER, na.rm = TRUE) / sum( p_des_int * ER, na.rm = TRUE ),
          p_des = p_des * sum( p_des_2018 * ER, na.rm = TRUE) / sum( p_des * ER, na.rm = TRUE ) ) %>%
  select(-p_des_2018, -lx, -ER)


# 7. Guardar la tasa de uso interpolada en un Rdata ---------------------------------------------------
message("\tGuardando tasa de retiro de fondos de cesantes")

save(p_des,
     file = paste0(parametros$RData_seg, "IESS_CES_p_des_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
