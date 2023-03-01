message(paste(rep("-", 100), collapse = ""))

# 0 . Cargando Rdata--------------------------------------------------------------------------------
message("\tCargando datos para la tasa de siniestralidad")
load(paste0(parametros$RData, "IESS_onu_pea_ecu_int.RData"))
load(paste0(parametros$RData, "IESS_estimacion_tasa_actividad.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_demografia.RData"))
load(paste0(parametros$RData_seg, "IESS_inventario_pensionistas.RData"))

# Configuración del período de calculo--------------------------------------------------------------
anio_ini <- 2013
anio_fin <- 2018

# 1. Se estima el número de entrada de jubilados----------------------------------------------------
message("\tCalculando el número de entradas de jubilados históricos por edad y sexo")

ER <- inventario_jubilados %>%
  filter( tipo_seguro  == 'SG',
          tipo_prestacion %in% c('JV', 'IN')) %>%
  mutate( fecha_derecho = as.Date( fecha_derecho, "%d/%m/%Y" )) %>%
  filter( fecha_nacimiento < fecha_derecho ) %>%
  filter( year(fecha_derecho) >= anio_ini, year(fecha_derecho) <= anio_fin) %>%
  mutate( x = round(age_calc(fecha_nacimiento,
                             enddate = fecha_derecho,
                             units = "years"),0)) %>%
  group_by(x, sexo) %>%
  mutate(ER = n()) %>%
  ungroup() %>%
  distinct(x, sexo, .keep_all = TRUE) %>%
  select( x, sexo, ER) %>%
  arrange( sexo, x )
  
# 2 . Se calcula la tasa de retiro de cesantía de los nuevos jubilados------------------------------
retiros_jub <- edad_sexo_ces_jub %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by(sexo,x) %>%
  mutate(lx=sum(lx, na.rm = TRUE)) %>%
  distinct(x, sexo, .keep_all = TRUE) %>%
  select(- anio, -fdp)

t_retiro_jub <- left_join(retiros_jub,
                               ER,
                               by = c("sexo", "x")) %>%
  mutate(p_jub = lx/ER) %>%
  mutate(p_jub = ifelse(  p_jub > 1, 0.95,  p_jub )) %>%
  arrange(sexo, x)

# 3 . Suavisamiento de la tasa de retiro en jubilados------------------------------------------------
message("\tSuavisamiento de la tasa de retiro en jubilados")
age.grid <- c(seq(15,115,1))

# 3 . 1 . Hombres-----------------------------------------------------------------------------------
p_jub_m <- t_retiro_jub %>%
  filter(sexo == "M", is.finite(p_jub))
aux <- p_jub_m %>% filter( !(x %in% c(93:101,89:91)) )

mod <- smooth.spline(aux$x, log(aux$p_jub), df = 6)

pred <- data.frame(x = age.grid, log_p_jub_int = predict(mod, age.grid, deriv = 0)[["y"]])

p_jub_m <- left_join(pred, p_jub_m, by = "x") %>%
  mutate(sexo := "M", p_jub_int = exp(log_p_jub_int) ) %>%
  mutate(sexo := "M", log_p_jub = log(p_jub) ) %>%
  select(x, sexo, p_jub, p_jub_int, log_p_jub, log_p_jub_int)

# 3 . 2 . Mujeres ----------------------------------------------------------------------------------
p_jub_f <- t_retiro_jub %>%
  filter(sexo == "F", is.finite(p_jub))
aux <- p_jub_f %>% filter( !(x %in% c( 96:97 )) )

mod <- smooth.spline(aux$x, log(aux$p_jub), df = 6)

pred <- data.frame(x = age.grid, log_p_jub_int = predict(mod, age.grid, deriv = 0)[["y"]])

p_jub_f <- left_join(pred, p_jub_f, by = "x") %>%
  mutate(sexo := "F", p_jub_int = exp(log_p_jub_int) ) %>%
  mutate(sexo := "F", log_p_jub = log(p_jub) ) %>%
  select(x, sexo, p_jub, p_jub_int, log_p_jub, log_p_jub_int)

# 4 . Gráfica del suvisamiento----------------------------------------------------------------------
# 4 . 1 . Hombres-----------------------------------------------------------------------------------
plot(p_jub_m$x,p_jub_m$log_p_jub)
lines(p_jub_m$x,p_jub_m$log_p_jub_int)

# 4 . 2 . Mujeres-----------------------------------------------------------------------------------
plot(p_jub_f$x,p_jub_f$log_p_jub)
lines(p_jub_f$x,p_jub_f$log_p_jub_int)

# 5 . Se aplica una proporción para coincidir con el año 2018---------------------------------------

factor_calibracion <- 1

# 6. Consolidad bases-------------------------------------------------------------------------------
p_jub <- rbind(
  p_jub_m,
  p_jub_f
)

p_jub$p_jub <- p_jub$p_jub * factor_calibracion

# 7. Guardar la tasa de uso interpolada en un Rdata ---------------------------------------------------
message("\tGuardando tasa de retiro de fondos de jubilados")

save(p_jub,
     file = paste0(parametros$RData_seg, "IESS_CES_p_jub_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
