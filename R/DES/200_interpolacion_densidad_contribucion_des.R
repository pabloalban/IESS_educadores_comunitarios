message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos cotizantes a cesantía y desempleo")
# load( paste0( parametros$RData, 'IESS_onu_pea_ecu_int.RData' ) )
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))
# load( paste0( parametros$RData_seg, 'IESS_DES_pagos_edad_sex.RData' ) )

# Calculando la densidad de cotizante de seguro de cesantía y desempleo -----------------------------
message("\tCalculando la densidad de cotización")

densidad_cotizacion <- left_join(cotizantes_sgo,
  cotizantes_ces_des,
  by = c("anio" = "aniper", "edad", "genero")
)
densidad_cotizacion[which(is.na(densidad_cotizacion$cotizantes)), ]$cotizantes <- 0

## Calculando densidad de cotización por año -------------------------------------------------------
# densidad_cotizacion<-densidad_cotizacion %>%
#                      mutate(den_cot=cotizantes/cotizantes_sgo) %>%
#                      mutate(den_cot=replace(den_cot, den_cot>1, 1)) %>%
#                      mutate(den_cot=replace(den_cot, den_cot== Inf, 1)) %>%
#                      filter(anio>2013)

# Calculado la densidad de cotización para los ultimos 5 años --------------------------------------

# Interpolación de la tasa de densidad -------------------------------------------------------------
# Se calcula para el período 2014 a 2018 (5 años)
densidad_cotizacion <- densidad_cotizacion %>%
  group_by(edad, genero) %>%
  filter(anio > 2013) %>%
  mutate(sum_cotizantes_sgo = sum(cotizantes_sgo)) %>%
  mutate(sum_cotizantes = sum(cotizantes)) %>%
  mutate(den_cot = sum_cotizantes / sum_cotizantes_sgo) %>%
  mutate(den_cot = replace(den_cot, den_cot > 1, 1)) %>%
  mutate(den_cot = replace(den_cot, den_cot == Inf, 1)) %>%
  ungroup() %>%
  distinct(edad, genero, .keep_all = TRUE) %>%
  select(edad, genero, den_cot)
densidad_cotizacion <- na.omit(densidad_cotizacion)


# Hombres ------------------------------------------------------------------------------------------
densidad_cotizacion_int_h <- densidad_cotizacion %>%
  filter(genero == "M")



# plot(densidad_cotizacion_int_h$edad,densidad_cotizacion_int_h$den_cot)
mod <- smooth.spline(densidad_cotizacion_int_h$edad, densidad_cotizacion_int_h$den_cot, df = 7) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  den_cot_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
densidad_cotizacion_int_h <- left_join(pred, densidad_cotizacion_int_h, by = "edad")

# Gráfico del ajuste para hombres ------------------------------------------------------------------
# plot(densidad_cotizacion_int_h$edad,densidad_cotizacion_int_h$den_cot,col="grey",xlab="Age",ylab="Wages")
# lines(predict(mod,newdata = list(edad=c(15:115)))$x,predict(mod,newdata = list(edad=c(15:115)))$y)

# Utilizando la funcción lm()---------------------
# mod<-lm(den_cot ~ bs(edad)-1,data = densidad_cotizacion_int_h )
# summary(mod)
# plot(densidad_cotizacion_int_h$edad,densidad_cotizacion_int_h$den_cot,col="grey",xlab="Age",ylab="Wages")
# points(c(15:115),predict(mod,newdata = list(edad=c(15:115))),col="darkgreen",lwd=2,type="l")

# Mujeres-------------------------------------------------------------------------------------------
densidad_cotizacion_int_m <- densidad_cotizacion %>%
  filter( genero == "F" )

mod <- smooth.spline( densidad_cotizacion_int_m$edad, densidad_cotizacion_int_m$den_cot, df = 7 ) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  den_cot_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
densidad_cotizacion_int_m <- left_join(pred, densidad_cotizacion_int_m, by = "edad")

# Gráfico del ajuste para mujeres-------------------------------------------------------------------
# plot(densidad_cotizacion_int_m$edad,densidad_cotizacion_int_m$den_cot,col="grey",xlab="Age",ylab="Wages")
# lines(predict(mod,newdata = list(edad=c(15:115)))$x,predict(mod,newdata = list(edad=c(15:115)))$y)

# Unir dataframes-----------------------------------------------------------------------------------
densidad_cotizacion_int_m <- as.data.table( densidad_cotizacion_int_m )
densidad_cotizacion_int_m[ is.na( genero ), genero := 'F' ]
densidad_cotizacion_int_h <- as.data.table( densidad_cotizacion_int_h )
densidad_cotizacion_int_h[ is.na( genero ), genero := 'M' ]
densidad_cotizacion_int <- rbind(densidad_cotizacion_int_m, densidad_cotizacion_int_h)
densidad_cotizacion_int[ den_cot_int < 0, den_cot_int := 0 ]

# Guarda la tasa de uso interpolada en un Rdata ----------------------------------------------------
message("\tGuardando tasa interpolada de uso del Seguro de Desempleo")

save(densidad_cotizacion_int,
  file = paste0(parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
