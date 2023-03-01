message(paste(rep("-", 100), collapse = ""))

# 0 . Cargando Rdata--------------------------------------------------------------------------------
message("\tCargando los egresos de cesatía")
load(paste0(parametros$RData, "IESS_onu_pea_ecu_int.RData"))
load(paste0(parametros$RData, "IESS_estimacion_tasa_actividad.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_demografia.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_cuentas_individuales.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_pagos_prestaciones.RData"))

anio_ini <- 2018
anio_fin <- 2018

# 1. Egresos en 2018 por prestaciones de cesantía---------------------------------------------------
message("\tCalculando los egresos de cesatía por edad y sexo")

# 1. 1. Egresos de cesantía normal------------------------------------------------------------------
B_ces <- pagos_cesantia  %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by(sexo, edad_siniestro) %>%
  mutate(B=sum(sc_valor_pagado, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo, edad_siniestro, .keep_all = TRUE) %>%
  select(sexo, x:= edad_siniestro, B)
  
# sum(B_ces$B) #293202755.2

# 1. 2. Egresos de cesantía por débitos automaticos-------------------------------------------------
B_deb_auto <- debitos_automaticos %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by(sexo, edad_siniestro) %>%
  mutate(B=sum(capital_normal_descont_da, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo, edad_siniestro, .keep_all = TRUE) %>%
  select(sexo, x:= edad_siniestro, B)

#sum(B_deb_auto$B) #83581897.2870671
# 1. 3. Egresos de cesantía por parte variable de desempleo-----------------------------------------
B_pv_des <- pagos_desempleo %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  filter(sd_tipo_pago =='V') %>%
  group_by(sexo,edad_siniestro) %>%
  mutate(B=sum(sd_valor_pagado, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(edad_siniestro, sexo, .keep_all = TRUE) %>%
  select(sexo,x:=edad_siniestro,B)


#sum(B_pv_des$B) #5653072.12

# 1. 4. Egresos totales-----------------------------------------------------------------------------
B <- rbind(B_ces, B_deb_auto, B_pv_des) %>%
  group_by(sexo,x) %>%
  mutate(B=sum(B, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo, x, .keep_all = TRUE)

#sum(B$B) #382437724.607067

# 2. Cálculo  del Fondos disponibles al 2018--------------------------------------------------------
S_F_edad_sexo <- full_join(Saldo_edad_sexo_cuentas_ind,B,by=c('sexo','x')) %>%
  mutate(B=ifelse(is.na(B),0,B)) %>%
  mutate(F=S+B)

f <- sum(S_F_edad_sexo$F) 
s <- sum(S_F_edad_sexo$S) 

# 3 . Suavisamiento del Fondo disponible por cohortes-----------------------------------------------
message("\tSuavisamiento del Fondo disponible")
age.grid <- c(seq(15,115,1))

# 3 . 1 . Hombres-----------------------------------------------------------------------------------
F_m <- S_F_edad_sexo %>%
  filter(sexo == "M", is.finite(F))
aux <- F_m
f <- sum(aux$F)

mod <- smooth.spline(aux$x, aux$F, df = 16)

pred <- data.frame(x = age.grid, F_int = predict(mod, age.grid, deriv = 0)[["y"]])

F_m <- left_join(pred, F_m, by = "x") %>%
  mutate(sexo := "M", F_int = ifelse(  F_int < 0, F,  F_int )) %>%
  mutate(F_int = F_int * f / sum(F_int)  ) %>%
  select(x, sexo, F, F_int)

# 3 . 2 . Mujeres ----------------------------------------------------------------------------------
F_f <- S_F_edad_sexo %>%
  filter(sexo == "F", is.finite(F))
aux <- F_f %>% filter( !(x %in% c('90','92')) )
f <- sum(aux$F)

mod <- smooth.spline(aux$x, aux$F, df = 16)

pred <- data.frame(x = age.grid, F_int = predict(mod, age.grid, deriv = 0)[["y"]])

F_f <- left_join(pred, F_f, by = "x") %>%
  mutate(sexo := "F", F_int = ifelse(  F_int < 0, F,  F_int)) %>%
  mutate(F_int = F_int * f / sum(F_int)  ) %>%
  select(x, sexo, F, F_int)

# 4 . Gráfica del suvisamiento----------------------------------------------------------------------
# 4 . 1 . Hombres-----------------------------------------------------------------------------------
plot(F_m$x,F_m$F)
lines(F_m$x,F_m$F_int)

# 4 . 2 . Mujeres-----------------------------------------------------------------------------------
plot(F_f$x,F_f$F)
lines(F_f$x,F_f$F_int)


# 5 . Suavisamiento del Saldo a 31/12/2018 por cohortes---------------------------------------------
message("\tSuavisamiento del Saldo")
age.grid <- c(seq(15,115,1))

# 5 . 1 . Hombres-----------------------------------------------------------------------------------
S_m <- S_F_edad_sexo %>%
  filter(sexo == "M", is.finite(S))
aux <- S_m
s <- sum(aux$S)

mod <- smooth.spline(aux$x, aux$S, df = 16)

pred <- data.frame(x = age.grid, S_int = predict(mod, age.grid, deriv = 0)[["y"]])

S_m <- left_join(pred, S_m, by = "x") %>%
  mutate(sexo := "M", S_int = ifelse(  S_int < 0, S,  S_int )) %>%
  mutate(S_int = S_int * s / sum(S_int)  ) %>%
  select(x, sexo, S, S_int)

# 5 . 2 . Mujeres ----------------------------------------------------------------------------------
S_f <- S_F_edad_sexo %>%
  filter(sexo == "F", is.finite(S))
aux <- S_f %>% filter( !(x %in% c('90','92')) )
s <- sum(aux$S)

mod <- smooth.spline(aux$x, aux$S, df = 14)

pred <- data.frame(x = age.grid, S_int = predict(mod, age.grid, deriv = 0)[["y"]])

S_f <- left_join(pred, S_f, by = "x") %>%
  mutate(sexo := "F", S_int = ifelse(  S_int < 0, S,  S_int)) %>%
  mutate(S_int = S_int * s / sum(S_int)  ) %>%
  select(x, sexo, S, S_int)

# 6 . Gráfica del suvisamiento----------------------------------------------------------------------
# 6 . 1 . Hombres-----------------------------------------------------------------------------------
plot(S_m$x, S_m$S)
lines(S_m$x, S_m$S_int)

# 6 . 2 . Mujeres-----------------------------------------------------------------------------------
plot(S_f$x, S_f$S)
lines(S_f$x, S_f$S_int)

# 7. Consolidad bases-------------------------------------------------------------------------------
F <- rbind(
  F_m,
  F_f
)

S <- rbind(
  S_m,
  S_f
)

S_F_edad_sexo <- left_join( F, S, by=c('x', 'sexo') )

# 7. Guardar la tasa de uso interpolada en un Rdata ---------------------------------------------------
message("\tGuardando tasa de retiro de fondos de cesantes")

save(F,
     S_F_edad_sexo,
     file = paste0(parametros$RData_seg, "IESS_CES_Fondo_disponible_inicial_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
