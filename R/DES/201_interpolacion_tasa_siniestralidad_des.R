message(paste(rep("-", 100), collapse = ""))

message("\tCargando datos para la tasa de siniestralidad")
load(paste0(parametros$RData, "IESS_onu_pea_ecu_int.RData"))
load(paste0(parametros$RData, "IESS_estimacion_tasa_actividad.RData"))
load(paste0(parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData"))
load(paste0(parametros$RData_seg, "IESS_DES_pagos_edad_sex.RData"))

# Calculando la exposición (numero de cesantes promedio por edad y sexo para cada año) -------------
# utilizando la tasa de actividad estimada
message("\tCalculando la evolución historica de cesantes por edad y sexo")
tau <- tasa_act_proy %>%
  select(sexo, x, tau)
colnames(cotizantes_sgo) <- c("x", "sexo", "anio", "l2_cot")

ajust_pob <- 1 # tasa de ajuste de población
cotizantes_sgo$l2_cot <- ajust_pob * cotizantes_sgo$l2_cot
pob_hist <- left_join(cotizantes_sgo, tau,
  by = c("x", "sexo")
) %>%
  mutate(l2 = l2_cot / tau) %>%
  mutate(l2_ces = l2 - l2_cot)
# Se empezó a entregar desde abril de 2016
pob_hist[which(pob_hist$anio == 2016), ]$l2_ces <- (12 / 12) * pob_hist[which(pob_hist$anio == 2016), ]$l2_ces

ER <- pob_hist %>%
  filter(anio >= 2016) %>%
  group_by(x, sexo) %>%
  mutate(ER = sum(l2_ces, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(x, sexo, .keep_all = TRUE) %>%
  select(x, sexo, ER)

# Calculando la probabilidad desiniestralidad ------------------------------------------------------
tasa_siniestralidad <- left_join(pagos_des_edad_sex,
  ER,
  by = c("sd_genero" = "sexo", "edad" = "x")
)
tasa_siniestralidad <- tasa_siniestralidad %>%
  group_by(sd_numero_pagos, edad, sd_genero) %>%
  mutate(suma_beneficiarios = sum(beneficiarios, na.rm = TRUE)) %>%
  distinct(sd_numero_pagos, edad, sd_genero, .keep_all = TRUE) %>%
  mutate(tasa_uso = suma_beneficiarios / ER) %>%
  ungroup() %>%
  select(sd_numero_pagos, edad, sd_genero, tasa_uso)

# Interpolando la tasa de uso del seguro de desempleo-----------------------------------------------
age.grid <- c(seq(1:115))

# Hombres-------------------------------------------------------------------------------------------
p1_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "1", sd_genero == "M")
aux <- p1_tasa_uso_int_m[which(p1_tasa_uso_int_m$edad != 94), ]

# Utilizando función lm()
# p1_tasa_uso_mod_m<-lm(tasa_uso ~ bs(edad)-1,data = aux )
# summary(p1_tasa_uso_mod_m)
# plot(aux$edad,aux$tasa_uso,col="grey",xlab="Age",ylab="Wages")
# points(age.grid,predict(p1_tasa_uso_mod_m,newdata = list(edad=age.grid)),col="darkgreen",lwd=2,type="l")

mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9)
# Gráfica del ajuste
# plot(aux$edad,aux$tasa_uso,col="grey",xlab="Age",ylab="Wages")
# lines(predict(mod,newdata = list(edad=c(19:94)))$x,
# predict(mod,newdata = list(edad=c(19:94)))$y)
pred <- data.frame(edad = c(15:115), tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]])
p1_tasa_uso_int_m <- left_join(pred, p1_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 1, sd_genero := "M")

p2_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "2", sd_genero == "M")
aux <- p2_tasa_uso_int_m[which(p2_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p2_tasa_uso_int_m <- left_join(pred, p2_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 2, sd_genero := "M")


p3_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "3", sd_genero == "M")
aux <- p3_tasa_uso_int_m[which(p3_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p3_tasa_uso_int_m <- left_join(pred, p3_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 3, sd_genero := "M")

p4_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "4", sd_genero == "M")
aux <- p4_tasa_uso_int_m[which(p4_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p4_tasa_uso_int_m <- left_join(pred, p4_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 4, sd_genero := "M")

p5_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "5", sd_genero == "M")
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p5_tasa_uso_int_m <- left_join(pred, p5_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 5, sd_genero := "M")

# Mujeres ------------------------------------------------------------------------------------------
p1_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "1", sd_genero == "F")
aux <- p1_tasa_uso_int_f[which(p1_tasa_uso_int_f$edad <= 76), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p1_tasa_uso_int_f <- left_join(pred, p1_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 1, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )


p2_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "2", sd_genero == "F")
aux <- p2_tasa_uso_int_f[which(p2_tasa_uso_int_f$edad <= 78), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p2_tasa_uso_int_f <- left_join(pred, p2_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 2, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

p3_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "3", sd_genero == "F")
aux <- p3_tasa_uso_int_f[which(p3_tasa_uso_int_f$edad <= 76), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p3_tasa_uso_int_f <- left_join(pred, p3_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 3, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

p4_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "4", sd_genero == "F")
aux <- p4_tasa_uso_int_f[which(p4_tasa_uso_int_f$edad <= 76), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p4_tasa_uso_int_f <- left_join(pred, p4_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 4, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

p5_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "5", sd_genero == "F")
aux <- p5_tasa_uso_int_f[which(p5_tasa_uso_int_f$edad <= 75), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p5_tasa_uso_int_f <- left_join(pred, p5_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 5, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

# Consolidad bases----------------------------------------------------------------------------------
tasa_siniestralidad <- rbind(
  p1_tasa_uso_int_f,
  p2_tasa_uso_int_f,
  p3_tasa_uso_int_f,
  p4_tasa_uso_int_f,
  p5_tasa_uso_int_f,
  p1_tasa_uso_int_m,
  p2_tasa_uso_int_m,
  p3_tasa_uso_int_m,
  p4_tasa_uso_int_m,
  p5_tasa_uso_int_m
) %>%
  select(sd_numero_pagos, edad, sd_genero, tasa_uso, tasa_uso_int)
tasa_siniestralidad[which(tasa_siniestralidad$tasa_uso_int < 0), ]$tasa_uso_int <- 0
tasa_1 <- tasa_siniestralidad

# Tasa de uso con pagos indebidos ------------------------------------------------------------------
tasa_siniestralidad <- left_join(pagos_con_ind_des_edad_sex,
  ER,
  by = c("sd_genero" = "sexo", "edad" = "x")
)
tasa_siniestralidad <- tasa_siniestralidad %>%
  group_by(sd_numero_pagos, edad, sd_genero) %>%
  mutate(suma_beneficiarios = sum(beneficiarios, na.rm = TRUE)) %>%
  distinct(sd_numero_pagos, edad, sd_genero, .keep_all = TRUE) %>%
  mutate(tasa_uso = suma_beneficiarios / ER) %>%
  ungroup() %>%
  select(sd_numero_pagos, edad, sd_genero, tasa_uso)

# Interpolando la tasa de uso del seguro de desempleo
age.grid <- c(seq(1:115))

# Hombres ------------------------------------------------------------------------------------------
p1_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "1", sd_genero == "M")
aux <- p1_tasa_uso_int_m[which(p1_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(edad = c(15:115), tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]])
p1_tasa_uso_int_m <- left_join(pred, p1_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 1, sd_genero := "M")

p2_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "2", sd_genero == "M")
aux <- p2_tasa_uso_int_m[which(p2_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p2_tasa_uso_int_m <- left_join(pred, p2_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 2, sd_genero := "M")


p3_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "3", sd_genero == "M")
aux <- p3_tasa_uso_int_m[which(p3_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p3_tasa_uso_int_m <- left_join(pred, p3_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 3, sd_genero := "M")

p4_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "4", sd_genero == "M")
aux <- p4_tasa_uso_int_m[which(p4_tasa_uso_int_m$edad != 94), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p4_tasa_uso_int_m <- left_join(pred, p4_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 4, sd_genero := "M")

p5_tasa_uso_int_m <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "5", sd_genero == "M")
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p5_tasa_uso_int_m <- left_join(pred, p5_tasa_uso_int_m, by = "edad") %>%
  mutate(sd_numero_pagos := 5, sd_genero := "M")

# Mujeres ------------------------------------------------------------------------------------------
p1_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "1", sd_genero == "F")
aux <- p1_tasa_uso_int_f[which(p1_tasa_uso_int_f$edad <= 76), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p1_tasa_uso_int_f <- left_join(pred, p1_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 1, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )


p2_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "2", sd_genero == "F")
aux <- p2_tasa_uso_int_f[which(p2_tasa_uso_int_f$edad <= 78), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p2_tasa_uso_int_f <- left_join(pred, p2_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 2, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )


p3_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "3", sd_genero == "F")
aux <- p3_tasa_uso_int_f[which(p3_tasa_uso_int_f$edad <= 76), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p3_tasa_uso_int_f <- left_join(pred, p3_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 3, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

p4_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "4", sd_genero == "F")
aux <- p4_tasa_uso_int_f[which(p4_tasa_uso_int_f$edad <= 77), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p4_tasa_uso_int_f <- left_join(pred, p4_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 4, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

p5_tasa_uso_int_f <- tasa_siniestralidad %>%
  filter(sd_numero_pagos == "5", sd_genero == "F")
aux <- p5_tasa_uso_int_f[which(p5_tasa_uso_int_f$edad <= 77), ]
mod <- smooth.spline(aux$edad, aux$tasa_uso, df = 9) # 16 degrees of freedom
pred <- data.frame(
  edad = c(15:115),
  tasa_uso_int = predict(mod, c(15:115), deriv = 0)[["y"]]
)
p5_tasa_uso_int_f <- left_join(pred, p5_tasa_uso_int_f, by = "edad") %>%
  mutate(sd_numero_pagos := 5, sd_genero := "F",
    tasa_uso_int = replace(tasa_uso_int, tasa_uso_int < 0, 0)
  )

# Consolidad bases ---------------------------------------------------------------------------------
tasa_siniestralidad <- rbind(
  p1_tasa_uso_int_f,
  p2_tasa_uso_int_f,
  p3_tasa_uso_int_f,
  p4_tasa_uso_int_f,
  p5_tasa_uso_int_f,
  p1_tasa_uso_int_m,
  p2_tasa_uso_int_m,
  p3_tasa_uso_int_m,
  p4_tasa_uso_int_m,
  p5_tasa_uso_int_m
) %>%
  select(sd_numero_pagos, edad, sd_genero, tasa_uso, tasa_uso_int)
tasa_siniestralidad[which(tasa_siniestralidad$tasa_uso_int < 0), ]$tasa_uso_int <- 0

# Guardar la tasa de uso interpolada en un Rdata ---------------------------------------------------
message("\tGuardando tasa interpolada de uso del Seguro de Desempleo")
tasa_siniestralidad_pagos_ind <- tasa_siniestralidad
tasa_siniestralidad <- tasa_1

save(tasa_siniestralidad, tasa_siniestralidad_pagos_ind,
  file = paste0(parametros$RData_seg, "IESS_DES_tasa_uso_edad_sexo_int.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
