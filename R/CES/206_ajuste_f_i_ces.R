message(paste(rep("-", 100), collapse = ""))

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

# 0 . Cargando Rdata--------------------------------------------------------------------------------
message("\tCargando datos para el calculo de f_i")
load( paste0( parametros$RData, "IESS_onu_pea_ecu_int.RData" ) )
load( paste0( parametros$RData, "IESS_estimacion_tasa_actividad.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_DES_cotizantes_historicos.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_demografia.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_Fondo_disponible_inicial_int.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_pagos_prestaciones.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_notacion_prestaciones.RData" ) )
load( paste0( parametros$RData, "IESS_proyeccion_poblacion.RData" ) )
load( paste0( parametros$RData_seg, "IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData" ) )

# Estableciendo la función de ajuste----------------------------------------------------------------
source( 'R/ces/207_funcion_ajuste_f_i_ces.R', encoding = 'UTF-8', echo = FALSE )

#Intervalo de tiempo del calculo
anio_ini <- 2018
anio_fin <- 2018

# 1. Egresos en 2018 por prestaciones de cesantía por cada motivo-----------------------------------
message("\tCalculando los egresos promedio en 2018 por prestaciones de cesantía por cada motivo")

# 1. 1. Egresos de cesantía normal------------------------------------------------------------------
B_ces <- pagos_cesantia  %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by(sexo, edad_siniestro, causales) %>%
  mutate(B=sum(sc_valor_pagado, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(sexo, edad_siniestro, causales, .keep_all = TRUE) %>%
  select(sexo, x:= edad_siniestro, causales, B)

# sum(B_ces$B) #293202755.2

# 1. 2. Egresos de cesantía por débitos automaticos-------------------------------------------------
B_deb_auto <- debitos_automaticos %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  group_by(sexo, edad_siniestro) %>%
  mutate(B=sum(capital_normal_descont_da, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(causales='Debitos') %>%
  distinct(sexo, edad_siniestro, .keep_all = TRUE) %>%
  select(sexo, x:= edad_siniestro, causales, B)

#sum(B_deb_auto$B) #83581897.2870671
# 1. 3. Egresos de cesantía por parte variable de desempleo-----------------------------------------
B_pv_des <- pagos_desempleo %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  filter(sd_tipo_pago =='V') %>%
  group_by(sexo,edad_siniestro) %>%
  mutate(B=sum(sd_valor_pagado, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(sd_numero_pagos=='1') %>%
  group_by(sexo,edad_siniestro) %>%
  ungroup() %>%
  mutate(causales='Desempleo') %>%
  distinct(edad_siniestro, sexo, .keep_all = TRUE) %>%
  select(sexo, x:= edad_siniestro, causales, B)


#sum(B_pv_des$B) #5653072.12

# 1. 4. Egresos promedio por motivo-----------------------------------------------------------------
B_i <- rbind(B_ces, B_deb_auto, B_pv_des) %>%
  left_join(.,codificacion_causales,by=c('causales'='causal')) %>%
  select(-causales)%>%
  mutate(codigo=as.character(codigo))

# 2. Consolidando numero de beneficarios en el año 2018---------------------------------------------
beneficiarios_l_x <- rbind(edad_sexo_debitos,
  edad_sexo_pv_des,
  edad_sexo_ces_jub,
  edad_sexo_ces_fal,
  edad_sexo_ces_normal,
  edad_sexo_ces_vol,
  edad_sexo_ces_lic_mat,
  edad_sexo_ces_obli,
  edad_sexo_ces_zaf,
  edad_sexo_ces_rel
  ) %>%
  filter(anio >= anio_ini, anio <= anio_fin ) %>%
  select(-anio,-fdp) %>%
  mutate(codigo=as.character(codigo))

# 3. Población cotizante a cesantía en 2018---------------------------------------------------------
message("\tCalculando la evolución historica de cesantes por edad y sexo")
den_cot <- as_tibble( densidad_cotizacion_int ) %>%
  select(sexo := genero, x := edad, den_cot_int)

l2_cotc <- left_join( as_tibble( pob_proy ), den_cot, by = c( 'sexo', 'x' )) %>%
  filter( t == 0 ) %>%
  mutate( l2_cotc = l2_cot * den_cot_int,
          l2_cesc = l2_ces * den_cot_int  ) %>%
  select( sexo, x, l2_cotc, l2_cesc)

# 4. Calculo de la función de distribuciín f^i_{g,x}------------------------------------------------
S_F <- S_F_edad_sexo %>%
  select(sexo, x, F := F, S := S)

f_i <- left_join( S_F, l2_cotc, by = c( 'x', 'sexo' )) %>%
  full_join( ., B_i, by=c('sexo', 'x' ) ) %>%
  full_join( ., beneficiarios_l_x, by=c('sexo', 'x', 'codigo' ) ) %>%
  filter(!is.na(codigo)) %>%
  mutate(B_i_prom = B / lx,
         F_prom = F / ( l2_cotc )) %>%
  mutate(f_i = B_i_prom / F_prom) %>%
  arrange( codigo, sexo, x)

# 4. 1. Generando grilla para toda edad, sexo y codigo----------------------------------------------
grid <- expand.grid( codigo = as.character( c( 9:18 ) ),
  x = c( 15:105 ),
  sexo = c('M', 'F'))

f_i <- left_join( grid, f_i, by = c( 'x', 'sexo', 'codigo' ) ) %>%
  arrange(codigo, sexo, x) %>%
  filter( f_i != 0)

# 5 . Ajuste de f_i---------------------------------------------------------------------------------
age_grid <- c(seq(15,115,1))
  
f_9_m <- ajuste_f_i( f_i, 9, 8, age_grid, 'M', c('92', '90') )  
f_9_f <- ajuste_f_i( f_i, 9, 12, age_grid, 'F', c('') ) 
f_10_m <- ajuste_f_i( f_i, 10, 6, age_grid, 'M', c('32', '33', '37', '36', '91', '92') )  
f_10_f <- ajuste_f_i( f_i, 10, 5, age_grid, 'F', c('33',  '92') ) 
f_11_m <- ajuste_f_i( f_i, 11, 8, age_grid, 'M', c('85', '82') )  
f_11_f <- ajuste_f_i( f_i, 11, 6, age_grid, 'F', c('85') )  
f_12_m <- ajuste_f_i( f_i, 12, 6, age_grid, 'M', c('') )  
f_12_f <- ajuste_f_i( f_i, 12, 6, age_grid, 'F', c('71') )  
f_13_m <- ajuste_f_i( f_i, 13, 6, age_grid, 'M', c('80', '85') )  
f_13_f <- ajuste_f_i( f_i, 13, 6, age_grid, 'F', c('') )  
f_14_m <- ajuste_f_i( f_i, 14, 6, age_grid, 'M', c('') )  
f_14_f <- ajuste_f_i( f_i, 14, 6, age_grid, 'F', c('') )  
f_16_m <- ajuste_f_i( f_i, 16, 6, age_grid, 'M', c('') )
f_16_f <- f_16_m
#No hay mujeres safreras

f_15_m <- f_i %>%
  filter( codigo == 15, sexo == 'M') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / F_prom ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)

f_15_f <- f_i %>%
  filter( codigo == 15, sexo == 'F') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / F_prom ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int )

f_16_m <- f_i %>%
  filter( codigo == 16, sexo == 'M') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)


f_16_f <- f_i %>%
  filter( codigo == 16, sexo == 'F') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)


f_17_m <- f_i %>%
  filter( codigo == 17, sexo == 'M') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)


f_17_f <- f_i %>%
  filter( codigo == 17, sexo == 'F') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)


f_18_m <- f_i %>%
  filter( codigo == 18, sexo == 'M') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)


f_18_f <- f_i %>%
  filter( codigo == 18, sexo == 'F') %>%
  mutate( log_f_i = log( ( sum( B ) / sum( lx ) ) / ( sum( F ) / sum( l2_cotc ) ) ) ) %>%
  mutate( log_f_i_int = log_f_i) %>%
  select( i := codigo, x, sexo, log_f_i, log_f_i_int)

f_i_int <- rbind( f_9_m,
                  f_9_f,
                  f_10_m,
                  f_10_f,
                  f_11_m,
                  f_11_f,
                  f_12_m,
                  f_12_f,
                  f_13_m,
                  f_13_f,
                  f_14_m,
                  f_14_f,
                  f_15_m,
                  f_15_f,
                  f_16_m,
                  f_17_m,
                  f_17_f,
                  f_18_m,
                  f_18_f
                  )

# 4. Guardar la función en un Rdata ----------------------------------------------------------------
message("\tGuardando tasa f_i")

save(f_i,
     f_i_int,
     file = paste0(parametros$RData_seg, "IESS_CES_f_i.RData")
)

# Borrar elementos restantes -----------------------------------------------------------------------
message(paste(rep("-", 100), collapse = ""))
rm(list = ls()[!(ls() %in% c("parametros"))])
gc()
