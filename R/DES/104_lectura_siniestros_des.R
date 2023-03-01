message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los beneficiarios al seguro de desempleo' )



#Cargando información financiera--------------------------------------------------------------------
file_siniestros<-paste0(parametros$Data_seg, 'Info_11095.xlsx' )

#Caraga de recursos administrados por el BIESS------------------------------------------------------
siniestros_desempleo <- read_excel(file_siniestros,
                             sheet = 1,
                             col_names = TRUE,
                             col_types = NULL,
                             na = "",
                             skip = 0) %>% clean_names()


siniestros_desempleo <- siniestros_desempleo %>%
  mutate( sd_fecha_ord = as.Date(fecha_orden_pago,"%Y-%m-%d"),
          fecha_cese = as.Date(fecha_cese,"%Y-%m-%d"),
          sd_genero = ifelse(genero == "MASCULINO", "M", "F") ) %>%
  mutate( anio_pago = year(fecha_orden_pago)) %>%
  gather(sd_tipo_pago, sd_valor_pagado, valor_parte_fija:valor_parte_variable, factor_key=TRUE) %>%
  mutate(sd_tipo_pago = ifelse(sd_tipo_pago == "valor_parte_fija", "F", "V") ) %>%
  mutate( sd_fecha_pago = sd_fecha_ord ) %>%
  dplyr::select(sd_cedula = cedula_afiliado,
         sd_genero,
         edad,
         anio_pago,
         sd_fecha_ord,
         sd_fecha_pago,
         sd_numero_pagos = numero_pagos,
         sd_tipo_pago,
         sd_valor_pagado
         ) %>%
  filter( sd_valor_pagado > 0)

pagos_des_edad_sex <- siniestros_desempleo %>%
  filter(sd_tipo_pago=='F') %>%
  group_by(edad,anio_pago,sd_genero,sd_numero_pagos) %>%
  mutate(beneficiarios=n()) %>%
  ungroup() %>%
  distinct(anio_pago,edad,sd_genero,sd_numero_pagos, .keep_all = TRUE) %>%
  select(sd_numero_pagos,anio_pago,edad,sd_genero,beneficiarios)

#Guardar resultados en un Rdata---------------------------------------------------------------------
message( '\tGuardando beneficiarios de desempleo' )

save( siniestros_desempleo, pagos_des_edad_sex,
      file = paste0( parametros$RData_seg, 'IESS_DES_pagos_edad_sex.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
