message( paste( rep('-', 100 ), collapse = '' ) )

#Cargando datos-------------------------------------------------------------------------------------
message( '\tCargando datos' )
load( paste0( parametros$RData_seg, 'IESS_ECO_aportes.RData' ) )

#Educadores por estado------------------------------------------------------------------------------

edu_estado <- edu_comunitarios %>%
  mutate( estado = ifelse( is.na( vej_fec_der ),
                           NA,
                           'Jubilados del SGO') ) %>%
  
  mutate( estado = ifelse( !is.na( vej_fec_der ) & is.na( imposiciones_sgo  ),
                           'Pensionistas de viudez sin cotizar al SGO',
                           estado) ) %>%
  
  mutate( estado = ifelse( is.na( estado ) & fecha_sal > as.Date("01/01/2022","%d/%m/%Y"),
                           'Afiliados Activos',
                           estado ) ) %>%
  mutate( estado = ifelse( !is.na( fecha_defuncion ) & (imposiciones_sgo > 0),
                           'Fallecidos con cotizaciones al SGO',
                           estado ) ) %>%
  
  mutate( estado = ifelse( !is.na( fecha_defuncion ) & is.na(imposiciones_sgo),
                           'Fallecidos sin cotizar al SGO',
                           estado) ) %>%
  
  mutate( estado = ifelse( !is.na( vej_fec_der ) & !is.na(fecha_defuncion),
                           'Fallecidos Jubilados del SGO',
                           estado) ) %>%
  
  mutate( estado = ifelse( !is.na( vej_fec_der ) & !is.na(fecha_defuncion) & is.na( imposiciones_sgo ),
                           'Fallecidos pensionistas de viudez sin cotizar al SGO',
                           estado) ) %>%
  
  mutate( estado = ifelse( is.na( estado ) & fecha_sal <= as.Date("01/01/2022","%d/%m/%Y") & is.na(imposiciones_sgo),
                           'Cesantes sin cotizar al SGO',
                           estado ) ) %>%
  
  mutate( estado = ifelse( is.na( estado ) & fecha_sal <= as.Date("01/01/2022","%d/%m/%Y") & (imposiciones_sgo > 0),
                           'Cesantes con cotizar al SGO',
                           estado ) ) %>%
  
  mutate( estado = ifelse( is.na( fecha_nacimiento ),
                           'No existe registro en el DGRCIC',
                           estado ) )
  
aux_1 <- edu_estado %>%
  group_by( estado, sexo ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  distinct( estado, sexo, .keep_all = TRUE ) %>%
  dplyr::select( estado, sexo, freq ) %>%
  drop_na() %>%
  #mutate( dist = 100 * freq/5025) %>%
  spread(.,sexo,value = c(freq ) )


aux_2 <- edu_estado %>%
  group_by( estado, sexo ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  distinct( estado, sexo, .keep_all = TRUE ) %>%
  dplyr::select( estado, sexo, freq ) %>%
  drop_na() %>%
  mutate( dist = 100 * freq/5010) %>%
  dplyr::select( -freq ) %>%
  spread(.,sexo,value = c(dist ), drop= FALSE ) %>%
  dplyr::select( estado, H_dist:= H, M_dist:= M)

tab_estados <- left_join( aux_1,
                          aux_2, 
                          by = 'estado' ) %>%
  dplyr::select( estado,
                 H,
                 H_dist,
                 M,
                 M_dist )  %>% 
  replace(is.na(.), 0) %>%
  mutate( total = H + M,
          total_dist = H_dist + M_dist )

#tab_estados <- rbind((tab_estados), c(as.character(c('Sin registro',rep(NA,4), 15, 15/5025)) ) )
aux <- rbind((tab_estados), c("Total", as.character(colSums(tab_estados[,2:ncol(tab_estados)],  na.rm =TRUE ))))


aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

tab_estados <- aux

#Educadores por edad y sexo-------------------------------------------------------------------------

tab_edad_sexo <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) & is.na(fecha_defuncion) ) %>%
  group_by( sexo, edad ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  mutate( dist = freq / n()) %>%
  distinct(edad, sexo, .keep_all = TRUE ) %>%
  arrange( edad, sexo ) %>%
  dplyr::select( edad,
                 sexo,
                 freq,
                 dist )

#Educadores por provincia---------------------------------------------------------------------------

aux_1 <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) & is.na( fecha_defuncion ) ) %>%
  group_by( provincia, sexo ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  distinct( provincia, sexo, .keep_all = TRUE ) %>%
  dplyr::select( provincia, sexo, freq ) %>%
  drop_na() %>%
  #mutate( dist = 100 * freq/5025) %>%
  spread(.,sexo,value = c( freq ) )


aux_2 <- edu_estado %>%
  group_by( provincia, sexo ) %>%
  mutate( freq = n() ) %>%
  ungroup() %>%
  distinct( provincia, sexo, .keep_all = TRUE ) %>%
  dplyr::select( provincia, sexo, freq ) %>%
  drop_na() %>%
  mutate( dist = 100 * freq/5010) %>%
  dplyr::select( -freq ) %>%
  spread(.,sexo,value = c(dist ), drop= FALSE ) %>%
  dplyr::select( provincia, H_dist:= H, M_dist:= M)

tab_provincia <- left_join( aux_1,
                          aux_2, 
                          by = 'provincia' ) %>%
  dplyr::select( provincia,
                 H,
                 H_dist,
                 M,
                 M_dist ) %>%
  mutate( total = H + M,
          total_dist = H_dist + M_dist )

#tab_estados <- rbind((tab_estados), c(as.character(c('Sin registro',rep(NA,4), 15, 15/5025)) ) )
aux <- rbind((tab_provincia), c("Total", as.character(colSums(tab_provincia[,2:ncol(tab_provincia)],  na.rm =TRUE ))))


aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

tab_provincia <- aux

#Tabla por rangos de edad---------------------------------------------------------------------------

cortes_edad<-c(19,seq(30,90,10),97)
etiquetas_edad<-c(paste0("(",formatC( c(20,seq(30,90,10)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "-",formatC( c(seq(30,90,10),96), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))
aux  <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) & is.na( fecha_defuncion ) ) %>%
  mutate(rango_edad=cut(edad, breaks = cortes_edad,
                         labels = etiquetas_edad,
                         #include.lowest = TRUE,
                         right = TRUE)) %>%
  group_by(sexo,rango_edad) %>%
  mutate(freq=n()) %>%
  ungroup() %>%
  mutate( dist = 100 * freq / n()) %>%
  distinct(rango_edad, sexo, .keep_all = TRUE ) %>%
  arrange( rango_edad, sexo ) %>%
  dplyr::select( rango_edad,
                 sexo,
                 freq,
                 dist )


aux_1 <- aux %>%
  dplyr::select( - dist ) %>%
  spread(.,sexo,value = c( freq ) ) %>% 
  replace(is.na(.), 0)


aux_2 <- aux %>%
  dplyr::select( - freq ) %>%
  spread(.,sexo,value = c( dist ) ) %>% 
  replace(is.na(.), 0) %>%
  dplyr::select( rango_edad,
                 H_dist:= H,
                 M_dist:= M)

tab_rango_edad <- left_join( aux_1,
                            aux_2, 
                            by = 'rango_edad' ) %>%
  dplyr::select( rango_edad,
                 H,
                 H_dist,
                 M,
                 M_dist ) %>%
  mutate( total = H + M,
          total_dist = H_dist + M_dist )

tab_rango_edad$rango_edad  <- as.character( tab_rango_edad$rango_edad )

aux <- rbind((tab_rango_edad), c("Total", as.character(colSums(tab_rango_edad[,2:ncol(tab_rango_edad)],  na.rm =TRUE ))))


aux[2:ncol(aux)] <- lapply(aux[2:ncol(aux)], function(x) as.numeric(x))

tab_rango_edad <- aux


#Tabla Rangos de imposiciones totales por rango de edad--------------------------------------------

cortes_edad<-c(19,seq(30,90,10),97)
etiquetas_edad<-c(paste0("(",formatC( c(20,seq(30,90,10)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "-",formatC( c(seq(30,90,10),96), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

cortes_imp<-c(-1,seq(5,45,5),52)
etiquetas_imp<-c(paste0("rango",formatC( c(0,seq(6,46,5)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "",formatC( c(seq(5,45,5),52), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),""))

aux  <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  filter( is.na( fecha_defuncion ) ) %>%
  mutate(rango_edad=cut(edad, 
                        breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  
  group_by( sexo, rango_edad ) %>%
  mutate( imp = as.integer( sum(imposiciones_tot/12, na.rm = TRUE) / n() ) ) %>%
  ungroup( ) %>%
  mutate(rango_imp=cut(imp, 
                       breaks = cortes_imp,
                       labels = etiquetas_imp,
                      #include.lowest = TRUE,
                       right = TRUE)) %>%
  group_by(sexo,rango_edad, rango_imp) %>%
  mutate(freq=n()) %>%
  ungroup() %>%
  distinct(rango_edad, sexo, rango_imp, .keep_all = TRUE ) %>%
  arrange( rango_edad, sexo ) %>%
  dplyr::select( rango_edad,
                 sexo,
                 freq,
                 rango_imp ) 
  
tab_imp_tot_h <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'H') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))


tab_imp_tot_m <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'M') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))




#Tabla Rangos de imposiciones como educadores comunitarios por rango de edad------------------------

cortes_edad<-c(19,seq(30,90,10),97)
etiquetas_edad<-c(paste0("(",formatC( c(20,seq(30,90,10)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "-",formatC( c(seq(30,90,10),96), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

cortes_imp<-c(-1,seq(5,45,5),52)
etiquetas_imp<-c(paste0("rango",formatC( c(0,seq(6,46,5)), 
                                         digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                        "",formatC( c(seq(5,45,5),52), 
                                    digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),""))

aux  <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  filter( is.na( fecha_defuncion ) ) %>%
  mutate(rango_edad=cut(edad, 
                        breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  
  group_by( sexo, rango_edad ) %>%
  mutate( imp = as.integer( sum(imposiciones_edu/12, na.rm = TRUE) / n() ) ) %>%
  ungroup( ) %>%
  mutate(rango_imp=cut(imp, 
                       breaks = cortes_imp,
                       labels = etiquetas_imp,
                       #include.lowest = TRUE,
                       right = TRUE)) %>%
  group_by(sexo,rango_edad, rango_imp) %>%
  mutate(freq=n()) %>%
  ungroup() %>%
  distinct(rango_edad, sexo, rango_imp, .keep_all = TRUE ) %>%
  arrange( rango_edad, sexo ) %>%
  dplyr::select( rango_edad,
                 sexo,
                 freq,
                 rango_imp ) 

tab_imp_edu_h <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'H') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))


tab_imp_edu_m <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'M') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))


#Tabla Rangos de imposiciones en el SGO por rango de edad-------------------------------------------

cortes_edad<-c(19,seq(30,90,10),97)
etiquetas_edad<-c(paste0("(",formatC( c(20,seq(30,90,10)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "-",formatC( c(seq(30,90,10),96), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

cortes_imp<-c(-1,seq(5,45,5),52)
etiquetas_imp<-c(paste0("rango",formatC( c(0,seq(6,46,5)), 
                                         digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                        "",formatC( c(seq(5,45,5),52), 
                                    digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),""))

aux  <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  filter( is.na( fecha_defuncion ) ) %>%
  mutate(rango_edad=cut(edad, 
                        breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  
  group_by( sexo, rango_edad ) %>%
  mutate( imp = as.integer( sum(imposiciones_sgo/12, na.rm = TRUE) / n() ) ) %>%
  ungroup( ) %>%
  mutate(rango_imp=cut(imp, 
                       breaks = cortes_imp,
                       labels = etiquetas_imp,
                       #include.lowest = TRUE,
                       right = TRUE)) %>%
  group_by(sexo,rango_edad, rango_imp) %>%
  mutate(freq=n()) %>%
  ungroup() %>%
  distinct(rango_edad, sexo, rango_imp, .keep_all = TRUE ) %>%
  arrange( rango_edad, sexo ) %>%
  dplyr::select( rango_edad,
                 sexo,
                 freq,
                 rango_imp ) 

tab_imp_sgo_h <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'H') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))


tab_imp_sgo_m <- aux %>%
  spread(.,rango_imp,value = c( freq ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'M') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  mutate( total = rowSums(.[2:ncol(.)]) ) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE ))))



#Tabla Rangos de salarios promedio en el educadores comunitarios por rango de edad--------------------------------------

ult_sueldo_edu <- planillas_edu %>%
  group_by( cedula ) %>%
  mutate( ult_pla = max( fecha_pla ) ) %>%
  ungroup() %>%
  filter( fecha_pla == ult_pla) %>%
  dplyr::select( cedula,
                 sueldo )

edu_comunitarios <- edu_comunitarios %>%
  left_join(., ult_sueldo_edu, by = 'cedula')


cortes_edad<-c(19,seq(30,90,10),97)
etiquetas_edad<-c(paste0("(",formatC( c(20,seq(30,90,10)), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                         "-",formatC( c(seq(30,90,10),96), 
                                      digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),"]"))

cortes_imp<-c(-1,seq(5,45,5),52)
etiquetas_imp<-c(paste0("rango",formatC( c(0,seq(6,46,5)), 
                                         digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),
                        "",formatC( c(seq(5,45,5),52), 
                                    digits = 0, format = 'f', big.mark = '.', decimal.mark = ',' ),""))

aux  <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) ) %>%
  filter( is.na( fecha_defuncion ) ) %>%
  mutate(rango_edad=cut(edad, 
                        breaks = cortes_edad,
                        labels = etiquetas_edad,
                        #include.lowest = TRUE,
                        right = TRUE)) %>%
  
  group_by( sexo, rango_edad ) %>%
  mutate( imp = as.integer( sum(imposiciones_edu/12, na.rm = TRUE) / n() ) ) %>%
  ungroup( ) %>%
  mutate(rango_imp=cut(imp, 
                       breaks = cortes_imp,
                       labels = etiquetas_imp,
                       #include.lowest = TRUE,
                       right = TRUE)) %>%
  group_by(sexo,rango_edad, rango_imp) %>%
  mutate( sueldo_pro = mean( sueldo, na.rm=TRUE ) ) %>%
  ungroup( ) %>%
  distinct( rango_edad, sexo, rango_imp, .keep_all = TRUE ) %>%
  arrange( rango_edad, sexo ) %>%
  dplyr::select( rango_edad,
                 sexo,
                 sueldo_pro,
                 rango_imp ) 


aux_1 <- aux %>%  
  filter( sexo == 'H') %>%
  group_by( rango_imp ) %>%
  mutate( sueldo_pro = mean( sueldo_pro ),
          rango_edad = 'Promedio') %>%
  ungroup( ) %>%
  distinct(.,rango_imp,.keep_all = TRUE) 

aux_2 <- aux %>%  
  filter( sexo == 'H') %>%
  group_by( rango_edad  ) %>%
  mutate( promedio = mean( sueldo_pro ) ) %>%
  ungroup( ) %>%
  distinct(.,rango_edad ,.keep_all = TRUE) %>%
  dplyr::select( rango_edad, promedio ) %>%
  mutate( rango_edad = as.character( rango_edad ) ) %>%
  rbind(., c('Promedio', mean( filter(edu_comunitarios, sexo=='H')$sueldo, na.rm = TRUE ) ) ) %>%
  mutate( promedio = as.numeric( promedio ))

tab_imp_sgo_h <- aux %>%
  rbind(., aux_1) %>%
  spread(.,rango_imp,value = c( sueldo_pro ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'H') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  left_join(., aux_2, by ='rango_edad')



aux_1 <- aux %>%  
  filter( sexo == 'M') %>%
  group_by( rango_imp ) %>%
  mutate( sueldo_pro = mean( sueldo_pro ),
          rango_edad = 'Promedio') %>%
  ungroup( ) %>%
  distinct(.,rango_imp,.keep_all = TRUE) 

aux_2 <- aux %>%  
  filter( sexo == 'M') %>%
  group_by( rango_edad  ) %>%
  mutate( promedio = mean( sueldo_pro ) ) %>%
  ungroup( ) %>%
  distinct(.,rango_edad ,.keep_all = TRUE) %>%
  dplyr::select( rango_edad, promedio ) %>%
  mutate( rango_edad = as.character( rango_edad ) ) %>%
  rbind(., c('Promedio', mean( filter(edu_comunitarios, sexo=='M')$sueldo, na.rm = TRUE ) ) ) %>%
  mutate( promedio = as.numeric( promedio ))

tab_imp_sgo_m <- aux %>%
  rbind(., aux_1) %>%
  spread(.,rango_imp,value = c( sueldo_pro ) ) %>% 
  replace(is.na(.), 0) %>%
  filter( sexo == 'M') %>%
  mutate( rango_edad = as.character( rango_edad  ) ) %>%
  dplyr::select(-sexo) %>%
  left_join(., aux_2, by ='rango_edad')


# Tabla de salario promedio en educadores comunitarios por edad y sexo------------------------------

tab_sal_edu_edad_sexo <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) & is.na( fecha_defuncion ) ) %>%
  group_by( edad,
            sexo ) %>%
  mutate( salario = mean( sueldo, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( edad,
                 sexo,
                 salario ) %>%
  arrange( sexo, edad )


aux <- planillas_sgo %>%
  mutate( fec_pla = as.Date( paste0(anio,'-', mes,'-01' ) ) ) %>%
  group_by( cedula ) %>%
  mutate( ult_pla = max( fec_pla, na.rm = TRUE )) %>%
  ungroup( ) %>%
  filter( fec_pla == ult_pla ) %>%
  dplyr::select( cedula,
                 sueldo_sgo:=sueldo )

edu_comunitarios <- edu_comunitarios %>%
  left_join(., aux, by ='cedula' )

tab_sal_sgo_edad_sexo <- edu_comunitarios %>%
  filter( !is.na( fecha_nacimiento ) & is.na( fecha_defuncion ) ) %>%
  group_by( edad,
            sexo ) %>%
  mutate( salario = mean( sueldo_sgo, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., edad, sexo, .keep_all = TRUE ) %>%
  dplyr::select( edad,
                 sexo,
                 salario ) %>%
  arrange( sexo, edad ) %>%
  filter( salario > 0)

# Tabla masa salarial de los educadores comunitarios por año y sexo---------------------------------

aux_edu <- planillas_edu %>%
  left_join(., edu_comunitarios %>%
              dplyr::select( cedula, sexo ) ,
            by ='cedula' ) %>%
  group_by( anio, sexo ) %>%
  mutate( masa_edu = sum( sueldo, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, masa_edu ) %>%
  arrange( sexo, anio ) %>%
  mutate( sexo = if_else( is.na( sexo ),
                          'no_registro',
                          sexo ) ) %>%
  spread(.,sexo, value = c(masa_edu ) ) %>%
  replace(is.na(.), 0) %>%
  mutate( total = rowSums(.[2:ncol(.)]) )



aux_sgo <- planillas_sgo %>%
  left_join(., edu_comunitarios %>%
              dplyr::select( cedula, sexo ) ,
            by ='cedula' ) %>%
  group_by( anio, sexo ) %>%
  mutate( masa_edu = sum( sueldo, na.rm = TRUE ) ) %>%
  ungroup( ) %>%
  distinct( ., anio, sexo, .keep_all = TRUE ) %>%
  dplyr::select( anio, sexo, masa_edu ) %>%
  arrange( sexo, anio ) %>%
  mutate( sexo = if_else( is.na( sexo ),
                          'no_registro',
                          sexo ) ) %>%
  spread(.,sexo, value = c(masa_edu ) ) %>%
  replace(is.na(.), 0) %>%
  mutate( total_sgo = rowSums(.[2:ncol(.)]) )


tab_masa <- aux_edu %>%
  full_join( aux_sgo, by ='anio') %>%
  replace(is.na(.), 0) %>%
  rbind((.), c("Total", as.character(colSums(.[,2:ncol(.)],  na.rm =TRUE )))) %>%
  filter (! duplicated( anio )) %>%
  arrange( anio )
  
colnames( tab_masa ) <- c('anio',
                          'hombres_edu',
                          'mujer_edu',
                          'sin_reg_edu',
                          'total_edu',
                          'hombre_sgo',
                          'mujer_sgo',
                          'total_sgo')

tab_masa[2:ncol(tab_masa)] <- lapply(tab_masa[2:ncol(tab_masa)], function(x) as.numeric(x))


# Tabla educadores comunitarios por año-------------------------------------------------------------

tab_edu_anio <- planillas_edu %>%
  distinct( cedula, anio, .keep_all = TRUE ) %>%
  group_by( anio ) %>%
  mutate( edu = n() ) %>%
  ungroup( ) %>%
  distinct( anio, .keep_all = TRUE ) %>%
  dplyr::select( anio, edu ) %>%
  arrange( anio )

# Tabla de educadores sin registro en la DGRCIC-----------------------------------------------------

tab_sin_registro <- edu_comunitarios %>%
  filter(is.na( fecha_nacimiento ) ) %>%
  mutate( fecha_sal_edu = format( fecha_sal_edu, "%B-%Y" )) %>%
  dplyr::select( cedula,
                 provincia,
                 imposiciones_edu,
                 fecha_sal_edu )

# Cálculos de estadísticas--------------------------------------------------------------------------
aux <- edu_comunitarios %>% 
  filter( !is.na( fecha_nacimiento ),
          is.na(fecha_defuncion ) ) %>%
  group_by( sexo ) %>%
  mutate( edad_promedio = mean( edad ) ) %>%
  mutate( sueldo_promedio_edu = mean( sueldo, na.rm=TRUE ) ) %>%
  mutate( sueldo_promedio_sgo = mean( sueldo_sgo, na.rm=TRUE ) ) %>%
  ungroup( )

#Guardar en un Rdata--------------------------------------------------------------------------------

message( '\tGuardando tablas' )

save( edu_comunitarios,
      tab_estados,
      tab_edad_sexo,
      tab_provincia,
      tab_rango_edad,
      tab_imp_tot_h,
      tab_imp_tot_m,
      tab_imp_edu_h,
      tab_imp_edu_m,
      tab_imp_sgo_h,
      tab_imp_sgo_m,
      tab_sal_sgo_edad_sexo,
      tab_sal_edu_edad_sexo,
      tab_masa,
      tab_edu_anio,
      tab_sin_registro,
      file = paste0( parametros$RData_seg, 'IESS_ECO_tablas_estadisticas.RData' ) )

#Borrar elementos restantes------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()