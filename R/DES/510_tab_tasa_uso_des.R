message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de la tasa de uso del seguro de desempleo' )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_DES_tasa_uso_edad_sexo_int.RData' ) )

#Tabla de la tasa de uso del seguro de desempleo----------------------------------------------------

aux<- tasa_siniestralidad
aux <- reshape2::dcast(aux, edad + sd_genero ~ sd_numero_pagos, value.var = c("tasa_uso_int"))
aux_m<- aux %>% filter(sd_genero=='M') %>% select( edad,
                                                   M_1=`1`,
                                                   M_2=`2`,
                                                   M_3=`3`,
                                                   M_4=`4`,
                                                   M_5=`5`)

aux_f<- aux %>% filter(sd_genero=='F') %>% select( edad,
                                                   F_1=`1`,
                                                   F_2=`2`,
                                                   F_3=`3`,
                                                   F_4=`4`,
                                                   F_5=`5`) 
aux<-left_join(aux_f,aux_m,by='edad')

xtb_aux <- xtable( aux, digits = c( 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tasa_uso' , '.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL)

#Tabla de la tasa de uso del seguro de desempleo con pagos indebidos--------------------------------
aux<- tasa_siniestralidad_pagos_ind
aux <- reshape2::dcast(aux, edad + sd_genero ~ sd_numero_pagos, value.var = c("tasa_uso_int"))
aux_m<- aux %>% filter(sd_genero=='M') %>% select( edad,
                                                   M_1=`1`,
                                                   M_2=`2`,
                                                   M_3=`3`,
                                                   M_4=`4`,
                                                   M_5=`5`)

aux_f<- aux %>% filter(sd_genero=='F') %>% select( edad,
                                                   F_1=`1`,
                                                   F_2=`2`,
                                                   F_3=`3`,
                                                   F_4=`4`,
                                                   F_5=`5`) 
aux<-left_join(aux_f,aux_m,by='edad')

xtb_aux <- xtable( aux, digits = c( 0, 0, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_tasa_uso_ind' , '.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL)

#Borrando los dataframes---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()