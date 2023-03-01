message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tGenerando tablas de la densidad de cotización' )

# Cargando datos -----------------------------------------------------------------------------------
load( paste0( parametros$RData_seg, 'IESS_CES_DES_tasa_den_cot_edad_sexo_int.RData' ) )


#Tabla de la densidad de cotización-----------------------------------------------------------------
aux<- densidad_cotizacion_int
aux_den_cot_int <- reshape2::dcast(aux, edad ~ genero, value.var = c("den_cot_int"))
colnames(aux_den_cot_int)<-c("edad","F_den_cot_int","M_den_cot_int")
aux_den_cot <- reshape2::dcast(aux, edad ~ genero, value.var = c("den_cot"))
colnames(aux_den_cot)<-c("edad","F_den_cot","M_den_cot")
aux<-left_join(aux_den_cot,aux_den_cot_int, by='edad') %>%
      select(edad,F_den_cot,F_den_cot_int,M_den_cot,M_den_cot_int)

xtb_aux <- xtable( aux, digits = c( 0, 0, 4, 4, 4, 4) )

print( xtb_aux,
       file = paste0( parametros$resultado_tablas, 'iess_den_aport_alisada_des' , '.tex' ),
       type = 'latex', 
       include.colnames = FALSE, include.rownames = FALSE, 
       format.args = list( decimal.mark = ',', big.mark = '.' ), 
       only.contents = TRUE, 
       hline.after = NULL)

#Borrando los dataframes---------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()