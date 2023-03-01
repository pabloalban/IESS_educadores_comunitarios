message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de las inversiones de desempleo y cesantía' )
#Archivos en texto a subir--------------------------------------------------------------------------
file_inv_des <- paste0( parametros$Data_seg, 'IESS_DES_inversiones.txt' )
file_gastos_ces <- paste0( parametros$Data_seg, 'IESS_CES_gastos_operativos.txt' )
file_ingresos_ces <- paste0( parametros$Data_seg, 'IESS_CES_ingresos.txt' )
file_renta_variable_ces <- paste0( parametros$Data_seg, 'IESS_CES_renta_variable.txt' )
file_renta_fija_privado_ces <- paste0( parametros$Data_seg, 'IESS_sector_privado_renta_fija.txt' )
file_renta_fija_publico_ces <- paste0( parametros$Data_seg, 'IESS_sector_publico_renta_fija.txt' )
file_inv_total_ces <- paste0( parametros$Data_seg, 'IESS_CES_inversiones_total.txt' )
file_rendimiento_ces <- paste0( parametros$Data_seg, 'IESS_CES_rendimiento.txt' )
file_creditos_ces <- paste0( parametros$Data_seg, 'IESS_CES_inversiones_creditos.txt' )
file_instrumento_ces <- paste0( parametros$Data_seg, 'IESS_inversiones_instrumento.txt' )


#Inversiones de desempleo---------------------------------------------------------------------------


inv_des <- (read.table(file_inv_des,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                              stringsAsFactors = FALSE)) %>% clean_names()

#Inversiones de cesantía----------------------------------------------------------------------------

inv_gastos_ces <- (read.table(file_gastos_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                       stringsAsFactors = FALSE)) %>% clean_names()

inv_ingresos_ces <- (read.table(file_ingresos_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                              stringsAsFactors = FALSE)) %>% clean_names()

inv_renta_variable_ces <- (read.table(file_renta_variable_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                stringsAsFactors = FALSE)) %>% clean_names()

inv_renta_fija_privado_ces <- (read.table(file_renta_fija_privado_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                      stringsAsFactors = FALSE)) %>% clean_names()

inv_renta_fija_publico_ces <- (read.table(file_renta_fija_publico_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                          stringsAsFactors = FALSE)) %>% clean_names()

inv_total_ces <- (read.table(file_inv_total_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                          stringsAsFactors = FALSE)) %>% clean_names()

inv_rendimiento_ces <- (read.table(file_rendimiento_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                             stringsAsFactors = FALSE)) %>% clean_names()

inv_creditos_ces <- (read.table(file_creditos_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                   stringsAsFactors = FALSE)) %>% clean_names()

inv_instrumento_ces <- (read.table(file_instrumento_ces,skip=0,dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                stringsAsFactors = FALSE)) %>% clean_names() %>% filter(fondo=='CESANTIA')


#Guardar resultados en un Rdata---------------------------------------------
message( '\tGuardando inversiones de desempleo y cesantía' )
save( inv_des,
      inv_gastos_ces,
      inv_ingresos_ces,
      inv_renta_variable_ces,
      inv_renta_fija_privado_ces,
      inv_renta_fija_publico_ces,
      inv_total_ces,
      inv_rendimiento_ces,
      inv_creditos_ces,
      inv_instrumento_ces, file = paste0( parametros$RData_seg, 'IESS_DES_inversiones.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()