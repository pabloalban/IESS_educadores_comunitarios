message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLectura de los salarios promedio de los cotizantes a cesantía y desempleo' )

#Cotizantes al SGO por año-------------------------
file_salarios <- paste0( parametros$Data_seg, 'IESS_CES_DES_cotizantes_salarios.txt' )
salarios_cotizantes<-read.table(file_salarios, dec = ".",header = TRUE,sep = "\t",na.strings = "NA",
                                 stringsAsFactors = FALSE,
                                 colClasses = c("integer",
                                                "integer",
                                                "character",
                                                "numeric"))
#Guardar resultados en un Rdata---------------------------------------------
message( '\tGuardando alarios promedio de los cotizantes a cesantía y desempleo' )

save( salarios_cotizantes,
      file = paste0( parametros$RData_seg, 'IESS_CES_DES_cotizantes_salarios.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()

