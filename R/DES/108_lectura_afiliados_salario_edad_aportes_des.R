# Lectura del número de afiliados y salario promedio por edad y aportaciones------------------------

message( '\tnúmero de afiliados de desempleo y salario promedio por edad y aportaciones' )
file <- paste0( parametros$Data_seg, 'IESS_DES_CES_afiliados_tiempo_aportacion.csv' )
message( paste( rep('-', 100 ), collapse = '' ) )

tabla <- read.table( file, dec = ".", header = FALSE, sep = ";", na.strings = "NA" )

message( '\tGuardando tiempo de aportación de afilidos' )
save( tabla , file = paste0( parametros$RData_seg, 'IESS_DES_CES_afi_tiempo_aportacion.RData' ) )

# Limpieza -----------------------------------------------------------------------------------------
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
