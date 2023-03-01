message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tLeyendo analisis demografico SSC del IESS' )

# Carga de datos
file <- paste0( parametros$Data_seg, 'IESS_SSC_inputs_actas_ssc.xlsx' )

#Selección del acta
i <- 1
message( '\tLeyendo actas del SSC' )

acta <-read_excel( file,
                   sheet=paste0("acta ", i ),
                   col_names=TRUE,
                   guess_max = 24000 )
acta <- as.data.table( acta )
tema <- acta[ 1, 1] 
fecha <- acta[ 1, 3] 
lugar <- acta[ 1, 5]
hini <- acta[ 1, 7]
hfin <- acta[ 1, 8]

parti <- acta[ , c('Nombres', 'Dependencia','Cargo', 'Correo')]

agenda <- acta[ , c('Nro...15', 'Agenda')]

resumen <- acta[ , c('Nro...18', 'Resumen')]

compromisos <- acta[ , c('Nro...21', 'Compromisos', 'Responsables', 'Fecha_Tentativa')]

lista <- c('tema', 'fecha', 'lugar', 'hini', 'hfin', 'parti', 'agenda', 'resumen', 'compromisos')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_inputs_actas_ssc.RData' ) )

###########################################################################
message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()

