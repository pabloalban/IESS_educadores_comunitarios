message( paste( rep('-', 100 ), collapse = '' ) )
message( '\tDireccionamiento de Outputs Modelo ILO PENSIONS para el SSC' )

if ( dir.exists( paste0( parametros$Data_seg, 'Ouputs/ILO_PENSIONS_out' ) ) == FALSE ) {
dir.create( paste0( parametros$Data_seg,'Ouputs/ILO_PENSIONS_out' ) )
}

setwd( parametros$ilo_out )
zip_out <- file.info( list.files( parametros$ilo_out,pattern = "*ILO_Actuarial.*.zip", recursive = TRUE), extra_cols = FALSE )
zip_out <- rownames( zip_out[ difftime(Sys.time(), zip_out[,"mtime"], units = "days") < 1, 1:4] )
unzip( zipfile = zip_out, overwrite = TRUE, 
       exdir = paste0( parametros$Data_seg,'Ouputs/ILO_PENSIONS_out' ) )

setwd( parametros$work_dir )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()