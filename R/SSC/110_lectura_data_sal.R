message( paste( rep('-', 100 ), collapse = '' ) )

# Número de threads que utilizará data table para operar
setDTthreads( threads = 8 )

# Nota ---------------------------------------------------------------------------------------------
# Esta carga consume mucha memoria RAM, no puede ser realizada en una computadora con poca RAM

file <- paste0( parametros$Data_seg, 'Afiliados.txt' )
afi_sal <- fread( file, header = TRUE, sep = '|', showProgress = TRUE )
afi_sal <- data.table( afi_sal )
save( afi_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_afiliados.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )

rm( afi_sal )
gc()

file <- paste0( parametros$Data_seg, 'pensionistas.txt' )
pen_sal <- fread( file, header = TRUE, sep = '|', showProgress = TRUE )
pen_sal <- data.table( pen_sal )
save( pen_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_pensionistas.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )

file <- paste0( parametros$Data_seg, 'hijos_18.txt' )
hijos_sal <- fread( file, header = TRUE, sep = ';', showProgress = TRUE )
hijos_sal <- data.table( hijos_sal )
save( hijos_sal, file = paste0( parametros$RData_seg, 'IESS_SAL_hijos.RData' ),
      ascii = FALSE, compress = 'bzip2', compression_level = 9 )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
