message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tConvirtiendo pdf a png para anexos' )

# Convirtiendo balances financieros de pdf a png ---------------------------------------------------
message( '\tConvirtiendo balances financieros de pdf a png' )

#2020 ----------------------------------------------------------------------------------------------
message( '\t\t2020' )

file <- paste0( parametros$resultado_seguro, 'IESS_SSC_balances_financieros_2020_1.pdf' )

dat <- pdf_convert( file, format = "png", pages = NULL, filenames = NULL, dpi = 90, antialias = TRUE, opw = "",
                    upw = "", verbose = TRUE )

n_ima <- NULL
for (i in 1:length(dat)) { # i <- 1
  n_ima <- image_read( dat[ i ] )
  n_ima <- n_ima%>%image_crop("370x850+10+125")
  image_write( n_ima, paste0( parametros$resultado_graficos, 'IESS_SSC_balances_financieros_2020_1_', i, parametros$graf_ext ))
  if(i==3){# i <- 3
    n_ima <- image_read( dat[ i ] )
    n_ima <- n_ima%>%image_crop("370x610+0+125")
    image_write( n_ima, paste0( parametros$resultado_graficos, 'IESS_SSC_balances_financieros_2020_1_', i, parametros$graf_ext ))
  }
}

del <- paste0( parametros$work_dir, dat )
unlink( del, recursive = TRUE )

file <- paste0( parametros$resultado_seguro, 'IESS_SSC_balances_financieros_2020_2.pdf' )

dat <- pdf_convert( file, format = "png", pages = NULL, filenames = NULL, dpi = 90, antialias = TRUE, opw = "",
                    upw = "", verbose = TRUE )

n_ima <- NULL
for (i in 1:length(dat)) { # i <- 1
  n_ima <- image_read( dat[ i ] )
  n_ima <- n_ima%>% image_crop("400x870+0+110")   #derechaxabajo izquierda+arriba
  image_write( n_ima, paste0( parametros$resultado_graficos, 'IESS_SSC_balances_financieros_2020_2_', i, parametros$graf_ext ))
  
  if(i==3){# i <- 3
    n_ima <- image_read( dat[ i ] )
    n_ima <- n_ima%>%image_crop("400x365+0+125")
    image_write( n_ima, paste0( parametros$resultado_graficos, 'IESS_SSC_balances_financieros_2020_2_', i, parametros$graf_ext ))
  }
  
}

del <- paste0( parametros$work_dir, dat )
unlink( del, recursive = TRUE )

# Convirtiendo script de pdf a png ---------------------------------------------------
message( '\tConvirtiendo script de pdf a png' )

file <- paste0( parametros$resultado_seguro, 'IESS_SSC_estudio_actuarial_script.pdf' )

dat <- pdf_convert( file, format = "png", pages = NULL, filenames = NULL, dpi = 100, antialias = TRUE, opw = "",
                    upw = "", verbose = TRUE )

n_ima <- NULL

if( length( dat ) == 1 ){
  eval(call("<-", as.name( paste0( 'imag_', 1 ) ), image_read( dat[ 1 ] ) ) )
} else{
  for( i in 1:length( dat ) ){ # i <- 1
    if( i==1){
      n_ima <- eval(call("<-", as.name( paste0( 'imag_', i ) ), image_read( dat[ i ] ) ) )
    } else {
      n_ima <- c( n_ima, eval(call("<-", as.name( paste0( 'imag_', i ) ), image_read( dat[ i ] ) ) ) )
    }
  }
}

image_write( n_ima, format = "pdf", paste0( parametros$resultado_graficos,
                                            "IESS_SSC_estudio_actuarial_script_png.pdf"))

del <- paste0( parametros$work_dir, dat )
unlink( del, recursive = TRUE )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()