message( paste( rep('-', 100 ), collapse = '' ) )

message( '\tAnálisis financiero  demográfico de la población cubierta ' )

# Porcentaje pagado por tipo de pago ---------------------------------------------------------------
col_nom <- c( 'anio', 'pagoP', 'pagoT', 'proceso', 'total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_demografico.xlsx' )

tipo_pago<-read_excel( file, sheet="tipo_pago", col_names = TRUE, guess_max = 24000 )
tipo_pago <- as.data.table( tipo_pago )
setnames( tipo_pago, col_nom )

# Número de beneficiarios --------------------------------------------------------------------------
col_nom <- c( 'anio', 'numero', 'tasa')
col_tip <- c( 'character', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_demografico.xlsx' )

num_benef<-read_excel( file, sheet = "beneficiarios", col_names = TRUE, guess_max = 24000 )
num_benef <- as.data.table( num_benef )
setnames( num_benef, col_nom )

# Montos pagados por año y por número de pagos -----------------------------------------------------
col_nom <- c( 'anio', 'pago1', 'pago2', 'pago3', 'pago4', 'pago5', 'total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_demografico.xlsx' )

monto_pag_tipo <-read_excel( file, sheet = "monto_pag_tipo", col_names = TRUE, guess_max = 24000 )
monto_pag_tipo  <- as.data.table( monto_pag_tipo  )
setnames( monto_pag_tipo , col_nom )

# Montos pagados por año y por número de pagos -----------------------------------------------------
col_nom <- c( 'anio', 'pago1', 'pago2', 'pago3', 'pago4', 'pago5', 'total')
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_demografico.xlsx' )

num_ben_tipo <-read_excel( file, sheet = "num_benef_tipo_pago", col_names = TRUE, 
                           guess_max = 24000 )
num_ben_tipo  <- as.data.table( num_ben_tipo  )
setnames( num_ben_tipo , col_nom )

# Valores pagados por rango, edad y sexo -----------------------------------------------------------
col_nom <- c( 'rango', 'H1', 'M1', 'total1', 'H2', 'M2', 'total2', 'H3', 'M3', 'total3' )
col_tip <- c( 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'
              , 'numeric', 'numeric', 'numeric', 'numeric' )

file <- paste0( parametros$Data_seg, 'IESS_DES_analisis_demografico.xlsx' )

tot_pag_rango_edad_sexo <-read_excel( file, sheet = "tot_pag_rango_edad_genero", col_names = TRUE, 
                                      guess_max = 24000 )
tot_pag_rango_edad_sexo  <- as.data.table( tot_pag_rango_edad_sexo  )
setnames( tot_pag_rango_edad_sexo , col_nom )

# Guardando datos ----------------------------------------------------------------------------------
message( '\tGuardando análisis financiero  demográfico de la población cubierta' )

save( tipo_pago, num_benef, monto_pag_tipo, num_ben_tipo, tot_pag_rango_edad_sexo, 
      file = paste0( parametros$RData_seg, 'IESS_DES_analisis_demografico.RData' ) )

message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% 'parametros' ) ]  )
gc()
