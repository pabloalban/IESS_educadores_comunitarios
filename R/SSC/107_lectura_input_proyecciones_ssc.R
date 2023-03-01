message( paste( rep('-', 100 ), collapse = '' ) )

# Carga de datos
file <- paste0( parametros$Data_seg, 'IESS_SSC_masa_crd.xlsx' )

# Lectura de Masa Salarial de afiliados del SGO ------------------- --------------------------------
message( '\tLectura de Masa Salarial de afiliados del SGO ' )

col_nom <- c( 'sexo', 'edad','masa_sgo')

masa_sgo <-read_excel( file,
                           sheet="Masa_Salarial_2020",
                           col_names=TRUE,
                           guess_max = 24000 )
masa_sgo <- as.data.table( masa_sgo)
setnames(masa_sgo, col_nom )

# Lectura de Masa Salarial de afiliados bajo relación de dependencia -------------------------------
message( '\tLectura de Masa Salarial de afiliados bajo relación de dependencia' )
col_nom <- c( 'sexo', 'edad',  'n_afi','masa_dep')
masa_dep <-read_excel( file,
                       sheet="Dependientes_2020",
                       col_names=TRUE,
                       guess_max = 24000 )
masa_dep <- as.data.table( masa_dep )
setnames( masa_dep, col_nom )

# Lectura de Masa Salarial del SGO Modelo ILO/PENSIONS ---------------------------------------------
message( '\tLectura de Masa Salarial del SGO Modelo ILO/PENSIONS' )
ivm_out <- list.files( paste0(parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL'),
                       pattern = "*ILO_Actuarial.*.zip", recursive = TRUE)
unzip( zipfile = ivm_out, overwrite = TRUE, 
       exdir = paste0( parametros$Data, 'IVM/OUTPUT/ILO_OUT_ALL' ) )

lista <- c( 'masa_sgo','masa_dep')

save( list=lista,
      file = paste0( parametros$RData_seg, 'IESS_SSC_input_proyeccion_aportes.RData' ) )

# # Lectura de aporte ISSPOL -----------------------------------------------------
# message( '\tLectura de aporte ISSPOL' )
# file <- paste0( parametros$Data_seg, 'IESS_SSC_analisis_contexto_economico.xlsx' )
# 
# col_nom <- c( 'fecha', 'issfa','isspol')
# 
# apo_isspol <-read_excel( file,
#                        sheet = "Aporte_ISSFA_ISSPOL",
#                        col_names = TRUE,
#                        guess_max = 24000 )
# apo_isspol <- as.data.table( apo_isspol )
# setnames( apo_isspol, col_nom )
# apo_isspol <- apo_isspol[ , list( item = c(1:44), isspol ) ]
# 
# # Lectura de aporte ISSPOL -----------------------------------------------------
# message( '\tLectura de aporte Seguros Privados' )
# file <- paste0( parametros$Data_seg, 'IESS_SSC_analisis_contexto_economico.xlsx' )
# 
# col_nom <- c( 'anio', 'contribu','inc_anu', 'porcen')
# 
# apo_segup <-read_excel( file,
#                          sheet = "Aporte_Seguros_Privados",
#                          col_names = TRUE,
#                          guess_max = 24000 )
# 
# apo_segup <- as.data.table( apo_segup )
# setnames( apo_segup, col_nom )
# 
# #-------------------------------------------------------------------------------
# 
# lista <- c( 'apo_isspol', 'apo_segup' )
# 
# save( list = lista,
#       file = paste0( parametros$RData_seg, 'IESS_SSC_aporte_isspol.RData' ) )


message( paste( rep('-', 100 ), collapse = '' ) )
rm( list = ls()[ !( ls() %in% c( 'parametros' ) ) ] )
gc()
